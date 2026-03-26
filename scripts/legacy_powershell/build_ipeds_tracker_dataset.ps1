param(
    [int]$StartYear = 2014,
    [int]$EndYear = 2024,
    [string]$OutputStem = "ipeds_financial_health"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$downloadRoot = Join-Path $root "downloads"
$dataRoot = Join-Path $downloadRoot "data"
$dictRoot = Join-Path $downloadRoot "dict"
$extractRoot = Join-Path $downloadRoot "extracted"
$catalogHtml = Join-Path $root "ipeds_datafiles.html"
$catalogCsv = Join-Path $root "${OutputStem}_selected_file_catalog.csv"
$datasetCsv = Join-Path $root "${OutputStem}_raw_${StartYear}_${EndYear}.csv"

foreach ($path in @($downloadRoot, $dataRoot, $dictRoot, $extractRoot)) {
    if (-not (Test-Path $path)) {
        New-Item -ItemType Directory -Path $path | Out-Null
    }
}

function Invoke-Download {
    param(
        [Parameter(Mandatory = $true)][string]$Url,
        [Parameter(Mandatory = $true)][string]$OutFile
    )

    if (Test-Path $OutFile) {
        return
    }

    & curl.exe -L $Url -o $OutFile | Out-Null
}

function Expand-IfZip {
    param(
        [Parameter(Mandatory = $true)][string]$ArchivePath,
        [Parameter(Mandatory = $true)][string]$DestinationPath
    )

    if (Test-Path $DestinationPath) {
        return
    }

    New-Item -ItemType Directory -Path $DestinationPath -Force | Out-Null
    Expand-Archive -Path $ArchivePath -DestinationPath $DestinationPath -Force
}

function Expand-NestedOfficePackage {
    param(
        [Parameter(Mandatory = $true)][string]$ArchivePath,
        [Parameter(Mandatory = $true)][string]$DestinationPath
    )

    $workbookPath = Join-Path $DestinationPath "xl\workbook.xml"
    if ((Test-Path $DestinationPath) -and (Test-Path $workbookPath)) {
        return
    }

    if (Test-Path $DestinationPath) {
        Remove-Item -Path $DestinationPath -Recurse -Force
    }

    $outer = Join-Path $DestinationPath "__outer"
    New-Item -ItemType Directory -Path $DestinationPath -Force | Out-Null
    Expand-IfZip -ArchivePath $ArchivePath -DestinationPath $outer

    $inner = Get-ChildItem -Path $outer -File | Select-Object -First 1
    if (-not $inner) {
        throw "No inner Office file found in $ArchivePath"
    }

    $innerZip = Join-Path $DestinationPath ($inner.BaseName + ".zip")
    Copy-Item -Path $inner.FullName -Destination $innerZip -Force
    Expand-Archive -Path $innerZip -DestinationPath $DestinationPath -Force
}

function ConvertFrom-HtmlText {
    param([AllowNull()][string]$Text)

    if ($null -eq $Text) {
        return $null
    }

    return [System.Net.WebUtility]::HtmlDecode(
        ($Text -replace "<br\s*/?>", " " -replace "<[^>]+>", "" -replace "\s+", " ").Trim()
    )
}

function Get-CatalogRows {
    param([Parameter(Mandatory = $true)][string]$HtmlPath)

    $html = Get-Content -Path $HtmlPath -Raw
    $regex = [regex]'<tr class="idc_gridviewrow">\s*<td[^>]*>(?<year>\d{4})</td><td[^>]*>(?<survey>.*?)</td><td[^>]*>(?<description>.*?)</td><td[^>]*><a href="(?<data>[^"]+)">(?<table>[^<]+)</a></td><td[^>]*><a href="(?<stata>[^"]+)">(?<stataName>[^<]+)</a></td><td[^>]*>.*?</td><td[^>]*><a href="(?<dict>[^"]+)">Dictionary</a></td>'
    $matches = $regex.Matches($html)
    $pageUrl = "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=5588c647-c6be-4540-b2d1-283f6c31aee7&rtid=1"
    $baseUri = [System.Uri]$pageUrl

    $rows = foreach ($match in $matches) {
        $dataHref = [System.Net.WebUtility]::HtmlDecode($match.Groups["data"].Value)
        $dictHref = [System.Net.WebUtility]::HtmlDecode($match.Groups["dict"].Value)
        [pscustomobject]@{
            year = [int]$match.Groups["year"].Value
            survey = ConvertFrom-HtmlText $match.Groups["survey"].Value
            description = ConvertFrom-HtmlText $match.Groups["description"].Value
            table_name = ConvertFrom-HtmlText $match.Groups["table"].Value
            data_url = ([System.Uri]::new($baseUri, $dataHref)).AbsoluteUri
            dictionary_url = ([System.Uri]::new($baseUri, $dictHref)).AbsoluteUri
        }
    }

    return $rows
}

function Get-XlsxSharedStrings {
    param([Parameter(Mandatory = $true)][string]$ExpandedFolder)

    $sharedPath = Join-Path $ExpandedFolder "xl\sharedStrings.xml"
    if (-not (Test-Path $sharedPath)) {
        return @()
    }

    [xml]$sharedXml = Get-Content -Path $sharedPath -Raw
    $ns = New-Object System.Xml.XmlNamespaceManager($sharedXml.NameTable)
    $ns.AddNamespace("x", "http://schemas.openxmlformats.org/spreadsheetml/2006/main")

    $items = @()
    foreach ($si in $sharedXml.SelectNodes("//x:si", $ns)) {
        $texts = $si.SelectNodes(".//x:t", $ns) | ForEach-Object { $_.InnerText }
        $items += (($texts -join "") -replace "_x000D_", " " -replace "_x0009_", " ").Trim()
    }
    return $items
}

function Get-XlsxSheetRows {
    param(
        [Parameter(Mandatory = $true)][string]$ExpandedFolder,
        [Parameter(Mandatory = $true)][string]$SheetName
    )

    [xml]$workbookXml = Get-Content -Path (Join-Path $ExpandedFolder "xl\workbook.xml") -Raw
    [xml]$relsXml = Get-Content -Path (Join-Path $ExpandedFolder "xl\_rels\workbook.xml.rels") -Raw
    $wbNs = New-Object System.Xml.XmlNamespaceManager($workbookXml.NameTable)
    $wbNs.AddNamespace("x", "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
    $wbNs.AddNamespace("r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
    $relNs = New-Object System.Xml.XmlNamespaceManager($relsXml.NameTable)
    $relNs.AddNamespace("r", "http://schemas.openxmlformats.org/package/2006/relationships")

    $sheet = $workbookXml.SelectNodes("//x:sheet", $wbNs) | Where-Object { [string]$_.name -ieq $SheetName } | Select-Object -First 1
    if (-not $sheet) {
        throw "Sheet $SheetName not found"
    }

    $relationshipId = [string]$sheet.Attributes["r:id"].Value
    $relationship = $relsXml.SelectNodes("//r:Relationship", $relNs) | Where-Object { [string]$_.Id -eq $relationshipId } | Select-Object -First 1
    if (-not $relationship) {
        throw "Relationship $relationshipId not found for sheet $SheetName"
    }

    $target = [string]$relationship.Target
    $sheetPath = Join-Path $ExpandedFolder ("xl\" + ($target -replace '/', '\'))
    [xml]$sheetXml = Get-Content -Path $sheetPath -Raw
    $sheetNs = New-Object System.Xml.XmlNamespaceManager($sheetXml.NameTable)
    $sheetNs.AddNamespace("x", "http://schemas.openxmlformats.org/spreadsheetml/2006/main")

    $shared = Get-XlsxSharedStrings -ExpandedFolder $ExpandedFolder
    $rows = @()

    foreach ($row in $sheetXml.SelectNodes("//x:sheetData/x:row", $sheetNs)) {
        $obj = [ordered]@{}
        foreach ($cell in $row.SelectNodes("./x:c", $sheetNs)) {
            $ref = [string]$cell.r
            $column = ($ref -replace '\d', '')
            $valueNode = $cell.SelectSingleNode("./x:v", $sheetNs)
            $value = if ($valueNode) { [string]$valueNode.InnerText } else { "" }
            $cellType = if ($cell.Attributes["t"]) { [string]$cell.Attributes["t"].Value } else { "" }
            if ($cellType -eq "s" -and $value -ne "") {
                $value = $shared[[int]$value]
            }
            $obj[$column] = $value
        }
        $rows += [pscustomobject]$obj
    }

    return $rows
}

function Get-Varlist {
    param(
        [Parameter(Mandatory = $true)][string]$DictionaryArchive,
        [Parameter(Mandatory = $true)][string]$TableName
    )

    $expanded = Join-Path $extractRoot ("dict_" + $TableName)
    Expand-NestedOfficePackage -ArchivePath $DictionaryArchive -DestinationPath $expanded
    try {
        $rows = Get-XlsxSheetRows -ExpandedFolder $expanded -SheetName "Varlist"
    }
    catch {
        $rows = Get-XlsxSheetRows -ExpandedFolder $expanded -SheetName "varlist"
    }

    return $rows | Where-Object {
        ($_.PSObject.Properties.Name -contains "A") -and
        ($_.PSObject.Properties.Name -contains "B") -and
        $_.A -match '^\d+$' -and
        $_.B
    } | ForEach-Object {
        [pscustomobject]@{
            var_number = $_.A
            var_name = $_.B
            data_type = if ($_.PSObject.Properties.Name -contains "C") { $_.C } else { $null }
            field_width = if ($_.PSObject.Properties.Name -contains "D") { $_.D } else { $null }
            format = if ($_.PSObject.Properties.Name -contains "E") { $_.E } else { $null }
            imputation_var = if ($_.PSObject.Properties.Name -contains "F") { $_.F } else { $null }
            var_title = if ($_.PSObject.Properties.Name -contains "G") { $_.G } else { $null }
        }
    }
}

function Find-VarName {
    param(
        [Parameter(Mandatory = $true)]$Varlist,
        [Parameter(Mandatory = $true)][string[]]$Patterns
    )

    foreach ($pattern in $Patterns) {
        $match = $Varlist | Where-Object { $_.var_title -match $pattern -or $_.var_name -match $pattern } | Select-Object -First 1
        if ($match) {
            return $match.var_name
        }
    }

    return $null
}

function Get-String {
    param($Row, [string]$FieldName)

    if ($null -eq $Row) {
        return $null
    }

    if (-not $FieldName) {
        return $null
    }

    if ($Row.PSObject.Properties.Name -contains $FieldName) {
        $value = $Row.$FieldName
        if ($null -eq $value -or $value -eq "") {
            return $null
        }
        return [string]$value
    }

    return $null
}

function Get-Number {
    param($Row, [string]$FieldName)

    $raw = Get-String -Row $Row -FieldName $FieldName
    if ([string]::IsNullOrWhiteSpace($raw)) {
        return $null
    }

    $clean = ($raw -replace ',', '').Trim()
    $number = 0.0
    if ([double]::TryParse($clean, [ref]$number)) {
        return $number
    }

    return $null
}

function First-NonNull {
    param([object[]]$Values)

    foreach ($value in $Values) {
        if ($null -ne $value -and $value -ne "") {
            return $value
        }
    }
    return $null
}

if (-not (Test-Path $catalogHtml)) {
    Invoke-Download -Url "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=5588c647-c6be-4540-b2d1-283f6c31aee7&rtid=1" -OutFile $catalogHtml
}

$targetTableRegex = @(
    '^HD\d{4}$',
    '^IC\d{4}$',
    '^EFFY\d{4}$',
    '^EFIA\d{4}$',
    '^FLAGS\d{4}$',
    '^COST1_\d{4}$',
    '^SAL\d{4}_IS$',
    '^SAL\d{4}_NIS$',
    '^EAP\d{4}$',
    '^F\d{4}_F1A$',
    '^F\d{4}_F2$',
    '^F\d{4}_F3$',
    '^DRVCOST\d{4}$',
    '^DRVEF\d{4}$',
    '^DRVEF12\d{4}$',
    '^DRVF\d{4}$',
    '^DRVHR\d{4}$',
    '^DRVADM\d{4}$',
    '^DRVOM\d{4}$',
    '^ADM\d{4}$',
    '^SFA\d{4}$'
)

$catalog = Get-CatalogRows -HtmlPath $catalogHtml |
    Where-Object {
        $row = $_
        $row.year -ge $StartYear -and $row.year -le $EndYear -and (
            $targetTableRegex | Where-Object { $row.table_name -match $_ }
        )
    } |
    Sort-Object year, table_name

$catalog | Export-Csv -Path $catalogCsv -NoTypeInformation

$catalogByYear = @{}
foreach ($item in $catalog) {
    if (-not $catalogByYear.ContainsKey($item.year)) {
        $catalogByYear[$item.year] = @{}
    }
    $catalogByYear[$item.year][$item.table_name] = $item
}

$fieldSpecs = @(
    @{ output = "institution_name"; table = "HD"; patterns = @("^Institution \(entity\) name$","^Institution name$") },
    @{ output = "city"; table = "HD"; patterns = @("^City location of institution$") },
    @{ output = "state"; table = "HD"; patterns = @("^State abbreviation$") },
    @{ output = "zip"; table = "HD"; patterns = @("^ZIP code$","^Zip code$") },
    @{ output = "county"; table = "HD"; patterns = @("^County name$") },
    @{ output = "longitude"; table = "HD"; patterns = @("^Longitude location of institution$") },
    @{ output = "latitude"; table = "HD"; patterns = @("^Latitude location of institution$") },
    @{ output = "status"; table = "HD"; patterns = @("^Status of institution$") },
    @{ output = "date_closed"; table = "HD"; patterns = @("^Date institution closed$") },
    @{ output = "is_active"; table = "HD"; patterns = @("^Institution is active in current year$") },
    @{ output = "multi_institution_campus_org"; table = "HD"; patterns = @("^Multi-institution or multi-campus organization$") },
    @{ output = "id_number_multi"; table = "HD"; patterns = @("^Identification number of multi-institution or multi-campus organization$") },
    @{ output = "name_multi"; table = "HD"; patterns = @("^Name of multi-institution or multi-campus organization$") },
    @{ output = "opeid"; table = "HD"; patterns = @("^Office of Postsecondary Education \(OPE\) ID Number$") },
    @{ output = "region"; table = "HD"; patterns = @("Bureau of Economic Analysis \(BEA\) regions") },
    @{ output = "hbcu"; table = "HD"; patterns = @("Historically Black College or University") },
    @{ output = "tribal_college"; table = "HD"; patterns = @("^Tribal college$") },
    @{ output = "sector"; table = "HD"; patterns = @("^Sector of institution$") },
    @{ output = "level"; table = "HD"; patterns = @("^Level of institution$") },
    @{ output = "control"; table = "HD"; patterns = @("^Control of institution$") },
    @{ output = "degree_granting_status"; table = "HD"; patterns = @("^Degree-granting status$") },
    @{ output = "highest_degree"; table = "HD"; patterns = @("^Highest degree offered$") },
    @{ output = "category"; table = "HD"; patterns = @("^Institutional category$") },
    @{ output = "grad_offering"; table = "HD"; patterns = @("^Graduate offering$") },
    @{ output = "urbanization"; table = "HD"; patterns = @("Urban-centric locale") },
    @{ output = "access_earnings"; table = "HD"; patterns = @("Student Access and Earnings") },
    @{ output = "size"; table = "HD"; patterns = @("Institutional Size") },
    @{ output = "undergrad_program_mix"; table = "HD"; patterns = @("Undergraduate Academic Program Mix") },
    @{ output = "grad_program_mix"; table = "HD"; patterns = @("Graduate Academic Program Mix") },
    @{ output = "control_or_affiliation"; table = "IC"; patterns = @("^Institutional control or affiliation$") },
    @{ output = "religious_affiliation"; table = "IC"; patterns = @("^Religious affiliation$") },
    @{ output = "has_full_time_first_time_undergrad"; table = "IC"; patterns = @("Full time first-time degree/certificate-seeking undergraduate students enrolled") },
    @{ output = "reporting_model"; table = "FLAGS"; patterns = @("Identifies reporting standards GASB, FASB, or modified FASB") },
    @{ output = "pell_accounting_method_fasb"; table = "FLAGS"; patterns = @("^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \(FASB\s+institutions\)\?$","^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \(FASB institutions\)\?$") },
    @{ output = "pell_accounting_method_pfp"; table = "FLAGS"; patterns = @("^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \(private-for-profit institutions\)\?$") },
    @{ output = "admissions_yield"; table = "DRVADM"; patterns = @("^Admissions yield") },
    @{ output = "fte_12_months"; table = "DRVEF12"; patterns = @("^12-month full-time equivalent enrollment$","^12-month full-time equivalent enrollment \(FTE\)$") },
    @{ output = "fte_undergrad"; table = "EFIA"; patterns = @("^Reported full-time equivalent \(FTE\) undergraduate enrollment","^Reported FTE undergraduate enrollment$") },
    @{ output = "fte_graduate"; table = "EFIA"; patterns = @("^Reported full-time equivalent \(FTE\) graduate enrollment","^Reported FTE graduate enrollment$") },
    @{ output = "fte_total_staff"; table = "DRVHR"; patterns = @("^Total full-time-equivalent staff$","^Total FTE staff$") },
    @{ output = "fte_instructional"; table = "DRVHR"; patterns = @("^Instructional staff, full-time-equivalent$","^Instructional FTE$") },
    @{ output = "state_approps_percent_core_gasb"; table = "DRVF"; patterns = @("State appropriations as percent of core revenues.*GASB") },
    @{ output = "gov_grants_fasb"; table = "DRVF"; patterns = @("Government grants and contracts as a percent of core revenues.*FASB") },
    @{ output = "state_revenue_fte_fasb"; table = "DRVF"; patterns = @("Revenues from state appropriations per FTE.*FASB") },
    @{ output = "gov_revenue_fte_fasb"; table = "DRVF"; patterns = @("Revenues from government grants and contracts per FTE.*FASB") },
    @{ output = "endowment_assets_per_fte_gasb"; table = "DRVF"; patterns = @("Endowment assets \(year end\) per FTE enrollment.*GASB") },
    @{ output = "endowment_assets_per_fte_fasb"; table = "DRVF"; patterns = @("Endowment assets \(year end\) per FTE enrollment.*FASB") },
    @{ output = "tuition_fees_after_discounts_allowances_gasb"; table = "F1A"; patterns = @("^Tuition and fees, after deducting discounts and allowances$") },
    @{ output = "federal_operating_grants_contracts_gasb"; table = "F1A"; patterns = @("^Federal operating grants and contracts$") },
    @{ output = "state_appropriations_gasb"; table = "F1A"; patterns = @("^State appropriations$") },
    @{ output = "total_operating_nonoperating_revenues_gasb"; table = "F1A"; patterns = @("^Total operating and nonoperating revenues$") },
    @{ output = "discounts_allowances_applied_tuition_fees_gasb"; table = "F1A"; patterns = @("Discounts and allowances applied to tuition and fees") },
    @{ output = "total_expenses_deductions_current_total_gasb"; table = "F1A"; patterns = @("^Total expenses and deductions.? Current year total$","^Total expenses and deductions - Current year total$") },
    @{ output = "total_assets_gasb"; table = "F1A"; patterns = @("^Total assets") },
    @{ output = "total_liabilities_gasb"; table = "F1A"; patterns = @("^Total liabilities") },
    @{ output = "unrestricted_public"; table = "F1A"; patterns = @("^Unrestricted$","^Unrestricted net position$") },
    @{ output = "value_endowment_assets_beginning_gasb"; table = "F1A"; patterns = @("Value of endowment assets at beginning of fiscal year","Value of endowment assets at the beginning of the fiscal year") },
    @{ output = "value_endowment_assets_end_gasb"; table = "F1A"; patterns = @("Value of endowment assets at end of fiscal year","Value of endowment assets at the end of the fiscal year") },
    @{ output = "new_gifts_additions_gasb"; table = "F1A"; patterns = @("New gifts and additions") },
    @{ output = "endowment_net_investment_return_gasb"; table = "F1A"; patterns = @("Endowment net investment return") },
    @{ output = "endowment_spending_distribution_current_use_gasb"; table = "F1A"; patterns = @("Spending distribution for current use") },
    @{ output = "total_assets_fasb"; table = "F2"; patterns = @("^Total assets$") },
    @{ output = "total_liabilities_fasb"; table = "F2"; patterns = @("^Total liabilities$") },
    @{ output = "total_unrestricted_net_assets_fasb"; table = "F2"; patterns = @("^Total unrestricted net assets$") },
    @{ output = "institutional_grants_funded_fasb"; table = "F2"; patterns = @("^Institutional grants \(funded\)$") },
    @{ output = "institutional_grants_unfunded_fasb"; table = "F2"; patterns = @("^Institutional grants \(unfunded\)$") },
    @{ output = "allowances_applied_to_tuition_fasb"; table = "F2"; patterns = @("^Allowances applied to tuition and fees$") },
    @{ output = "pell_grants"; table = "F2"; patterns = @("^Pell grants$") },
    @{ output = "tuition_and_fees_fasb"; table = "F2"; patterns = @("^Tuition and fees \(total\)$","^Tuition and fees - Total$") },
    @{ output = "state_approps_fasb"; table = "F2"; patterns = @("^State appropriations.? Total$","^State appropriations - Total$") },
    @{ output = "federal_grants_contracts_fasb"; table = "F2"; patterns = @("^Federal grants and contracts.? Total$","^Federal grants and contracts - Total$") },
    @{ output = "total_revenues_investment_return_fasb"; table = "F2"; patterns = @("^Total revenues and investment return.? Total$","^Total revenues and investment return - Total$") },
    @{ output = "total_expenses_fasb"; table = "F2"; patterns = @("^Total expenses.? Total amount$","^Total expenses - Total amount$") },
    @{ output = "unrestricted_operating_rev_fasb"; table = "F2"; patterns = @("^Total unrestricted operating revenues$") },
    @{ output = "value_endowment_beginning_fasb"; table = "F2"; patterns = @("Value of endowment assets at beginning of fiscal year","Value of endowment assets at the beginning of the fiscal year") },
    @{ output = "value_endowment_end_fasb"; table = "F2"; patterns = @("Value of endowment assets at end of fiscal year","Value of endowment assets at the end of the fiscal year") },
    @{ output = "new_endowment_gifts_fasb"; table = "F2"; patterns = @("New gifts and additions") },
    @{ output = "endowment_net_investment_return_fasb"; table = "F2"; patterns = @("Endowment net investment return") },
    @{ output = "spending_distribution_for_current_use_fasb"; table = "F2"; patterns = @("Spending distribution for current use") },
    @{ output = "total_assets_pfp"; table = "F3"; patterns = @("^Total assets$") },
    @{ output = "total_liabilities_pfp"; table = "F3"; patterns = @("^Total liabilities$") },
    @{ output = "institutional_grants_pfp"; table = "F3"; patterns = @("^Institutional grants$") },
    @{ output = "discounts_allowances_applied_tuition_fees_pfp"; table = "F3"; patterns = @("Discounts and allowances applied to tuition and fees","Allowances applied to tuition and fees") },
    @{ output = "tuition_fees_pfp"; table = "F3"; patterns = @("^Tuition and fees$") },
    @{ output = "federal_grants_contracts_pfp"; table = "F3"; patterns = @("^Federal grants and contracts","^Federal appropriations, grants and contracts") },
    @{ output = "state_appropriations_pfp"; table = "F3"; patterns = @("^State appropriations","^State appropriations, grants and contracts") },
    @{ output = "total_revenues_investment_return_pfp"; table = "F3"; patterns = @("^Total revenues and investment return$") },
    @{ output = "total_revenues_pfp"; table = "F3"; patterns = @("^Total revenues$") },
    @{ output = "total_expenses_total_amount_pfp"; table = "F3"; patterns = @("^Total expenses.? Total amount$","^Total expenses-Total amount$") }
)

$allRows = New-Object System.Collections.Generic.List[object]

foreach ($year in ($StartYear..$EndYear)) {
    if (-not $catalogByYear.ContainsKey($year)) {
        continue
    }

    $yearCatalog = $catalogByYear[$year]
    $tableAliases = @{
        HD = ($yearCatalog.Keys | Where-Object { $_ -match '^HD\d{4}$' } | Select-Object -First 1)
        IC = ($yearCatalog.Keys | Where-Object { $_ -match '^IC\d{4}$' } | Select-Object -First 1)
        FLAGS = ($yearCatalog.Keys | Where-Object { $_ -match '^FLAGS\d{4}$' } | Select-Object -First 1)
        EFIA = ($yearCatalog.Keys | Where-Object { $_ -match '^EFIA\d{4}$' } | Select-Object -First 1)
        DRVEF12 = ($yearCatalog.Keys | Where-Object { $_ -match '^DRVEF12\d{4}$' } | Select-Object -First 1)
        DRVADM = ($yearCatalog.Keys | Where-Object { $_ -match '^DRVADM\d{4}$' } | Select-Object -First 1)
        DRVHR = ($yearCatalog.Keys | Where-Object { $_ -match '^DRVHR\d{4}$' } | Select-Object -First 1)
        DRVF = ($yearCatalog.Keys | Where-Object { $_ -match '^DRVF\d{4}$' } | Select-Object -First 1)
        F1A = ($yearCatalog.Keys | Where-Object { $_ -match '^F\d{4}_F1A$' } | Select-Object -First 1)
        F2 = ($yearCatalog.Keys | Where-Object { $_ -match '^F\d{4}_F2$' } | Select-Object -First 1)
        F3 = ($yearCatalog.Keys | Where-Object { $_ -match '^F\d{4}_F3$' } | Select-Object -First 1)
    }

    $dataTables = @{}
    $dictionaries = @{}
    $resolvedFields = @{}

    foreach ($alias in $tableAliases.Keys) {
        $tableName = $tableAliases[$alias]
        if (-not $tableName) {
            continue
        }

        $entry = $yearCatalog[$tableName]
        $dataArchive = Join-Path $dataRoot ($tableName + ".zip")
        $dataFolder = Join-Path $extractRoot ("data_" + $tableName)
        $dictArchive = Join-Path $dictRoot ($tableName + ".zip")

        Invoke-Download -Url $entry.data_url -OutFile $dataArchive
        Expand-IfZip -ArchivePath $dataArchive -DestinationPath $dataFolder
        Invoke-Download -Url $entry.dictionary_url -OutFile $dictArchive
        $varlist = Get-Varlist -DictionaryArchive $dictArchive -TableName $tableName
        $dictionaries[$alias] = $varlist

        $csvFile = Get-ChildItem -Path $dataFolder -Filter "*.csv" | Select-Object -First 1
        if ($csvFile) {
            $tableData = Import-Csv -Path $csvFile.FullName
            $lookup = @{}
            foreach ($row in $tableData) {
                $lookup[$row.UNITID] = $row
            }
            $dataTables[$alias] = $lookup
        }
    }

    foreach ($spec in $fieldSpecs) {
        $alias = [string]$spec.table
        if ($dictionaries.ContainsKey($alias)) {
            $resolvedFields[$spec.output] = Find-VarName -Varlist $dictionaries[$alias] -Patterns $spec.patterns
        }
    }

    if (-not $dataTables.ContainsKey("HD")) {
        continue
    }

    foreach ($unitid in $dataTables["HD"].Keys) {
        $hd = $dataTables["HD"][$unitid]
        $ic = if ($dataTables.ContainsKey("IC")) { $dataTables["IC"][$unitid] } else { $null }
        $flags = if ($dataTables.ContainsKey("FLAGS")) { $dataTables["FLAGS"][$unitid] } else { $null }
        $efia = if ($dataTables.ContainsKey("EFIA")) { $dataTables["EFIA"][$unitid] } else { $null }
        $drvef12 = if ($dataTables.ContainsKey("DRVEF12")) { $dataTables["DRVEF12"][$unitid] } else { $null }
        $drvadm = if ($dataTables.ContainsKey("DRVADM")) { $dataTables["DRVADM"][$unitid] } else { $null }
        $drvhr = if ($dataTables.ContainsKey("DRVHR")) { $dataTables["DRVHR"][$unitid] } else { $null }
        $drvf = if ($dataTables.ContainsKey("DRVF")) { $dataTables["DRVF"][$unitid] } else { $null }
        $f1 = if ($dataTables.ContainsKey("F1A")) { $dataTables["F1A"][$unitid] } else { $null }
        $f2 = if ($dataTables.ContainsKey("F2")) { $dataTables["F2"][$unitid] } else { $null }
        $f3 = if ($dataTables.ContainsKey("F3")) { $dataTables["F3"][$unitid] } else { $null }

        $institutionName = Get-String -Row $hd -FieldName $resolvedFields["institution_name"]
        $city = Get-String -Row $hd -FieldName $resolvedFields["city"]
        $state = Get-String -Row $hd -FieldName $resolvedFields["state"]

        $fteUndergrad = Get-Number -Row $efia -FieldName $resolvedFields["fte_undergrad"]
        $fteGraduate = Get-Number -Row $efia -FieldName $resolvedFields["fte_graduate"]
        $fte12Months = Get-Number -Row $drvef12 -FieldName $resolvedFields["fte_12_months"]
        if ($null -eq $fte12Months -and ($null -ne $fteUndergrad -or $null -ne $fteGraduate)) {
            $fte12Months = (First-NonNull @($fteUndergrad, 0)) + (First-NonNull @($fteGraduate, 0))
        }

        $valueEndowmentBeginningGasb = First-NonNull @(
            (Get-Number -Row $f1 -FieldName $resolvedFields["value_endowment_assets_beginning_gasb"]),
            (Get-Number -Row $f1 -FieldName "F1H01"),
            (Get-Number -Row $f1 -FieldName "F1H02")
        )
        $valueEndowmentEndGasb = First-NonNull @(
            (Get-Number -Row $f1 -FieldName $resolvedFields["value_endowment_assets_end_gasb"]),
            (Get-Number -Row $f1 -FieldName "F1H02"),
            (Get-Number -Row $f1 -FieldName "F1H01")
        )
        $valueEndowmentBeginningFasb = First-NonNull @(
            (Get-Number -Row $f2 -FieldName $resolvedFields["value_endowment_beginning_fasb"]),
            (Get-Number -Row $f2 -FieldName "F2H01"),
            (Get-Number -Row $f2 -FieldName "F2H02")
        )
        $valueEndowmentEndFasb = First-NonNull @(
            (Get-Number -Row $f2 -FieldName $resolvedFields["value_endowment_end_fasb"]),
            (Get-Number -Row $f2 -FieldName "F2H02"),
            (Get-Number -Row $f2 -FieldName "F2H01")
        )

        $row = [ordered]@{
            unitid = $unitid
            institution_name = $institutionName
            institution_unique_name = (($institutionName, $city, $state) | Where-Object { $_ }) -join " | "
            year = $year
            access_earnings = Get-String -Row $hd -FieldName $resolvedFields["access_earnings"]
            admissions_yield = Get-Number -Row $drvadm -FieldName $resolvedFields["admissions_yield"]
            city = $city
            state = $state
            zip = Get-String -Row $hd -FieldName $resolvedFields["zip"]
            county = Get-String -Row $hd -FieldName $resolvedFields["county"]
            longitude = Get-Number -Row $hd -FieldName $resolvedFields["longitude"]
            latitude = Get-Number -Row $hd -FieldName $resolvedFields["latitude"]
            status = Get-String -Row $hd -FieldName $resolvedFields["status"]
            date_closed = Get-String -Row $hd -FieldName $resolvedFields["date_closed"]
            is_active = Get-String -Row $hd -FieldName $resolvedFields["is_active"]
            multi_institution_campus_org = Get-String -Row $hd -FieldName $resolvedFields["multi_institution_campus_org"]
            id_number_multi = Get-String -Row $hd -FieldName $resolvedFields["id_number_multi"]
            name_multi = Get-String -Row $hd -FieldName $resolvedFields["name_multi"]
            opeid = Get-String -Row $hd -FieldName $resolvedFields["opeid"]
            region = Get-String -Row $hd -FieldName $resolvedFields["region"]
            hbcu = Get-String -Row $hd -FieldName $resolvedFields["hbcu"]
            tribal_college = Get-String -Row $hd -FieldName $resolvedFields["tribal_college"]
            sector = Get-String -Row $hd -FieldName $resolvedFields["sector"]
            level = Get-String -Row $hd -FieldName $resolvedFields["level"]
            control = Get-String -Row $hd -FieldName $resolvedFields["control"]
            degree_granting_status = Get-String -Row $hd -FieldName $resolvedFields["degree_granting_status"]
            highest_degree = Get-String -Row $hd -FieldName $resolvedFields["highest_degree"]
            category = Get-String -Row $hd -FieldName $resolvedFields["category"]
            grad_offering = Get-String -Row $hd -FieldName $resolvedFields["grad_offering"]
            urbanization = Get-String -Row $hd -FieldName $resolvedFields["urbanization"]
            size = Get-String -Row $hd -FieldName $resolvedFields["size"]
            undergrad_program_mix = Get-String -Row $hd -FieldName $resolvedFields["undergrad_program_mix"]
            grad_program_mix = Get-String -Row $hd -FieldName $resolvedFields["grad_program_mix"]
            control_or_affiliation = Get-String -Row $ic -FieldName $resolvedFields["control_or_affiliation"]
            religious_affiliation = Get-String -Row $ic -FieldName $resolvedFields["religious_affiliation"]
            has_full_time_first_time_undergrad = Get-String -Row $ic -FieldName $resolvedFields["has_full_time_first_time_undergrad"]
            reporting_model = Get-String -Row $flags -FieldName $resolvedFields["reporting_model"]
            fte_12_months = $fte12Months
            fte_undergrad = $fteUndergrad
            fte_graduate = $fteGraduate
            fte_total_staff = Get-Number -Row $drvhr -FieldName $resolvedFields["fte_total_staff"]
            fte_instructional = Get-Number -Row $drvhr -FieldName $resolvedFields["fte_instructional"]
            state_approps_percent_core_gasb = Get-Number -Row $drvf -FieldName $resolvedFields["state_approps_percent_core_gasb"]
            gov_grants_fasb = Get-Number -Row $drvf -FieldName $resolvedFields["gov_grants_fasb"]
            state_revenue_fte_fasb = Get-Number -Row $drvf -FieldName $resolvedFields["state_revenue_fte_fasb"]
            gov_revenue_fte_fasb = Get-Number -Row $drvf -FieldName $resolvedFields["gov_revenue_fte_fasb"]
            endowment_assets_per_fte_gasb = Get-Number -Row $drvf -FieldName $resolvedFields["endowment_assets_per_fte_gasb"]
            endowment_assets_per_fte_fasb = Get-Number -Row $drvf -FieldName $resolvedFields["endowment_assets_per_fte_fasb"]
            tuition_fees_after_discounts_allowances_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["tuition_fees_after_discounts_allowances_gasb"]
            federal_operating_grants_contracts_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["federal_operating_grants_contracts_gasb"]
            state_appropriations_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["state_appropriations_gasb"]
            total_operating_nonoperating_revenues_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["total_operating_nonoperating_revenues_gasb"]
            discounts_allowances_applied_tuition_fees_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["discounts_allowances_applied_tuition_fees_gasb"]
            total_expenses_deductions_current_total_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["total_expenses_deductions_current_total_gasb"]
            total_assets_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["total_assets_gasb"]
            total_liabilities_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["total_liabilities_gasb"]
            unrestricted_public = Get-Number -Row $f1 -FieldName $resolvedFields["unrestricted_public"]
            value_endowment_assets_beginning_gasb = $valueEndowmentBeginningGasb
            value_endowment_assets_end_gasb = $valueEndowmentEndGasb
            new_gifts_additions_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["new_gifts_additions_gasb"]
            endowment_net_investment_return_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["endowment_net_investment_return_gasb"]
            endowment_spending_distribution_current_use_gasb = Get-Number -Row $f1 -FieldName $resolvedFields["endowment_spending_distribution_current_use_gasb"]
            total_assets_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["total_assets_fasb"]
            total_liabilities_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["total_liabilities_fasb"]
            total_unrestricted_net_assets_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["total_unrestricted_net_assets_fasb"]
            institutional_grants_funded_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["institutional_grants_funded_fasb"]
            institutional_grants_unfunded_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["institutional_grants_unfunded_fasb"]
            allowances_applied_to_tuition_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["allowances_applied_to_tuition_fasb"]
            pell_grants = Get-Number -Row $f2 -FieldName $resolvedFields["pell_grants"]
            pell_accounting_method = @(
                Get-String -Row $flags -FieldName $resolvedFields["pell_accounting_method_fasb"]
                Get-String -Row $flags -FieldName $resolvedFields["pell_accounting_method_pfp"]
            ) | Where-Object { -not [string]::IsNullOrWhiteSpace($_) } | Select-Object -First 1
            tuition_and_fees_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["tuition_and_fees_fasb"]
            state_approps_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["state_approps_fasb"]
            federal_grants_contracts_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["federal_grants_contracts_fasb"]
            total_revenues_investment_return_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["total_revenues_investment_return_fasb"]
            total_expenses_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["total_expenses_fasb"]
            unrestricted_operating_rev_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["unrestricted_operating_rev_fasb"]
            value_endowment_beginning_fasb = $valueEndowmentBeginningFasb
            value_endowment_end_fasb = $valueEndowmentEndFasb
            new_endowment_gifts_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["new_endowment_gifts_fasb"]
            endowment_net_investment_return_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["endowment_net_investment_return_fasb"]
            spending_distribution_for_current_use_fasb = Get-Number -Row $f2 -FieldName $resolvedFields["spending_distribution_for_current_use_fasb"]
            total_assets_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["total_assets_pfp"]
            total_liabilities_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["total_liabilities_pfp"]
            institutional_grants_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["institutional_grants_pfp"]
            discounts_allowances_applied_tuition_fees_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["discounts_allowances_applied_tuition_fees_pfp"]
            tuition_fees_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["tuition_fees_pfp"]
            federal_grants_contracts_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["federal_grants_contracts_pfp"]
            state_appropriations_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["state_appropriations_pfp"]
            total_revenues_investment_return_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["total_revenues_investment_return_pfp"]
            total_revenues_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["total_revenues_pfp"]
            total_expenses_total_amount_pfp = Get-Number -Row $f3 -FieldName $resolvedFields["total_expenses_total_amount_pfp"]
        }

        $row["assets"] = First-NonNull @($row["total_assets_gasb"], $row["total_assets_fasb"], $row["total_assets_pfp"])
        $row["liabilities"] = First-NonNull @($row["total_liabilities_gasb"], $row["total_liabilities_fasb"], $row["total_liabilities_pfp"])
        $allRows.Add([pscustomobject]$row)
    }
}

$allRows | Sort-Object year, unitid | Export-Csv -Path $datasetCsv -NoTypeInformation

Write-Host "Saved catalog to $catalogCsv"
Write-Host "Saved dataset to $datasetCsv"
