param(
    [string]$RawCsv = ".\ipeds_financial_health_raw_2014_2024.csv",
    [string]$CatalogCsv = ".\ipeds_financial_health_selected_file_catalog.csv",
    [string]$OutputCsv = ".\ipeds_financial_health_looker_ready_2014_2024.csv",
    [string]$ExpandedOutputCsv = ".\ipeds_financial_health_reporting_2014_2024.csv"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$rawPath = Join-Path $root $RawCsv
$catalogPath = Join-Path $root $CatalogCsv
$outputPath = Join-Path $root $OutputCsv
$expandedOutputPath = Join-Path $root $ExpandedOutputCsv
$auxRoot = Join-Path $root "looker_aux"
$auxDataRoot = Join-Path $auxRoot "data"
$auxExtractRoot = Join-Path $auxRoot "extracted"

foreach ($path in @($auxRoot, $auxDataRoot, $auxExtractRoot)) {
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

function To-Number {
    param([AllowNull()]$Value)

    if ($null -eq $Value) { return $null }
    $text = [string]$Value
    if ([string]::IsNullOrWhiteSpace($text)) { return $null }
    $number = 0.0
    if ([double]::TryParse(($text -replace ',', '').Trim(), [ref]$number)) {
        return $number
    }
    return $null
}

function Safe-PctChange {
    param($NewValue, $OldValue)

    if ($null -eq $NewValue -or $null -eq $OldValue -or $OldValue -eq 0) {
        return $null
    }
    return (($NewValue - $OldValue) / [math]::Abs($OldValue)) * 100
}

function Safe-Divide {
    param($Numerator, $Denominator)

    if ($null -eq $Numerator -or $null -eq $Denominator -or $Denominator -eq 0) {
        return $null
    }
    return $Numerator / $Denominator
}

function Zero-IfNull {
    param([AllowNull()]$Value)

    if ($null -eq $Value) { return 0 }
    return $Value
}

function Has-Property {
    param(
        [Parameter(Mandatory = $true)]$Row,
        [Parameter(Mandatory = $true)][string]$Name
    )

    return $Row.PSObject.Properties.Name -contains $Name
}

function Get-ControlLabel {
    param([AllowNull()][string]$ControlCode)

    $normalized = if ($null -eq $ControlCode) { "" } else { ([string]$ControlCode).Trim() }
    switch ($normalized) {
        "1" { return "Public" }
        "2" { return "Private not-for-profit" }
        "3" { return "Private for-profit" }
        default { return $ControlCode }
    }
}

function Get-StateName {
    param([AllowNull()][string]$StateAbbr)

    $lookup = @{
        AL = "Alabama"; AK = "Alaska"; AZ = "Arizona"; AR = "Arkansas"; CA = "California"; CO = "Colorado"
        CT = "Connecticut"; DE = "Delaware"; DC = "District of Columbia"; FL = "Florida"; GA = "Georgia"
        HI = "Hawaii"; ID = "Idaho"; IL = "Illinois"; IN = "Indiana"; IA = "Iowa"; KS = "Kansas"
        KY = "Kentucky"; LA = "Louisiana"; ME = "Maine"; MD = "Maryland"; MA = "Massachusetts"
        MI = "Michigan"; MN = "Minnesota"; MS = "Mississippi"; MO = "Missouri"; MT = "Montana"
        NE = "Nebraska"; NV = "Nevada"; NH = "New Hampshire"; NJ = "New Jersey"; NM = "New Mexico"
        NY = "New York"; NC = "North Carolina"; ND = "North Dakota"; OH = "Ohio"; OK = "Oklahoma"
        OR = "Oregon"; PA = "Pennsylvania"; RI = "Rhode Island"; SC = "South Carolina"; SD = "South Dakota"
        TN = "Tennessee"; TX = "Texas"; UT = "Utah"; VT = "Vermont"; VA = "Virginia"; WA = "Washington"
        WV = "West Virginia"; WI = "Wisconsin"; WY = "Wyoming"; PR = "Puerto Rico"; GU = "Guam"
        VI = "U.S. Virgin Islands"; AS = "American Samoa"; MP = "Northern Mariana Islands"
        FM = "Federated States of Micronesia"; MH = "Marshall Islands"; PW = "Palau"
    }

    if ($null -eq $StateAbbr) { return $null }
    $key = ([string]$StateAbbr).Trim().ToUpperInvariant()
    if ($lookup.ContainsKey($key)) { return $lookup[$key] }
    return $StateAbbr
}

function Get-YearValueLookup {
    param(
        [Parameter(Mandatory = $true)]$Rows,
        [Parameter(Mandatory = $true)][string]$Property
    )

    $lookup = @{}
    foreach ($row in $Rows) {
        $lookup[[int]$row.year] = $row.$Property
    }
    return $lookup
}

function Get-YearValue {
    param(
        [Parameter(Mandatory = $true)]$Lookup,
        [Parameter(Mandatory = $true)][int]$Year
    )

    if ($Lookup.ContainsKey($Year)) {
        return $Lookup[$Year]
    }
    return $null
}

function Get-LatestNonNullValue {
    param(
        [Parameter(Mandatory = $true)]$Rows,
        [Parameter(Mandatory = $true)][string]$Property
    )

    foreach ($row in ($Rows | Sort-Object year -Descending)) {
        $value = $row.$Property
        if ($null -ne $value -and "$value" -ne "") {
            return @{
                year = [int]$row.year
                value = $value
            }
        }
    }

    return $null
}

function Get-LossFrequency {
    param(
        [Parameter(Mandatory = $true)]$Lookup,
        [Parameter(Mandatory = $true)][int]$EndYear,
        [Parameter(Mandatory = $true)][int]$WindowYears
    )

    $startYear = $EndYear - $WindowYears + 1
    $count = 0
    foreach ($year in $startYear..$EndYear) {
        $value = Get-YearValue -Lookup $Lookup -Year $year
        if ($null -ne $value -and $value -lt 0) {
            $count++
        }
    }
    return $count
}

function Count-NegativeYears {
    param(
        [Parameter(Mandatory = $true)]$Lookup,
        [Parameter(Mandatory = $true)][int[]]$Years,
        [double]$Threshold = 0
    )

    $count = 0
    foreach ($year in $Years) {
        $value = Get-YearValue -Lookup $Lookup -Year $year
        if ($null -ne $value -and $value -lt $Threshold) {
            $count++
        }
    }
    return $count
}

function Count-DeclineYears {
    param(
        [Parameter(Mandatory = $true)]$Lookup,
        [Parameter(Mandatory = $true)][int]$StartYear,
        [Parameter(Mandatory = $true)][int]$EndYear,
        [double]$ThresholdPct = 0
    )

    $count = 0
    foreach ($year in $StartYear..$EndYear) {
        $oldValue = Get-YearValue -Lookup $Lookup -Year $year
        $newValue = Get-YearValue -Lookup $Lookup -Year ($year + 1)
        $pct = Safe-PctChange -NewValue $newValue -OldValue $oldValue
        if ($null -ne $pct -and $pct -le $ThresholdPct) {
            $count++
        }
    }
    return $count
}

function Get-PercentileMap {
    param(
        [Parameter(Mandatory = $true)]$Rows,
        [Parameter(Mandatory = $true)][string]$Property,
        [Parameter(Mandatory = $true)][string]$OutputProperty
    )

    $grouped = $Rows | Group-Object year
    foreach ($group in $grouped) {
        $values = $group.Group |
            Where-Object { $_.control_label -eq "Private not-for-profit" -and $null -ne $_.$Property } |
            Sort-Object { [double]$_.$Property }

        if ($values.Count -eq 0) { continue }

        for ($i = 0; $i -lt $values.Count; $i++) {
            $rank = (($i + 1) / $values.Count) * 100
            $values[$i] | Add-Member -NotePropertyName $OutputProperty -NotePropertyValue ([math]::Round($rank, 1)) -Force
        }
    }
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
    $innerZip = Join-Path $DestinationPath ($inner.BaseName + ".zip")
    Copy-Item -Path $inner.FullName -Destination $innerZip -Force
    Expand-Archive -Path $innerZip -DestinationPath $DestinationPath -Force
}

function Get-XlsxSharedStrings {
    param([Parameter(Mandatory = $true)][string]$ExpandedFolder)

    $sharedPath = Join-Path $ExpandedFolder "xl\sharedStrings.xml"
    if (-not (Test-Path $sharedPath)) { return @() }

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
    $relNs = New-Object System.Xml.XmlNamespaceManager($relsXml.NameTable)
    $relNs.AddNamespace("r", "http://schemas.openxmlformats.org/package/2006/relationships")

    $sheet = $workbookXml.SelectNodes("//x:sheet", $wbNs) | Where-Object { [string]$_.name -ieq $SheetName } | Select-Object -First 1
    if (-not $sheet) { throw "Sheet $SheetName not found" }

    $relationshipId = [string]$sheet.Attributes["r:id"].Value
    $relationship = $relsXml.SelectNodes("//r:Relationship", $relNs) | Where-Object { [string]$_.Id -eq $relationshipId } | Select-Object -First 1
    $sheetPath = Join-Path $ExpandedFolder ("xl\" + (([string]$relationship.Target) -replace '/', '\'))
    [xml]$sheetXml = Get-Content -Path $sheetPath -Raw
    $sheetNs = New-Object System.Xml.XmlNamespaceManager($sheetXml.NameTable)
    $sheetNs.AddNamespace("x", "http://schemas.openxmlformats.org/spreadsheetml/2006/main")

    $shared = Get-XlsxSharedStrings -ExpandedFolder $ExpandedFolder
    $rows = @()
    foreach ($row in $sheetXml.SelectNodes("//x:sheetData/x:row", $sheetNs)) {
        $obj = [ordered]@{}
        foreach ($cell in $row.SelectNodes("./x:c", $sheetNs)) {
            $column = ([string]$cell.r -replace '\d', '')
            $valueNode = $cell.SelectSingleNode("./x:v", $sheetNs)
            $value = if ($valueNode) { [string]$valueNode.InnerText } else { "" }
            $cellType = if ($cell.Attributes["t"]) { [string]$cell.Attributes["t"].Value } else { "" }
            if ($cellType -eq "s" -and $value -ne "") { $value = $shared[[int]$value] }
            $obj[$column] = $value
        }
        $rows += [pscustomobject]$obj
    }
    return $rows
}

function Get-FrequencyLookup {
    param(
        [Parameter(Mandatory = $true)][string]$DictionaryArchive,
        [Parameter(Mandatory = $true)][string]$TableName,
        [Parameter(Mandatory = $true)][string]$VarName
    )

    $expanded = Join-Path $auxExtractRoot ("dict_" + $TableName)
    Expand-NestedOfficePackage -ArchivePath $DictionaryArchive -DestinationPath $expanded
    $rows = Get-XlsxSheetRows -ExpandedFolder $expanded -SheetName "Frequencies"

    $lookup = @{}
    foreach ($row in $rows) {
        if (($row.PSObject.Properties.Name -contains "A") -and ($row.PSObject.Properties.Name -contains "D") -and ($row.PSObject.Properties.Name -contains "E")) {
            if ([string]$row.A -eq $VarName -and -not [string]::IsNullOrWhiteSpace([string]$row.D)) {
                $lookup[([string]$row.D).Trim()] = ([string]$row.E).Trim()
            }
        }
    }
    return $lookup
}

function Decode-Value {
    param(
        [AllowNull()][string]$Code,
        [Parameter(Mandatory = $true)]$Lookup
    )

    if ($null -eq $Code) { return $null }
    $key = ([string]$Code).Trim()
    if ($Lookup.ContainsKey($key)) { return $Lookup[$key] }
    return $Code
}

$rawRows = Import-Csv -Path $rawPath
$catalog = Import-Csv -Path $catalogPath

$endowmentValueByKey = @{}
foreach ($rawRow in $rawRows) {
    $rawUnitid = [string]$rawRow.unitid
    $rawYear = [int](To-Number $rawRow.year)
    if ([string]::IsNullOrWhiteSpace($rawUnitid) -or $rawYear -eq 0) { continue }

    $rawControlLabel = Get-ControlLabel $rawRow.control
    $rawEndowmentValue = switch ($rawControlLabel) {
        "Public" {
            $primary = To-Number $rawRow.value_endowment_assets_end_gasb
            if ($null -ne $primary) {
                $primary
            }
            elseif (Has-Property -Row $rawRow -Name "F1H01") {
                To-Number $rawRow.F1H01
            }
            else {
                $null
            }
        }
        "Private not-for-profit" {
            $primary = To-Number $rawRow.value_endowment_end_fasb
            if ($null -ne $primary) {
                $primary
            }
            elseif (Has-Property -Row $rawRow -Name "F2H01") {
                To-Number $rawRow.F2H01
            }
            else {
                $null
            }
        }
        default { $null }
    }

    if ($null -ne $rawEndowmentValue) {
        $endowmentValueByKey["$rawUnitid|$rawYear"] = $rawEndowmentValue
    }
}

$hd2024Dict = Join-Path $root "downloads\dict\HD2024.zip"
$flags2024Dict = Join-Path $root "downloads\dict\FLAGS2024.zip"
$hdSectorLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "SECTOR" } else { @{} }
$hdLevelLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "ICLEVEL" } else { @{} }
$hdActLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "ACT" } else { @{} }
$hdActiveLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "CYACTIVE" } else { @{} }
$hdHbcuLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "HBCU" } else { @{} }
$hdTribalLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "TRIBAL" } else { @{} }
$hdGradOfferingLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "GROFFER" } else { @{} }
$hdCategoryLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "INSTCAT" } else { @{} }
$hdLocaleLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "LOCALE" } else { @{} }
$hdAccessLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "CARNEGIESAEC" } else { @{} }
$hdSizeLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "CARNEGIESIZE" } else { @{} }
$hdUgMixLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "CARNEGIEAPM" } else { @{} }
$hdGradMixLookup = if (Test-Path $hd2024Dict) { Get-FrequencyLookup -DictionaryArchive $hd2024Dict -TableName "HD2024" -VarName "CARNEGIEGPM" } else { @{} }
$flagsFormLookup = if (Test-Path $flags2024Dict) { Get-FrequencyLookup -DictionaryArchive $flags2024Dict -TableName "FLAGS2024" -VarName "FORM_F" } else { @{} }

$eapByYearUnit = @{}
$effyByYearUnit = @{}
$sfaByYearUnit = @{}
$drvfByYearUnit = @{}

$eapTables = $catalog | Where-Object { $_.table_name -match '^EAP\d{4}$' }
foreach ($entry in $eapTables) {
    $tableName = [string]$entry.table_name
    $year = [int]$entry.year
    $zipPath = Join-Path $auxDataRoot ($tableName + ".zip")
    $extractPath = Join-Path $auxExtractRoot $tableName
    Invoke-Download -Url $entry.data_url -OutFile $zipPath
    Expand-IfZip -ArchivePath $zipPath -DestinationPath $extractPath
    $csvFile = Get-ChildItem -Path $extractPath -Filter "*.csv" | Select-Object -First 1
    if (-not $csvFile) { continue }

    $tableLookup = @{}
    $rows = Import-Csv -Path $csvFile.FullName
    foreach ($row in $rows) {
        if (-not $tableLookup.ContainsKey($row.UNITID)) {
            $tableLookup[$row.UNITID] = @{
                staff_headcount_total = $null
                staff_headcount_instructional = $null
            }
        }

        switch ($row.EAPCAT) {
            "10000" { $tableLookup[$row.UNITID].staff_headcount_total = To-Number $row.EAPTOT }
            "21000" { $tableLookup[$row.UNITID].staff_headcount_instructional = To-Number $row.EAPTOT }
        }
    }

    $eapByYearUnit[$year] = $tableLookup
}

$effyTables = $catalog | Where-Object { $_.table_name -match '^EFFY\d{4}$' }
foreach ($entry in $effyTables) {
    $tableName = [string]$entry.table_name
    $year = [int]$entry.year
    $zipPath = Join-Path $auxDataRoot ($tableName + ".zip")
    $extractPath = Join-Path $auxExtractRoot $tableName
    Invoke-Download -Url $entry.data_url -OutFile $zipPath
    Expand-IfZip -ArchivePath $zipPath -DestinationPath $extractPath
    $csvFile = Get-ChildItem -Path $extractPath -Filter "*.csv" | Select-Object -First 1
    if (-not $csvFile) { continue }

    $tableLookup = @{}
    $rows = Import-Csv -Path $csvFile.FullName
    foreach ($row in $rows) {
        if (-not $tableLookup.ContainsKey($row.UNITID)) {
            $tableLookup[$row.UNITID] = @{
                enrollment_headcount_total = $null
                enrollment_headcount_undergrad = $null
                enrollment_headcount_graduate = $null
                enrollment_nonresident_total = $null
                enrollment_nonresident_undergrad = $null
                enrollment_nonresident_graduate = $null
            }
        }

        if (Has-Property -Row $row -Name "EFFYALEV") {
            switch ($row.EFFYALEV) {
                "1" {
                    $tableLookup[$row.UNITID].enrollment_headcount_total = To-Number $row.EFYTOTLT
                    $tableLookup[$row.UNITID].enrollment_nonresident_total = To-Number $row.EFYNRALT
                }
                "2" {
                    $tableLookup[$row.UNITID].enrollment_headcount_undergrad = To-Number $row.EFYTOTLT
                    $tableLookup[$row.UNITID].enrollment_nonresident_undergrad = To-Number $row.EFYNRALT
                }
                "12" {
                    $tableLookup[$row.UNITID].enrollment_headcount_graduate = To-Number $row.EFYTOTLT
                    $tableLookup[$row.UNITID].enrollment_nonresident_graduate = To-Number $row.EFYNRALT
                }
            }
        }
        else {
            if ($row.PSObject.Properties.Name -contains "LSTUDY") {
                switch ([string]$row.LSTUDY) {
                    "999" {
                        $tableLookup[$row.UNITID].enrollment_headcount_total = To-Number $row.EFYTOTLT
                        $tableLookup[$row.UNITID].enrollment_nonresident_total = To-Number $row.EFYNRALT
                    }
                    "1" {
                        $tableLookup[$row.UNITID].enrollment_headcount_undergrad = To-Number $row.EFYTOTLT
                        $tableLookup[$row.UNITID].enrollment_nonresident_undergrad = To-Number $row.EFYNRALT
                    }
                    "3" {
                        $tableLookup[$row.UNITID].enrollment_headcount_graduate = To-Number $row.EFYTOTLT
                        $tableLookup[$row.UNITID].enrollment_nonresident_graduate = To-Number $row.EFYNRALT
                    }
                }
            }
        }
    }

    $effyByYearUnit[$year] = $tableLookup
}

$sfaTables = $catalog | Where-Object { $_.table_name -match '^SFA\d{4}$' }
foreach ($entry in $sfaTables) {
    $tableName = [string]$entry.table_name
    $year = [int]$entry.year
    $zipPath = Join-Path $auxDataRoot ($tableName + ".zip")
    $extractPath = Join-Path $auxExtractRoot $tableName
    Invoke-Download -Url $entry.data_url -OutFile $zipPath
    Expand-IfZip -ArchivePath $zipPath -DestinationPath $extractPath
    $csvFile = Get-ChildItem -Path $extractPath -Filter "*.csv" | Select-Object -First 1
    if (-not $csvFile) { continue }

    $tableLookup = @{}
    $rows = Import-Csv -Path $csvFile.FullName
    foreach ($row in $rows) {
        $tableLookup[$row.UNITID] = @{
            loan_pct_undergrad_federal = if (Has-Property -Row $row -Name "UFLOANP") { To-Number $row.UFLOANP } else { $null }
            loan_avg_undergrad_federal = if (Has-Property -Row $row -Name "UFLOANA") { To-Number $row.UFLOANA } else { $null }
            loan_count_undergrad_federal = if (Has-Property -Row $row -Name "UFLOANN") { To-Number $row.UFLOANN } else { $null }
        }
    }

    $sfaByYearUnit[$year] = $tableLookup
}

$drvfTables = $catalog | Where-Object { $_.table_name -match '^DRVF\d{4}$' }
foreach ($entry in $drvfTables) {
    $tableName = [string]$entry.table_name
    $year = [int]$entry.year
    $zipPath = Join-Path $auxDataRoot ($tableName + ".zip")
    $extractPath = Join-Path $auxExtractRoot $tableName
    Invoke-Download -Url $entry.data_url -OutFile $zipPath
    Expand-IfZip -ArchivePath $zipPath -DestinationPath $extractPath
    $csvFile = Get-ChildItem -Path $extractPath -Filter "*.csv" | Select-Object -First 1
    if (-not $csvFile) { continue }

    $tableLookup = @{}
    $rows = Import-Csv -Path $csvFile.FullName
    foreach ($row in $rows) {
        $coreRevenue = $null
        if ((Has-Property -Row $row -Name "F1CORREV") -and "$($row.F1CORREV)" -ne "") {
            $coreRevenue = To-Number $row.F1CORREV
        }
        elseif ((Has-Property -Row $row -Name "F2CORREV") -and "$($row.F2CORREV)" -ne "") {
            $coreRevenue = To-Number $row.F2CORREV
        }

        $tableLookup[$row.UNITID] = @{
            core_revenue = $coreRevenue
            gov_grants_contracts_pct_core_revenue_gasb = if (Has-Property -Row $row -Name "F1GVGCPC") { To-Number $row.F1GVGCPC } else { $null }
            gov_grants_contracts_pct_core_revenue_fasb = if (Has-Property -Row $row -Name "F2GVGCPC") { To-Number $row.F2GVGCPC } else { $null }
            state_appropriations_pct_core_revenue_gasb = if (Has-Property -Row $row -Name "F1STAPPC") { To-Number $row.F1STAPPC } else { $null }
        }
    }

    $drvfByYearUnit[$year] = $tableLookup
}

$preparedRows = foreach ($row in $rawRows) {
    $year = [int]$row.year
    $unitid = [string]$row.unitid
    $rowKey = "$unitid|$year"
    $controlLabel = Get-ControlLabel -ControlCode ([string]$row.control)
    $eapYear = if ($eapByYearUnit.ContainsKey($year)) { $eapByYearUnit[$year] } else { $null }
    $effyYear = if ($effyByYearUnit.ContainsKey($year)) { $effyByYearUnit[$year] } else { $null }
    $eap = if ($null -ne $eapYear -and $eapYear.ContainsKey($unitid)) { $eapYear[$unitid] } else { $null }
    $effy = if ($null -ne $effyYear -and $effyYear.ContainsKey($unitid)) { $effyYear[$unitid] } else { $null }
    $sfaYear = if ($sfaByYearUnit.ContainsKey($year)) { $sfaByYearUnit[$year] } else { $null }
    $drvfYear = if ($drvfByYearUnit.ContainsKey($year)) { $drvfByYearUnit[$year] } else { $null }
    $sfa = if ($null -ne $sfaYear -and $sfaYear.ContainsKey($unitid)) { $sfaYear[$unitid] } else { $null }
    $drvf = if ($null -ne $drvfYear -and $drvfYear.ContainsKey($unitid)) { $drvfYear[$unitid] } else { $null }

    $sectorDecoded = Decode-Value -Code $row.sector -Lookup $hdSectorLookup
    $levelDecoded = Decode-Value -Code $row.level -Lookup $hdLevelLookup
    $statusDecoded = Decode-Value -Code $row.status -Lookup $hdActLookup
    $isActiveDecoded = Decode-Value -Code $row.is_active -Lookup $hdActiveLookup
    $stateFullName = Get-StateName -StateAbbr ([string]$row.state)

    if ($controlLabel -notin @("Public", "Private not-for-profit", "Private for-profit")) { continue }
    if ($levelDecoded -ne "Four or more years") { continue }
    if ($null -ne $sectorDecoded -and $sectorDecoded -like "Administrative Unit*") { continue }
    if ($null -ne $isActiveDecoded -and $isActiveDecoded -notin @("Yes", "Imputed as active")) { continue }

    $fte12 = To-Number $row.fte_12_months
    $revenue = switch ($controlLabel) {
        "Public" { To-Number $row.total_operating_nonoperating_revenues_gasb }
        "Private not-for-profit" { To-Number $row.total_revenues_investment_return_fasb }
        "Private for-profit" { To-Number $row.total_revenues_investment_return_pfp }
        default { $null }
    }
    $expenses = switch ($controlLabel) {
        "Public" { To-Number $row.total_expenses_deductions_current_total_gasb }
        "Private not-for-profit" { To-Number $row.total_expenses_fasb }
        "Private for-profit" { To-Number $row.total_expenses_total_amount_pfp }
        default { $null }
    }
    $netTuitionTotal = switch ($controlLabel) {
        "Public" {
            To-Number $row.tuition_fees_after_discounts_allowances_gasb
        }
        "Private not-for-profit" {
            $tf = To-Number $row.tuition_and_fees_fasb
            $allow = To-Number $row.allowances_applied_to_tuition_fasb
            $funded = To-Number $row.institutional_grants_funded_fasb
            $unfunded = To-Number $row.institutional_grants_unfunded_fasb
            if ($null -eq $tf) { $null } else { ($tf + (Zero-IfNull $allow)) - ((Zero-IfNull $funded) + (Zero-IfNull $unfunded)) }
        }
        "Private for-profit" {
            $tf = To-Number $row.tuition_fees_pfp
            $disc = To-Number $row.discounts_allowances_applied_tuition_fees_pfp
            $grants = To-Number $row.institutional_grants_pfp
            if ($null -eq $tf) { $null } else { ($tf + (Zero-IfNull $disc)) - (Zero-IfNull $grants) }
        }
        default { $null }
    }
    $federalFunding = switch ($controlLabel) {
        "Public" { To-Number $row.federal_operating_grants_contracts_gasb }
        "Private not-for-profit" { To-Number $row.federal_grants_contracts_fasb }
        "Private for-profit" { To-Number $row.federal_grants_contracts_pfp }
        default { $null }
    }
    $stateFunding = switch ($controlLabel) {
        "Public" { To-Number $row.state_appropriations_gasb }
        "Private not-for-profit" { To-Number $row.state_approps_fasb }
        "Private for-profit" { To-Number $row.state_appropriations_pfp }
        default { $null }
    }
    $assets = To-Number $row.assets
    $liabilities = To-Number $row.liabilities
    $unrestrictedAssets = switch ($controlLabel) {
        "Public" { To-Number $row.unrestricted_public }
        "Private not-for-profit" { To-Number $row.total_unrestricted_net_assets_fasb }
        default { $null }
    }
    $grossTuition = switch ($controlLabel) {
        "Public" {
            $discounts = To-Number $row.discounts_allowances_applied_tuition_fees_gasb
            $net = To-Number $row.tuition_fees_after_discounts_allowances_gasb
            if ($null -eq $net -and $null -eq $discounts) { $null } else { (Zero-IfNull $net) + (Zero-IfNull $discounts) }
        }
        "Private not-for-profit" {
            $tf = To-Number $row.tuition_and_fees_fasb
            $allow = To-Number $row.allowances_applied_to_tuition_fasb
            if ($null -eq $tf -and $null -eq $allow) { $null } else { (Zero-IfNull $tf) + (Zero-IfNull $allow) }
        }
        default { $null }
    }
    $discountRate = switch ($controlLabel) {
        "Public" {
            Safe-Divide -Numerator (To-Number $row.discounts_allowances_applied_tuition_fees_gasb) -Denominator $grossTuition
        }
        "Private not-for-profit" {
            Safe-Divide -Numerator (To-Number $row.institutional_grants_unfunded_fasb) -Denominator $grossTuition
        }
        default { $null }
    }
    $endowmentValue = if ($endowmentValueByKey.ContainsKey($rowKey)) {
        $endowmentValueByKey[$rowKey]
    }
    elseif ($controlLabel -eq "Public") {
        To-Number $row.value_endowment_assets_end_gasb
    }
    else {
        To-Number $row.value_endowment_end_fasb
    }
    $governmentFunding = if ($null -eq $federalFunding -and $null -eq $stateFunding) { $null } else { (Zero-IfNull $federalFunding) + (Zero-IfNull $stateFunding) }
    $coreRevenue = if ($drvf) { $drvf.core_revenue } else { $null }
    $lossAmount = if ($null -eq $revenue -or $null -eq $expenses) { $null } else { $revenue - $expenses }
    $pellAccountingMethod = if ($null -eq $row.pell_accounting_method) { "" } else { ([string]$row.pell_accounting_method).Trim() }
    $pellGrants = To-Number $row.pell_grants
    $federalGrantsContractsPellAdjusted = switch ($controlLabel) {
        "Public" {
            $federalFunding
        }
        "Private not-for-profit" {
            if ($null -eq $federalFunding) {
                $null
            }
            elseif ($pellAccountingMethod -eq "2") {
                [math]::Max(($federalFunding - (Zero-IfNull $pellGrants)), 0)
            }
            else {
                $federalFunding
            }
        }
        default { $null }
    }

    [pscustomobject]@{
        unitid = $unitid
        institution_name = $row.institution_name
        institution_unique_name = (($row.institution_name, $row.city, $stateFullName) | Where-Object { $_ }) -join " | "
        year = $year
        control_label = $controlLabel
        state = $stateFullName
        city = $row.city
        urbanization = Decode-Value -Code $row.urbanization -Lookup $hdLocaleLookup
        sector = $sectorDecoded
        level = $levelDecoded
        category = Decode-Value -Code $row.category -Lookup $hdCategoryLookup
        institution_status = $statusDecoded
        is_active = $isActiveDecoded
        hbcu = Decode-Value -Code $row.hbcu -Lookup $hdHbcuLookup
        tribal_college = Decode-Value -Code $row.tribal_college -Lookup $hdTribalLookup
        grad_offering = Decode-Value -Code $row.grad_offering -Lookup $hdGradOfferingLookup
        reporting_model = Decode-Value -Code $row.reporting_model -Lookup $flagsFormLookup
        access_earnings = Decode-Value -Code $row.access_earnings -Lookup $hdAccessLookup
        size = Decode-Value -Code $row.size -Lookup $hdSizeLookup
        grad_program_mix = Decode-Value -Code $row.grad_program_mix -Lookup $hdGradMixLookup
        undergrad_program_mix = Decode-Value -Code $row.undergrad_program_mix -Lookup $hdUgMixLookup
        fte_12_months = $fte12
        fte_undergrad = To-Number $row.fte_undergrad
        fte_graduate = To-Number $row.fte_graduate
        enrollment_headcount_total = if ($effy) { $effy.enrollment_headcount_total } else { $null }
        enrollment_headcount_undergrad = if ($effy) { $effy.enrollment_headcount_undergrad } else { $null }
        enrollment_headcount_graduate = if ($effy) { $effy.enrollment_headcount_graduate } else { $null }
        enrollment_nonresident_total = if ($effy) { $effy.enrollment_nonresident_total } else { $null }
        enrollment_nonresident_undergrad = if ($effy) { $effy.enrollment_nonresident_undergrad } else { $null }
        enrollment_nonresident_graduate = if ($effy) { $effy.enrollment_nonresident_graduate } else { $null }
        pct_international_all = if ($effy) { Safe-Divide -Numerator $effy.enrollment_nonresident_total -Denominator $effy.enrollment_headcount_total } else { $null }
        pct_international_undergraduate = if ($effy) { Safe-Divide -Numerator $effy.enrollment_nonresident_undergrad -Denominator $effy.enrollment_headcount_undergrad } else { $null }
        pct_international_graduate = if ($effy) { Safe-Divide -Numerator $effy.enrollment_nonresident_graduate -Denominator $effy.enrollment_headcount_graduate } else { $null }
        staff_fte_total = To-Number $row.fte_total_staff
        staff_fte_instructional = To-Number $row.fte_instructional
        staff_headcount_total = if ($eap) { $eap.staff_headcount_total } else { $null }
        staff_headcount_instructional = if ($eap) { $eap.staff_headcount_instructional } else { $null }
        loan_pct_undergrad_federal = if ($sfa) { $sfa.loan_pct_undergrad_federal } else { $null }
        loan_avg_undergrad_federal = if ($sfa) { $sfa.loan_avg_undergrad_federal } else { $null }
        loan_count_undergrad_federal = if ($sfa) { $sfa.loan_count_undergrad_federal } else { $null }
        revenue_total = $revenue
        expenses_total = $expenses
        loss_amount = $lossAmount
        ended_year_at_loss = if ($null -eq $lossAmount) { $null } elseif ($lossAmount -lt 0) { "Yes" } else { "No" }
        operating_margin = if ($null -eq $revenue -or $revenue -eq 0 -or $null -eq $expenses) { $null } else { (($revenue - $expenses) / $revenue) * 100 }
        net_tuition_total = $netTuitionTotal
        net_tuition_per_fte = Safe-Divide -Numerator $netTuitionTotal -Denominator $fte12
        tuition_dependence_ratio = Safe-Divide -Numerator $netTuitionTotal -Denominator $revenue
        tuition_dependence_pct = if ($null -eq (Safe-Divide -Numerator $netTuitionTotal -Denominator $revenue)) { $null } else { (Safe-Divide -Numerator $netTuitionTotal -Denominator $revenue) * 100 }
        admissions_yield = To-Number $row.admissions_yield
        federal_funding = $federalFunding
        federal_grants_contracts_pell_adjusted = $federalGrantsContractsPellAdjusted
        federal_grants_contracts_pell_adjusted_pct_core_revenue = Safe-Divide -Numerator $federalGrantsContractsPellAdjusted -Denominator $coreRevenue
        state_funding = $stateFunding
        core_revenue = $coreRevenue
        state_funding_pct_core_revenue = Safe-Divide -Numerator $stateFunding -Denominator $coreRevenue
        gov_grants_contracts_pct_core_revenue_gasb = if ($drvf) { $drvf.gov_grants_contracts_pct_core_revenue_gasb } else { $null }
        gov_grants_contracts_pct_core_revenue_fasb = if ($drvf) { $drvf.gov_grants_contracts_pct_core_revenue_fasb } else { $null }
        state_appropriations_pct_core_revenue_gasb = if ($drvf) { $drvf.state_appropriations_pct_core_revenue_gasb } else { $null }
        government_funding_total = $governmentFunding
        government_funding_pct_total_revenue = Safe-Divide -Numerator $governmentFunding -Denominator $revenue
        endowment_value = $endowmentValue
        endowment_assets_per_fte = if ($controlLabel -eq "Public") { To-Number $row.endowment_assets_per_fte_gasb } else { To-Number $row.endowment_assets_per_fte_fasb }
        leverage = Safe-Divide -Numerator $liabilities -Denominator $assets
        liquidity = Safe-Divide -Numerator $unrestrictedAssets -Denominator $assets
        discount_rate = $discountRate
        assets = $assets
        liabilities = $liabilities
    }
}

$rowList = New-Object System.Collections.Generic.List[object]
foreach ($row in $preparedRows) {
    $rowList.Add($row)
}

$byInstitution = $rowList | Group-Object unitid
foreach ($group in $byInstitution) {
    $rows = $group.Group | Sort-Object year
    $enrollmentHeadcountLookup = Get-YearValueLookup -Rows $rows -Property "enrollment_headcount_total"
    $enrollmentFteLookup = Get-YearValueLookup -Rows $rows -Property "fte_12_months"
    $staffFteLookup = Get-YearValueLookup -Rows $rows -Property "staff_fte_total"
    $staffInstrFteLookup = Get-YearValueLookup -Rows $rows -Property "staff_fte_instructional"
    $staffHeadcountLookup = Get-YearValueLookup -Rows $rows -Property "staff_headcount_total"
    $staffInstrHeadcountLookup = Get-YearValueLookup -Rows $rows -Property "staff_headcount_instructional"
    $revenueLookup = Get-YearValueLookup -Rows $rows -Property "revenue_total"
    $opMarginLookup = Get-YearValueLookup -Rows $rows -Property "operating_margin"
    $netTuitionLookup = Get-YearValueLookup -Rows $rows -Property "net_tuition_total"
    $netTuitionPerFteLookup = Get-YearValueLookup -Rows $rows -Property "net_tuition_per_fte"
    $yieldLookup = Get-YearValueLookup -Rows $rows -Property "admissions_yield"
    $discountLookup = Get-YearValueLookup -Rows $rows -Property "discount_rate"
    $governmentFundingLookup = Get-YearValueLookup -Rows $rows -Property "government_funding_total"
    $endowmentLookup = Get-YearValueLookup -Rows $rows -Property "endowment_value"
    $internationalHeadcountLookup = Get-YearValueLookup -Rows $rows -Property "enrollment_nonresident_total"
    $lossLookup = Get-YearValueLookup -Rows $rows -Property "loss_amount"
    $federalPellAdjustedLookup = Get-YearValueLookup -Rows $rows -Property "federal_grants_contracts_pell_adjusted"
    $stateFundingLookup = Get-YearValueLookup -Rows $rows -Property "state_funding"
    $loanPctLatest = Get-LatestNonNullValue -Rows $rows -Property "loan_pct_undergrad_federal"
    $loanCountLatest = Get-LatestNonNullValue -Rows $rows -Property "loan_count_undergrad_federal"
    $loanAvgLatest = Get-LatestNonNullValue -Rows $rows -Property "loan_avg_undergrad_federal"
    $loss2024 = Get-YearValue -Lookup $lossLookup -Year 2024

    foreach ($row in $rows) {
        $year = [int]$row.year
        $row | Add-Member -NotePropertyName enrollment_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.enrollment_headcount_total -OldValue (Get-YearValue -Lookup $enrollmentHeadcountLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName enrollment_decreased_5yr -NotePropertyValue $(if ($null -eq (Safe-PctChange -NewValue $row.enrollment_headcount_total -OldValue (Get-YearValue -Lookup $enrollmentHeadcountLookup -Year ($year - 5)))) { $null } elseif ((Safe-PctChange -NewValue $row.enrollment_headcount_total -OldValue (Get-YearValue -Lookup $enrollmentHeadcountLookup -Year ($year - 5))) -lt 0) { "Yes" } else { "No" }) -Force
        $row | Add-Member -NotePropertyName enroll_fte_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.fte_12_months -OldValue (Get-YearValue -Lookup $enrollmentFteLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName enrollment_decline_last_3_of_5 -NotePropertyValue ($(if ((Count-DeclineYears -Lookup $enrollmentHeadcountLookup -StartYear ($year - 5) -EndYear ($year - 1) -ThresholdPct 0) -ge 3) { "Yes" } else { "No" })) -Force
        $row | Add-Member -NotePropertyName enroll_fte_decline_last_3_of_5 -NotePropertyValue ($(if ((Count-DeclineYears -Lookup $enrollmentFteLookup -StartYear ($year - 5) -EndYear ($year - 1) -ThresholdPct 0) -ge 3) { "Yes" } else { "No" })) -Force
        $row | Add-Member -NotePropertyName staff_total_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.staff_fte_total -OldValue (Get-YearValue -Lookup $staffFteLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName staff_instructional_fte_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.staff_fte_instructional -OldValue (Get-YearValue -Lookup $staffInstrFteLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName staff_total_headcount_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.staff_headcount_total -OldValue (Get-YearValue -Lookup $staffHeadcountLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName staff_instructional_headcount_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.staff_headcount_instructional -OldValue (Get-YearValue -Lookup $staffInstrHeadcountLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName revenue_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.revenue_total -OldValue (Get-YearValue -Lookup $revenueLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName revenue_decreased_5yr -NotePropertyValue $(if ($null -eq (Safe-PctChange -NewValue $row.revenue_total -OldValue (Get-YearValue -Lookup $revenueLookup -Year ($year - 5)))) { $null } elseif ((Safe-PctChange -NewValue $row.revenue_total -OldValue (Get-YearValue -Lookup $revenueLookup -Year ($year - 5))) -lt 0) { "Yes" } else { "No" }) -Force
        $row | Add-Member -NotePropertyName revenue_change_1yr -NotePropertyValue (Safe-PctChange -NewValue $row.revenue_total -OldValue (Get-YearValue -Lookup $revenueLookup -Year ($year - 1))) -Force
        $row | Add-Member -NotePropertyName enrollment_change_1yr -NotePropertyValue (Safe-PctChange -NewValue $row.enrollment_headcount_total -OldValue (Get-YearValue -Lookup $enrollmentHeadcountLookup -Year ($year - 1))) -Force
        $row | Add-Member -NotePropertyName staff_change_1yr -NotePropertyValue (Safe-PctChange -NewValue $row.staff_headcount_total -OldValue (Get-YearValue -Lookup $staffHeadcountLookup -Year ($year - 1))) -Force
        $row | Add-Member -NotePropertyName revenue_10pct_drop_last_3_of_5 -NotePropertyValue ($(if ((Count-DeclineYears -Lookup $revenueLookup -StartYear ($year - 5) -EndYear ($year - 1) -ThresholdPct -10) -ge 3) { "Yes" } else { "No" })) -Force
        $row | Add-Member -NotePropertyName losses_last_3_of_5 -NotePropertyValue ($(if ((Count-NegativeYears -Lookup $opMarginLookup -Years (($year - 4)..$year) -Threshold 0) -ge 3) { "Yes" } else { "No" })) -Force
        $row | Add-Member -NotePropertyName net_tuition_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.net_tuition_total -OldValue (Get-YearValue -Lookup $netTuitionLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName net_tuition_per_fte_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.net_tuition_per_fte -OldValue (Get-YearValue -Lookup $netTuitionPerFteLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName yield_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.admissions_yield -OldValue (Get-YearValue -Lookup $yieldLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName discount_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.discount_rate -OldValue (Get-YearValue -Lookup $discountLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName government_funding_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.government_funding_total -OldValue (Get-YearValue -Lookup $governmentFundingLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName federal_grants_contracts_pell_adjusted_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.federal_grants_contracts_pell_adjusted -OldValue (Get-YearValue -Lookup $federalPellAdjustedLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName state_funding_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.state_funding -OldValue (Get-YearValue -Lookup $stateFundingLookup -Year ($year - 5))) -Force
        $row | Add-Member -NotePropertyName endowment_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $row.endowment_value -OldValue (Get-YearValue -Lookup $endowmentLookup -Year ($year - 5))) -Force
        $intlOld = Get-YearValue -Lookup $internationalHeadcountLookup -Year ($year - 10)
        $intlNew = $row.enrollment_nonresident_total
        $row | Add-Member -NotePropertyName international_enrollment_change_10yr -NotePropertyValue $(if ($null -eq $intlOld -or $null -eq $intlNew) { $null } else { $intlNew - $intlOld }) -Force
        $row | Add-Member -NotePropertyName international_enrollment_pct_change_10yr -NotePropertyValue (Safe-PctChange -NewValue $intlNew -OldValue $intlOld) -Force
        $row | Add-Member -NotePropertyName international_enrollment_increase_10yr -NotePropertyValue $(if ($null -eq $intlOld -or $null -eq $intlNew) { $null } elseif ($intlNew -gt $intlOld) { "Yes" } else { "No" }) -Force
        $intl5Old = Get-YearValue -Lookup $internationalHeadcountLookup -Year ($year - 5)
        $row | Add-Member -NotePropertyName international_student_count_change_5yr -NotePropertyValue $(if ($null -eq $intl5Old -or $null -eq $intlNew) { $null } else { $intlNew - $intl5Old }) -Force
        $row | Add-Member -NotePropertyName international_enrollment_pct_change_5yr -NotePropertyValue (Safe-PctChange -NewValue $intlNew -OldValue $intl5Old) -Force
        $row | Add-Member -NotePropertyName international_enrollment_increase_5yr -NotePropertyValue $(if ($null -eq $intl5Old -or $null -eq $intlNew) { $null } elseif ($intlNew -gt $intl5Old) { "Yes" } else { "No" }) -Force
        $row | Add-Member -NotePropertyName share_grad_students -NotePropertyValue (Safe-Divide -Numerator $row.enrollment_headcount_graduate -Denominator $row.enrollment_headcount_total) -Force
        $row | Add-Member -NotePropertyName loan_year_latest -NotePropertyValue $(if ($loanPctLatest) { $loanPctLatest.year } elseif ($loanCountLatest) { $loanCountLatest.year } elseif ($loanAvgLatest) { $loanAvgLatest.year } else { $null }) -Force
        $row | Add-Member -NotePropertyName loan_pct_undergrad_federal_latest -NotePropertyValue $(if ($loanPctLatest) { $loanPctLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName loan_count_undergrad_federal_latest -NotePropertyValue $(if ($loanCountLatest) { $loanCountLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName loan_avg_undergrad_federal_latest -NotePropertyValue $(if ($loanAvgLatest) { $loanAvgLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName federal_loan_pct_most_recent -NotePropertyValue $(if ($loanPctLatest) { $loanPctLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName federal_loan_count_most_recent -NotePropertyValue $(if ($loanCountLatest) { $loanCountLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName federal_loan_avg_most_recent -NotePropertyValue $(if ($loanAvgLatest) { $loanAvgLatest.value } else { $null }) -Force
        $row | Add-Member -NotePropertyName ended_2024_at_loss -NotePropertyValue $(if ($null -eq $loss2024) { $null } elseif ($loss2024 -lt 0) { "Yes" } else { "No" }) -Force
        $row | Add-Member -NotePropertyName loss_amount_2024 -NotePropertyValue $loss2024 -Force
        $row | Add-Member -NotePropertyName loss_years_last_10 -NotePropertyValue (Get-LossFrequency -Lookup $lossLookup -EndYear $year -WindowYears 10) -Force
        $row | Add-Member -NotePropertyName loss_years_last_5 -NotePropertyValue (Get-LossFrequency -Lookup $lossLookup -EndYear $year -WindowYears 5) -Force

        $intlAllPct = if ($null -ne $row.pct_international_all) { [math]::Round($row.pct_international_all * 100, 1) } else { $null }
        $intlUgPct = if ($null -ne $row.pct_international_undergraduate) { [math]::Round($row.pct_international_undergraduate * 100, 1) } else { $null }
        $intlGradPct = if ($null -ne $row.pct_international_graduate) { [math]::Round($row.pct_international_graduate * 100, 1) } else { $null }
        $internationalSentence = $null
        if ($null -ne $intlAllPct) {
            if ($null -ne $intlUgPct -and $null -ne $intlGradPct) {
                $internationalSentence = "$intlAllPct% of students are international. That includes $intlUgPct% of undergraduates and $intlGradPct% of graduate students."
            }
            else {
                $internationalSentence = "$intlAllPct% of students are international."
            }
        }
        $row | Add-Member -NotePropertyName international_students_sentence -NotePropertyValue $internationalSentence -Force

        $row | Add-Member -NotePropertyName enrollment_change_sentence -NotePropertyValue $(if ($null -eq $row.enrollment_pct_change_5yr) { $null } else { "12-month unduplicated headcount changed by $([math]::Round($row.enrollment_pct_change_5yr,1))% over the past five years." }) -Force
        $row | Add-Member -NotePropertyName revenue_change_sentence -NotePropertyValue $(if ($null -eq $row.revenue_pct_change_5yr) { $null } else { "Total revenue changed by $([math]::Round($row.revenue_pct_change_5yr,1))% over the past five years." }) -Force
        $row | Add-Member -NotePropertyName staffing_change_sentence -NotePropertyValue $(if ($null -eq $row.staff_total_headcount_pct_change_5yr) { $null } else { "Total staff headcount changed by $([math]::Round($row.staff_total_headcount_pct_change_5yr,1))% over the past five years." }) -Force
        $row | Add-Member -NotePropertyName federal_grants_contracts_dependence_sentence -NotePropertyValue $(if ($null -eq $row.federal_grants_contracts_pell_adjusted_pct_core_revenue) { $null } else { "$([math]::Round($row.federal_grants_contracts_pell_adjusted_pct_core_revenue * 100,1))% of core revenue came from Pell-adjusted federal grants and contracts." }) -Force
        $row | Add-Member -NotePropertyName state_funding_sentence -NotePropertyValue $(if ($null -eq $row.state_funding_pct_core_revenue) { $null } else { "$([math]::Round($row.state_funding_pct_core_revenue * 100,1))% of core revenue came from state appropriations." }) -Force
    }
}

Get-PercentileMap -Rows $rowList -Property "liquidity" -OutputProperty "liquidity_percentile_private_nfp"
Get-PercentileMap -Rows $rowList -Property "leverage" -OutputProperty "leverage_percentile_private_nfp"

$sortedRows = $rowList | Sort-Object year, unitid

$lookerColumns = @(
    "unitid",
    "institution_name",
    "institution_unique_name",
    "year",
    "control_label",
    "state",
    "city",
    "sector",
    "level",
    "urbanization",
    "category",
    "institution_status",
    "is_active",
    "hbcu",
    "tribal_college",
    "grad_offering",
    "reporting_model",
    "access_earnings",
    "size",
    "grad_program_mix",
    "undergrad_program_mix",
    "fte_12_months",
    "fte_undergrad",
    "fte_graduate",
    "enrollment_headcount_total",
    "enrollment_headcount_undergrad",
    "enrollment_headcount_graduate",
    "enrollment_nonresident_total",
    "enrollment_nonresident_undergrad",
    "enrollment_nonresident_graduate",
    "pct_international_all",
    "pct_international_undergraduate",
    "pct_international_graduate",
    "share_grad_students",
    "international_students_sentence",
    "international_student_count_change_5yr",
    "international_enrollment_pct_change_5yr",
    "international_enrollment_increase_5yr",
    "international_enrollment_change_10yr",
    "international_enrollment_increase_10yr",
    "staff_fte_total",
    "staff_fte_instructional",
    "staff_headcount_total",
    "staff_headcount_instructional",
    "revenue_total",
    "expenses_total",
    "loss_amount",
    "ended_year_at_loss",
    "losses_last_3_of_5",
    "loss_years_last_5",
    "loss_years_last_10",
    "net_tuition_total",
    "net_tuition_per_fte",
    "net_tuition_per_fte_change_5yr",
    "tuition_dependence_ratio",
    "tuition_dependence_pct",
    "discount_rate",
    "discount_pct_change_5yr",
    "federal_grants_contracts_pell_adjusted",
    "federal_grants_contracts_pell_adjusted_pct_core_revenue",
    "federal_grants_contracts_pell_adjusted_pct_change_5yr",
    "federal_grants_contracts_dependence_sentence",
    "state_funding",
    "state_funding_pct_core_revenue",
    "state_funding_pct_change_5yr",
    "state_funding_sentence",
    "endowment_value",
    "endowment_assets_per_fte",
    "endowment_pct_change_5yr",
    "liquidity",
    "liquidity_percentile_private_nfp",
    "leverage",
    "leverage_percentile_private_nfp",
    "loan_year_latest",
    "loan_pct_undergrad_federal_latest",
    "loan_count_undergrad_federal_latest",
    "loan_avg_undergrad_federal_latest",
    "federal_loan_pct_most_recent",
    "federal_loan_count_most_recent",
    "federal_loan_avg_most_recent",
    "enrollment_decline_last_3_of_5",
    "enrollment_pct_change_5yr",
    "enrollment_decreased_5yr",
    "staff_total_pct_change_5yr",
    "staff_instructional_fte_pct_change_5yr",
    "revenue_10pct_drop_last_3_of_5",
    "revenue_pct_change_5yr",
    "revenue_decreased_5yr",
    "enrollment_change_sentence",
    "revenue_change_sentence",
    "staffing_change_sentence"
)

$reportingColumns = @(
    $lookerColumns
    "expenses_total"
    "operating_margin"
    "admissions_yield"
    "yield_pct_change_5yr"
    "core_revenue"
    "gov_grants_contracts_pct_core_revenue_gasb"
    "gov_grants_contracts_pct_core_revenue_fasb"
    "state_appropriations_pct_core_revenue_gasb"
    "government_funding_total"
    "government_funding_pct_total_revenue"
    "government_funding_pct_change_5yr"
    "assets"
    "liabilities"
    "revenue_change_1yr"
    "enrollment_change_1yr"
    "staff_change_1yr"
    "net_tuition_pct_change_5yr"
    "enroll_fte_pct_change_5yr"
    "enroll_fte_decline_last_3_of_5"
    "staff_total_headcount_pct_change_5yr"
    "staff_instructional_headcount_pct_change_5yr"
    "international_enrollment_pct_change_10yr"
    "loan_pct_undergrad_federal"
    "loan_avg_undergrad_federal"
    "loan_count_undergrad_federal"
    "loss_amount_2024"
    "ended_2024_at_loss"
) | Select-Object -Unique

$lookerReady = $sortedRows | Select-Object -Property $lookerColumns
$reportingReady = $sortedRows | Select-Object -Property $reportingColumns

$lookerReady | Export-Csv -Path $outputPath -NoTypeInformation
$reportingReady | Export-Csv -Path $expandedOutputPath -NoTypeInformation
Write-Host "Saved Looker-ready dataset to $outputPath"
Write-Host "Saved reporting dataset to $expandedOutputPath"
