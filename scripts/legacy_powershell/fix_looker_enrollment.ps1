param(
    [string]$LookerCsv = ".\ipeds_financial_health_looker_ready_2014_2024.csv",
    [string]$AuxExtractRoot = ".\looker_aux\extracted"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$lookerPath = Join-Path $root $LookerCsv
$extractRoot = Join-Path $root $AuxExtractRoot

function To-Number {
    param([AllowNull()]$Value)
    if ($null -eq $Value) { return $null }
    $text = [string]$Value
    if ([string]::IsNullOrWhiteSpace($text)) { return $null }
    $number = 0.0
    if ([double]::TryParse(($text -replace ',', '').Trim(), [ref]$number)) { return $number }
    return $null
}

function Safe-Divide {
    param($Numerator, $Denominator)
    if ($null -eq $Numerator -or $null -eq $Denominator -or $Denominator -eq 0) { return $null }
    return $Numerator / $Denominator
}

$effyByYearUnit = @{}
Get-ChildItem -Path $extractRoot -Directory | Where-Object { $_.Name -match '^EFFY\d{4}$' } | ForEach-Object {
    $year = [int]($_.Name -replace '\D', '')
    $csvFile = Get-ChildItem -Path $_.FullName -Filter "*.csv" | Select-Object -First 1
    if (-not $csvFile) { return }

    $lookup = @{}
    Import-Csv -Path $csvFile.FullName | ForEach-Object {
        $row = $_
        if (-not $lookup.ContainsKey($row.UNITID)) {
            $lookup[$row.UNITID] = @{
                total = $null
                undergrad = $null
                graduate = $null
                nr_total = $null
                nr_undergrad = $null
                nr_graduate = $null
            }
        }

        if ($row.PSObject.Properties.Name -contains "EFFYALEV") {
            switch ([string]$row.EFFYALEV) {
                "1" { $lookup[$row.UNITID].total = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_total = To-Number $row.EFYNRALT }
                "2" { $lookup[$row.UNITID].undergrad = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_undergrad = To-Number $row.EFYNRALT }
                "12" { $lookup[$row.UNITID].graduate = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_graduate = To-Number $row.EFYNRALT }
            }
        }
        else {
            switch ([string]$row.LSTUDY) {
                "999" { $lookup[$row.UNITID].total = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_total = To-Number $row.EFYNRALT }
                "1" { $lookup[$row.UNITID].undergrad = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_undergrad = To-Number $row.EFYNRALT }
                "3" { $lookup[$row.UNITID].graduate = To-Number $row.EFYTOTLT; $lookup[$row.UNITID].nr_graduate = To-Number $row.EFYNRALT }
            }
        }
    }

    $effyByYearUnit[$year] = $lookup
}

$rows = Import-Csv -Path $lookerPath
foreach ($row in $rows) {
    $year = [int]$row.year
    $unitid = [string]$row.unitid
    if (-not $effyByYearUnit.ContainsKey($year)) { continue }
    $yearLookup = $effyByYearUnit[$year]
    if (-not $yearLookup.ContainsKey($unitid)) { continue }
    $effy = $yearLookup[$unitid]

    $row.enrollment_headcount_total = $effy.total
    $row.enrollment_headcount_undergrad = $effy.undergrad
    $row.enrollment_headcount_graduate = $effy.graduate
    $row.enrollment_nonresident_total = $effy.nr_total
    $row.enrollment_nonresident_undergrad = $effy.nr_undergrad
    $row.enrollment_nonresident_graduate = $effy.nr_graduate

    $pctAll = Safe-Divide -Numerator $effy.nr_total -Denominator $effy.total
    $pctUg = Safe-Divide -Numerator $effy.nr_undergrad -Denominator $effy.undergrad
    $pctGrad = Safe-Divide -Numerator $effy.nr_graduate -Denominator $effy.graduate
    $row.pct_international_all = $pctAll
    $row.pct_international_undergraduate = $pctUg
    $row.pct_international_graduate = $pctGrad

    if ($null -ne $pctAll) {
        $intlAll = [math]::Round($pctAll * 100, 1)
        if ($null -ne $pctUg -and $null -ne $pctGrad) {
            $intlUg = [math]::Round($pctUg * 100, 1)
            $intlGrad = [math]::Round($pctGrad * 100, 1)
            $row.international_students_sentence = "$intlAll% of students are international. That includes $intlUg% of undergraduates and $intlGrad% of graduate students."
        }
        else {
            $row.international_students_sentence = "$intlAll% of students are international."
        }
    }
    else {
        $row.international_students_sentence = $null
    }
}

$rows | Export-Csv -Path $lookerPath -NoTypeInformation
Write-Host "Updated enrollment headcount and international sentence fields in $lookerPath"
