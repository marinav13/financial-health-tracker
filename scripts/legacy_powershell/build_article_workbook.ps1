param(
    [string]$InputCsv = ".\ipeds_financial_health_reporting_2014_2024.csv",
    [string]$OutputXlsx = ".\ipeds_financial_health_article_workbook.xml"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$inputPath = Join-Path $root $InputCsv
$outputPath = Join-Path $root $OutputXlsx

$flagshipUnitIds = @(
    102553,100751,106397,104179,110635,126614,129020,130943,134130,139959,141574,153658,142285,
    145637,151351,155317,157085,159391,166629,163286,161253,170976,174066,178396,176017,180489,
    199120,200280,181464,183044,186380,187985,182290,196088,204796,207500,209551,214777,217484,
    218663,219471,221759,228778,230764,234076,231174,236948,240444,238032,240727
)

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

function To-YesNoFlag {
    param([AllowNull()]$Value)
    return [string]::Equals(([string]$Value).Trim(), "Yes", [System.StringComparison]::OrdinalIgnoreCase)
}

function Safe-Pct {
    param(
        [double]$Numerator,
        [double]$Denominator
    )

    if ($Denominator -eq 0) { return $null }
    return ($Numerator / $Denominator) * 100
}

function Make-Row {
    param(
        [string]$Metric,
        [string]$Statistic,
        [AllowNull()]$All,
        [AllowNull()]$Public,
        [AllowNull()]$PrivateNfp,
        [AllowNull()]$PrivateFp,
        [string]$Notes = ""
    )

    [pscustomobject]@{
        metric = $Metric
        statistic = $Statistic
        all = $All
        public = $Public
        private_nfp = $PrivateNfp
        private_fp = $PrivateFp
        notes = $Notes
    }
}

function Get-Groups {
    param(
        [Parameter(Mandatory = $true)]$Rows
    )

    return [ordered]@{
        all = $Rows
        public = @($Rows | Where-Object { $_.control_label -eq "Public" })
        private_nfp = @($Rows | Where-Object { $_.control_label -eq "Private not-for-profit" })
        private_fp = @($Rows | Where-Object { $_.control_label -eq "Private for-profit" })
    }
}

function Count-ConditionByGroup {
    param(
        [Parameter(Mandatory = $true)]$Groups,
        [Parameter(Mandatory = $true)][scriptblock]$Predicate
    )

    $result = [ordered]@{}
    foreach ($name in $Groups.Keys) {
        $result[$name] = @($Groups[$name] | Where-Object $Predicate).Count
    }
    return $result
}

function Percent-ConditionByGroup {
    param(
        [Parameter(Mandatory = $true)]$Groups,
        [Parameter(Mandatory = $true)][scriptblock]$Predicate
    )

    $result = [ordered]@{}
    foreach ($name in $Groups.Keys) {
        $rows = @($Groups[$name])
        if ($rows.Count -eq 0) {
            $result[$name] = $null
        }
        else {
            $count = @($rows | Where-Object $Predicate).Count
            $result[$name] = Safe-Pct -Numerator $count -Denominator $rows.Count
        }
    }
    return $result
}

function Weighted-InternationalPct {
    param(
        [Parameter(Mandatory = $true)]$Rows,
        [Parameter(Mandatory = $true)][string]$NumeratorProperty,
        [Parameter(Mandatory = $true)][string]$DenominatorProperty
    )

    $valid = @($Rows | Where-Object {
        $num = To-Number $_.$NumeratorProperty
        $den = To-Number $_.$DenominatorProperty
        $null -ne $num -and $null -ne $den -and $den -gt 0
    })

    if ($valid.Count -eq 0) { return $null }

    $numSum = ($valid | Measure-Object -Property $NumeratorProperty -Sum).Sum
    $denSum = ($valid | Measure-Object -Property $DenominatorProperty -Sum).Sum
    return Safe-Pct -Numerator ([double]$numSum) -Denominator ([double]$denSum)
}

function Get-TopMetricByGroup {
    param(
        [Parameter(Mandatory = $true)]$Groups,
        [Parameter(Mandatory = $true)][string]$MetricProperty
    )

    $result = [ordered]@{}
    foreach ($name in $Groups.Keys) {
        $top = @($Groups[$name] | Where-Object { $null -ne $_.$MetricProperty } | Sort-Object @{ Expression = $MetricProperty; Descending = $true } | Select-Object -First 1)
        $result[$name] = if ($top.Count -gt 0) { $top[0] } else { $null }
    }
    return $result
}

function Add-WorksheetData {
    param()
}

function Escape-Xml {
    param([AllowNull()]$Value)

    if ($null -eq $Value) { return "" }
    return [System.Security.SecurityElement]::Escape([string]$Value)
}

function Get-ExcelXmlCell {
    param([AllowNull()]$Value, [string]$StyleId = "")

    $styleAttr = if ([string]::IsNullOrWhiteSpace($StyleId)) { "" } else { " ss:StyleID=`"$StyleId`"" }
    if ($null -eq $Value -or "$Value" -eq "") {
        return "<Cell$styleAttr><Data ss:Type=`"String`"></Data></Cell>"
    }

    $number = 0.0
    if ([double]::TryParse(([string]$Value), [ref]$number)) {
        return "<Cell$styleAttr><Data ss:Type=`"Number`">$([string]$number)</Data></Cell>"
    }

    return "<Cell$styleAttr><Data ss:Type=`"String`">$(Escape-Xml $Value)</Data></Cell>"
}

function Get-WorksheetXml {
    param(
        [Parameter(Mandatory = $true)][string]$Name,
        [Parameter(Mandatory = $true)]$Rows
    )

    $rowArray = @($Rows | ForEach-Object { $_ })
    if ($rowArray.Count -eq 0) {
        return @"
<Worksheet ss:Name="$(Escape-Xml $Name)">
  <Table>
    <Row><Cell ss:StyleID="Header"><Data ss:Type="String">No rows</Data></Cell></Row>
  </Table>
</Worksheet>
"@
    }

    $headers = @($rowArray[0].PSObject.Properties.Name)
    $sb = New-Object System.Text.StringBuilder
    [void]$sb.AppendLine("<Worksheet ss:Name=""$(Escape-Xml $Name)"">")
    [void]$sb.AppendLine("  <Table>")
    [void]$sb.Append("    <Row>")
    foreach ($header in $headers) {
        [void]$sb.Append((Get-ExcelXmlCell -Value $header -StyleId "Header"))
    }
    [void]$sb.AppendLine("</Row>")

    foreach ($row in $rowArray) {
        [void]$sb.Append("    <Row>")
        foreach ($header in $headers) {
            [void]$sb.Append((Get-ExcelXmlCell -Value $row.$header))
        }
        [void]$sb.AppendLine("</Row>")
    }

    [void]$sb.AppendLine("  </Table>")
    [void]$sb.AppendLine("</Worksheet>")
    return $sb.ToString()
}

$rows = Import-Csv -Path $inputPath
$latest = @($rows | Where-Object { $_.year -eq "2024" })

$latestPrepared = foreach ($row in $latest) {
    $warningScoreCore = @(
        To-YesNoFlag $row.enrollment_decline_last_3_of_5,
        To-YesNoFlag $row.revenue_10pct_drop_last_3_of_5,
        To-YesNoFlag $row.losses_last_3_of_5,
        To-YesNoFlag $row.ended_year_at_loss,
        ($null -ne (To-Number $row.staff_total_headcount_pct_change_5yr) -and (To-Number $row.staff_total_headcount_pct_change_5yr) -lt 0),
        ($null -ne (To-Number $row.net_tuition_per_fte_change_5yr) -and (To-Number $row.net_tuition_per_fte_change_5yr) -lt 0)
    ) | Where-Object { $_ } | Measure-Object | Select-Object -ExpandProperty Count

    [pscustomobject]@{
        unitid = [string]$row.unitid
        institution_name = $row.institution_name
        state = $row.state
        city = $row.city
        control_label = $row.control_label
        sector = $row.sector
        category = $row.category
        urbanization = $row.urbanization
        enrollment_headcount_total = To-Number $row.enrollment_headcount_total
        enrollment_headcount_undergrad = To-Number $row.enrollment_headcount_undergrad
        enrollment_headcount_graduate = To-Number $row.enrollment_headcount_graduate
        enrollment_nonresident_total = To-Number $row.enrollment_nonresident_total
        enrollment_nonresident_undergrad = To-Number $row.enrollment_nonresident_undergrad
        enrollment_nonresident_graduate = To-Number $row.enrollment_nonresident_graduate
        enrollment_pct_change_5yr = To-Number $row.enrollment_pct_change_5yr
        enrollment_decreased_5yr = $row.enrollment_decreased_5yr
        enrollment_decline_last_3_of_5 = $row.enrollment_decline_last_3_of_5
        enrollment_change_1yr = To-Number $row.enrollment_change_1yr
        share_grad_students = To-Number $row.share_grad_students
        pct_international_all = To-Number $row.pct_international_all
        pct_international_undergraduate = To-Number $row.pct_international_undergraduate
        pct_international_graduate = To-Number $row.pct_international_graduate
        international_student_count_change_5yr = To-Number $row.international_student_count_change_5yr
        international_enrollment_pct_change_5yr = To-Number $row.international_enrollment_pct_change_5yr
        international_enrollment_increase_5yr = $row.international_enrollment_increase_5yr
        international_enrollment_change_10yr = To-Number $row.international_enrollment_change_10yr
        international_enrollment_pct_change_10yr = To-Number $row.international_enrollment_pct_change_10yr
        international_enrollment_increase_10yr = $row.international_enrollment_increase_10yr
        staff_fte_total = To-Number $row.staff_fte_total
        staff_fte_instructional = To-Number $row.staff_fte_instructional
        staff_total_pct_change_5yr = To-Number $row.staff_total_pct_change_5yr
        staff_instructional_fte_pct_change_5yr = To-Number $row.staff_instructional_fte_pct_change_5yr
        staff_headcount_total = To-Number $row.staff_headcount_total
        staff_headcount_instructional = To-Number $row.staff_headcount_instructional
        staff_total_headcount_pct_change_5yr = To-Number $row.staff_total_headcount_pct_change_5yr
        staff_instructional_headcount_pct_change_5yr = To-Number $row.staff_instructional_headcount_pct_change_5yr
        staff_change_1yr = To-Number $row.staff_change_1yr
        revenue_total = To-Number $row.revenue_total
        revenue_pct_change_5yr = To-Number $row.revenue_pct_change_5yr
        revenue_decreased_5yr = $row.revenue_decreased_5yr
        revenue_change_1yr = To-Number $row.revenue_change_1yr
        revenue_10pct_drop_last_3_of_5 = $row.revenue_10pct_drop_last_3_of_5
        expenses_total = To-Number $row.expenses_total
        loss_amount = To-Number $row.loss_amount
        ended_year_at_loss = $row.ended_year_at_loss
        losses_last_3_of_5 = $row.losses_last_3_of_5
        loss_years_last_5 = To-Number $row.loss_years_last_5
        loss_years_last_10 = To-Number $row.loss_years_last_10
        net_tuition_per_fte = To-Number $row.net_tuition_per_fte
        net_tuition_per_fte_change_5yr = To-Number $row.net_tuition_per_fte_change_5yr
        tuition_dependence_pct = To-Number $row.tuition_dependence_pct
        admissions_yield = To-Number $row.admissions_yield
        yield_pct_change_5yr = To-Number $row.yield_pct_change_5yr
        discount_rate = To-Number $row.discount_rate
        discount_pct_change_5yr = To-Number $row.discount_pct_change_5yr
        federal_grants_contracts_pell_adjusted = To-Number $row.federal_grants_contracts_pell_adjusted
        federal_grants_contracts_pell_adjusted_pct_core_revenue = To-Number $row.federal_grants_contracts_pell_adjusted_pct_core_revenue
        federal_grants_contracts_pell_adjusted_pct_change_5yr = To-Number $row.federal_grants_contracts_pell_adjusted_pct_change_5yr
        state_funding = To-Number $row.state_funding
        state_funding_pct_core_revenue = To-Number $row.state_funding_pct_core_revenue
        state_funding_pct_change_5yr = To-Number $row.state_funding_pct_change_5yr
        endowment_value = To-Number $row.endowment_value
        endowment_pct_change_5yr = To-Number $row.endowment_pct_change_5yr
        liquidity = To-Number $row.liquidity
        liquidity_percentile_private_nfp = To-Number $row.liquidity_percentile_private_nfp
        leverage = To-Number $row.leverage
        leverage_percentile_private_nfp = To-Number $row.leverage_percentile_private_nfp
        loan_year_latest = To-Number $row.loan_year_latest
        federal_loan_pct_most_recent = To-Number $row.federal_loan_pct_most_recent
        federal_loan_count_most_recent = To-Number $row.federal_loan_count_most_recent
        federal_loan_avg_most_recent = To-Number $row.federal_loan_avg_most_recent
        warning_score_core = $warningScoreCore
    }
}

$groups = Get-Groups -Rows $latestPrepared
$summaryRows = New-Object System.Collections.Generic.List[object]

function Add-CountRow {
    param(
        [string]$Metric,
        [scriptblock]$Predicate,
        [string]$Notes = ""
    )
    $counts = Count-ConditionByGroup -Groups $groups -Predicate $Predicate
    $summaryRows.Add((Make-Row -Metric $Metric -Statistic "count" -All $counts.all -Public $counts.public -PrivateNfp $counts.private_nfp -PrivateFp $counts.private_fp -Notes $Notes))
}

function Add-PctRow {
    param(
        [string]$Metric,
        [scriptblock]$Predicate,
        [string]$Notes = ""
    )
    $pcts = Percent-ConditionByGroup -Groups $groups -Predicate $Predicate
    $summaryRows.Add((Make-Row -Metric $Metric -Statistic "percent" -All $pcts.all -Public $pcts.public -PrivateNfp $pcts.private_nfp -PrivateFp $pcts.private_fp -Notes $Notes))
}

function Add-CountAndPctRows {
    param(
        [string]$Metric,
        [scriptblock]$Predicate,
        [string]$Notes = ""
    )

    Add-CountRow -Metric $Metric -Predicate $Predicate -Notes $Notes
    Add-PctRow -Metric $Metric -Predicate $Predicate -Notes $Notes
}

Add-CountAndPctRows -Metric "Enrollment decline in 3 of last 5 years" -Predicate { To-YesNoFlag $_.enrollment_decline_last_3_of_5 }
Add-CountAndPctRows -Metric "Revenue decline in 3 of last 5 years" -Predicate { To-YesNoFlag $_.revenue_10pct_drop_last_3_of_5 }
Add-CountAndPctRows -Metric "In the red in 3 of last 5 years" -Predicate { To-YesNoFlag $_.losses_last_3_of_5 }
Add-CountAndPctRows -Metric "Enrollment and revenue decline in 3 of last 5 years" -Predicate { (To-YesNoFlag $_.enrollment_decline_last_3_of_5) -and (To-YesNoFlag $_.revenue_10pct_drop_last_3_of_5) }
Add-CountAndPctRows -Metric "Enrollment, revenue decline and in the red in 3 of last 5 years" -Predicate { (To-YesNoFlag $_.enrollment_decline_last_3_of_5) -and (To-YesNoFlag $_.revenue_10pct_drop_last_3_of_5) -and (To-YesNoFlag $_.losses_last_3_of_5) }
Add-CountAndPctRows -Metric "Enrollment decline and in the red in 3 of last 5 years" -Predicate { (To-YesNoFlag $_.enrollment_decline_last_3_of_5) -and (To-YesNoFlag $_.losses_last_3_of_5) }
Add-CountAndPctRows -Metric "Revenue decline and in the red in 3 of last 5 years" -Predicate { (To-YesNoFlag $_.revenue_10pct_drop_last_3_of_5) -and (To-YesNoFlag $_.losses_last_3_of_5) }
Add-CountAndPctRows -Metric "Enrollment decline in 2024" -Predicate { $null -ne $_.enrollment_change_1yr -and $_.enrollment_change_1yr -lt 0 }
Add-CountAndPctRows -Metric "Revenue decline in 2024" -Predicate { $null -ne $_.revenue_change_1yr -and $_.revenue_change_1yr -lt 0 }
Add-CountAndPctRows -Metric "In the red in 2024" -Predicate { To-YesNoFlag $_.ended_year_at_loss }
Add-CountAndPctRows -Metric "International enrollment increased over past decade" -Predicate { To-YesNoFlag $_.international_enrollment_increase_10yr }
Add-CountAndPctRows -Metric "International enrollment increased over past 5 years" -Predicate { To-YesNoFlag $_.international_enrollment_increase_5yr }
Add-CountAndPctRows -Metric "Staffing cut over past 5 years" -Predicate { $null -ne $_.staff_total_headcount_pct_change_5yr -and $_.staff_total_headcount_pct_change_5yr -lt 0 } -Notes "Using staff headcount"
Add-CountAndPctRows -Metric "Instructional staffing cut over past 5 years" -Predicate { $null -ne $_.staff_instructional_headcount_pct_change_5yr -and $_.staff_instructional_headcount_pct_change_5yr -lt 0 } -Notes "Using staff headcount"
Add-CountAndPctRows -Metric "Ended 2024 fiscal year at a loss" -Predicate { To-YesNoFlag $_.ended_year_at_loss }
Add-CountAndPctRows -Metric "Net tuition per FTE decreased over past 5 years" -Predicate { $null -ne $_.net_tuition_per_fte_change_5yr -and $_.net_tuition_per_fte_change_5yr -lt 0 } -Notes "Using net tuition per FTE"
Add-CountAndPctRows -Metric "State appropriations decreased over past 5 years" -Predicate { $null -ne $_.state_funding_pct_change_5yr -and $_.state_funding_pct_change_5yr -lt 0 }
Add-CountAndPctRows -Metric "State appropriations increased over past 5 years" -Predicate { $null -ne $_.state_funding_pct_change_5yr -and $_.state_funding_pct_change_5yr -gt 0 }
Add-CountAndPctRows -Metric "Endowment increased over past 5 years" -Predicate { $null -ne $_.endowment_pct_change_5yr -and $_.endowment_pct_change_5yr -gt 0 }
Add-CountAndPctRows -Metric "Endowment decreased over past 5 years" -Predicate { $null -ne $_.endowment_pct_change_5yr -and $_.endowment_pct_change_5yr -lt 0 }
Add-CountAndPctRows -Metric "Enrollment decreased over past 5 years" -Predicate { $null -ne $_.enrollment_pct_change_5yr -and $_.enrollment_pct_change_5yr -lt 0 }
Add-CountAndPctRows -Metric "Revenue decreased over past 5 years" -Predicate { $null -ne $_.revenue_pct_change_5yr -and $_.revenue_pct_change_5yr -lt 0 }

$privateOnlyDiscountCount = Count-ConditionByGroup -Groups $groups -Predicate { $_.control_label -eq "Private not-for-profit" -and $null -ne $_.discount_pct_change_5yr -and $_.discount_pct_change_5yr -gt 0 }
$summaryRows.Add((Make-Row -Metric "Discount rate increased over past 5 years" -Statistic "count" -All $privateOnlyDiscountCount.all -Public "" -PrivateNfp $privateOnlyDiscountCount.private_nfp -PrivateFp "" -Notes "Private nonprofit only"))
$privateOnlyDiscountPct = Percent-ConditionByGroup -Groups ([ordered]@{ all = $groups.private_nfp; public = @(); private_nfp = $groups.private_nfp; private_fp = @() }) -Predicate { $null -ne $_.discount_pct_change_5yr -and $_.discount_pct_change_5yr -gt 0 }
$summaryRows.Add((Make-Row -Metric "Discount rate increased over past 5 years" -Statistic "percent" -All $privateOnlyDiscountPct.all -Public "" -PrivateNfp $privateOnlyDiscountPct.private_nfp -PrivateFp "" -Notes "Private nonprofit only"))

$intlAll = [ordered]@{}
foreach ($name in $groups.Keys) {
    $intlAll[$name] = Weighted-InternationalPct -Rows $groups[$name] -NumeratorProperty "enrollment_nonresident_total" -DenominatorProperty "enrollment_headcount_total"
}
$summaryRows.Add((Make-Row -Metric "Students who are international" -Statistic "percent" -All $intlAll.all -Public $intlAll.public -PrivateNfp $intlAll.private_nfp -PrivateFp $intlAll.private_fp))

$intlGrad = [ordered]@{}
foreach ($name in $groups.Keys) {
    $intlGrad[$name] = Weighted-InternationalPct -Rows $groups[$name] -NumeratorProperty "enrollment_nonresident_graduate" -DenominatorProperty "enrollment_headcount_graduate"
}
$summaryRows.Add((Make-Row -Metric "Graduate students who are international" -Statistic "percent" -All $intlGrad.all -Public $intlGrad.public -PrivateNfp $intlGrad.private_nfp -PrivateFp $intlGrad.private_fp))

$intlUg = [ordered]@{}
foreach ($name in $groups.Keys) {
    $intlUg[$name] = Weighted-InternationalPct -Rows $groups[$name] -NumeratorProperty "enrollment_nonresident_undergrad" -DenominatorProperty "enrollment_headcount_undergrad"
}
$summaryRows.Add((Make-Row -Metric "Undergraduate students who are international" -Statistic "percent" -All $intlUg.all -Public $intlUg.public -PrivateNfp $intlUg.private_nfp -PrivateFp $intlUg.private_fp))

$topFederalDependence = Get-TopMetricByGroup -Groups $groups -MetricProperty "federal_grants_contracts_pell_adjusted_pct_core_revenue"
$summaryRows.Add((Make-Row -Metric "Highest federal funding share of core revenue" -Statistic "institution" -All $(if ($topFederalDependence.all) { $topFederalDependence.all.institution_name } else { $null }) -Public $(if ($topFederalDependence.public) { $topFederalDependence.public.institution_name } else { $null }) -PrivateNfp $(if ($topFederalDependence.private_nfp) { $topFederalDependence.private_nfp.institution_name } else { $null }) -PrivateFp $(if ($topFederalDependence.private_fp) { $topFederalDependence.private_fp.institution_name } else { $null }) -Notes "Pell-adjusted federal grants/contracts"))
$summaryRows.Add((Make-Row -Metric "Highest federal funding share of core revenue" -Statistic "percent" -All $(if ($topFederalDependence.all) { $topFederalDependence.all.federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 } else { $null }) -Public $(if ($topFederalDependence.public) { $topFederalDependence.public.federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 } else { $null }) -PrivateNfp $(if ($topFederalDependence.private_nfp) { $topFederalDependence.private_nfp.federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 } else { $null }) -PrivateFp $(if ($topFederalDependence.private_fp) { $topFederalDependence.private_fp.federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 } else { $null }) -Notes "Pell-adjusted federal grants/contracts"))

$topStateDependence = Get-TopMetricByGroup -Groups $groups -MetricProperty "state_funding_pct_core_revenue"
$summaryRows.Add((Make-Row -Metric "Highest state funding share of core revenue" -Statistic "institution" -All $(if ($topStateDependence.all) { $topStateDependence.all.institution_name } else { $null }) -Public $(if ($topStateDependence.public) { $topStateDependence.public.institution_name } else { $null }) -PrivateNfp $(if ($topStateDependence.private_nfp) { $topStateDependence.private_nfp.institution_name } else { $null }) -PrivateFp $(if ($topStateDependence.private_fp) { $topStateDependence.private_fp.institution_name } else { $null }) -Notes "State appropriations"))
$summaryRows.Add((Make-Row -Metric "Highest state funding share of core revenue" -Statistic "percent" -All $(if ($topStateDependence.all) { $topStateDependence.all.state_funding_pct_core_revenue * 100 } else { $null }) -Public $(if ($topStateDependence.public) { $topStateDependence.public.state_funding_pct_core_revenue * 100 } else { $null }) -PrivateNfp $(if ($topStateDependence.private_nfp) { $topStateDependence.private_nfp.state_funding_pct_core_revenue * 100 } else { $null }) -PrivateFp $(if ($topStateDependence.private_fp) { $topStateDependence.private_fp.state_funding_pct_core_revenue * 100 } else { $null }) -Notes "State appropriations"))

$allSheetColumns = @(
    "unitid","institution_name","state","city","control_label","sector","category","urbanization",
    "warning_score_core","enrollment_decline_last_3_of_5","revenue_10pct_drop_last_3_of_5","losses_last_3_of_5","ended_year_at_loss","loss_years_last_5","loss_years_last_10",
    "enrollment_headcount_total","enrollment_pct_change_5yr","enrollment_decreased_5yr","enrollment_change_1yr",
    "share_grad_students","pct_international_all","pct_international_undergraduate","pct_international_graduate","international_student_count_change_5yr","international_enrollment_pct_change_5yr","international_enrollment_increase_5yr","international_enrollment_change_10yr","international_enrollment_pct_change_10yr","international_enrollment_increase_10yr",
    "staff_headcount_total","staff_headcount_instructional","staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","staff_change_1yr",
    "revenue_total","revenue_pct_change_5yr","revenue_decreased_5yr","revenue_change_1yr","expenses_total","loss_amount",
    "net_tuition_per_fte","net_tuition_per_fte_change_5yr","tuition_dependence_pct",
    "discount_rate","discount_pct_change_5yr",
    "federal_grants_contracts_pell_adjusted","federal_grants_contracts_pell_adjusted_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
    "state_funding","state_funding_pct_core_revenue","state_funding_pct_change_5yr",
    "endowment_value","endowment_pct_change_5yr",
    "liquidity","liquidity_percentile_private_nfp","leverage","leverage_percentile_private_nfp",
    "loan_year_latest","federal_loan_pct_most_recent","federal_loan_count_most_recent","federal_loan_avg_most_recent"
)

$allSheet = $latestPrepared | Select-Object -Property $allSheetColumns

$declSort = @(
    @{ Expression = "enrollment_pct_change_5yr"; Descending = $false },
    @{ Expression = "revenue_pct_change_5yr"; Descending = $false },
    @{ Expression = "loss_amount"; Descending = $false }
)
$revSort = @(
    @{ Expression = "revenue_pct_change_5yr"; Descending = $false },
    @{ Expression = "loss_amount"; Descending = $false },
    @{ Expression = "enrollment_pct_change_5yr"; Descending = $false }
)
$intlSort = @(
    @{ Expression = "international_enrollment_change_10yr"; Descending = $true },
    @{ Expression = "international_student_count_change_5yr"; Descending = $true }
)
$lossSort = @(
    @{ Expression = "loss_amount"; Descending = $false },
    @{ Expression = "loss_years_last_10"; Descending = $true }
)

$enr35 = @($allSheet | Where-Object { $_.enrollment_decline_last_3_of_5 -eq "Yes" } | Sort-Object $declSort)
$rev35 = @($allSheet | Where-Object { $_.revenue_10pct_drop_last_3_of_5 -eq "Yes" } | Sort-Object $revSort)
$red35 = @($allSheet | Where-Object { $_.losses_last_3_of_5 -eq "Yes" } | Sort-Object $lossSort)
$enrrev = @($allSheet | Where-Object { $_.enrollment_decline_last_3_of_5 -eq "Yes" -and $_.revenue_10pct_drop_last_3_of_5 -eq "Yes" } | Sort-Object $declSort)
$enrred = @($allSheet | Where-Object { $_.enrollment_decline_last_3_of_5 -eq "Yes" -and $_.losses_last_3_of_5 -eq "Yes" } | Sort-Object $declSort)
$all3 = @($allSheet | Where-Object { $_.enrollment_decline_last_3_of_5 -eq "Yes" -and $_.revenue_10pct_drop_last_3_of_5 -eq "Yes" -and $_.losses_last_3_of_5 -eq "Yes" } | Sort-Object $declSort)
$intl10 = @($allSheet | Where-Object { $_.international_enrollment_increase_10yr -eq "Yes" } | Sort-Object $intlSort)
$flg = @($allSheet | Where-Object { [int]$_.unitid -in $flagshipUnitIds } | Sort-Object $declSort)
$loss24 = @($allSheet | Where-Object { $_.ended_year_at_loss -eq "Yes" } | Sort-Object $lossSort)
$stdec = @($allSheet | Where-Object { $null -ne $_.state_funding_pct_change_5yr -and $_.state_funding_pct_change_5yr -lt 0 } | Sort-Object @{ Expression = "state_funding_pct_change_5yr"; Descending = $false })
$enddec = @($allSheet | Where-Object { $null -ne $_.endowment_pct_change_5yr -and $_.endowment_pct_change_5yr -lt 0 } | Sort-Object @{ Expression = "endowment_pct_change_5yr"; Descending = $false })
$discup = @($allSheet | Where-Object { $null -ne $_.discount_pct_change_5yr -and $_.discount_pct_change_5yr -gt 0 } | Sort-Object @{ Expression = "discount_pct_change_5yr"; Descending = $true })
$enrdec5 = @($allSheet | Where-Object { $null -ne $_.enrollment_pct_change_5yr -and $_.enrollment_pct_change_5yr -lt 0 } | Sort-Object @{ Expression = "enrollment_pct_change_5yr"; Descending = $false })
$revdec5 = @($allSheet | Where-Object { $null -ne $_.revenue_pct_change_5yr -and $_.revenue_pct_change_5yr -lt 0 } | Sort-Object @{ Expression = "revenue_pct_change_5yr"; Descending = $false })
$feddep = @($allSheet | Where-Object { $null -ne $_.federal_grants_contracts_pell_adjusted_pct_core_revenue } | Sort-Object @{ Expression = "federal_grants_contracts_pell_adjusted_pct_core_revenue"; Descending = $true }, @{ Expression = "federal_grants_contracts_pell_adjusted"; Descending = $true })
$statedep = @($allSheet | Where-Object { $null -ne $_.state_funding_pct_core_revenue } | Sort-Object @{ Expression = "state_funding_pct_core_revenue"; Descending = $true }, @{ Expression = "state_funding"; Descending = $true })
$worksheets = [ordered]@{
    Summary = $summaryRows
    All_2024 = $allSheet
    EnrollDecl3of5 = $enr35
    RevDecl3of5 = $rev35
    Red3of5 = $red35
    EnrollRev3of5 = $enrrev
    EnrollRed3of5 = $enrred
    All3Signals = $all3
    IntlUp10yr = $intl10
    Flagships = $flg
    Loss2024 = $loss24
    StateDown5yr = $stdec
    EndowDown5yr = $enddec
    DiscRateUp5yr = $discup
    EnrollDown5yr = $enrdec5
    RevDown5yr = $revdec5
    FedDepend = $feddep
    StateDepend = $statedep
}

$wbXml = New-Object System.Text.StringBuilder
[void]$wbXml.AppendLine('<?xml version="1.0"?>')
[void]$wbXml.AppendLine('<?mso-application progid="Excel.Sheet"?>')
[void]$wbXml.AppendLine('<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet" xmlns:html="http://www.w3.org/TR/REC-html40">')
[void]$wbXml.AppendLine('  <Styles>')
[void]$wbXml.AppendLine('    <Style ss:ID="Default" ss:Name="Normal"><Alignment ss:Vertical="Bottom"/></Style>')
[void]$wbXml.AppendLine('    <Style ss:ID="Header"><Font ss:Bold="1"/></Style>')
[void]$wbXml.AppendLine('  </Styles>')
foreach ($sheetName in $worksheets.Keys) {
    try {
        [void]$wbXml.AppendLine([string](Get-WorksheetXml -Name $sheetName -Rows $worksheets[$sheetName]))
    }
    catch {
        throw "Worksheet $sheetName failed: $($_.Exception.Message)"
    }
}
[void]$wbXml.AppendLine('</Workbook>')

Set-Content -Path $outputPath -Value $wbXml.ToString() -Encoding UTF8
Write-Host "Saved article workbook to $outputPath"
