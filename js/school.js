function getParam(name) {
  return new URLSearchParams(window.location.search).get(name);
}

function asNumber(value) {
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function fmtPct(value, digits = 0) {
  const n = asNumber(value);
  if (n === null) return "No data";
  const normalized = Math.abs(n) < (0.5 / (10 ** digits)) ? 0 : n;
  const sign = normalized > 0 ? "+" : "";
  return `${sign}${normalized.toFixed(digits)}%`;
}

function fmtPlainPct(value, digits = 0) {
  const n = asNumber(value);
  if (n === null) return "No data";
  return `${n.toFixed(digits)}%`;
}

function fmtRoundedPct(value, includePlus = false) {
  const n = asNumber(value);
  if (n === null) return "No data";
  const abs = Math.abs(n);
  const rounded = abs < 1 ? Math.round(n * 10) / 10 : Math.round(n);
  const sign = includePlus && rounded > 0 ? "+" : "";
  const decimals = Math.abs(rounded) < 1 && rounded !== 0 ? 1 : 0;
  return `${sign}${rounded.toFixed(decimals)}%`;
}

function fmtCurrency(value) {
  const n = asNumber(value);
  if (n === null) return "No data";
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
    maximumFractionDigits: 0
  }).format(n);
}

function setText(id, value) {
  const node = document.getElementById(id);
  if (node) node.textContent = value ?? "No data";
}

function toSeries(values) {
  return (values || []).map((point) => ({
    year: Number(point.year),
    value: Number(point.value)
  })).filter((point) => Number.isFinite(point.year) && Number.isFinite(point.value));
}

function latestPoint(values) {
  const series = toSeries(values);
  return series.length ? series[series.length - 1] : null;
}

function hasNegativePoint(values) {
  return toSeries(values).some((point) => point.value < 0);
}

function recentFiveYearRangeText(seriesValues) {
  const values = toSeries(seriesValues);
  if (values.length < 6) return "from 2019 to 2024";
  const end = values[values.length - 1].year;
  const start = values[values.length - 6].year;
  return `from ${start} to ${end}`;
}

function sentimentClass(value) {
  const n = asNumber(value);
  if (n === null) return "neutral";
  if (n <= -5) return "negative";
  if (n >= 5) return "positive";
  return "neutral";
}

function yesNoClass(value) {
  if (value === "Yes") return "negative";
  if (value === "No") return "positive";
  return "neutral";
}

function applyStrip(id, text, state = "neutral") {
  const node = document.getElementById(id);
  if (!node) return;
  node.className = `metric-strip ${state}`;
  node.innerHTML = `<div class="metric-statement">${text}</div>`;
}

function applyQuestionValueStrip(id, question, value, state = "neutral") {
  applyStrip(id, `${question}<br><strong>${value}</strong>`, state);
}

function setHidden(id, hidden) {
  const node = document.getElementById(id);
  if (!node) return;
  node.classList.toggle("is-hidden", Boolean(hidden));
}

function buildIntlSentence(summary, series) {
  if (summary.international_students_sentence) return summary.international_students_sentence;
  const all = asNumber(summary.pct_international_all);
  const ug = asNumber(summary.pct_international_undergraduate);
  const grad = asNumber(summary.pct_international_graduate);

  if (all !== null && ug !== null && grad !== null) {
    return `${fmtRoundedPct(all * 100)} of students are international. That includes ${fmtRoundedPct(ug * 100)} of undergraduates and ${fmtRoundedPct(grad * 100)} of graduate students.`;
  }

  if (all !== null) {
    return `${fmtRoundedPct(all * 100)} of students are international.`;
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return `${fmtRoundedPct(pct)} of students are international.`;
  }

  return "International student data are not available.";
}

function buildResearchSpendingSentence(profile, summary) {
  const perFte = asNumber(summary.research_expense_per_fte);
  const sectorLabel = String(profile.control_label || profile.sector || "").toLowerCase();
  const shareOfCoreExpenses = asNumber(summary.research_expense_pct_core_expenses);
  const sectorMedian = asNumber(summary.sector_median_research_expense_per_fte_positive);
  const reportingShare = asNumber(summary.sector_research_spending_reporting_share_pct);

  if (perFte === null) {
    return "Research spending data are not available.";
  }

  let medianComparison = null;
  if (sectorMedian !== null) {
    if (sectorMedian === 0) {
      medianComparison = "about";
    } else {
      const pctDiff = Math.abs(perFte - sectorMedian) / sectorMedian;
      if (pctDiff <= 0.05) {
        medianComparison = "about";
      } else if (perFte > sectorMedian) {
        medianComparison = "above";
      } else {
        medianComparison = "below";
      }
    }
  }

  if (shareOfCoreExpenses !== null && sectorMedian !== null && reportingShare !== null) {
    const sectorPhrase = sectorLabel ? `${sectorLabel} colleges` : "colleges in the same sector";
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time student on research in 2024. That is ${medianComparison} the median of ${fmtCurrency(sectorMedian)} for the ${fmtRoundedPct(reportingShare)} of ${sectorPhrase} who reported research spending.`;
  }

  if (shareOfCoreExpenses !== null) {
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time student on research in 2024.`;
  }

  return `This institution spent about ${fmtCurrency(perFte)} per full-time student on research in 2024.`;
}

function buildGradLoanSentence(profile, summary) {
  const sentences = [];
  const gradShare = asNumber(summary.share_grad_students);
  const sectorGradShare = asNumber(summary.sector_avg_share_grad_students);
  const gradPlusPerRecipient = asNumber(summary.grad_plus_disbursements_per_recipient);
  const sectorGradPlusMedian = asNumber(summary.sector_median_grad_plus_disbursements_per_recipient);
  const sectorLabel = String(profile.control_label || "").toLowerCase();

  if (gradShare !== null && sectorGradShare !== null && sectorLabel) {
    sentences.push(
      `${fmtPlainPct(gradShare, 0)} of students at this institution are graduate students, compared to ${fmtPlainPct(sectorGradShare, 0)} at other ${sectorLabel} institutions.`
    );
  }

  if (gradPlusPerRecipient !== null && sectorGradPlusMedian !== null && sectorLabel) {
    sentences.push(
      `On average, graduate students who take out Grad PLUS loans at this institution borrow about ${fmtCurrency(gradPlusPerRecipient)}, compared to ${fmtCurrency(sectorGradPlusMedian)} at other ${sectorLabel} institutions.`
    );
  }

  return sentences.join(" ");
}

function deriveEnrollmentFlag(summary, series) {
  if (summary.enrollment_decline_last_3_of_5) return summary.enrollment_decline_last_3_of_5;
  const values = toSeries(series.enrollment_headcount_total);
  if (values.length < 6) return "No data";
  const recent = values.slice(-6);
  let declines = 0;
  for (let i = 1; i < recent.length; i += 1) {
    if (recent[i].value < recent[i - 1].value) declines += 1;
  }
  return declines >= 3 ? "Yes" : "No";
}

function hasData(values) {
  return toSeries(values).length > 0;
}

function hasMeaningfulData(values) {
  const points = toSeries(values);
  return points.some((point) => point.value !== 0);
}

function setSectionVisibility(id, show) {
  const node = document.getElementById(id);
  if (!node) return;
  node.classList.toggle("is-hidden", !show);
}

function csvEscape(value) {
  if (value === null || value === undefined) return "";
  const text = String(value);
  if (/[",\n]/.test(text)) {
    return `"${text.replace(/"/g, '""')}"`;
  }
  return text;
}

function slugify(value) {
  return String(value || "college")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function downloadSchoolCsv(school) {
  const rows = [["section", "field", "year", "value"]];

  Object.entries(school.profile || {}).forEach(([field, value]) => {
    rows.push(["profile", field, "", value ?? ""]);
  });

  Object.entries(school.summary || {}).forEach(([field, value]) => {
    rows.push(["summary", field, "", value ?? ""]);
  });

  Object.entries(school.series || {}).forEach(([field, points]) => {
    (points || []).forEach((point) => {
      rows.push(["series", field, point.year ?? "", point.value ?? ""]);
    });
  });

  const csv = rows.map((row) => row.map(csvEscape).join(",")).join("\n");
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = `${slugify(school.profile?.institution_name)}-displayed-data.csv`;
  document.body.appendChild(anchor);
  anchor.click();
  anchor.remove();
  URL.revokeObjectURL(url);
}

function syncSearchToggle() {
  const wrap = document.getElementById("school-search-wrap");
  if (!wrap) return;
  if (window.innerWidth <= 700) {
    wrap.removeAttribute("open");
  } else {
    wrap.setAttribute("open", "");
  }
}

function syncTabs(unitid) {
  const finances = document.getElementById("tab-finances");
  if (finances) {
    finances.href = unitid ? `school.html?unitid=${encodeURIComponent(unitid)}` : "index.html";
  }

  const cuts = document.getElementById("tab-cuts");
  if (cuts) {
    cuts.href = "cuts.html";
  }

  const accreditation = document.getElementById("tab-accreditation");
  if (accreditation) {
    accreditation.href = "accreditation.html";
  }

  const research = document.getElementById("tab-research");
  if (research) {
    research.href = "research.html";
  }
}

function styleAnswerCard(answerId, value) {
  const answer = document.getElementById(answerId);
  if (!answer) return;
  const card = answer.closest(".metric-strip");
  if (!card) return;
  const state = yesNoClass(value);
  card.className = `metric-strip ${state}`;
  answer.className = `metric-answer ${state}`;
}

async function init() {
  syncSearchToggle();
  window.addEventListener("resize", syncSearchToggle);

  const unitid = getParam("unitid");
  syncTabs(unitid);
  if (!unitid) {
    setText("school-name", "No school selected");
    return;
  }

  const school = await loadJson(`data/schools/${unitid}.json`);
  const p = school.profile;
  const s = school.summary;
  const series = school.series;
  const fiveYearRangeText = recentFiveYearRangeText(series.revenue_total_adjusted || series.enrollment_headcount_total || []);

  const downloadButton = document.getElementById("download-school-data");
  if (downloadButton) {
    downloadButton.onclick = () => downloadSchoolCsv(school);
  }

  setText("school-name", p.institution_name);
  setText("school-location", [p.city, p.state].filter(Boolean).join(", "));
  setText("school-urbanization", p.urbanization);
  setText("school-control", p.sector);
  setText("school-graduation-rate", asNumber(s.graduation_rate_6yr) === null ? "No data" : fmtPlainPct(s.graduation_rate_6yr, 0));
  setText("school-median-earnings", fmtCurrency(s.median_earnings_10yr));
  setText("school-median-debt", fmtCurrency(s.median_debt_completers));

  applyStrip(
    "revenue-change-card",
    asNumber(s.revenue_pct_change_5yr) === null
      ? "Revenue data are not available."
      : `Revenue ${asNumber(s.revenue_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.revenue_pct_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.revenue_pct_change_5yr)
  );

  setText("loss-latest", s.ended_year_at_loss || "No data");
  styleAnswerCard("loss-latest", s.ended_year_at_loss);
  setText("loss-repeat", s.losses_last_3_of_5 || "No data");
  styleAnswerCard("loss-repeat", s.losses_last_3_of_5);
  setText("loss-years", s.loss_years_last_10 ?? "No data");

  applyStrip(
    "net-tuition-change-card",
    asNumber(s.net_tuition_per_fte_change_5yr) === null
      ? "Net tuition revenue per student data are not available."
      : `Net tuition revenue per student ${asNumber(s.net_tuition_per_fte_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.net_tuition_per_fte_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.net_tuition_per_fte_change_5yr)
  );

  applyStrip(
    "tuition-sentence-card",
    s.tuition_dependence_vs_sector_median_sentence || "No tuition dependence benchmark is available.",
    "neutral"
  );

  applyStrip(
    "research-spending-card",
    buildResearchSpendingSentence(p, s),
    "neutral"
  );

  applyStrip(
    "enrollment-change-card",
    asNumber(s.enrollment_pct_change_5yr) === null
      ? "Enrollment data are not available."
      : `Enrollment ${asNumber(s.enrollment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.enrollment_pct_change_5yr)))} ${fiveYearRangeText}.`,
    sentimentClass(s.enrollment_pct_change_5yr)
  );

  const enrollmentFlag = deriveEnrollmentFlag(s, series);
  setText("enrollment-flag", enrollmentFlag);
  styleAnswerCard("enrollment-flag", enrollmentFlag);

  applyStrip("intl-sentence-card", buildIntlSentence(s, series), "neutral");

  applyStrip(
    "intl-change-card",
    asNumber(s.international_enrollment_pct_change_5yr) === null
      ? "The number of international students is not available."
      : `The number of international students ${asNumber(s.international_enrollment_pct_change_5yr) >= 0 ? "increased" : "decreased"} ${fmtRoundedPct(Math.abs(asNumber(s.international_enrollment_pct_change_5yr)))} ${fiveYearRangeText}.`,
    "neutral"
  );

  const gradLoanSentence = buildGradLoanSentence(p, s);
  setHidden("grad-loan-intro", !gradLoanSentence);
  setHidden("loan-card", !gradLoanSentence);
  if (gradLoanSentence) {
    applyStrip("loan-card", gradLoanSentence, "neutral");
  }

  applyStrip(
    "staff-change-card",
    asNumber(s.staff_total_headcount_pct_change_5yr) === null
      ? "Staffing data are not available."
      : `Total staff headcount ${asNumber(s.staff_total_headcount_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.staff_total_headcount_pct_change_5yr)))} ${fiveYearRangeText}.`,
    sentimentClass(s.staff_total_headcount_pct_change_5yr)
  );

  applyStrip(
    "endowment-change-card",
    asNumber(s.endowment_pct_change_5yr) === null
      ? "Endowment data are not available."
      : `The institution's endowment ${asNumber(s.endowment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.endowment_pct_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.endowment_pct_change_5yr)
  );

  const hasFederal =
    (asNumber(s.federal_grants_contracts_pell_adjusted_pct_core_revenue) ?? 0) !== 0 ||
    asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) !== null ||
    hasMeaningfulData(series.federal_grants_contracts_pell_adjusted) ||
    hasMeaningfulData(series.federal_grants_contracts_pell_adjusted_adjusted);
  const hasState =
    (asNumber(s.state_funding_pct_core_revenue) ?? 0) !== 0 ||
    ((asNumber(s.state_funding_pct_change_5yr) ?? 0) !== 0) ||
    hasMeaningfulData(series.state_funding_adjusted);
  const hasResearchSpending = asNumber(s.research_expense_per_fte) !== null;

  setSectionVisibility("federal-group", hasFederal);
  setSectionVisibility("state-group", hasState);
  setHidden("research-aid-intro", !hasResearchSpending);
  setHidden("research-spending-card", !hasResearchSpending);
  setHidden("state-negative-note", !(hasState && hasNegativePoint(series.state_funding_adjusted)));

  const aidTitle = document.getElementById("aid-section-title");
  if (aidTitle) {
    if (hasFederal && hasState) aidTitle.textContent = "Federal And State Aid";
    else if (hasFederal) aidTitle.textContent = "Federal Aid";
    else if (hasState) aidTitle.textContent = "State Aid";
    else aidTitle.textContent = "Aid";
  }

  if (hasFederal) {
    applyStrip(
      "federal-share-card",
      `${fmtPlainPct(s.federal_grants_contracts_pell_adjusted_pct_core_revenue || 0)} of core revenue came from federal grants and contracts, excluding Pell grants.`,
      "neutral"
    );

    applyQuestionValueStrip(
      "federal-change-card",
      "How much have federal grants and contracts changed over the past 5 years?",
      asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) === null
        ? "No data"
        : fmtRoundedPct(s.federal_grants_contracts_pell_adjusted_pct_change_5yr, true),
      asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) === null ? "neutral" : sentimentClass(s.federal_grants_contracts_pell_adjusted_pct_change_5yr)
    );
  }

  if (hasState) {
    applyStrip(
      "state-share-card",
      `${fmtPlainPct(s.state_funding_pct_core_revenue || 0)} of core revenue came from state appropriations.`,
      "neutral"
    );

    applyQuestionValueStrip(
      "state-change-card",
      "How much has state aid changed over the past 5 years?",
      asNumber(s.state_funding_pct_change_5yr) === null
        ? "No data"
        : fmtRoundedPct(s.state_funding_pct_change_5yr, true),
      asNumber(s.state_funding_pct_change_5yr) === null ? "neutral" : sentimentClass(s.state_funding_pct_change_5yr)
    );
  }

  if (hasResearchSpending) {
    applyStrip(
      "research-spending-card",
      buildResearchSpendingSentence(p, s),
      "neutral"
    );
  }

  renderLineChart("chart-revenue", {
    title: "Revenue vs Expenses (adjusted for inflation)",
    format: "currency",
    showTooltip: false,
    series: [
      { label: "Revenue", color: "#005ab5", values: toSeries(series.revenue_total_adjusted) },
      { label: "Expenses", color: "#dc3220", values: toSeries(series.expenses_total_adjusted) }
    ]
  });

  renderLineChart("chart-net-tuition", {
    title: "Net Tuition Revenue over time (per full-time equivalent student, adjusted for inflation)",
    format: "currency",
    showTooltip: false,
    showLegend: false,
    series: [
      { label: "Net Tuition Revenue", color: "#005ab5", values: toSeries(series.net_tuition_per_fte_adjusted) }
    ]
  });

  renderLineChart("chart-enrollment", {
    title: "Enrollment trends (12-month unduplicated headcount)",
    format: "number",
    showLegend: false,
    series: [
      { label: "Enrollment", color: "#005ab5", values: toSeries(series.enrollment_headcount_total) }
    ]
  });

  renderLineChart("chart-international", {
    title: "International enrollment",
    format: "number",
    series: [
      { label: "International Student Total", color: "#005ab5", values: toSeries(series.enrollment_nonresident_total) },
      { label: "International Graduate Students", color: "#56b4e9", values: toSeries(series.enrollment_nonresident_graduate) },
      { label: "International Undergraduate Students", color: "#cc79a7", values: toSeries(series.enrollment_nonresident_undergrad) }
    ]
  });

  renderLineChart("chart-staffing", {
    title: "Staffing levels",
    format: "number",
    series: [
      { label: "Total Staff Headcount", color: "#005ab5", values: toSeries(series.staff_headcount_total) },
      { label: "Total Instructional Staff", color: "#dc3220", values: toSeries(series.staff_headcount_instructional) }
    ]
  });

  renderLineChart("chart-endowment", {
    title: "Endowment value over time",
    format: "currency",
    showTooltip: false,
    showLegend: false,
    series: [
      { label: "Endowment Value", color: "#005ab5", values: toSeries(series.endowment_value_adjusted) }
    ]
  });

  if (hasFederal) {
    renderLineChart("chart-federal", {
      title: "Revenue from federal grants and contracts (excluding Pell grants, adjusted for inflation)",
      format: "currency",
      showTooltip: false,
      showLegend: false,
      series: [
        {
          label: "Federal Grants",
          color: "#005ab5",
          values: toSeries(
            series.federal_grants_contracts_pell_adjusted ||
            series.federal_grants_contracts_pell_adjusted_adjusted
          )
        }
      ]
    });
  }

  if (hasState) {
    renderLineChart("chart-state", {
      title: "State government appropriations over time (adjusted for inflation)",
      format: "currency",
      showTooltip: false,
      showLegend: false,
      series: [
        { label: "State Funding", color: "#005ab5", values: toSeries(series.state_funding_adjusted) }
      ]
    });
  }
}

init().catch((error) => {
  console.error(error);
  setText("school-name", "This school page could not be loaded.");
});

