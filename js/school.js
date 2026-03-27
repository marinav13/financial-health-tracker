function getParam(name) {
  return new URLSearchParams(window.location.search).get(name);
}

function asNumber(value) {
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function fmtPct(value, digits = 1) {
  const n = asNumber(value);
  if (n === null) return "No data";
  const sign = n > 0 ? "+" : "";
  return `${sign}${n.toFixed(digits)}%`;
}

function fmtPlainPct(value, digits = 1) {
  const n = asNumber(value);
  if (n === null) return "No data";
  return `${n.toFixed(digits)}%`;
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

function sentimentClass(value) {
  const n = asNumber(value);
  if (n === null) return "neutral";
  if (n < 0) return "negative";
  if (n > 0) return "positive";
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

function buildIntlSentence(summary, series) {
  if (summary.international_students_sentence) return summary.international_students_sentence;
  const all = asNumber(summary.pct_international_all);
  const ug = asNumber(summary.pct_international_undergraduate);
  const grad = asNumber(summary.pct_international_graduate);

  if (all !== null && ug !== null && grad !== null) {
    return `${fmtPlainPct(all)} of students are international. That includes ${fmtPlainPct(ug)} of undergraduates and ${fmtPlainPct(grad)} of graduate students.`;
  }

  if (all !== null) {
    return `${fmtPlainPct(all)} of students are international.`;
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return `${fmtPlainPct(pct)} of students are international.`;
  }

  return "International student data are not available.";
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

function setSectionVisibility(id, show) {
  const node = document.getElementById(id);
  if (!node) return;
  node.classList.toggle("is-hidden", !show);
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
  const unitid = getParam("unitid");
  if (!unitid) {
    setText("school-name", "No school selected");
    return;
  }

  const school = await loadJson(`data/schools/${unitid}.json`);
  const p = school.profile;
  const s = school.summary;
  const series = school.series;

  setText("school-name", p.institution_name);
  setText("school-urbanization", p.urbanization);
  setText("school-control", p.sector);
  setText("school-category", p.category);

  applyStrip(
    "revenue-change-card",
    asNumber(s.revenue_pct_change_5yr) === null
      ? "Revenue data are not available."
      : `Revenue has ${asNumber(s.revenue_pct_change_5yr) < 0 ? "decreased" : "increased"} ${Math.abs(asNumber(s.revenue_pct_change_5yr)).toFixed(1)}% over the past five years.`,
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
      : `Net tuition revenue per student has ${asNumber(s.net_tuition_per_fte_change_5yr) < 0 ? "decreased" : "increased"} ${Math.abs(asNumber(s.net_tuition_per_fte_change_5yr)).toFixed(1)}% over the past five years.`,
    sentimentClass(s.net_tuition_per_fte_change_5yr)
  );

  applyStrip(
    "tuition-sentence-card",
    s.tuition_dependence_vs_sector_median_sentence || "No tuition dependence benchmark is available.",
    "neutral"
  );

  applyStrip(
    "enrollment-change-card",
    asNumber(s.enrollment_pct_change_5yr) === null
      ? "Enrollment data are not available."
      : `Enrollment has ${asNumber(s.enrollment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${Math.abs(asNumber(s.enrollment_pct_change_5yr)).toFixed(1)}% over the past five years.`,
    sentimentClass(s.enrollment_pct_change_5yr)
  );

  const enrollmentFlag = deriveEnrollmentFlag(s, series);
  setText("enrollment-flag", enrollmentFlag);
  styleAnswerCard("enrollment-flag", enrollmentFlag);

  applyStrip("intl-sentence-card", buildIntlSentence(s, series), "neutral");

  applyStrip(
    "intl-change-card",
    asNumber(s.international_student_count_change_5yr) === null
      ? "The number of international students is not available."
      : `The number of international students has ${asNumber(s.international_student_count_change_5yr) >= 0 ? "increased" : "decreased"} ${Math.abs(asNumber(s.international_student_count_change_5yr)).toFixed(1)}% over the past five years.`,
    asNumber(s.international_student_count_change_5yr) === null ? "neutral" : sentimentClass(s.international_student_count_change_5yr)
  );

  applyStrip(
    "loan-card",
    asNumber(s.federal_loan_pct_most_recent) === null
      ? "Federal loan data are not available."
      : `${fmtPlainPct(s.federal_loan_pct_most_recent, 0)} of undergraduates at this institution took out federal loans in the most recent year available.<br>${asNumber(s.sector_avg_federal_loan_pct_most_recent) === null ? "No sector benchmark is available." : `For ${p.control_label.toLowerCase()} colleges, the average was ${fmtPlainPct(s.sector_avg_federal_loan_pct_most_recent, 0)}.`}`,
    "neutral"
  );

  applyStrip(
    "staff-change-card",
    asNumber(s.staff_total_headcount_pct_change_5yr) === null
      ? "Staffing data are not available."
      : `Total staff headcount has ${asNumber(s.staff_total_headcount_pct_change_5yr) < 0 ? "decreased" : "increased"} ${Math.abs(asNumber(s.staff_total_headcount_pct_change_5yr)).toFixed(1)}% over the past five years.`,
    sentimentClass(s.staff_total_headcount_pct_change_5yr)
  );

  applyStrip(
    "endowment-change-card",
    asNumber(s.endowment_pct_change_5yr) === null
      ? "Endowment data are not available."
      : `The institution's endowment has ${asNumber(s.endowment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${Math.abs(asNumber(s.endowment_pct_change_5yr)).toFixed(1)}% over the past five years.`,
    sentimentClass(s.endowment_pct_change_5yr)
  );

  const hasFederal = asNumber(s.federal_grants_contracts_pell_adjusted_pct_core_revenue) !== null || hasData(series.federal_grants_contracts_pell_adjusted_adjusted);
  const hasState = asNumber(s.state_funding_pct_core_revenue) !== null || asNumber(s.state_funding_pct_change_5yr) !== null || hasData(series.state_funding_adjusted);

  setSectionVisibility("federal-group", hasFederal);
  setSectionVisibility("state-group", hasState);

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
        : fmtPct(s.federal_grants_contracts_pell_adjusted_pct_change_5yr, 0),
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
        : fmtPct(s.state_funding_pct_change_5yr, 0),
      asNumber(s.state_funding_pct_change_5yr) === null ? "neutral" : sentimentClass(s.state_funding_pct_change_5yr)
    );
  }

  renderLineChart("chart-revenue", {
    title: "Revenue vs Expenses",
    series: [
      { label: "Revenue", color: "#009e73", values: toSeries(series.revenue_total_adjusted) },
      { label: "Expenses", color: "#d55e00", values: toSeries(series.expenses_total_adjusted) }
    ]
  });

  renderLineChart("chart-net-tuition", {
    title: "Net Tuition Revenue over time (per full-time equivalent student)",
    series: [
      { label: "Net Tuition Revenue", color: "#0072b2", values: toSeries(series.net_tuition_per_fte_adjusted) }
    ]
  });

  renderLineChart("chart-enrollment", {
    title: "Enrollment trends (12-month unduplicated headcount)",
    series: [
      { label: "Enrollment", color: "#0072b2", values: toSeries(series.enrollment_headcount_total) }
    ]
  });

  renderLineChart("chart-international", {
    title: "International enrollment",
    series: [
      { label: "International Student Total", color: "#0072b2", values: toSeries(series.enrollment_nonresident_total) },
      { label: "International Graduate Students", color: "#56b4e9", values: toSeries(series.enrollment_nonresident_graduate) },
      { label: "International Undergraduate Students", color: "#cc79a7", values: toSeries(series.enrollment_nonresident_undergrad) }
    ]
  });

  renderLineChart("chart-staffing", {
    title: "Staffing levels",
    series: [
      { label: "Total Staff Headcount", color: "#0072b2", values: toSeries(series.staff_headcount_total) },
      { label: "Total Instructional Staff", color: "#e69f00", values: toSeries(series.staff_headcount_instructional) }
    ]
  });

  renderLineChart("chart-endowment", {
    title: "Endowment value over time",
    series: [
      { label: "Endowment Value", color: "#0072b2", values: toSeries(series.endowment_value_adjusted) }
    ]
  });

  if (hasFederal) {
    renderLineChart("chart-federal", {
      title: "Revenue from federal grants and contracts (excluding Pell grants)",
      series: [
        { label: "Federal Grants", color: "#0072b2", values: toSeries(series.federal_grants_contracts_pell_adjusted_adjusted) }
      ]
    });
  }

  if (hasState) {
    renderLineChart("chart-state", {
      title: "State government appropriations over time",
      series: [
        { label: "State Funding", color: "#0072b2", values: toSeries(series.state_funding_adjusted) }
      ]
    });
  }
}

init().catch((error) => {
  console.error(error);
  setText("school-name", "This school page could not be loaded.");
});

