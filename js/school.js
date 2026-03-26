async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Failed to load ${path}`);
  return response.json();
}

function getParam(name) {
  return new URLSearchParams(window.location.search).get(name);
}

function fmtPct(value) {
  if (value === null || value === undefined || Number.isNaN(Number(value))) return "No data";
  const n = Number(value);
  const sign = n > 0 ? "+" : "";
  return `${sign}${n.toFixed(1)}%`;
}

function fmtMaybePct(value) {
  if (value === null || value === undefined || Number.isNaN(Number(value))) return "No data";
  return `${Number(value).toFixed(1)}%`;
}

function toSeries(values) {
  return (values || []).map((point) => ({
    year: Number(point.year),
    value: Number(point.value)
  }));
}

function setText(id, value) {
  const node = document.getElementById(id);
  if (node) node.textContent = value || "No data";
}

async function init() {
  const unitid = getParam("unitid");
  if (!unitid) {
    document.getElementById("school-name").textContent = "No school selected";
    return;
  }

  const school = await loadJson(`data/schools/${unitid}.json`);

  setText("school-name", school.profile.institution_name);
  setText(
    "school-subtitle",
    `${school.profile.city}, ${school.profile.state} · ${school.profile.control_label} · ${school.profile.urbanization}`
  );

  setText("revenue-change", fmtPct(school.summary.revenue_pct_change_5yr));
  setText("loss-latest", school.summary.ended_year_at_loss || "No data");
  setText("tuition-sentence", school.summary.tuition_dependence_vs_sector_median_sentence || "No data");

  setText("enrollment-change", fmtPct(school.summary.enrollment_pct_change_5yr));
  setText("intl-sentence", school.summary.international_students_sentence || "No data");

  setText("staff-change", fmtPct(school.summary.staff_total_headcount_pct_change_5yr));
  setText("endowment-change", fmtPct(school.summary.endowment_pct_change_5yr));
  setText("federal-share", fmtMaybePct(school.summary.federal_grants_contracts_pell_adjusted_pct_core_revenue));
  setText("state-share", fmtMaybePct(school.summary.state_funding_pct_core_revenue));

  renderLineChart("chart-revenue", {
    title: "Revenue vs Expenses (Adjusted Dollars)",
    series: [
      { label: "Revenue", color: "#2563eb", values: toSeries(school.series.revenue_total_adjusted) },
      { label: "Expenses", color: "#dc2626", values: toSeries(school.series.expenses_total_adjusted) }
    ]
  });

  renderLineChart("chart-net-tuition", {
    title: "Net Tuition Revenue Per FTE",
    series: [
      { label: "Net tuition per FTE", color: "#059669", values: toSeries(school.series.net_tuition_per_fte_adjusted) }
    ]
  });

  renderLineChart("chart-enrollment", {
    title: "Enrollment",
    series: [
      { label: "Total enrollment", color: "#2563eb", values: toSeries(school.series.enrollment_headcount_total) }
    ]
  });

  renderLineChart("chart-international", {
    title: "International Enrollment",
    series: [
      { label: "International students", color: "#7c3aed", values: toSeries(school.series.enrollment_nonresident_total) }
    ]
  });

  renderLineChart("chart-staffing", {
    title: "Staffing Levels",
    series: [
      { label: "Total staff", color: "#2563eb", values: toSeries(school.series.staff_headcount_total) },
      { label: "Instructional staff", color: "#f59e0b", values: toSeries(school.series.staff_headcount_instructional) }
    ]
  });

  renderLineChart("chart-endowment", {
    title: "Endowment Value (Adjusted Dollars)",
    series: [
      { label: "Endowment", color: "#0f766e", values: toSeries(school.series.endowment_value_adjusted) }
    ]
  });

  renderLineChart("chart-federal", {
    title: "Federal Grants And Contracts (Adjusted Dollars)",
    series: [
      { label: "Federal grants", color: "#2563eb", values: toSeries(school.series.federal_grants_contracts_pell_adjusted_adjusted) }
    ]
  });

  renderLineChart("chart-state", {
    title: "State Funding (Adjusted Dollars)",
    series: [
      { label: "State funding", color: "#dc2626", values: toSeries(school.series.state_funding_adjusted) }
    ]
  });
}

init().catch((error) => {
  console.error(error);
  document.getElementById("school-name").textContent = "This school page could not be loaded.";
});
