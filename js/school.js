function getParam(name) {
  return new URLSearchParams(window.location.search).get(name);
}

const SHOW_CLOSURE_FLAGS = false;
const CHART_COLOR_PRIMARY = "#005ab5";
const CHART_COLOR_SECONDARY = "#e69f00";
const CHART_COLOR_TERTIARY = "#009e73";

function asNumber(value) {
  if (value === null || value === undefined || value === "") return null;
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

function fmtNumber(value, digits = 1) {
  const n = asNumber(value);
  if (n === null) return "No data";
  return new Intl.NumberFormat("en-US", {
    minimumFractionDigits: 0,
    maximumFractionDigits: digits
  }).format(n);
}

const setText = window.TrackerApp.setText;

async function loadJsonOrNull(path) {
  try {
    return await loadJson(path);
  } catch (error) {
    console.warn(`Optional data file could not be loaded: ${path}`, error);
    return null;
  }
}

const SCHOOL_SOURCE_URLS = {
  ipeds: "https://nces.ed.gov/ipeds/use-the-data"
};

const IPEDS_COMPONENT_LABELS = {
  finance: "Finance Component",
  "fall enrollment": "Fall Enrollment Component",
  "12-month enrollment": "12-month Enrollment Component",
  "human resources": "Human Resources Component"
};

function formatSourceDate(value) {
  if (!value) return "the latest update";
  const parsed = new Date(`${value}T00:00:00`);
  if (Number.isNaN(parsed.getTime())) return value;
  return new Intl.DateTimeFormat("en-US", {
    month: "long",
    day: "numeric",
    year: "numeric",
    timeZone: "America/New_York"
  }).format(parsed);
}

function formatIpedsCollectionYear(year) {
  const numericYear = Number(year);
  if (!Number.isFinite(numericYear)) return String(year || "latest");
  const priorYear = numericYear - 1;
  const trailingYear = String(numericYear).slice(-2);
  return `${priorYear}-${trailingYear}`;
}

function normalizeIpedsComponentLabel(value) {
  const key = String(value || "").trim().toLowerCase();
  return IPEDS_COMPONENT_LABELS[key] || String(value || "").trim();
}

function createSourceLink(url) {
  try {
    const parsed = new URL(url);
    if (!/^https?:$/.test(parsed.protocol)) return document.createTextNode(url);
    const link = document.createElement("a");
    link.href = parsed.href;
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.textContent = parsed.href;
    return link;
  } catch (_error) {
    return document.createTextNode(String(url || ""));
  }
}

function createSourceCitation(textBeforeUrl, url, textAfterUrl = ".") {
  const sentence = document.createElement("span");
  sentence.append(document.createTextNode(textBeforeUrl));
  sentence.append(createSourceLink(url));
  sentence.append(document.createTextNode(textAfterUrl));
  return sentence;
}

function createIpedsCitation(collectionYear, surveyComponent, retrievedAt) {
  return createSourceCitation(
    `U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), ${formatIpedsCollectionYear(collectionYear)}, ${normalizeIpedsComponentLabel(surveyComponent)}, Retrieved ${formatSourceDate(retrievedAt)}, from `,
    SCHOOL_SOURCE_URLS.ipeds,
    "."
  );
}

function setOutcomesGridLayout(visibleCount) {
  const grid = document.getElementById("school-outcomes-grid");
  if (!grid) return;
  grid.classList.remove("outcomes-count-1", "outcomes-count-2", "outcomes-count-3");
  const normalizedCount = visibleCount >= 3 ? 3 : Math.max(1, visibleCount);
  grid.classList.add(`outcomes-count-${normalizedCount}`);
}

function upsertSectionSourceNote(sectionId, citations) {
  const section = document.getElementById(sectionId);
  if (!section) return;

  const existing = section.querySelector(".graphic-source");
  if (existing) existing.remove();

  const sourceItems = Array.isArray(citations) ? citations.filter(Boolean) : [];
  if (!sourceItems.length) return;

  const note = document.createElement("p");
  note.className = "graphic-source";

  const label = document.createElement("strong");
  label.textContent = sourceItems.length > 1 ? "Sources: " : "Source: ";
  note.append(label);

  sourceItems.forEach((citation, index) => {
    if (index > 0) {
      note.append(document.createTextNode(" "));
    }
    note.append(citation);
  });

  section.append(note);
}

function moveChartNoteBelowSource(noteId, sectionId, show) {
  const note = document.getElementById(noteId);
  const section = document.getElementById(sectionId);
  if (!note || !section) return;

  note.classList.toggle("is-hidden", !show);
  note.setAttribute("aria-hidden", show ? "false" : "true");
  if (!show) return;

  const sourceNote = section.querySelector(".graphic-source");
  if (sourceNote?.nextSibling) {
    section.insertBefore(note, sourceNote.nextSibling);
  } else {
    section.appendChild(note);
  }
}

function toSeries(values) {
  return (values || [])
    .filter((point) => point != null && point.year != null && point.value != null)
    .map((point) => ({
      year: Number(point.year),
      value: Number(point.value)
    }))
    .filter((point) => Number.isFinite(point.year) && Number.isFinite(point.value));
}

function latestPoint(values) {
  const series = toSeries(values);
  return series.length ? series[series.length - 1] : null;
}

function latestYearFromSeries(series) {
  const years = Object.values(series || {})
    .flatMap((values) => toSeries(values).map((point) => point.year))
    .filter((year) => Number.isFinite(year));
  return years.length ? Math.max(...years) : null;
}

function yearPhrase(year) {
  return Number.isFinite(year) ? `in ${year}` : "in the latest year";
}

function hasNegativePoint(values) {
  return toSeries(values).some((point) => point.value < 0);
}

function recentFiveYearRangeText(seriesValues) {
  const values = toSeries(seriesValues);
  if (values.length === 0) return "over the most recent available period";
  if (values.length < 6) {
    const start = values[0].year;
    const end = values[values.length - 1].year;
    return start === end ? `in ${end}` : `from ${start} to ${end}`;
  }
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
  const normalized = String(value).trim().toLowerCase();
  if (value === true || value === 1 || normalized === "yes" || normalized === "1") return "negative";
  if (value === false || value === 0 || normalized === "no" || normalized === "0") return "positive";
  return "neutral";
}

function applyStrip(id, text, state = "neutral") {
  const node = document.getElementById(id);
  if (!node) return;
  node.className = `metric-strip ${state}`;
  node.textContent = "";
  const statement = document.createElement("div");
  statement.className = "metric-statement";
  statement.textContent = text ?? "";
  node.appendChild(statement);
}

function applyQuestionValueStrip(id, question, value, state = "neutral") {
  const node = document.getElementById(id);
  if (!node) return;
  node.className = `metric-strip ${state}`;
  node.textContent = "";
  const statement = document.createElement("div");
  statement.className = "metric-statement";
  statement.append(document.createTextNode(question ?? ""));
  statement.append(document.createElement("br"));
  const strong = document.createElement("strong");
  strong.textContent = value ?? "";
  statement.append(strong);
  node.appendChild(statement);
}

function strongSegment(text) {
  return { strong: text };
}

function setBodyCopy(id, paragraphs) {
  const node = document.getElementById(id);
  if (!node) return;
  node.replaceChildren();

  const entries = Array.isArray(paragraphs) ? paragraphs.filter(Boolean) : [];
  if (!entries.length) {
    node.classList.add("is-hidden");
    node.setAttribute("aria-hidden", "true");
    return;
  }

  entries.forEach((paragraph) => {
    const p = document.createElement("p");
    p.className = "section-copy";
    const segments = Array.isArray(paragraph) ? paragraph : [paragraph];
    segments.forEach((segment) => {
      if (segment && typeof segment === "object" && Object.prototype.hasOwnProperty.call(segment, "strong")) {
        const strong = document.createElement("strong");
        strong.textContent = segment.strong ?? "";
        p.appendChild(strong);
      } else {
        p.append(document.createTextNode(String(segment ?? "")));
      }
    });
    node.appendChild(p);
  });

  node.classList.remove("is-hidden");
  node.setAttribute("aria-hidden", "false");
}

function setHidden(id, hidden) {
  const node = document.getElementById(id);
  if (!node) return;
  node.classList.toggle("is-hidden", Boolean(hidden));
  node.setAttribute("aria-hidden", hidden ? "true" : "false");
}

function setEnrollmentTotal(id, latestEnrollment) {
  const node = document.getElementById(id);
  if (!node) return;
  node.replaceChildren();
  if (!latestEnrollment) return;
  node.append(`As of ${latestEnrollment.year}, this institution reported a headcount of `);
  const value = document.createElement("strong");
  value.textContent = fmtNumber(latestEnrollment.value, 0);
  node.append(value);
  node.append(" students.");
}

function setClosestMetricHidden(id, hidden) {
  const node = document.getElementById(id);
  const metric = node?.closest(".metric-strip");
  if (!metric) return;
  metric.classList.toggle("is-hidden", Boolean(hidden));
}

function hasIndexedRelatedRecord(record, countField) {
  if (!record) return false;
  const count = asNumber(record[countField]);
  return count === null ? true : count > 0;
}

function findRelatedIndexRecord(index, unitid, countField) {
  const numericUnitid = String(unitid || "");
  if (!numericUnitid) return null;
  const direct = index?.[numericUnitid];
  if (hasIndexedRelatedRecord(direct, countField)) return direct;
  return Object.values(index || {}).find((record) =>
    String(record?.financial_unitid || "") === numericUnitid &&
    hasIndexedRelatedRecord(record, countField)
  ) || null;
}

function renderSchoolRelatedPages(unitid, relatedIndexes = {}) {
  const section = document.getElementById("school-related-section");
  const container = document.getElementById("school-related-pages");
  if (!section || !container) return;

  const relatedPages = [
    {
      label: "College Cuts",
      page: "cuts.html",
      record: findRelatedIndexRecord(relatedIndexes.cuts, unitid, "cut_count")
    },
    {
      label: "Accreditation",
      page: "accreditation.html",
      record: findRelatedIndexRecord(relatedIndexes.accreditation, unitid, "action_count")
    },
    {
      label: "Research Funding Cuts",
      page: "research.html",
      record: findRelatedIndexRecord(relatedIndexes.research, unitid, "total_disrupted_grants")
    }
  ].filter((relatedPage) => relatedPage.record);

  if (!relatedPages.length) {
    container.replaceChildren();
    setSectionVisibility("school-related-section", false);
    return;
  }

  // Editorial Calm: school.html's #school-related-section is now an
  // <aside class="related-links"> containing a static
  // <p><strong>Explore this institution:</strong></p> heading and a
  // sibling <ul id="school-related-pages" class="link-list">. We only
  // need to populate the <ul> with one <li> per related section —
  // matching the pattern emitted by app.js's renderRelatedInstitution-
  // Links so all four institution-mode pages share visual treatment.
  container.replaceChildren();
  relatedPages.forEach((relatedPage) => {
    const relatedUnitid = relatedPage.record.unitid || unitid;
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = `${relatedPage.page}?unitid=${encodeURIComponent(relatedUnitid)}`;
    link.textContent = relatedPage.label;
    item.appendChild(link);
    container.appendChild(item);
  });
  setSectionVisibility("school-related-section", true);
}

function buildIntlSentence(summary, series, latestDataYear) {
  const all = asNumber(summary.pct_international_all);
  const ug = asNumber(summary.pct_international_undergraduate);
  const grad = asNumber(summary.pct_international_graduate);
  const latestYear = latestPoint(series.enrollment_headcount_total)?.year || latestDataYear;
  const prefixLatestYear = Number.isFinite(latestYear) ? `In ${latestYear}, ` : "In the latest year, ";

  if (summary.international_students_sentence && /^In \d{4},/i.test(summary.international_students_sentence)) {
    return summary.international_students_sentence;
  }

  if (all !== null && ug !== null && grad !== null) {
    return `${prefixLatestYear}${fmtRoundedPct(all)} of students were international. That includes ${fmtRoundedPct(ug)} of undergraduates and ${fmtRoundedPct(grad)} of graduate students.`;
  }

  if (all !== null) {
    return `${prefixLatestYear}${fmtRoundedPct(all)} of students were international.`;
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return `${prefixLatestYear}${fmtRoundedPct(pct)} of students were international.`;
  }

  return "International student data are not available.";
}

function buildIntlSentenceParagraph(summary, series, latestDataYear) {
  const all = asNumber(summary.pct_international_all);
  const ug = asNumber(summary.pct_international_undergraduate);
  const grad = asNumber(summary.pct_international_graduate);
  const latestYear = latestPoint(series.enrollment_headcount_total)?.year || latestDataYear;
  const prefixLatestYear = Number.isFinite(latestYear) ? `In ${latestYear}, ` : "In the latest year, ";

  if (all !== null && ug !== null && grad !== null) {
    return [
      prefixLatestYear,
      strongSegment(`${fmtRoundedPct(all)} of students were international`),
      `. That includes ${fmtRoundedPct(ug)} of undergraduates and ${fmtRoundedPct(grad)} of graduate students.`
    ];
  }

  if (all !== null) {
    return [
      prefixLatestYear,
      strongSegment(`${fmtRoundedPct(all)} of students were international`),
      "."
    ];
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return [
      prefixLatestYear,
      strongSegment(`${fmtRoundedPct(pct)} of students were international`),
      "."
    ];
  }

  return null;
}

function buildResearchSpendingSentence(profile, summary, latestDataYear) {
  const perFte = asNumber(summary.research_expense_per_fte);
  const sectorLabel = String(profile.control_label || profile.sector || "").toLowerCase();
  const shareOfCoreExpenses = asNumber(summary.research_expense_pct_core_expenses);
  const sectorMedian = asNumber(summary.sector_median_research_expense_per_fte_positive);
  const reportingShare = asNumber(summary.sector_research_spending_reporting_share_pct);
  const latestYearPhrase = yearPhrase(latestDataYear);

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
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time student on research ${latestYearPhrase}. That is ${medianComparison} the median of ${fmtCurrency(sectorMedian)} for the ${fmtRoundedPct(reportingShare)} of ${sectorPhrase} who reported research spending.`;
  }

  if (shareOfCoreExpenses !== null) {
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time student on research ${latestYearPhrase}.`;
  }

  return `This institution spent about ${fmtCurrency(perFte)} per full-time student on research ${latestYearPhrase}.`;
}

function buildResearchSpendingParagraph(profile, summary, latestDataYear) {
  const perFte = asNumber(summary.research_expense_per_fte);
  const sectorLabel = String(profile.control_label || profile.sector || "").toLowerCase();
  const shareOfCoreExpenses = asNumber(summary.research_expense_pct_core_expenses);
  const sectorMedian = asNumber(summary.sector_median_research_expense_per_fte_positive);
  const reportingShare = asNumber(summary.sector_research_spending_reporting_share_pct);
  const yearLabel = Number.isFinite(latestDataYear) ? latestDataYear : "the latest year";

  if (perFte === null) return null;

  let medianComparison = null;
  if (sectorMedian !== null) {
    if (sectorMedian === 0) {
      medianComparison = "about";
    } else {
      const pctDiff = Math.abs(perFte - sectorMedian) / sectorMedian;
      if (pctDiff <= 0.05) medianComparison = "about";
      else if (perFte > sectorMedian) medianComparison = "above";
      else medianComparison = "below";
    }
  }

  if (shareOfCoreExpenses !== null && sectorMedian !== null && reportingShare !== null) {
    const sectorPhrase = sectorLabel ? `${sectorLabel} colleges` : "colleges in the same sector";
    return [
      "Research expenses accounted for ",
      strongSegment(`${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses`),
      " at this institution, which spent ",
      strongSegment(`${fmtCurrency(perFte)} per full-time student`),
      ` on research in ${yearLabel}. That is ${medianComparison} the median of ${fmtCurrency(sectorMedian)} for the ${fmtRoundedPct(reportingShare)} of ${sectorPhrase} who reported research spending.`
    ];
  }

  if (shareOfCoreExpenses !== null) {
    return [
      "Research expenses accounted for ",
      strongSegment(`${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses`),
      " at this institution, which spent ",
      strongSegment(`${fmtCurrency(perFte)} per full-time student`),
      ` on research in ${yearLabel}.`
    ];
  }

  return [
    "This institution spent ",
    strongSegment(`${fmtCurrency(perFte)} per full-time student`),
    ` on research in ${yearLabel}.`
  ];
}

function buildTuitionDependenceSentence(profile, summary, latestDataYear) {
  const tuitionDependence = asNumber(summary.tuition_dependence_pct);
  const sectorMedian = asNumber(summary.sector_median_tuition_dependence_pct);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  const latestYearPhrase = yearPhrase(latestDataYear);

  if (tuitionDependence === null) {
    return summary.tuition_dependence_vs_sector_median_sentence || "No tuition dependence benchmark is available.";
  }

  if (sectorMedian !== null && sectorLabel) {
    const relation = tuitionDependence >= sectorMedian ? "above" : "below";
    return `This college got ${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition ${latestYearPhrase}, ${relation} the median of ${fmtRoundedPct(sectorMedian)} for ${sectorLabel} colleges.`;
  }

  return `This college got ${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition ${latestYearPhrase}.`;
}

function buildTuitionDependenceParagraph(profile, summary, latestDataYear) {
  const tuitionDependence = asNumber(summary.tuition_dependence_pct);
  const sectorMedian = asNumber(summary.sector_median_tuition_dependence_pct);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  const yearLabel = Number.isFinite(latestDataYear) ? latestDataYear : "the latest year";

  if (tuitionDependence === null) return null;

  if (sectorMedian !== null && sectorLabel) {
    const relation = tuitionDependence >= sectorMedian ? "above" : "below";
    return [
      "This college got ",
      strongSegment(`${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition`),
      ` in ${yearLabel}, ${relation} the median of ${fmtRoundedPct(sectorMedian)} for ${sectorLabel} colleges.`
    ];
  }

  return [
    "This college got ",
    strongSegment(`${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition`),
    ` in ${yearLabel}.`
  ];
}

function isPrivateNotForProfitProfile(profile) {
  return String(profile?.control_label || "").trim().toLowerCase() === "private not-for-profit";
}

function discountRateState(discountRate) {
  const rate = asNumber(discountRate);
  if (rate === null) return "neutral";
  if (rate >= 50) return "negative";
  if (rate >= 35) return "neutral";
  return "positive";
}

function discountRateTrendState(change) {
  const delta = asNumber(change);
  if (delta === null) return "neutral";
  if (delta >= 5) return "negative";
  if (delta <= -5) return "positive";
  return "neutral";
}

function lossYearsState(value) {
  const count = asNumber(value);
  if (count === null) return "neutral";
  if (count >= 5) return "negative";
  if (count <= 1) return "positive";
  return "neutral";
}

function getDiscountRateChange(summary, series, latestDataYear) {
  const latestSeriesYear = Number.isFinite(latestDataYear) ? latestDataYear : latestPoint(series.unfunded_discount_rate)?.year ?? null;
  if (latestSeriesYear !== null) {
    const latestRate = asNumber(summary.unfunded_discount_rate) ?? latestPoint(series.unfunded_discount_rate)?.value ?? null;
    const priorPoint = findPointByYear(series.unfunded_discount_rate, latestSeriesYear - 5);
    if (latestRate !== null && priorPoint) {
      return latestRate - priorPoint.value;
    }
  }
  return asNumber(summary.unfunded_discount_pct_change_5yr);
}

function findPointByYear(values, year) {
  const numericYear = Number(year);
  return toSeries(values).find((point) => point.year === numericYear) || null;
}

function buildDiscountRateSentence(profile, summary, series, latestDataYear) {
  if (!isPrivateNotForProfitProfile(profile)) return null;

  const institutionName = profile?.institution_name || "This institution";
  const latestRate = asNumber(summary.unfunded_discount_rate) ?? latestPoint(series.unfunded_discount_rate)?.value ?? null;
  const latestYear = Number.isFinite(latestDataYear) ? latestDataYear : latestPoint(series.unfunded_discount_rate)?.year ?? null;
  if (latestRate === null || latestYear === null) return null;

  const priorYear = latestYear - 5;
  const priorPoint = findPointByYear(series.unfunded_discount_rate, priorYear);
  if (priorPoint) {
    const delta = latestRate - priorPoint.value;
    if (Math.abs(delta) < 0.25) {
      return `${institutionName} had a discount rate of ${fmtRoundedPct(latestRate)} in ${latestYear}, about the same as ${fmtRoundedPct(priorPoint.value)} in ${priorYear}.`;
    }
    return `${institutionName} had a discount rate of ${fmtRoundedPct(latestRate)} in ${latestYear}, ${delta > 0 ? "up" : "down"} from ${fmtRoundedPct(priorPoint.value)} in ${priorYear}.`;
  }

  const change = asNumber(summary.unfunded_discount_pct_change_5yr);
  if (change === null) {
    return `${institutionName} had a discount rate of ${fmtRoundedPct(latestRate)} in ${latestYear}.`;
  }

  const priorRate = latestRate - change;
  if (Math.abs(change) < 0.25) {
    return `${institutionName} had a discount rate of ${fmtRoundedPct(latestRate)} in ${latestYear}, about the same as ${fmtRoundedPct(priorRate)} five years earlier.`;
  }

  return `${institutionName} had a discount rate of ${fmtRoundedPct(latestRate)} in ${latestYear}, ${change > 0 ? "up" : "down"} from ${fmtRoundedPct(priorRate)} five years earlier.`;
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
      `On average, graduate students who took out Grad PLUS loans at this institution borrowed about ${fmtCurrency(gradPlusPerRecipient)} in the most recent year, compared to ${fmtCurrency(sectorGradPlusMedian)} at other ${sectorLabel} institutions.`
    );
  }

  return sentences.join(" ");
}

function buildGradLoanParagraphs(profile, summary) {
  const paragraphs = [];
  const gradShare = asNumber(summary.share_grad_students);
  const sectorGradShare = asNumber(summary.sector_avg_share_grad_students);
  const gradPlusPerRecipient = asNumber(summary.grad_plus_disbursements_per_recipient);
  const sectorGradPlusMedian = asNumber(summary.sector_median_grad_plus_disbursements_per_recipient);
  const sectorLabel = String(profile.control_label || "").toLowerCase();

  if (gradShare !== null && sectorGradShare !== null && sectorLabel) {
    paragraphs.push([
      "At this institution, ",
      strongSegment(`${fmtPlainPct(gradShare, 0)} of students are graduate students.`),
      ` That's compared to ${fmtPlainPct(sectorGradShare, 0)} at other ${sectorLabel} institutions.`
    ]);
  }

  if (gradPlusPerRecipient !== null && sectorGradPlusMedian !== null && sectorLabel) {
    paragraphs.push([
      "On average, ",
      strongSegment(`graduate students who took out Grad PLUS loans at this institution borrowed ${fmtCurrency(gradPlusPerRecipient)}`),
      ` in the most recent year, compared to ${fmtCurrency(sectorGradPlusMedian)} at other ${sectorLabel} institutions.`
    ]);
  }

  return paragraphs;
}

function buildInstructionalStaffRatioSentence(profile, summary, latestDataYear) {
  const ratio = asNumber(summary.students_per_instructional_staff_fte);
  const benchmark = asNumber(summary.sector_median_students_per_instructional_staff_fte);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  if (ratio === null || benchmark === null || !sectorLabel) return null;
  const prefix = Number.isFinite(latestDataYear) ? `In ${latestDataYear}` : "In the latest year";
  return `${prefix}, this institution had about ${fmtNumber(ratio)} students per 1 instructional staff member, compared with the sector median of ${fmtNumber(benchmark)} at ${sectorLabel} colleges. This ratio uses full-time-equivalent students and staff so colleges with different mixes of full-time and part-time students can be compared more fairly.`;
}

function buildInstructionalStaffRatioParagraph(profile, summary, latestDataYear) {
  const ratio = asNumber(summary.students_per_instructional_staff_fte);
  const benchmark = asNumber(summary.sector_median_students_per_instructional_staff_fte);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  if (ratio === null || benchmark === null || !sectorLabel) return null;
  const prefix = Number.isFinite(latestDataYear) ? `In ${latestDataYear}, this institution had ` : "In the latest year, this institution had ";
  return [
    prefix,
    strongSegment(`${fmtNumber(ratio)} students per 1 instructional staff member`),
    `, compared with the sector median of ${fmtNumber(benchmark)} at ${sectorLabel} colleges. This ratio uses full-time-equivalent students and staff so colleges with different mixes of full-time and part-time students can be compared more fairly.`
  ];
}

function federalCompositeState(score) {
  const n = asNumber(score);
  if (n === null) return "neutral";
  if (n > 1.5) return "positive";
  if (n === 1.5) return "neutral";
  return "negative";
}

function buildFederalCompositeSentence(composite) {
  if (!composite) return null;
  const score = asNumber(composite.federal_composite_score_2022_2023);
  const yearLabel = composite.federal_composite_score_year_label || "2022-23";
  if (score === null) return null;

  if (score > 1.5) {
    return `In ${yearLabel}, this institution received a federal composite financial score of ${fmtNumber(score, 1)}. That is above the 1.5 threshold the federal government uses to consider an institution financially responsible.`;
  }
  if (score === 1.5) {
    return `In ${yearLabel}, this institution received a federal composite financial score of ${fmtNumber(score, 1)}. That meets the 1.5 threshold the federal government uses to consider an institution financially responsible.`;
  }
  if (score >= 1.0) {
    return `In ${yearLabel}, this institution received a federal composite financial score of ${fmtNumber(score, 1)}. That falls in the federal oversight range from 1.0 to less than 1.5.`;
  }
  return `In ${yearLabel}, this institution received a federal composite financial score of ${fmtNumber(score, 1)}. A score below 1.0 means the federal government does not consider the institution financially responsible without additional safeguards.`;
}

function hcm2State(record) {
  if (!record) return "neutral";
  return record.on_latest_snapshot ? "negative" : "neutral";
}

function buildHcm2Sentence(profile, record, hcmLookup) {
  if (!record) return null;

  const latestLabel = hcmLookup?.summary?.latest_snapshot_label || "December 2025";
  const institutionName = profile.institution_name || record.institution_name || "This institution";
  const reason = String(record.latest_reason_on_description || "").trim();
  const reasonSentence = reason
    ? ` The most recent federal reason listed was: ${reason}.`
    : "";

  if (record.downgraded_to_hcm1_after_hcm2) {
    return `In ${record.first_hcm2_snapshot_before_downgrade_label}, ${institutionName} was on heightened cash monitoring level 2.${reasonSentence} By ${record.first_hcm1_snapshot_after_hcm2_label}, this institution was moved to the lower level of oversight, called heightened cash monitoring level 1.`;
  }

  if (record.on_latest_snapshot) {
    if (record.first_snapshot_label === latestLabel) {
      return `${institutionName} was on heightened cash monitoring level 2 as of ${latestLabel}.${reasonSentence}`;
    }
    return `${institutionName} was on heightened cash monitoring level 2 as of ${record.first_snapshot_label}. This institution is still on the list as of ${latestLabel}.${reasonSentence}`;
  }

  if (record.first_snapshot_absent_after_last_presence) {
    return `${institutionName} was on heightened cash monitoring level 2 as of ${record.first_snapshot_label}. This institution is no longer on the list as of ${record.first_snapshot_absent_after_last_presence}.${reasonSentence}`;
  }

  return `${institutionName} was on heightened cash monitoring level 2 as of ${record.latest_snapshot_label_present}.${reasonSentence}`;
}

function buildClosureSentence(closureRecord) {
  if (!closureRecord) return null;
  const closeDate = String(closureRecord.close_date || "").trim();
  if (closeDate) {
    const parsed = new Date(`${closeDate}T00:00:00`);
    if (!Number.isNaN(parsed.getTime())) {
      const formatted = parsed.toLocaleDateString("en-US", {
        month: "long",
        day: "numeric",
        year: "numeric"
      });
      return `This institution closed as of ${formatted} according to federal data.`;
    }
    return `This institution closed as of ${closeDate} according to federal data.`;
  }
  const year = asNumber(closureRecord.close_year);
  if (year === null) return null;
  return `This institution closed as of ${Math.round(year)} according to federal data.`;
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
  if (show) {
    node.removeAttribute("aria-hidden");
  } else {
    node.setAttribute("aria-hidden", "true");
  }
}

function showSchoolLoadError(message) {
  setText("school-name", message);
  setText("school-location", "");
  setText("school-urbanization", "");
  setText("school-control", "");
  setText("school-closure-flag", "");
  setHidden("school-closure-flag", true);
  setHidden("school-intro-callout", false);
  setHidden("school-meta-wrap", true);
  setSectionVisibility("school-outcomes-section", false);
  setSectionVisibility("financial-section", false);
  setSectionVisibility("enrollment-section", false);
  setSectionVisibility("graduate-section", false);
  setSectionVisibility("staffing-section", false);
  setSectionVisibility("endowment-section", false);
  setSectionVisibility("aid-section", false);
  setSectionVisibility("federal-composite-section", false);
  setSectionVisibility("hcm2-section", false);
  setSectionVisibility("school-related-section", false);
  setHidden("download-school-data", true);
  setHidden("share-school-profile", true);
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

  window.TrackerApp.downloadRowsCsv(
    `${slugify(school.profile?.institution_name)}-displayed-data.csv`,
    rows[0],
    rows.slice(1)
  );
}

function schoolProfileUrl(unitid) {
  const url = new URL("school.html", window.location.href);
  url.search = "";
  url.searchParams.set("unitid", unitid);
  return url.href;
}

async function shareSchoolProfile(school, unitid) {
  const status = document.getElementById("share-school-status");
  const name = school.profile?.institution_name || "this college";
  const url = schoolProfileUrl(unitid);
  const shareData = {
    title: `${name} profile`,
    text: `View ${name}'s College Financial Health Explorer profile:`,
    url
  };

  if (navigator.share) {
    try {
      await navigator.share(shareData);
      if (status) status.textContent = "Share options opened.";
      return;
    } catch (error) {
      if (error?.name === "AbortError") return;
      console.warn("Native share failed; falling back to link copy/email.", error);
    }
  }

  const subject = encodeURIComponent(`${name} profile`);
  const body = encodeURIComponent(`${shareData.text}\n\n${url}`);
  window.location.href = `mailto:?subject=${subject}&body=${body}`;
  if (status) status.textContent = "Email share opened.";
}

function syncSearchToggle() {
  // Search stays expanded at every viewport — the previous mobile
  // behavior collapsed the panel behind a <summary> dropdown the user
  // had to tap to reach the input. The mobile CSS now hides that
  // summary and keeps .search-panel visible, so we always force the
  // <details> element open here too. This still uses .school-search-wrap
  // as the lookup so the no-op safely returns when other pages call it.
  const wrap = document.getElementById("school-search-wrap");
  if (!wrap) return;
  wrap.setAttribute("open", "");
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
  window.TrackerApp.syncTabs(unitid, { active: "finances" });
  setHidden("school-intro-callout", Boolean(unitid));
  setHidden("school-meta-wrap", false);
  if (!unitid) {
    showSchoolLoadError("No school selected");
    return;
  }

  const [school, compositeLookup, hcmLookup, closureLookup, cutsIndex, accreditationIndex, researchIndex] = await Promise.all([
    loadJson(`data/schools/${unitid}.json`),
    loadJsonOrNull("data/federal_composite_scores_by_unitid.json"),
    loadJsonOrNull("data/hcm2_by_unitid.json"),
    SHOW_CLOSURE_FLAGS ? loadJsonOrNull("data/closure_status_by_unitid.json") : Promise.resolve(null),
    loadJsonOrNull("data/college_cuts_index.json"),
    loadJsonOrNull("data/accreditation_index.json"),
    loadJsonOrNull("data/research_funding_index.json")
  ]);
  const p = school.profile;
  const s = school.summary;
  const series = school.series;
  const latestDataYear = asNumber(s.latest_year) || latestYearFromSeries(series);
  const composite = compositeLookup?.schools?.[unitid] || null;
  const hcmRecord = hcmLookup?.schools?.[unitid] || null;
  const closureRecord = SHOW_CLOSURE_FLAGS ? closureLookup?.schools?.[unitid] || null : null;
  const schoolRetrievedAt = school.generated_at || null;
  const graduationRate = asNumber(s.graduation_rate_6yr);
  const medianEarnings = asNumber(s.median_earnings_10yr);
  const medianDebt = asNumber(s.median_debt_completers);
  const fiveYearRangeText = recentFiveYearRangeText(series.revenue_total_adjusted || series.enrollment_headcount_total || []);
  const revenueSeries = toSeries(series.revenue_total_adjusted);
  const expensesSeries = toSeries(series.expenses_total_adjusted);
  const netTuitionSeries = toSeries(series.net_tuition_per_fte_adjusted);
  const discountRateSeries = toSeries(series.unfunded_discount_rate);
  const enrollmentSeries = toSeries(series.enrollment_headcount_total);
  const enrollmentUndergradSeries = toSeries(series.enrollment_headcount_undergrad);
  const enrollmentGraduateSeries = toSeries(series.enrollment_headcount_graduate);
  const staffTotalSeries = toSeries(series.staff_headcount_total);
  const staffInstructionalSeries = toSeries(series.staff_headcount_instructional);
  const endowmentValueSeries = toSeries(series.endowment_value_adjusted);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);

  const downloadButton = document.getElementById("download-school-data");
  if (downloadButton) {
    downloadButton.onclick = () => downloadSchoolCsv(school);
  }
  const shareButton = document.getElementById("share-school-profile");
  if (shareButton) {
    shareButton.onclick = () => shareSchoolProfile(school, unitid);
  }

  renderSchoolRelatedPages(unitid, {
    cuts: cutsIndex,
    accreditation: accreditationIndex,
    research: researchIndex
  });

  setText("school-name", p.institution_name);
  const closureSentence = buildClosureSentence(closureRecord);
  setText("school-closure-flag", closureSentence || "");
  setHidden("school-closure-flag", !closureSentence);
  // Editorial Calm: these three paragraphs are joined into a single
  // inline italic line by .school-mast .meta CSS using an :empty filter
  // and a sibling-combinator '·' separator. Pass an empty string (not
  // setText's "No data" fallback) when a field is missing so the slot
  // collapses cleanly instead of injecting a stray "No data" between
  // separators on the meta line.
  setText("school-location", [p.city, p.state].filter(Boolean).join(", ") || "");
  setText("school-control", p.sector || "");
  setText("school-urbanization", p.urbanization || "");
  const hasGraduationRate = graduationRate !== null;
  const hasMedianEarnings = medianEarnings !== null;
  const hasMedianDebt = medianDebt !== null;
  const visibleOutcomeCount = [hasGraduationRate, hasMedianEarnings, hasMedianDebt].filter(Boolean).length;
  setText("school-graduation-rate", hasGraduationRate ? fmtPlainPct(graduationRate, 0) : "");
  setText("school-median-earnings", hasMedianEarnings ? fmtCurrency(medianEarnings) : "");
  setText("school-median-debt", hasMedianDebt ? fmtCurrency(medianDebt) : "");
  setHidden("school-graduation-card", !hasGraduationRate);
  setHidden("school-earnings-card", !hasMedianEarnings);
  setHidden("school-debt-card", !hasMedianDebt);
  setHidden("school-outcomes-section", !(hasGraduationRate || hasMedianEarnings || hasMedianDebt));
  if (visibleOutcomeCount > 0) {
    setOutcomesGridLayout(visibleOutcomeCount);
  }

  applyStrip(
    "revenue-change-card",
    asNumber(s.revenue_pct_change_5yr) === null
      ? "Revenue data are not available."
      : `Revenue ${asNumber(s.revenue_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.revenue_pct_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.revenue_pct_change_5yr)
  );
  const hasRevenueCard = asNumber(s.revenue_pct_change_5yr) !== null;
  setHidden("revenue-change-card", !hasRevenueCard);

  setText("loss-latest", s.ended_year_at_loss || "No data");
  styleAnswerCard("loss-latest", s.ended_year_at_loss);
  setClosestMetricHidden("loss-latest", !s.ended_year_at_loss);
  setText("loss-repeat", s.losses_last_3_of_5 || "No data");
  styleAnswerCard("loss-repeat", s.losses_last_3_of_5);
  setClosestMetricHidden("loss-repeat", !s.losses_last_3_of_5);
  setText("loss-years", s.loss_years_last_10 ?? "No data");
  const lossYearsStateValue = lossYearsState(s.loss_years_last_10);
  const lossYearsAnswer = document.getElementById("loss-years");
  const lossYearsCard = lossYearsAnswer?.closest(".metric-strip");
  if (lossYearsAnswer) {
    lossYearsAnswer.className = `metric-answer ${lossYearsStateValue}`;
  }
  if (lossYearsCard) {
    lossYearsCard.className = `metric-strip ${lossYearsStateValue}`;
  }
  setClosestMetricHidden("loss-years", s.loss_years_last_10 === null || s.loss_years_last_10 === undefined || s.loss_years_last_10 === "");

  applyStrip(
    "net-tuition-change-card",
    asNumber(s.net_tuition_per_fte_change_5yr) === null
      ? "Net tuition revenue per student data are not available."
      : `Net tuition revenue per student ${asNumber(s.net_tuition_per_fte_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.net_tuition_per_fte_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.net_tuition_per_fte_change_5yr)
  );
  const hasNetTuitionCard = asNumber(s.net_tuition_per_fte_change_5yr) !== null;
  setHidden("net-tuition-change-card", !hasNetTuitionCard);

  const tuitionDependenceParagraph = buildTuitionDependenceParagraph(p, s, latestDataYear);
  const hasTuitionSentence = Array.isArray(tuitionDependenceParagraph) && tuitionDependenceParagraph.length > 0;
  setBodyCopy("tuition-sentence-copy", hasTuitionSentence ? [tuitionDependenceParagraph] : []);

  const discountRateSentence = buildDiscountRateSentence(p, s, series, latestDataYear);
  const discountRateChange = getDiscountRateChange(s, series, latestDataYear);
  const hasDiscountRateChart = isPrivateNotForProfitProfile(p) && discountRateSeries.length > 0;
  const hasDiscountRateSection = Boolean(discountRateSentence) || hasDiscountRateChart;
  setSectionVisibility("discount-rate-section", hasDiscountRateSection);
  if (hasDiscountRateSection) {
    applyStrip(
      "discount-rate-card",
      discountRateSentence || "Discount-rate data are not available.",
      discountRateTrendState(discountRateChange)
    );
  }
  setHidden("discount-rate-card", !discountRateSentence);

  const researchSpendingParagraph = buildResearchSpendingParagraph(p, s, latestDataYear);

  applyStrip(
    "enrollment-change-card",
    asNumber(s.enrollment_pct_change_5yr) === null
      ? "Enrollment data are not available."
      : `Enrollment ${asNumber(s.enrollment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.enrollment_pct_change_5yr)))} ${fiveYearRangeText}.`,
    sentimentClass(s.enrollment_pct_change_5yr)
  );
  const hasEnrollmentCard = asNumber(s.enrollment_pct_change_5yr) !== null;
  setHidden("enrollment-change-card", !hasEnrollmentCard);

  const enrollmentFlag = deriveEnrollmentFlag(s, series);
  setEnrollmentTotal("enrollment-total", latestEnrollment);
  setHidden("enrollment-total", !latestEnrollment);
  setText("enrollment-flag", enrollmentFlag);
  styleAnswerCard("enrollment-flag", enrollmentFlag);
  setClosestMetricHidden("enrollment-flag", enrollmentFlag === "No data");

  const intlTotalSeries = toSeries(series.enrollment_nonresident_total);
  const intlGradSeries = toSeries(series.enrollment_nonresident_graduate);
  const intlUndergradSeries = toSeries(series.enrollment_nonresident_undergrad);
  const hasAnyInternationalEnrollment = [
    ...intlTotalSeries,
    ...intlGradSeries,
    ...intlUndergradSeries
  ].some((point) => point.value > 0);

  const intlSentenceParagraph = buildIntlSentenceParagraph(s, series, latestDataYear);
  const hasIntlSentence = asNumber(s.pct_international_all) !== null || (latestPoint(series.enrollment_nonresident_total) && latestPoint(series.enrollment_headcount_total));
  setBodyCopy("intl-sentence-copy", hasIntlSentence && intlSentenceParagraph ? [intlSentenceParagraph] : []);

  if (hasAnyInternationalEnrollment) {
    const intlChange = asNumber(s.international_enrollment_pct_change_5yr);
    if (intlChange === null) {
      setBodyCopy("intl-change-copy", []);
    } else {
      setBodyCopy("intl-change-copy", [[
        "The number of international students ",
        strongSegment(`${intlChange >= 0 ? "increased" : "decreased"} ${fmtRoundedPct(Math.abs(intlChange))}`),
        ` ${fiveYearRangeText}.`
      ]]);
    }
  } else {
    setBodyCopy("intl-change-copy", []);
  }

  const gradLoanParagraphs = buildGradLoanParagraphs(p, s);
  const hasGradLoanCopy = gradLoanParagraphs.length > 0;
  setHidden("grad-loan-intro", !hasGradLoanCopy);
  setBodyCopy("loan-copy", gradLoanParagraphs);

  applyStrip(
    "staff-change-card",
    asNumber(s.staff_total_headcount_pct_change_5yr) === null
      ? "Staffing data are not available."
      : `Total staff headcount ${asNumber(s.staff_total_headcount_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.staff_total_headcount_pct_change_5yr)))} ${fiveYearRangeText}.`,
    sentimentClass(s.staff_total_headcount_pct_change_5yr)
  );
  const hasStaffCard = asNumber(s.staff_total_headcount_pct_change_5yr) !== null;
  setHidden("staff-change-card", !hasStaffCard);

  const ratioParagraph = buildInstructionalStaffRatioParagraph(p, s, latestDataYear);
  setBodyCopy("staff-ratio-copy", ratioParagraph ? [ratioParagraph] : []);

  applyStrip(
    "endowment-change-card",
    asNumber(s.endowment_pct_change_5yr) === null
      ? "Endowment data are not available."
      : `The institution's endowment ${asNumber(s.endowment_pct_change_5yr) < 0 ? "decreased" : "increased"} ${fmtRoundedPct(Math.abs(asNumber(s.endowment_pct_change_5yr)))} ${fiveYearRangeText}, after adjusting for inflation.`,
    sentimentClass(s.endowment_pct_change_5yr)
  );
  const hasEndowmentCard = asNumber(s.endowment_pct_change_5yr) !== null;
  setHidden("endowment-change-card", !hasEndowmentCard);

  const hasEndowmentValue = hasMeaningfulData(series.endowment_value_adjusted);

  const hasFederal =
    (asNumber(s.federal_grants_contracts_pell_adjusted_pct_core_revenue) ?? 0) !== 0 ||
    asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) !== null ||
    hasMeaningfulData(series.federal_grants_contracts_pell_adjusted) ||
    hasMeaningfulData(series.federal_grants_contracts_pell_adjusted_adjusted);
  const hasState =
    (asNumber(s.state_funding_pct_core_revenue) ?? 0) !== 0 ||
    ((asNumber(s.state_funding_pct_change_5yr) ?? 0) !== 0) ||
    hasMeaningfulData(series.state_funding_adjusted);
  const hasResearchSpending = (asNumber(s.research_expense_per_fte) ?? 0) > 0;

  const endowmentSpendingSeries = toSeries(series.endowment_spending_current_use_adjusted);
  const hasEndowmentSpending = endowmentSpendingSeries.some((point) => Number(point.value) !== 0);
  const showEndowmentSection = hasEndowmentValue || hasEndowmentSpending;
  const hasRevenueChart = revenueSeries.length > 0 || expensesSeries.length > 0;
  const hasNetTuitionChart = netTuitionSeries.length > 0;
  const hasEnrollmentChart = enrollmentSeries.length > 0 || enrollmentUndergradSeries.length > 0 || enrollmentGraduateSeries.length > 0;
  const hasStaffingChart = staffTotalSeries.length > 0 || staffInstructionalSeries.length > 0;
  const hasLossBlock = !!s.ended_year_at_loss || !!s.losses_last_3_of_5 || !(s.loss_years_last_10 === null || s.loss_years_last_10 === undefined || s.loss_years_last_10 === "");
  const showFinancialSection = hasRevenueCard || hasRevenueChart || hasLossBlock || hasNetTuitionCard || hasNetTuitionChart || hasTuitionSentence || hasDiscountRateSection;
  const showEnrollmentSection = hasEnrollmentCard || hasEnrollmentChart || enrollmentFlag !== "No data" || hasIntlSentence || hasAnyInternationalEnrollment;
  const showGraduateSection = hasGradLoanCopy;
  const showStaffingSection = hasStaffCard || hasStaffingChart || !!ratioParagraph;

  setSectionVisibility("financial-section", showFinancialSection);
  setSectionVisibility("enrollment-section", showEnrollmentSection);
  setSectionVisibility("graduate-section", showGraduateSection);
  setSectionVisibility("staffing-section", showStaffingSection);
  setSectionVisibility("endowment-section", showEndowmentSection);
  setSectionVisibility("federal-group", hasFederal);
  setSectionVisibility("state-group", hasState);
  setSectionVisibility("aid-section", hasFederal || hasState || hasResearchSpending);
  setHidden("research-aid-intro", !hasResearchSpending);
  setBodyCopy("research-spending-copy", hasResearchSpending && researchSpendingParagraph ? [researchSpendingParagraph] : []);
  const aidTitle = document.getElementById("aid-section-title");
  if (aidTitle) {
    if (hasFederal && hasState) aidTitle.textContent = "Federal And State Aid";
    else if (hasFederal) aidTitle.textContent = "Federal Aid";
    else if (hasState) aidTitle.textContent = "State Aid";
    else aidTitle.textContent = "Aid";
  }

  if (hasFederal) {
    setText(
      "federal-share-copy",
      `${Number.isFinite(latestDataYear) ? `In ${latestDataYear}, ` : "In the latest year, "}${fmtPlainPct(s.federal_grants_contracts_pell_adjusted_pct_core_revenue || 0)} of core revenue came from federal grants and contracts, excluding Pell grants.${asNumber(s.federal_grants_contracts_pell_adjusted_pct_core_revenue) === 0 ? " Many colleges saw a big increase in federal aid amid the COVID-19 pandemic." : ""}`
    );
    setHidden("federal-share-copy", false);

    applyQuestionValueStrip(
      "federal-change-card",
      "How much have federal grants and contracts changed over the past 5 years?",
      asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) === null
        ? "No data"
        : fmtRoundedPct(s.federal_grants_contracts_pell_adjusted_pct_change_5yr, true),
      asNumber(s.federal_grants_contracts_pell_adjusted_pct_change_5yr) === null ? "neutral" : sentimentClass(s.federal_grants_contracts_pell_adjusted_pct_change_5yr)
    );
  } else {
    setHidden("federal-share-copy", true);
  }

  if (hasState) {
    setText(
      "state-share-copy",
      `${Number.isFinite(latestDataYear) ? `In ${latestDataYear}, ` : "In the latest year, "}${fmtPlainPct(s.state_funding_pct_core_revenue || 0)} of core revenue came from state appropriations.`
    );
    setHidden("state-share-copy", false);

    applyQuestionValueStrip(
      "state-change-card",
      "How much has state aid changed over the past 5 years?",
      asNumber(s.state_funding_pct_change_5yr) === null
        ? "No data"
        : fmtRoundedPct(s.state_funding_pct_change_5yr, true),
      asNumber(s.state_funding_pct_change_5yr) === null ? "neutral" : sentimentClass(s.state_funding_pct_change_5yr)
    );
  } else {
    setHidden("state-share-copy", true);
  }

  if (hasResearchSpending) {
    setBodyCopy("research-spending-copy", researchSpendingParagraph ? [researchSpendingParagraph] : []);
  }

  const financeTooltip2024Config = {
    showTooltip: true,
    tooltipYear: 2024,
    tooltipPointOnly: true
  };

  renderLineChart("chart-revenue", {
    title: "Revenue compared to expenses (adjusted for inflation)",
    format: "currency",
    ...financeTooltip2024Config,
    series: [
      { label: "Revenue", color: CHART_COLOR_PRIMARY, values: toSeries(series.revenue_total_adjusted) },
      { label: "Expenses", color: CHART_COLOR_SECONDARY, values: toSeries(series.expenses_total_adjusted) }
    ]
  });
  setHidden("chart-revenue", !hasRevenueChart);
  upsertSectionSourceNote("chart-revenue", hasRevenueChart ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  renderLineChart("chart-net-tuition", {
    title: "Net tuition revenue over time (per full-time equivalent student, adjusted for inflation)",
    format: "currency",
    ...financeTooltip2024Config,
    showLegend: false,
    series: [
      { label: "Net Tuition Revenue", color: CHART_COLOR_PRIMARY, values: toSeries(series.net_tuition_per_fte_adjusted) }
    ]
  });
  setHidden("chart-net-tuition", !hasNetTuitionChart);
  upsertSectionSourceNote("chart-net-tuition", hasNetTuitionChart ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  renderLineChart("chart-discount-rate", {
    title: "Discount rate over time",
    format: "percent",
    showLegend: false,
    series: [
      { label: "Unfunded Discount Rate", color: CHART_COLOR_SECONDARY, values: discountRateSeries }
    ]
  });
  setHidden("chart-discount-rate", !hasDiscountRateChart);
  upsertSectionSourceNote("chart-discount-rate", hasDiscountRateChart ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  renderLineChart("chart-enrollment", {
    title: "Enrollment trends (12-month unduplicated headcount)",
    format: "number",
    series: [
      { label: "Total Enrollment", color: CHART_COLOR_PRIMARY, values: enrollmentSeries },
      { label: "Undergraduate Enrollment", color: CHART_COLOR_SECONDARY, values: enrollmentUndergradSeries },
      { label: "Graduate Enrollment", color: CHART_COLOR_TERTIARY, values: enrollmentGraduateSeries }
    ],
    enableSeriesToggle: true
  });
  setHidden("chart-enrollment", !hasEnrollmentChart);
  upsertSectionSourceNote("chart-enrollment", hasEnrollmentChart ? [
    createIpedsCitation(latestDataYear || "latest", "12-month Enrollment", schoolRetrievedAt)
  ] : []);

  if (hasAnyInternationalEnrollment) {
    renderLineChart("chart-international", {
      title: "International enrollment over time",
      format: "number",
      enableSeriesToggle: true,
      series: [
        { label: "International Student Total", color: CHART_COLOR_PRIMARY, values: intlTotalSeries },
        { label: "International Graduate Students", color: CHART_COLOR_SECONDARY, values: intlGradSeries },
        { label: "International Undergraduate Students", color: CHART_COLOR_TERTIARY, values: intlUndergradSeries }
      ]
    });
  }
  setHidden("chart-international", !hasAnyInternationalEnrollment);
  upsertSectionSourceNote("chart-international", hasAnyInternationalEnrollment ? [
    createIpedsCitation(latestDataYear || "latest", "Fall Enrollment", schoolRetrievedAt)
  ] : []);

  renderLineChart("chart-staffing", {
    title: "Staffing levels over time",
    format: "number",
    series: [
      { label: "Total Staff Headcount", color: CHART_COLOR_PRIMARY, values: toSeries(series.staff_headcount_total) },
      { label: "Total Instructional Staff", color: CHART_COLOR_SECONDARY, values: toSeries(series.staff_headcount_instructional) }
    ]
  });
  setHidden("chart-staffing", !hasStaffingChart);
  upsertSectionSourceNote("chart-staffing", hasStaffingChart ? [
    createIpedsCitation(latestDataYear || "latest", "Human Resources", schoolRetrievedAt)
  ] : []);

  renderLineChart("chart-endowment", {
    title: "Endowment value over time (adjusted for inflation)",
    format: "currency",
    ...financeTooltip2024Config,
    showLegend: false,
    series: [
      { label: "Endowment Value", color: CHART_COLOR_PRIMARY, values: toSeries(series.endowment_value_adjusted) }
    ]
  });
  setHidden("chart-endowment", !hasEndowmentValue);
  upsertSectionSourceNote("chart-endowment", hasEndowmentValue ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  const endowmentSpendingShareSeries = toSeries(series.endowment_spending_current_use_pct_core_revenue)
    .map((point) => ({ year: point.year, value: point.value * 100 }));
  const endowmentSpendingShareByYear = new Map(
    endowmentSpendingShareSeries.map((point) => [Number(point.year), Number(point.value)])
  );
  setHidden("endowment-spending-copy", !hasEndowmentSpending);
  setHidden("chart-endowment-spending", !hasEndowmentSpending);
  if (hasEndowmentSpending) {
    renderLineChart("chart-endowment-spending", {
      title: "Withdrawals from endowments to fund the institution's expenses (adjusted for inflation)",
      format: "currency",
      showLegend: false,
      series: [
        { label: "Spending Distribution For Current Use", color: CHART_COLOR_SECONDARY, values: endowmentSpendingSeries }
      ],
      tooltipRows: (year, seriesList, formatValue) => {
        const point = seriesList[0]?.values?.find((value) => Number(value.year) === Number(year));
        if (!point) return [];
        const share = endowmentSpendingShareByYear.get(Number(year));
        const rows = [
          `<span class="chart-tooltip-row">Spending Distribution For Current Use: ${formatValue(Number(point.value), "currency")}</span>`
        ];
        if (Number.isFinite(share)) {
          rows.push(`<span class="chart-tooltip-row">Share of Core Revenue: ${formatValue(share, "percent")}</span>`);
        }
        return rows;
      }
    });
  }
  upsertSectionSourceNote("chart-endowment-spending", hasEndowmentSpending ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  if (hasFederal) {
    renderLineChart("chart-federal", {
      title: "Revenue from federal grants and contracts (excluding Pell grants, adjusted for inflation)",
      format: "currency",
      ...financeTooltip2024Config,
      showLegend: false,
      series: [
        {
          label: "Federal Grants",
          color: CHART_COLOR_PRIMARY,
          values: toSeries(
            series.federal_grants_contracts_pell_adjusted ||
            series.federal_grants_contracts_pell_adjusted_adjusted
          )
        }
      ]
    });
  }
  upsertSectionSourceNote("chart-federal", hasFederal ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);

  if (hasState) {
    renderLineChart("chart-state", {
      title: "State government appropriations over time (adjusted for inflation)",
      format: "currency",
      ...financeTooltip2024Config,
      showLegend: false,
      series: [
        { label: "State Funding", color: CHART_COLOR_PRIMARY, values: toSeries(series.state_funding_adjusted) }
      ]
    });
  }
  upsertSectionSourceNote("chart-state", hasState ? [
    createIpedsCitation(latestDataYear || "latest", "Finance", schoolRetrievedAt)
  ] : []);
  moveChartNoteBelowSource("state-negative-note", "chart-state", hasState && hasNegativePoint(series.state_funding_adjusted));

  const compositeSentence = buildFederalCompositeSentence(composite);
  setHidden("federal-composite-section", !compositeSentence);
  if (compositeSentence) {
    applyStrip("federal-composite-card", compositeSentence, federalCompositeState(composite.federal_composite_score_2022_2023));
  }
  const hcmSentence = buildHcm2Sentence(p, hcmRecord, hcmLookup);
  setHidden("hcm2-section", !hcmSentence);
  if (hcmSentence) {
    const trendCopy = document.getElementById("hcm2-trend-copy");
    if (trendCopy) {
      const fromCount = hcmLookup?.summary?.trump_administration_drop_from;
      const toCount = hcmLookup?.summary?.trump_administration_drop_to;
      trendCopy.textContent = Number.isFinite(fromCount) && Number.isFinite(toCount)
        ? `Under the Trump administration, the number of colleges in that category dropped from ${fromCount} in December 2024 to ${toCount} as of December 2025.`
        : "";
    }
    applyStrip("hcm2-card", hcmSentence, hcm2State(hcmRecord));
  }
}

init().catch((error) => {
  console.error(error);
  showSchoolLoadError("This school page could not be loaded.");
});
