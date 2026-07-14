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

function roundedPercentDisplayValue(value) {
  const n = asNumber(value);
  if (n === null) return null;
  return Math.abs(n) < 1 ? Math.round(n * 10) / 10 : Math.round(n);
}

function startsWithVowelSoundNumber(value) {
  let numeric = Math.abs(asNumber(value) ?? NaN);
  if (!Number.isFinite(numeric) || numeric < 1) return false;

  while (numeric >= 1000) {
    numeric /= 1000;
  }

  if (numeric >= 100) {
    return Math.floor(numeric / 100) === 8;
  }
  if (numeric >= 20) {
    return Math.floor(numeric / 10) === 8;
  }
  if (numeric >= 10) {
    const whole = Math.floor(numeric);
    return whole === 11 || whole === 18;
  }
  return numeric >= 8 && numeric < 9;
}

function compareDisplayedPercentages(value, benchmark) {
  const displayedValue = roundedPercentDisplayValue(value);
  const displayedBenchmark = roundedPercentDisplayValue(benchmark);
  if (displayedValue === null || displayedBenchmark === null) return null;
  if (displayedValue === displayedBenchmark) return "about the same as";
  return displayedValue > displayedBenchmark ? "above" : "below";
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

const TREND_RED_THRESHOLD = -10;
const TREND_BLUE_THRESHOLD = 10;

function sentimentClass(value) {
  const n = asNumber(value);
  if (n === null) return "neutral";
  if (n <= TREND_RED_THRESHOLD) return "negative";
  if (n >= TREND_BLUE_THRESHOLD) return "positive";
  return "neutral";
}

function yesNoClass(value) {
  const normalized = String(value).trim().toLowerCase();
  if (value === true || value === 1 || normalized === "yes" || normalized === "1") return "negative";
  if (value === false || value === 0 || normalized === "no" || normalized === "0") return "positive";
  return "neutral";
}

function hasNonEmptyValue(value) {
  if (value === null || value === undefined) return false;
  if (typeof value === "string") return value.trim() !== "";
  return true;
}

function stateIsNegative(state) {
  return state === "negative" || state === "neg";
}

function revenueChangeState(summary) {
  return sentimentClass(summary?.revenue_pct_change_5yr);
}

function lossPatternState(summary) {
  return yesNoClass(summary?.losses_last_3_of_5);
}

function netTuitionChangeState(summary) {
  return sentimentClass(summary?.net_tuition_per_fte_change_5yr);
}

function enrollmentChangeState(summary) {
  return sentimentClass(summary?.enrollment_pct_change_5yr);
}

function enrollmentDeclineState(value) {
  return yesNoClass(value);
}

function staffChangeState(summary) {
  return sentimentClass(summary?.staff_total_headcount_pct_change_5yr);
}

function endowmentChangeState(summary) {
  return sentimentClass(summary?.endowment_pct_change_5yr);
}

function isRevenueRed(summary) {
  return stateIsNegative(revenueChangeState(summary));
}

function isLossPatternRed(summary) {
  return stateIsNegative(lossPatternState(summary));
}

function isNetTuitionRed(summary) {
  return stateIsNegative(netTuitionChangeState(summary));
}

function isEnrollmentRed(summary) {
  return stateIsNegative(enrollmentChangeState(summary));
}

function isEnrollmentDeclineRed(value) {
  return stateIsNegative(enrollmentDeclineState(value));
}

function isStaffRed(summary) {
  return stateIsNegative(staffChangeState(summary));
}

function isEndowmentRed(summary) {
  return stateIsNegative(endowmentChangeState(summary));
}

function trendDirection(value) {
  const numeric = asNumber(value);
  if (numeric === null || numeric === 0) return "";
  return numeric > 0 ? "up" : "down";
}

function setStripArrow(node, state, direction) {
  if (!node) return;
  if ((state === "positive" || state === "negative") && direction) {
    node.dataset.arrow = direction;
    return;
  }
  delete node.dataset.arrow;
}

function createWarningTooltipNode({ leadingText = "", emphasizedOne = "", middleText = "", emphasizedTwo = "", trailingText = "" } = {}) {
  const tooltip = document.createElement("span");
  tooltip.className = "warning-sign-tooltip";
  tooltip.setAttribute("role", "tooltip");

  if (leadingText) tooltip.append(document.createTextNode(leadingText));
  if (emphasizedOne) {
    const strong = document.createElement("strong");
    strong.textContent = emphasizedOne;
    tooltip.append(strong);
  }
  if (middleText) tooltip.append(document.createTextNode(middleText));
  if (emphasizedTwo) {
    const strong = document.createElement("strong");
    strong.textContent = emphasizedTwo;
    tooltip.append(strong);
  }
  if (trailingText) tooltip.append(document.createTextNode(trailingText));

  return tooltip;
}

function defaultWarningTooltipNode() {
  return createWarningTooltipNode({
    leadingText: "Red with a warning sign means a trend is growing in a concerning direction. ",
    emphasizedOne: "1 or 2 red boxes",
    middleText: " don't automatically signal crisis. ",
    emphasizedTwo: "Multiple red indicators",
    trailingText: " may signal deeper financial pressure."
  });
}

function syncWarningTooltip(node, state) {
  if (!node) return;
  const showWarning = state === "negative" || state === "neg";
  const existingTooltip = node.querySelector(".warning-sign-tooltip");
  if (showWarning) {
    node.dataset.warningTooltip = "true";
    node.setAttribute("tabindex", "0");
    if (!existingTooltip) {
      node.appendChild(defaultWarningTooltipNode());
    }
    return;
  }
  delete node.dataset.warningTooltip;
  node.removeAttribute("tabindex");
  existingTooltip?.remove();
}

function applyStrip(id, text, state = "neutral", direction = "") {
  const node = document.getElementById(id);
  if (!node) return;
  node.className = `metric-strip ${state}`;
  node.textContent = "";
  const statement = document.createElement("div");
  statement.className = "metric-statement";
  statement.textContent = text ?? "";
  node.appendChild(statement);
  setStripArrow(node, state, direction);
  syncWarningTooltip(node, state);
}

function appendSegmentContent(node, segments) {
  (Array.isArray(segments) ? segments : [segments]).forEach((segment) => {
    if (segment && typeof segment === "object" && Object.prototype.hasOwnProperty.call(segment, "strong")) {
      const strong = document.createElement("strong");
      strong.textContent = segment.strong ?? "";
      node.appendChild(strong);
      return;
    }
    node.append(document.createTextNode(String(segment ?? "")));
  });
}

function applyStripLines(id, lines, state = "neutral", direction = "") {
  const node = document.getElementById(id);
  if (!node) return;
  node.className = `metric-strip ${state}`;
  node.textContent = "";
  const statement = document.createElement("div");
  statement.className = "metric-statement";
  (lines || []).forEach((line, index) => {
    if (index > 0) {
      statement.appendChild(document.createElement("br"));
    }
    appendSegmentContent(statement, line);
  });
  node.appendChild(statement);
  setStripArrow(node, state, direction);
  syncWarningTooltip(node, state);
}

function applyQuestionValueStrip(id, question, value, state = "neutral", direction = "") {
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
  setStripArrow(node, state, direction);
  syncWarningTooltip(node, state);
}

function schoolWarningTypeLabel(profile) {
  const control = String(profile?.control_label || profile?.sector || "").trim().toLowerCase();
  if (control === "public") return "public school";
  if (control === "private not-for-profit") return "private nonprofit school";
  if (control === "private for-profit") return "for-profit school";
  return "school";
}

const WIDESPREAD_WARNING_MIN_REDS = 6;
const PATTERN_YEAR_END = 2024;
const PATTERN_YEAR_START = PATTERN_YEAR_END - 5;

function appendSchoolWarningContext(tooltip, count, totalVisible, typeLabel) {
  const countStrong = document.createElement("strong");
  countStrong.textContent = `${count} ${count === 1 ? "is" : "are"} flagged as concerning`;
  tooltip.append(document.createTextNode(` Across all ${totalVisible} potential indicators for a ${typeLabel}, `));
  tooltip.append(countStrong);
  tooltip.append(document.createTextNode(". This is a sign of financial stress. Check out this institution's "));
  const auditsLink = document.createElement("a");
  auditsLink.href = "https://www.fac.gov/";
  auditsLink.target = "_blank";
  auditsLink.rel = "noopener noreferrer";
  auditsLink.textContent = "audits";
  tooltip.append(auditsLink);
  tooltip.append(document.createTextNode(" for more context."));
}

function buildSchoolWarningBadge({ label, tooltipLabel, variantClass = "", buildTooltipContent }) {
  const badge = document.createElement("span");
  badge.className = `school-warning-summary${variantClass ? ` ${variantClass}` : ""}`;
  badge.setAttribute("role", "img");
  badge.setAttribute("tabindex", "0");
  badge.setAttribute("aria-label", tooltipLabel);

  const icon = document.createElement("span");
  icon.className = "guide-warning-icon";
  icon.setAttribute("aria-hidden", "true");

  const text = document.createElement("span");
  text.textContent = label;

  const tooltip = document.createElement("span");
  tooltip.className = "warning-sign-tooltip";
  tooltip.setAttribute("role", "tooltip");
  buildTooltipContent(tooltip);

  badge.append(icon, text, tooltip);
  return badge;
}

function trackerSectorCollegeLabel(profile) {
  const control = String(profile?.control_label || profile?.sector || "").trim().toLowerCase();
  if (control === "public") return "public colleges";
  if (control === "private not-for-profit") return "private nonprofit colleges";
  if (control === "private for-profit") return "for-profit colleges";
  return "colleges in the same sector";
}

function firstNumericValue(...values) {
  for (const value of values) {
    const numeric = asNumber(value);
    if (numeric !== null) return numeric;
  }
  return null;
}

function buildSectorComparisonLine(benchmarkValue, profile) {
  const benchmark = asNumber(benchmarkValue);
  if (benchmark === null) return null;
  const displayedBenchmark = roundedPercentDisplayValue(benchmark);
  let comparisonPhrase;
  if (displayedBenchmark === 0) {
    comparisonPhrase = strongSegment("about no change");
  } else if (benchmark < 0) {
    const declinePct = fmtRoundedPct(Math.abs(benchmark));
    comparisonPhrase = strongSegment(`${declinePct} decline`);
  } else {
    const increasePct = fmtRoundedPct(Math.abs(benchmark));
    comparisonPhrase = strongSegment(`${increasePct} increase`);
  }
  const comparisonArticle = displayedBenchmark === 0
    ? ""
    : `${startsWithVowelSoundNumber(displayedBenchmark) ? "an" : "a"} `;
  return [
    "That compares to ",
    comparisonArticle,
    comparisonPhrase,
    ` for all ${trackerSectorCollegeLabel(profile)} we track.`
  ];
}

function buildTrendCardLines(subject, value, rangeText, profile, benchmarkValue, options = {}) {
  const numeric = asNumber(value);
  if (numeric === null) return null;
  const mainLine = [
    `${subject} ${numeric < 0 ? "decreased" : "increased"} `,
    strongSegment(fmtRoundedPct(Math.abs(numeric))),
    ` ${rangeText}${options.afterAdjustingForInflation ? ", after adjusting for inflation." : "."}`
  ];
  const comparisonLine = buildSectorComparisonLine(benchmarkValue, profile);
  return comparisonLine ? [mainLine, comparisonLine] : [mainLine];
}

let teardownProfileJumpLinkTracking = null;

function renderProfileJumpLinks(visibility = {}) {
  const showFinancialStatements = Boolean(visibility.showFinancialStatementsSection);
  setSectionVisibility("profile-jump-links", Boolean(
    visibility.showFinancialSection ||
    visibility.showEnrollmentSection ||
    visibility.showStaffingSection ||
    visibility.showEndowmentSection ||
    showFinancialStatements
  ));
  setElementVisible("profile-jump-link-revenue", Boolean(visibility.showFinancialSection));
  setElementVisible("profile-jump-link-enrollment", Boolean(visibility.showEnrollmentSection));
  setElementVisible("profile-jump-link-staffing", Boolean(visibility.showStaffingSection));
  setElementVisible("profile-jump-link-endowment", Boolean(visibility.showEndowmentSection));
  setElementVisible("profile-jump-link-financial-statements", showFinancialStatements);
}

// Maps each tracked section ID to its nav link ID. Sections mapping to the
// same link ID are collapsed: whichever of them crossed the viewport anchor
// last is used as the tiebreaker (the last one in DOM order wins).
const PROFILE_NAV_SECTION_TO_LINK = {
  "financial-section": "profile-jump-link-revenue",
  "net-tuition-section": "profile-jump-link-revenue",
  "state-aid-section": "profile-jump-link-revenue",
  "aid-section": "profile-jump-link-revenue",
  "enrollment-section": "profile-jump-link-enrollment",
  "intl-section": "profile-jump-link-enrollment",
  "staffing-section": "profile-jump-link-staffing",
  "endowment-section": "profile-jump-link-endowment",
  "more-financial-detail-section": "profile-jump-link-financial-statements",
  "school-related-section": "profile-jump-link-financial-statements",
  "school-bottom-search-section": "profile-jump-link-financial-statements"
};

// Called once at the very end of init(), after every section has its final
// visible/hidden state. Building visibleSections here (not in renderProfileJumpLinks)
// ensures the tracker reflects the page's settled layout.
function setupProfileJumpLinkTracking() {
  if (typeof teardownProfileJumpLinkTracking === "function") {
    teardownProfileJumpLinkTracking();
    teardownProfileJumpLinkTracking = null;
  }

  const nav = document.getElementById("profile-jump-links");
  if (!nav || nav.classList.contains("is-hidden")) return;

  // Snapshot visible sections in DOM order after all visibility updates land.
  const visibleSections = Object.keys(PROFILE_NAV_SECTION_TO_LINK)
    .map((id) => document.getElementById(id))
    .filter((el) => el && !el.classList.contains("is-hidden") &&
                    el.getAttribute("aria-hidden") !== "true")
    .sort((a, b) =>
      a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1);

  const links = Array.from(nav.querySelectorAll("a"))
    .filter((el) => !el.classList.contains("is-hidden"));

  if (!links.length || !visibleSections.length) return;

  let pinnedLinkId = null;
  let pinUntil = 0;
  let rafId = 0;

  function clearActive() {
    links.forEach((link) => {
      link.classList.remove("is-active");
      link.removeAttribute("aria-current");
    });
  }

  function setActive(linkId) {
    links.forEach((link) => {
      const active = link.id === linkId;
      link.classList.toggle("is-active", active);
      if (active) link.setAttribute("aria-current", "location");
      else link.removeAttribute("aria-current");
    });
  }

  // Trigger point for each section: the top of its heading.
  function getSectionTriggerTop(section) {
    const heading = section.querySelector(
      ".section-title, .section-disclosure-summary, .more-detail-subhead"
    );
    return (heading || section).getBoundingClientRect().top;
  }

  function update() {
    rafId = 0;
    const now = Date.now();
    if (pinnedLinkId && now < pinUntil) {
      setActive(pinnedLinkId);
      return;
    }
    pinnedLinkId = null;

    // Anchor = bottom of the sticky nav bar + small breathing room.
    // Walk sections in DOM order; keep the last one whose trigger is at or
    // above the anchor. That section "owns" the current viewport position.
    const anchor = nav.getBoundingClientRect().bottom + 8;
    let activeLinkId = links[0].id;
    for (const section of visibleSections) {
      if (getSectionTriggerTop(section) <= anchor) {
        activeLinkId = PROFILE_NAV_SECTION_TO_LINK[section.id] || activeLinkId;
      }
    }
    setActive(activeLinkId);
  }

  function requestUpdate() {
    if (rafId) return;
    rafId = window.requestAnimationFrame(update);
  }

  // Pin the clicked link for 700 ms, then let scroll take over.
  const clickHandlers = links.map((link) => {
    const handler = () => {
      pinnedLinkId = link.id;
      pinUntil = Date.now() + 700;
      setActive(link.id);
      requestUpdate();
    };
    link.addEventListener("click", handler);
    return { link, handler };
  });

  window.addEventListener("scroll", requestUpdate, { passive: true });
  window.addEventListener("resize", requestUpdate);
  requestUpdate();

  teardownProfileJumpLinkTracking = () => {
    window.removeEventListener("scroll", requestUpdate);
    window.removeEventListener("resize", requestUpdate);
    clickHandlers.forEach(({ link, handler }) =>
      link.removeEventListener("click", handler));
    if (rafId) {
      window.cancelAnimationFrame(rafId);
      rafId = 0;
    }
    clearActive();
  };
}

function syncSchoolWarningSummaryBadge(warningSummary, profile = null) {
  const node = document.getElementById("school-warning-summary");
  if (!node) return;

  if (!warningSummary?.showPatternBadge && !warningSummary?.showBroadBadge) {
    node.replaceChildren();
    node.classList.add("is-hidden");
    node.setAttribute("aria-hidden", "true");
    node.removeAttribute("aria-label");
    return;
  }

  const count = warningSummary.count;
  const totalVisible = warningSummary.totalVisible;
  const typeLabel = schoolWarningTypeLabel(profile);
  const badges = [];

  if (warningSummary.showPatternBadge) {
    const tooltipLabel = `This school shows a pattern of declines of at least 10% in both enrollment and net tuition revenue per student over five years, plus operating losses in at least 3 of the last 5 years. Across all ${totalVisible} potential indicators for a ${typeLabel}, ${count} ${count === 1 ? "is" : "are"} flagged as concerning. This is a sign of financial stress. Check out this institution's audits for more context.`;
    badges.push(buildSchoolWarningBadge({
      label: "Significant enrollment declines and losses",
      tooltipLabel,
      buildTooltipContent: (tooltip) => {
        tooltip.append(document.createTextNode("This school shows a pattern of "));
        const patternStrong = document.createElement("strong");
        patternStrong.textContent = "declines of at least 10%";
        tooltip.append(patternStrong);
        tooltip.append(document.createTextNode(" in both enrollment and net tuition revenue per student over five years, plus operating losses in at least 3 of the last 5 years."));
        appendSchoolWarningContext(tooltip, count, totalVisible, typeLabel);
      }
    }));
  }

  if (warningSummary.showBroadBadge) {
    const tooltipLabel = `This school shows at least 6 warning signs across visible indicators on this profile. At least 6 of its visible warning indicators are flagged as concerning. Across all ${totalVisible} potential indicators for a ${typeLabel}, ${count} ${count === 1 ? "is" : "are"} flagged as concerning. This is a signal of financial stress. Check out this institution's audits for more context.`;
    badges.push(buildSchoolWarningBadge({
      label: "At least 6 warning signs",
      tooltipLabel,
      variantClass: "is-broad",
      buildTooltipContent: (tooltip) => {
        tooltip.append(document.createTextNode("This school "));
        const broadStrong = document.createElement("strong");
        broadStrong.textContent = "shows at least 6 warning signs";
        tooltip.append(broadStrong);
        tooltip.append(document.createTextNode(" across visible indicators on this profile. At least 6 of its visible warning indicators are flagged as concerning."));
        appendSchoolWarningContext(tooltip, count, totalVisible, typeLabel);
      }
    }));
  }

  node.replaceChildren(...badges);
  node.classList.remove("is-hidden");
  node.setAttribute("aria-hidden", "false");
  node.removeAttribute("aria-label");
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
  node.append(`In ${latestEnrollment.year}, this institution reported a headcount of `);
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

// EMMA search URL.
// EMMA's Muni Search page does not prefill via URL query parameter;
// link directly to the search form and tell users the institution name
// to enter in the Issuer Name field.
const EMMA_SEARCH_URL = "https://emma.msrb.org/Search/Search.aspx";

function renderMoreFinancialDetailSection(institutionName, profile) {
  const name = String(institutionName || "").trim();
  const displayName = name || "this institution";

  const link = document.getElementById("emma-search-link");
  if (link) {
    link.textContent = `Search a database of bond disclosures for ${displayName}`;
    link.setAttribute("href", EMMA_SEARCH_URL);
    link.setAttribute("aria-label", `Search a database of bond disclosures for ${displayName} (opens in new tab)`);
  }

  const auditSpan = document.getElementById("federal-audit-school-name");
  if (auditSpan) auditSpan.textContent = displayName;

  const isNonprofit = isPrivateNotForProfitProfile(profile);
  setHidden("tax-filings-subsection", !isNonprofit);
  if (isNonprofit) {
    const taxSpan = document.getElementById("tax-filing-school-name");
    if (taxSpan) taxSpan.textContent = displayName;
  }

  setSectionVisibility("more-financial-detail-section", true);
}

function buildEndowmentPerFteParagraph(profile, summary, latestDataYear, endowmentPerFteRecord) {
  const institutionValue = firstNumericValue(
    summary?.endowment_assets_per_fte_adjusted,
    endowmentPerFteRecord?.endowment_assets_per_fte_adjusted
  );
  const sectorMedian = firstNumericValue(
    summary?.sector_median_endowment_assets_per_fte_adjusted,
    endowmentPerFteRecord?.sector_median_endowment_assets_per_fte_adjusted
  );
  if (institutionValue === null || sectorMedian === null) return null;
  const yearLabel = Number.isFinite(latestDataYear) ? latestDataYear : "the latest year";
  return [
    `In ${yearLabel}, the institution had an endowment of about `,
    strongSegment(fmtCurrency(institutionValue)),
    " per full-time equivalent student, compared with a sector median of ",
    strongSegment(fmtCurrency(sectorMedian)),
    ` at ${trackerSectorCollegeLabel(profile)}.`
  ];
}

function renderSchoolRelatedPages(unitid, schoolName, relatedIndexes = {}) {
  const section = document.getElementById("school-related-section");
  const container = document.getElementById("school-related-pages");
  if (!section || !container) return;

  const relatedPages = [
    {
      label: "College cuts",
      page: "cuts.html",
      record: findRelatedIndexRecord(relatedIndexes.cuts, unitid, "cut_count")
    },
    {
      label: "Accreditation history",
      page: "accreditation.html",
      record: findRelatedIndexRecord(relatedIndexes.accreditation, unitid, "action_count")
    },
    {
      label: "Research funding cuts",
      page: "research.html",
      record: findRelatedIndexRecord(relatedIndexes.research, unitid, "total_disrupted_grants")
    }
  ].filter((relatedPage) => relatedPage.record);

  const links = [];
  relatedPages.forEach((relatedPage) => {
    const relatedUnitid = relatedPage.record.unitid || unitid;
    links.push({
      href: `${relatedPage.page}?unitid=${encodeURIComponent(relatedUnitid)}`,
      label: relatedPage.label,
      external: false
    });
  });

  if (isNumericUnitid(unitid) && schoolName) {
    links.push({
      href: tuitionTrackerSchoolUrl(schoolName, unitid),
      label: "Tuition trends",
      external: true
    });
  }

  if (!links.length) {
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
  links.forEach((relatedLink) => {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = relatedLink.href;
    link.textContent = relatedLink.label;
    if (relatedLink.external) {
      link.target = "_blank";
      link.rel = "noopener noreferrer";
    }
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

  if (all !== null && ug !== null && grad !== null) {
    return `${prefixLatestYear}${fmtRoundedPct(all)} of students at this college were international. That includes ${fmtRoundedPct(ug)} of undergraduates and ${fmtRoundedPct(grad)} of graduate students.`;
  }

  if (all !== null) {
    return `${prefixLatestYear}${fmtRoundedPct(all)} of students at this college were international.`;
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return `${prefixLatestYear}${fmtRoundedPct(pct)} of students at this college were international.`;
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
      strongSegment(`${fmtRoundedPct(all)} of students at this college were international`),
      `. That includes ${fmtRoundedPct(ug)} of undergraduates and ${fmtRoundedPct(grad)} of graduate students.`
    ];
  }

  if (all !== null) {
    return [
      prefixLatestYear,
      strongSegment(`${fmtRoundedPct(all)} of students at this college were international`),
      "."
    ];
  }

  const latestIntl = latestPoint(series.enrollment_nonresident_total);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  if (latestIntl && latestEnrollment && latestEnrollment.value > 0) {
    const pct = (latestIntl.value / latestEnrollment.value) * 100;
    return [
      prefixLatestYear,
      strongSegment(`${fmtRoundedPct(pct)} of students at this college were international`),
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
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time equivalent student on research ${latestYearPhrase}. That is ${medianComparison} the median of ${fmtCurrency(sectorMedian)} for the ${fmtRoundedPct(reportingShare)} of ${sectorPhrase} who reported research spending.`;
  }

  if (shareOfCoreExpenses !== null) {
    return `Research expenses accounted for ${fmtRoundedPct(shareOfCoreExpenses)} of total core expenses at this institution, which spent about ${fmtCurrency(perFte)} per full-time equivalent student on research ${latestYearPhrase}.`;
  }

  return `This institution spent about ${fmtCurrency(perFte)} per full-time equivalent student on research ${latestYearPhrase}.`;
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
    const relation = compareDisplayedPercentages(tuitionDependence, sectorMedian) || "about the same as";
    return `This college took in ${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition ${latestYearPhrase}, ${relation} the median of ${fmtRoundedPct(sectorMedian)} for ${sectorLabel} colleges.`;
  }

  return `This college took in ${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition ${latestYearPhrase}.`;
}

function buildTuitionDependenceParagraph(profile, summary, latestDataYear) {
  const tuitionDependence = asNumber(summary.tuition_dependence_pct);
  const sectorMedian = asNumber(summary.sector_median_tuition_dependence_pct);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  const yearLabel = Number.isFinite(latestDataYear) ? latestDataYear : "the latest year";

  if (tuitionDependence === null) return null;

  if (sectorMedian !== null && sectorLabel) {
    const relation = compareDisplayedPercentages(tuitionDependence, sectorMedian) || "about the same as";
    return [
      "This college took in ",
      strongSegment(`${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition`),
      ` in ${yearLabel}, ${relation} the median of ${fmtRoundedPct(sectorMedian)} for ${sectorLabel} colleges.`
    ];
  }

  return [
    "This college took in ",
    strongSegment(`${fmtRoundedPct(tuitionDependence)} of its revenue from net tuition`),
    ` in ${yearLabel}.`
  ];
}

function isPrivateNotForProfitProfile(profile) {
  return String(profile?.control_label || "").trim().toLowerCase() === "private not-for-profit";
}

function isPublicProfile(profile) {
  return String(profile?.control_label || profile?.sector || "").trim().toLowerCase() === "public";
}

function lossYearsState(value) {
  const count = asNumber(value);
  if (count === null) return "neutral";
  if (count >= 5) return "negative";
  if (count <= 1) return "positive";
  return "neutral";
}

function findPointByYear(values, year) {
  const numericYear = Number(year);
  return toSeries(values).find((point) => point.year === numericYear) || null;
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
      `At this institution, ${fmtPlainPct(gradShare, 0)} of students are graduate students, compared to ${fmtPlainPct(sectorGradShare, 0)} at other ${sectorLabel} institutions.`
    );
  }

  if (gradPlusPerRecipient !== null && sectorGradPlusMedian !== null && sectorLabel) {
    sentences.push(
      `On average, graduate students who took out Grad PLUS loans at this institution borrowed ${fmtCurrency(gradPlusPerRecipient)} in the most recent year, compared to ${fmtCurrency(sectorGradPlusMedian)} at other ${sectorLabel} institutions.`
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
      ` That compares to ${fmtPlainPct(sectorGradShare, 0)} at other ${sectorLabel} institutions.`
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
  return `${prefix}, this institution had a student-to-faculty ratio for undergraduates of about ${fmtNumber(ratio)} to 1, compared with the median of ${fmtNumber(benchmark)} to 1 at ${sectorLabel} colleges.`;
}

function buildInstructionalStaffRatioParagraph(profile, summary, latestDataYear) {
  const ratio = asNumber(summary.students_per_instructional_staff_fte);
  const benchmark = asNumber(summary.sector_median_students_per_instructional_staff_fte);
  const sectorLabel = String(profile.control_label || "").toLowerCase();
  if (ratio === null || benchmark === null || !sectorLabel) return null;
  const prefix = Number.isFinite(latestDataYear) ? `In ${latestDataYear}, this institution had ` : "In the latest year, this institution had ";
  return [
    prefix,
    strongSegment(`a student-to-faculty ratio for undergraduates of about ${fmtNumber(ratio)} to 1`),
    `, compared with the median of ${fmtNumber(benchmark)} to 1 at ${sectorLabel} colleges.`
  ];
}

function normalizeInstitutionClosureText(...parts) {
  return parts
    .map((value) => String(value || "").trim())
    .filter(Boolean)
    .join(" ")
    .toLowerCase();
}

function findInstitutionClosureAnnouncement(cutsRecord) {
  if (cutsRecord?.confirmed_closure_announcement !== true) return null;
  return (cutsRecord?.landing_cuts ?? []).find((cut) => cut?.cut_type === "institution_closure") || null;
}

function isAbsorptionAnnouncement(cut, cutsRecord) {
  const text = normalizeInstitutionClosureText(
    cut?.cut_label_public,
    cut?.program_name,
    cutsRecord?.latest_cut_label
  );
  return /\babsor(?:b|bed|ption)\b/.test(text);
}

function announcedClosureYear(cutsRecord) {
  const cut = findInstitutionClosureAnnouncement(cutsRecord);
  if (!cut || isAbsorptionAnnouncement(cut, cutsRecord)) return null;
  const explicit = Number(cut.announcement_year || "");
  if (Number.isFinite(explicit) && explicit > 1900) return explicit;
  const m = normalizeInstitutionClosureText(cut.announcement_date, cut.cut_label_public, cut.program_name).match(/\b(20\d{2})\b/);
  return m ? Number(m[1]) : null;
}

function hasAnnouncedAbsorption(cutsRecord) {
  const cut = findInstitutionClosureAnnouncement(cutsRecord);
  return isAbsorptionAnnouncement(cut, cutsRecord);
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

function computeSchoolWarningSummary(summary, enrollmentFlag, visibility) {
  const visibleMetrics = [];
  const pushMetric = (key, label, visible, isRed) => {
    if (!visible) return;
    visibleMetrics.push({ key, label, isRed: Boolean(isRed) });
  };

  pushMetric("revenue", "Revenue", visibility.hasRevenueCard, isRevenueRed(summary));
  pushMetric("loss_pattern", "In the red in 3 of the last 5 years", visibility.hasLossRepeatCard, isLossPatternRed(summary));
  pushMetric("loss_years", "Loss years in the last 10", visibility.hasLossYearsCard, stateIsNegative(lossYearsState(summary?.loss_years_last_10)));
  pushMetric("net_tuition", "Net tuition revenue per FTE", visibility.hasNetTuitionCard, isNetTuitionRed(summary));
  pushMetric("enrollment", "Enrollment", visibility.hasEnrollmentCard, isEnrollmentRed(summary));
  pushMetric("enrollment_decline", "Enrollment declined in 3 of the previous 5 years", visibility.hasEnrollmentFlagCard, isEnrollmentDeclineRed(enrollmentFlag));
  pushMetric("staff", "Staff headcount", visibility.hasStaffCard, isStaffRed(summary));
  pushMetric("endowment", "Endowment", visibility.hasEndowmentCard, isEndowmentRed(summary));

  const coreIndicatorsMissing = !visibility.hasEnrollmentCard || !visibility.hasNetTuitionCard || !visibility.hasLossRepeatCard;
  const count = visibleMetrics.filter((metric) => metric.isRed).length;
  const totalVisible = visibleMetrics.length;
  const redRatio = totalVisible > 0 ? count / totalVisible : 0;
  const showPatternBadge = !coreIndicatorsMissing &&
    isEnrollmentRed(summary) &&
    isNetTuitionRed(summary) &&
    isLossPatternRed(summary);
  const showBroadBadge = totalVisible > 0 && count >= WIDESPREAD_WARNING_MIN_REDS;

  return {
    count,
    totalVisible,
    redRatio,
    contributingVisibleMetrics: visibleMetrics.filter((metric) => metric.isRed),
    showBadge: showPatternBadge || showBroadBadge,
    showPatternBadge,
    showBroadBadge
  };
}

const PROFILE_SHELL_SECTION_IDS = [
  "school-profile-mast"
];

const PROFILE_DATA_SECTION_IDS = [
  "profile-jump-links",
  "school-outcomes-section",
  "financial-section",
  "net-tuition-section",
  "state-aid-section",
  "tuition-dependence-section",
  "enrollment-section",
  "intl-section",
  "staffing-section",
  "endowment-section",
  "aid-section",
  "more-financial-detail-section",
  "tax-filings-subsection",
  "school-related-section",
  "school-bottom-search-section",
  "school-guide-link-section"
];

function setElementVisible(id, show) {
  const node = document.getElementById(id);
  if (!node) return;
  node.classList.toggle("is-hidden", !show);
}

function setGuideOnlyVisible(show) {
  document.querySelectorAll(".guide-only").forEach((node) => {
    node.classList.toggle("is-hidden", !show);
    if (show) {
      node.removeAttribute("aria-hidden");
    } else {
      node.setAttribute("aria-hidden", "true");
    }
  });
}

function setElementsVisible(ids, show) {
  ids.forEach((id) => setElementVisible(id, show));
}

function setSectionsVisible(ids, show) {
  ids.forEach((id) => setSectionVisibility(id, show));
}

function initGuideCalloutReveal() {
  const callouts = Array.from(document.querySelectorAll(".guide-callout"));
  if (!callouts.length) return;
  if (window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    callouts.forEach((callout) => callout.classList.add("is-visible"));
    return;
  }
  const observer = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      if (!entry.isIntersecting) return;
      entry.target.classList.add("is-visible");
      observer.unobserve(entry.target);
    });
  }, {
    threshold: 0.2,
    rootMargin: "0px 0px -10% 0px"
  });
  callouts.forEach((callout, index) => {
    callout.style.setProperty("--callout-delay", `${(index % 3) * 90}ms`);
    observer.observe(callout);
  });
}

function initMetricArrowReveal() {
  const strips = Array.from(document.querySelectorAll(".metric-strip[data-arrow]"));
  if (!strips.length) return;
  if (window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    strips.forEach((strip) => strip.classList.add("is-visible"));
    return;
  }
  const observer = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      if (!entry.isIntersecting) return;
      entry.target.classList.add("is-visible");
      observer.unobserve(entry.target);
    });
  }, {
    threshold: 0.2,
    rootMargin: "0px 0px -10% 0px"
  });
  strips.forEach((strip, index) => {
    strip.style.setProperty("--arrow-delay", `${(index % 6) * 60}ms`);
    observer.observe(strip);
  });
}

function showSchoolGuideLanding() {
  if (typeof teardownProfileJumpLinkTracking === "function") {
    teardownProfileJumpLinkTracking();
    teardownProfileJumpLinkTracking = null;
  }
  setGuideOnlyVisible(true);
  setElementVisible("school-profile-banner", false);
  setSectionsVisible(PROFILE_SHELL_SECTION_IDS, false);
  setSectionsVisible(PROFILE_DATA_SECTION_IDS, false);
  setText("school-name", "No school selected");
  setText("school-location", "");
  setText("school-control", "");
  setText("school-urbanization", "");
  setText("school-closure-flag", "");
  setHidden("school-closure-flag", true);
  setText("school-announced-closure", "");
  setHidden("school-announced-closure", true);
  setText("school-announced-merger", "");
  setHidden("school-announced-merger", true);
  setHidden("school-meta-wrap", true);
  setHidden("download-school-data", true);
  setHidden("share-school-profile", true);
  syncSchoolWarningSummaryBadge(null);
}

function showSchoolProfileShell() {
  if (typeof teardownProfileJumpLinkTracking === "function") {
    teardownProfileJumpLinkTracking();
    teardownProfileJumpLinkTracking = null;
  }
  setGuideOnlyVisible(false);
  setElementVisible("school-profile-banner", true);
  setSectionsVisible(PROFILE_SHELL_SECTION_IDS, true);
  setSectionsVisible(PROFILE_DATA_SECTION_IDS, false);
  setSectionVisibility("school-bottom-search-section", true);
  setSectionVisibility("school-guide-link-section", true);
  setHidden("school-meta-wrap", false);
  setHidden("download-school-data", false);
  setHidden("share-school-profile", false);
  syncSchoolWarningSummaryBadge(null);
}

function showSchoolLoadError(message) {
  setGuideOnlyVisible(false);
  setElementVisible("school-profile-banner", false);
  setSectionsVisible(PROFILE_SHELL_SECTION_IDS, true);
  setSectionsVisible(PROFILE_DATA_SECTION_IDS, false);
  setText("school-name", message);
  setText("school-location", "");
  setText("school-urbanization", "");
  setText("school-control", "");
  setText("school-closure-flag", "");
  setHidden("school-closure-flag", true);
  setText("school-announced-closure", "");
  setHidden("school-announced-closure", true);
  setText("school-announced-merger", "");
  setHidden("school-announced-merger", true);
  setHidden("school-meta-wrap", true);
  setHidden("download-school-data", true);
  setHidden("share-school-profile", true);
  syncSchoolWarningSummaryBadge(null);
}

function slugify(value) {
  return String(value || "college")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function tuitionTrackerSchoolUrl(name, unitid) {
  if (!isNumericUnitid(unitid)) return "https://www.tuitiontracker.org/";
  return `https://www.tuitiontracker.org/schools/${slugify(name)}-${encodeURIComponent(unitid)}`;
}

const SCHOOL_GRAD_PLUS_DOWNLOAD_FIELDS = new Set([
  "grad_plus_recipients",
  "grad_plus_disbursements_amt",
  "grad_plus_disbursements_per_recipient",
  "sector_median_grad_plus_disbursements_per_recipient"
]);

function hasCampusGradPlusSummary(summary) {
  return [
    "grad_plus_recipients",
    "grad_plus_disbursements_amt",
    "grad_plus_disbursements_per_recipient"
  ].some((field) => {
    const value = summary?.[field];
    return value !== null
      && value !== undefined
      && !(typeof value === "string" && value.trim() === "");
  });
}

function downloadSchoolCsv(school) {
  const rows = [["section", "field", "year", "value"]];
  const hasGradPlusSummary = hasCampusGradPlusSummary(school.summary || {});

  Object.entries(school.profile || {}).forEach(([field, value]) => {
    rows.push(["profile", field, "", value ?? ""]);
  });

  Object.entries(school.summary || {}).forEach(([field, value]) => {
    if (SCHOOL_GRAD_PLUS_DOWNLOAD_FIELDS.has(field) && !hasGradPlusSummary) return;
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
    text: `View ${name}'s College Financial Health Tracker profile:`,
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

function styleAnswerCard(answerId, value, resolveState = yesNoClass) {
  const answer = document.getElementById(answerId);
  if (!answer) return;
  const card = answer.closest(".metric-strip");
  if (!card) return;
  const state = resolveState(value);
  card.className = `metric-strip ${state}`;
  answer.className = `metric-answer ${state}`;
  syncWarningTooltip(card, state);
}

// Google indexes the rendered DOM, so per-school canonical/title must be
// set at render time: the static head tags point every ?unitid= URL at
// bare school.html, which would fold all school pages into one search
// result. The canonical base is read from the existing tag so the same
// code works on any host (preview and production domains differ).
function updateSchoolPageMetadata(institutionName, unitid) {
  const name = String(institutionName || "").trim();
  if (!name || !isNumericUnitid(unitid)) return;
  const pageTitle = `${name} \u2014 College Financial Health Tracker`;
  document.title = pageTitle;
  const setMetaContent = (selector, value) => {
    const node = document.querySelector(selector);
    if (node) node.setAttribute("content", value);
  };
  const canonical = document.querySelector('link[rel="canonical"]');
  if (canonical && canonical.href) {
    const canonicalUrl = new URL(canonical.href);
    canonicalUrl.search = `?unitid=${encodeURIComponent(unitid)}`;
    canonical.setAttribute("href", canonicalUrl.toString());
    setMetaContent('meta[property="og:url"]', canonicalUrl.toString());
  }
  setMetaContent('meta[property="og:title"]', pageTitle);
  setMetaContent('meta[name="twitter:title"]', pageTitle);
}

async function init() {
  syncSearchToggle();
  window.addEventListener("resize", syncSearchToggle);

  const unitid = getParam("unitid");
  window.TrackerApp.syncTabs({ active: "finances" });
  if (!unitid) {
    showSchoolGuideLanding();
    initGuideCalloutReveal();
    return;
  }

  showSchoolProfileShell();

  const [school, closureLookup, cutsIndex, accreditationIndex, researchIndex, headlineBenchmarks, endowmentPerFteLookup] = await Promise.all([
    loadJson(`data/schools/${unitid}.json`),
    SHOW_CLOSURE_FLAGS ? loadJsonOrNull("data/closure_status_by_unitid.json") : Promise.resolve(null),
    loadJsonOrNull("data/college_cuts_index.json"),
    loadJsonOrNull("data/accreditation_index.json"),
    loadJsonOrNull("data/research_funding_index.json"),
    loadJsonOrNull("data/sector_headline_benchmarks.json"),
    loadJsonOrNull("data/endowment_per_fte_by_unitid.json")
  ]);
  const p = school.profile;
  updateSchoolPageMetadata(p.institution_name, unitid);
  const s = school.summary;
  const series = school.series;
  const latestDataYear = asNumber(s.latest_year) || latestYearFromSeries(series);
  const closureRecord = SHOW_CLOSURE_FLAGS ? closureLookup?.schools?.[unitid] || null : null;
  const schoolRetrievedAt = school.generated_at || null;
  const graduationRate = asNumber(s.graduation_rate_6yr);
  const medianEarnings = asNumber(s.median_earnings_10yr);
  const medianDebt = asNumber(s.median_debt_completers);
  const fiveYearRangeText = recentFiveYearRangeText(series.revenue_total_adjusted || series.enrollment_headcount_total || []);
  const revenueSeries = toSeries(series.revenue_total_adjusted);
  const expensesSeries = toSeries(series.expenses_total_adjusted);
  const netTuitionSeries = toSeries(series.net_tuition_per_fte_adjusted);
  const enrollmentSeries = toSeries(series.enrollment_headcount_total);
  const enrollmentUndergradSeries = toSeries(series.enrollment_headcount_undergrad);
  const enrollmentGraduateSeries = toSeries(series.enrollment_headcount_graduate);
  const staffTotalSeries = toSeries(series.staff_headcount_total);
  const staffInstructionalSeries = toSeries(series.staff_headcount_instructional);
  const endowmentValueSeries = toSeries(series.endowment_value_adjusted);
  const latestEnrollment = latestPoint(series.enrollment_headcount_total);
  const sectorHeadlineBenchmarks = headlineBenchmarks?.[p.control_label] || null;
  const endowmentPerFteRecord = endowmentPerFteLookup?.schools?.[unitid] || null;
  const revenueBenchmark = firstNumericValue(s.sector_median_revenue_pct_change_5yr, sectorHeadlineBenchmarks?.median_revenue_pct_change_5yr);
  const netTuitionBenchmark = firstNumericValue(s.sector_median_net_tuition_per_fte_change_5yr, sectorHeadlineBenchmarks?.median_net_tuition_per_fte_change_5yr);
  const enrollmentBenchmark = firstNumericValue(s.sector_median_enrollment_pct_change_5yr, sectorHeadlineBenchmarks?.median_enrollment_pct_change_5yr);
  const staffBenchmark = firstNumericValue(s.sector_median_staff_total_headcount_pct_change_5yr, sectorHeadlineBenchmarks?.median_staff_total_headcount_pct_change_5yr);
  const endowmentBenchmark = firstNumericValue(s.sector_median_endowment_pct_change_5yr, sectorHeadlineBenchmarks?.median_endowment_pct_change_5yr);
  const stateAidBenchmark = sectorHeadlineBenchmarks?.median_state_funding_pct_change_5yr;

  const downloadButton = document.getElementById("download-school-data");
  if (downloadButton) {
    downloadButton.onclick = () => downloadSchoolCsv(school);
  }
  const shareButton = document.getElementById("share-school-profile");
  if (shareButton) {
    shareButton.onclick = () => shareSchoolProfile(school, unitid);
  }

  renderSchoolRelatedPages(unitid, p.institution_name, {
    cuts: cutsIndex,
    accreditation: accreditationIndex,
    research: researchIndex
  });
  renderMoreFinancialDetailSection(p.institution_name, p);

  setText("school-name", p.institution_name);
  const closureSentence = buildClosureSentence(closureRecord);
  setText("school-closure-flag", closureSentence || "");
  setHidden("school-closure-flag", !closureSentence);
  const cutsRecord = findRelatedIndexRecord(cutsIndex, unitid, "cut_count");
  const cutsHref = `cuts.html?unitid=${encodeURIComponent(unitid)}`;
  const absorptionFlag = hasAnnouncedAbsorption(cutsRecord);
  const closureYear = absorptionFlag ? null : announcedClosureYear(cutsRecord);

  function buildBadge(text, ariaText, buildTipContent) {
    const badge = document.createElement("span");
    badge.className = "school-announced-closure-badge";
    badge.setAttribute("role", "img");
    badge.setAttribute("tabindex", "0");
    badge.setAttribute("aria-label", ariaText);
    badge.textContent = text;
    const tip = document.createElement("span");
    tip.className = "warning-sign-tooltip";
    tip.setAttribute("role", "tooltip");
    tip.setAttribute("aria-hidden", "true");
    buildTipContent(tip);
    badge.append(tip);
    return badge;
  }

  function tipLink(href, word) {
    const a = document.createElement("a");
    a.href = href;
    a.textContent = word;
    return a;
  }

  function renderBadgeWrap(id, show, badgeNode) {
    const wrap = document.getElementById(id);
    if (!wrap) return;
    if (show && badgeNode) {
      wrap.replaceChildren(badgeNode);
      wrap.classList.remove("is-hidden");
      wrap.setAttribute("aria-hidden", "false");
    } else {
      wrap.replaceChildren();
      wrap.classList.add("is-hidden");
      wrap.setAttribute("aria-hidden", "true");
    }
  }

  const absorptionBadge = absorptionFlag
    ? buildBadge("Absorption announced", "This institution is being absorbed by another institution.", (tip) => {
        tip.append("This institution is being ");
        tip.append(tipLink(cutsHref, "absorbed"));
        tip.append(" by another institution.");
      })
    : null;
  renderBadgeWrap("school-announced-merger", absorptionFlag, absorptionBadge);

  const closureYearSuffix = closureYear ? ` in ${closureYear}` : "";
  const closureBadge = closureYear !== null
    ? buildBadge("Closure announced", `This institution announced its closure${closureYearSuffix}.`, (tip) => {
        tip.append("This institution announced its ");
        tip.append(tipLink(cutsHref, "closure"));
        tip.append(`${closureYearSuffix}.`);
      })
    : null;
  renderBadgeWrap("school-announced-closure", closureYear !== null, closureBadge);
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

  if (asNumber(s.revenue_pct_change_5yr) === null) {
    applyStrip("revenue-change-card", "Revenue data are not available.", revenueChangeState(s), trendDirection(s.revenue_pct_change_5yr));
  } else {
    applyStripLines(
      "revenue-change-card",
      buildTrendCardLines(
        "Revenue",
        s.revenue_pct_change_5yr,
        fiveYearRangeText,
        p,
        revenueBenchmark,
        { afterAdjustingForInflation: true }
      ),
      revenueChangeState(s),
      trendDirection(s.revenue_pct_change_5yr)
    );
  }
  const hasRevenueCard = asNumber(s.revenue_pct_change_5yr) !== null;
  setHidden("revenue-change-card", !hasRevenueCard);

  setText("loss-latest", s.ended_year_at_loss || "No data");
  styleAnswerCard("loss-latest", s.ended_year_at_loss);
  setClosestMetricHidden("loss-latest", !s.ended_year_at_loss);
  const hasLossRepeatCard = hasNonEmptyValue(s.losses_last_3_of_5);
  setText("loss-repeat", s.losses_last_3_of_5 || "No data");
  styleAnswerCard("loss-repeat", s.losses_last_3_of_5, () => lossPatternState(s));
  setClosestMetricHidden("loss-repeat", !hasLossRepeatCard);
  const hasLossYearsCard = hasNonEmptyValue(s.loss_years_last_10);
  setText("loss-years", s.loss_years_last_10 ?? "No data");
  const lossYearsStateValue = lossYearsState(s.loss_years_last_10);
  const lossYearsAnswer = document.getElementById("loss-years");
  const lossYearsCard = lossYearsAnswer?.closest(".metric-strip");
  if (lossYearsAnswer) {
    lossYearsAnswer.className = `metric-answer ${lossYearsStateValue}`;
  }
  if (lossYearsCard) {
    lossYearsCard.className = `metric-strip ${lossYearsStateValue}`;
    syncWarningTooltip(lossYearsCard, lossYearsStateValue);
  }
  setClosestMetricHidden("loss-years", !hasLossYearsCard);

  if (asNumber(s.net_tuition_per_fte_change_5yr) === null) {
    applyStrip("net-tuition-change-card", "Net tuition revenue per full-time equivalent student data are not available.", netTuitionChangeState(s), trendDirection(s.net_tuition_per_fte_change_5yr));
  } else {
    applyStripLines(
      "net-tuition-change-card",
      buildTrendCardLines(
        "Net tuition revenue per full-time equivalent student",
        s.net_tuition_per_fte_change_5yr,
        fiveYearRangeText,
        p,
        netTuitionBenchmark,
        { afterAdjustingForInflation: true }
      ),
      netTuitionChangeState(s),
      trendDirection(s.net_tuition_per_fte_change_5yr)
    );
  }
  const hasNetTuitionCard = asNumber(s.net_tuition_per_fte_change_5yr) !== null;
  setHidden("net-tuition-change-card", !hasNetTuitionCard);

  const tuitionDependenceParagraph = buildTuitionDependenceParagraph(p, s, latestDataYear);
  const hasTuitionSentence = Array.isArray(tuitionDependenceParagraph) && tuitionDependenceParagraph.length > 0;
  setBodyCopy("tuition-sentence-copy", hasTuitionSentence ? [tuitionDependenceParagraph] : []);


  if (asNumber(s.enrollment_pct_change_5yr) === null) {
    applyStrip("enrollment-change-card", "Enrollment data are not available.", enrollmentChangeState(s), trendDirection(s.enrollment_pct_change_5yr));
  } else {
    applyStripLines(
      "enrollment-change-card",
      buildTrendCardLines(
        "Enrollment",
        s.enrollment_pct_change_5yr,
        fiveYearRangeText,
        p,
        enrollmentBenchmark
      ),
      enrollmentChangeState(s),
      trendDirection(s.enrollment_pct_change_5yr)
    );
  }
  const hasEnrollmentCard = asNumber(s.enrollment_pct_change_5yr) !== null;
  setHidden("enrollment-change-card", !hasEnrollmentCard);

  const enrollmentFlag = deriveEnrollmentFlag(s, series);
  const hasEnrollmentFlagCard = enrollmentFlag !== "No data";
  setEnrollmentTotal("enrollment-total", latestEnrollment);
  setHidden("enrollment-total", !latestEnrollment);
  setText("enrollment-flag", enrollmentFlag);
  styleAnswerCard("enrollment-flag", enrollmentFlag, enrollmentDeclineState);
  setClosestMetricHidden("enrollment-flag", !hasEnrollmentFlagCard);

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

  if (asNumber(s.staff_total_headcount_pct_change_5yr) === null) {
    applyStrip("staff-change-card", "Staffing data are not available.", staffChangeState(s), trendDirection(s.staff_total_headcount_pct_change_5yr));
  } else {
    applyStripLines(
      "staff-change-card",
      buildTrendCardLines(
        "Total staff headcount",
        s.staff_total_headcount_pct_change_5yr,
        fiveYearRangeText,
        p,
        staffBenchmark
      ),
      staffChangeState(s),
      trendDirection(s.staff_total_headcount_pct_change_5yr)
    );
  }
  const hasStaffCard = asNumber(s.staff_total_headcount_pct_change_5yr) !== null;
  setHidden("staff-change-card", !hasStaffCard);

  const ratioParagraph = buildInstructionalStaffRatioParagraph(p, s, latestDataYear);
  setBodyCopy("staff-ratio-copy", ratioParagraph ? [ratioParagraph] : []);

  if (asNumber(s.endowment_pct_change_5yr) === null) {
    applyStrip("endowment-change-card", "Endowment data are not available.", endowmentChangeState(s), trendDirection(s.endowment_pct_change_5yr));
  } else {
    applyStripLines(
      "endowment-change-card",
      buildTrendCardLines(
        "The institution's endowment",
        s.endowment_pct_change_5yr,
        fiveYearRangeText,
        p,
        endowmentBenchmark,
        { afterAdjustingForInflation: true }
      ),
      endowmentChangeState(s),
      trendDirection(s.endowment_pct_change_5yr)
    );
  }
  const hasEndowmentCard = asNumber(s.endowment_pct_change_5yr) !== null;
  setHidden("endowment-change-card", !hasEndowmentCard);

  const hasEndowmentValue = hasMeaningfulData(series.endowment_value_adjusted);

  const hasState =
    (asNumber(s.state_funding_pct_core_revenue) ?? 0) !== 0 ||
    ((asNumber(s.state_funding_pct_change_5yr) ?? 0) !== 0) ||
    hasMeaningfulData(series.state_funding_adjusted);
  const isPublic = isPublicProfile(p);
  const showPublicStateAidSection = isPublic && hasState;
  const showAidDropdownState = hasState && !isPublic;

  const endowmentSpendingSeries = toSeries(series.endowment_spending_current_use_adjusted);
  const hasEndowmentSpending = endowmentSpendingSeries.some((point) => Number(point.value) !== 0);
  const showEndowmentSection = hasEndowmentValue || hasEndowmentSpending;
  const hasRevenueChart = revenueSeries.length > 0 || expensesSeries.length > 0;
  const hasNetTuitionChart = netTuitionSeries.length > 0;
  const hasEnrollmentChart = enrollmentSeries.length > 0 || enrollmentUndergradSeries.length > 0 || enrollmentGraduateSeries.length > 0;
  const hasStaffingChart = staffTotalSeries.length > 0 || staffInstructionalSeries.length > 0;
  const hasLossBlock = !!s.ended_year_at_loss || !!s.losses_last_3_of_5 || !(s.loss_years_last_10 === null || s.loss_years_last_10 === undefined || s.loss_years_last_10 === "");
  const showFinancialSection = hasRevenueCard || hasRevenueChart || hasLossBlock || hasNetTuitionCard || hasNetTuitionChart || hasTuitionSentence;
  const showNetTuitionSection = hasNetTuitionCard || hasNetTuitionChart || hasTuitionSentence;
  const showTuitionDependenceSection = hasTuitionSentence;
  const showEnrollmentSection = hasEnrollmentCard || hasEnrollmentChart || enrollmentFlag !== "No data" || hasGradLoanCopy || hasIntlSentence || hasAnyInternationalEnrollment;
  const showIntlSection = hasIntlSentence || hasAnyInternationalEnrollment;
  const showStaffingSection = hasStaffCard || hasStaffingChart || !!ratioParagraph;
  const showAidSection = showAidDropdownState;
  const warningSummary = computeSchoolWarningSummary(s, enrollmentFlag, {
    hasRevenueCard,
    hasLossRepeatCard,
    hasLossYearsCard,
    hasNetTuitionCard,
    hasEnrollmentCard,
    hasEnrollmentFlagCard,
    hasStaffCard,
    hasEndowmentCard
  });
  syncSchoolWarningSummaryBadge(warningSummary, p);

  setSectionVisibility("financial-section", showFinancialSection);
  setSectionVisibility("net-tuition-section", showNetTuitionSection);
  setSectionVisibility("state-aid-section", showPublicStateAidSection);
  setSectionVisibility("tuition-dependence-section", showTuitionDependenceSection);
  setSectionVisibility("enrollment-section", showEnrollmentSection);
  setSectionVisibility("intl-section", showIntlSection);
  setSectionVisibility("staffing-section", showStaffingSection);
  setSectionVisibility("endowment-section", showEndowmentSection);
  setSectionVisibility("state-group", hasState);
  setSectionVisibility("aid-section", showAidSection);
  renderProfileJumpLinks({
    showFinancialSection,
    showEnrollmentSection,
    showStaffingSection,
    showEndowmentSection,
    showFinancialStatementsSection: true
  });
  const stateGroup = document.getElementById("state-group");
  const stateAidAnchor = document.getElementById("state-aid-anchor");
  const aidStateAnchor = document.getElementById("aid-state-anchor");
  if (stateGroup && stateAidAnchor && aidStateAnchor) {
    if (showPublicStateAidSection) {
      stateAidAnchor.appendChild(stateGroup);
    } else {
      aidStateAnchor.appendChild(stateGroup);
    }
  }
  const aidSection = document.getElementById("aid-section");
  if (aidSection && showAidSection) {
    aidSection.open = false;
  }
  const aidIntro = document.getElementById("aid-section-intro");
  if (aidIntro) {
    if (showAidDropdownState) {
      aidIntro.textContent = "Some public colleges depend more than others on state funding. A higher share means the college is more exposed if that funding changes.";
      setHidden("aid-section-intro", false);
    } else {
      setHidden("aid-section-intro", true);
    }
  }
  const aidTitle = document.getElementById("aid-section-title");
  if (aidTitle) {
    aidTitle.textContent = "Want details about state aid?";
  }

  if (hasState) {
    const stateChange5yr = asNumber(s.state_funding_pct_change_5yr);
    setText(
      "state-share-copy",
      `${Number.isFinite(latestDataYear) ? `In ${latestDataYear}, ` : "In the latest year, "}${fmtPlainPct(s.state_funding_pct_core_revenue || 0)} of this college's core revenue came from state appropriations.`
    );
    setHidden("state-share-copy", false);

    if (stateChange5yr === null) {
      applyStrip(
        "state-change-card",
        "State aid data are not available.",
        "neutral",
        ""
      );
    } else {
      applyStripLines(
        "state-change-card",
        buildTrendCardLines(
          "State aid",
          s.state_funding_pct_change_5yr,
          fiveYearRangeText,
          p,
          stateAidBenchmark
        ),
        sentimentClass(s.state_funding_pct_change_5yr),
        trendDirection(s.state_funding_pct_change_5yr)
      );
    }
    setHidden("state-change-card", false);
  } else {
    setHidden("state-share-copy", true);
    setHidden("state-change-card", true);
  }

  const financeTooltip2024Config = {
    showTooltip: true,
    tooltipYear: 2024,
    tooltipPointOnly: true,
    showNativePointTitle: false
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
  const endowmentPerFteParagraph = buildEndowmentPerFteParagraph(p, s, latestDataYear, endowmentPerFteRecord);
  setBodyCopy("endowment-per-fte-copy", endowmentPerFteParagraph ? [endowmentPerFteParagraph] : []);
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
      title: "Withdrawals from endowment to fund expenses (adjusted for inflation)",
      format: "currency",
      ...financeTooltip2024Config,
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

  // Nav tracker is built here after all section visibility updates land.
  setupProfileJumpLinkTracking();
  initMetricArrowReveal();
}

init().catch((error) => {
  console.error(error);
  showSchoolLoadError("This school page could not be loaded.");
});
