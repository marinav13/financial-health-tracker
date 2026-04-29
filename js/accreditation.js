/**
 * FILE: accreditation.js
 * Displays accreditation actions and regulatory history for colleges.
 * Shows all recent actions (paginated) or school-specific detail view via ?unitid= URL param.
 * Tracks warnings, probations, monitoring, closures from 2019 onward.
 */

(function () {
  const {
    loadJson,
    renderPaginationButtons,
    paginateItems,
    escapeHtml,
    getParam,
    renderEmpty,
    renderSortableHeader,
    bindSortControls,
    setDataCardVisible,
    downloadRowsCsv,
    compareText,
    compareDateDesc,
    renderHistoryTable,
    renderSchoolLinkCell,
    renderExternalLinkCell,
    findRelatedIndexRecord,
    isNumericUnitid,
    isPrimaryTrackerInstitution,
    syncTabs,
    renderRelatedInstitutionLinks,
    renderDataAsOf,
    makeTableController
  } = window.TrackerApp;

  // ------ Constants & Lookups ------

  const PAGE_SIZE = 20;
  const OTHER_PAGE_SIZE = 5;
  const TODAY = new Date();
  const MIN_ACTION_YEAR = 2019;

  const ACCREDITOR_NAMES = {
    HLC: "Higher Learning Commission",
    MSCHE: "Middle States Commission on Higher Education",
    SACSCOC: "Southern Association of Colleges and Schools Commission on Colleges",
    NECHE: "New England Commission of Higher Education",
    NWCCU: "Northwest Commission on Colleges and Universities",
    WSCUC: "WASC Senior College and University Commission",
    ACCJC: "Accrediting Commission for Community and Junior Colleges"
  };

  const ACCREDITOR_STATES = {
    HLC:     "Arizona, Arkansas, Colorado, Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, New Mexico, North Dakota, Ohio, Oklahoma, South Dakota, West Virginia, Wisconsin, Wyoming",
    MSCHE:   "Delaware, District of Columbia, Maryland, New Jersey, New York, Pennsylvania, Puerto Rico, U.S. Virgin Islands",
    NECHE:   "Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, Vermont",
    NWCCU:   "Alaska, Idaho, Montana, Nevada, Oregon, Washington",
    SACSCOC: "Alabama, Florida, Georgia, Kentucky, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, Texas, Virginia",
    WSCUC:   "California, Hawaii, Guam, American Samoa, Federated States of Micronesia, Republic of Palau, Commonwealth of the Northern Mariana Islands",
    ACCJC:   "California, Hawaii, Guam, American Samoa, Federated States of Micronesia, Republic of Palau, Commonwealth of the Northern Mariana Islands"
  };

  const ACCREDITOR_URLS = {
    HLC:     "https://www.hlcommission.org/",
    MSCHE:   "https://www.msche.org/",
    SACSCOC: "https://sacscoc.org/",
    NECHE:   "https://www.neche.org/",
    NWCCU:   "https://nwccu.org/",
    WSCUC:   "https://www.wscuc.org/",
    ACCJC:   "https://accjc.org/"
  };

  const STATE_ABBR_TO_NAME = {
    AL: "Alabama", AK: "Alaska", AZ: "Arizona", AR: "Arkansas", CA: "California", CO: "Colorado",
    CT: "Connecticut", DE: "Delaware", FL: "Florida", GA: "Georgia", HI: "Hawaii", ID: "Idaho",
    IL: "Illinois", IN: "Indiana", IA: "Iowa", KS: "Kansas", KY: "Kentucky", LA: "Louisiana",
    ME: "Maine", MD: "Maryland", MA: "Massachusetts", MI: "Michigan", MN: "Minnesota",
    MS: "Mississippi", MO: "Missouri", MT: "Montana", NE: "Nebraska", NV: "Nevada",
    NH: "New Hampshire", NJ: "New Jersey", NM: "New Mexico", NY: "New York",
    NC: "North Carolina", ND: "North Dakota", OH: "Ohio", OK: "Oklahoma", OR: "Oregon",
    PA: "Pennsylvania", RI: "Rhode Island", SC: "South Carolina", SD: "South Dakota",
    TN: "Tennessee", TX: "Texas", UT: "Utah", VT: "Vermont", VA: "Virginia",
    WA: "Washington", WV: "West Virginia", WI: "Wisconsin", WY: "Wyoming", DC: "District of Columbia"
  };

  // ------ Utility Functions ------

  function isPrimaryBachelorsInstitution(record) {
    return isPrimaryTrackerInstitution(record);
  }

  function titleCaseSlug(slug) {
    return String(slug || "")
      .split("-")
      .filter(Boolean)
      .map((part) => part.charAt(0).toUpperCase() + part.slice(1).toLowerCase())
      .join(" ");
  }

  function inferInstitutionNameFromUrl(url) {
    const value = String(url || "").trim();
    if (!value) return "";
    try {
      const parsed = new URL(value, window.location.origin);
      const segments = parsed.pathname.split("/").filter(Boolean);
      const slug = segments[segments.length - 1] || "";
      if (!slug || !/[a-z]/i.test(slug)) return "";
      return titleCaseSlug(slug);
    } catch (_) {
      return "";
    }
  }

  function resolveInstitutionName(rawName, action = {}) {
    const direct = String(rawName || "").trim();
    if (direct) return direct;
    const sourceTitle = String(action.source_title || "").trim();
    const titleMatch = sourceTitle.match(/\s(?:\u2013|-)\s(.+)$/);
    const titleName = String(titleMatch?.[1] || "").trim();
    if (titleName && !/^n\/?a$/i.test(titleName)) return titleName;
    return inferInstitutionNameFromUrl(action.source_page_url || action.source_url || "");
  }

  function expandAccreditors(value) {
    if (!value) return "";
    return String(value)
      .split(",")
      .map((part) => part.trim())
      .filter(Boolean)
      .map((part) => ACCREDITOR_NAMES[part] || part)
      .join(", ");
  }

  function parseActionDate(action) {
    const raw = String(action.action_date || "").trim();
    if (!raw) return null;
    if (/^\d{4}-\d{2}-\d{2}$/.test(raw)) {
      const parsed = new Date(`${raw}T00:00:00`);
      return Number.isNaN(parsed.getTime()) ? null : parsed;
    }
    if (/^\d{4}-\d{2}$/.test(raw)) {
      const parsed = new Date(`${raw}-01T00:00:00`);
      return Number.isNaN(parsed.getTime()) ? null : parsed;
    }
    const parsedMonthDate = new Date(raw);
    if (!Number.isNaN(parsedMonthDate.getTime())) {
      return parsedMonthDate;
    }
    return null;
  }

  function hasOccurred(action) {
    const actionDate = parseActionDate(action);
    if (actionDate) return actionDate.getTime() <= TODAY.getTime();
    const year = getActionYear(action);
    return !Number.isNaN(year) && year >= MIN_ACTION_YEAR && year <= TODAY.getFullYear();
  }

  function isDisplayAction(action) {
    return action?.display_action !== false;
  }

  // Builds the action cell for the GLOBAL recent-actions table. Prefers
  // action_label_short (the compact bucket label set by build_web_exports.R
  // for MSCHE per-institution rows whose verbatim sentences are too long
  // for a table view) and falls back through action_label, action_label_raw,
  // and action_type. The per-school detail view must NOT use this helper --
  // it explicitly references action_label_raw / action_label so the full
  // source sentence stays visible one click away.
  //
  // When the scraper captured a scope qualifier (e.g. "Master of Social
  // Work degree at its Bedford, Cape Cod, and Fall River locations" for
  // NECHE program-level actions) we append it inline in parentheses after
  // the label. Returned as plain text; renderHistoryTable's default branch
  // runs it through escapeHtml.
  function actionLabelCell(action) {
    const label = action.action_label_short
      || action.action_label
      || action.action_label_raw
      || action.action_type
      || "";
    const scope = String(action.action_scope || "").trim();
    if (!scope) return label;
    if (!label) return `(${scope})`;
    return `${label} (${scope})`;
  }

  function actionSortLabel(action) {
    return String(actionLabelCell(action) || "").trim();
  }

  // Normalize action text for matching
  function normalizeActionText(text) {
    return String(text || "").toLowerCase().replace(/\s+/g, " ").trim();
  }

  // Complex regex patterns to identify significant actions (warnings, probations, monitoring, closures)
  // while filtering out minor actions like substantive changes or program additions
  // Phase 1: types the R-side classifier upgrades from raw text are
  // trusted as real actions, regardless of any "substantive change"
  // wording in the body. Without this exemption, Saint Rose's Feb 20 +
  // Dec 18 2023/2024 closure-prep rows (whose bodies say "substantive
  // change request for institutional closure") got dropped by the
  // keyword exclusion despite classify_action correctly tagging them
  // as adverse_action.
// MSCHE administrative/procedural action shapes that don't fit the
  // page's stated scope (warnings/probation/closure/sanction-removed).
  // These are not sanctions or status changes -- they're procedural
  // requests for follow-up reports, scheduled visit notes, or candidate
  // assessment paperwork. Drop them from the global table even when
  // classify_action upstream typed them as adverse_action (because the
  // body mentions a closure-related noun like "teach-out plan" in a
  // requirement-to-submit context, not an approval context).
  const MSCHE_PROCEDURAL_DROP_PATTERNS = [
    // Phase 4 hotfix: optional staff-acted prefix is "Staff acted on
    // behalf of the Commission " (no trailing "to") -- the "to" comes
    // from the verb that follows. Earlier shape "(commission to )?"
    // double-consumed "to" when the prefix was present. R-side fallback
    // also strips this prefix from action_label_short, but action_label
    // still carries the full text so we cover both via labelText below.
    /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?supplemental information report/i,
    /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?monitoring report/i,
    /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?candidate assessment/i,
    /^\s*(?:staff acted on behalf of the commission )?to request an? updated teach-?out plan/i,
    // Length cap raised from 80 to 200 because Rider-style preambles
    // ("To require that the institution complete and submit for
    // approval, by June 1, 2026, an updated, comprehensive, and
    // implementable") regularly run 100-150 chars before "teach-out plan".
    // Use [^.] instead of . to avoid crossing sentence boundaries.
    /^\s*to require [^.]{0,200}?teach-?out plan/i,
    /^\s*to request [^.]{0,200}?teach-?out plan/i,
    /^\s*to note the follow-up team visit/i,
    /^\s*to note that the complex substantive change visit occurred/i,
    /^\s*to note that an? updated teach-?out plan [^.]{0,80}? will not be required/i,
    // COVID-19 / pandemic distance-learning waiver -- temporary
    // procedural relief, not a sanction or status change.
    /^\s*(?:staff acted on behalf of the commission )?to temporarily waive substantive change policy/i,
    // Generic regulatory teach-out plan compliance for candidate
    // institutions (federal regulation 34 CFR 602.23(f)(1)(ii)). These
    // are accreditation-application paperwork, not real teach-outs
    // with named partner institutions.
    /^\s*to approve the teach-?out plan as required of candidate/i,
    // Phase 4 v2: rejection of a previously-required teach-out plan
    // submission. This is procedural feedback ("the plan you submitted
    // wasn't acceptable; submit another"), not a sanction action.
    /^\s*to reject the teach-?out plan/i,
    // Phase 4 v2: supplemental info report deemed inadequate. Same
    // procedural-feedback shape as above; the institution is still
    // operating under whatever sanction prompted the report request.
    /^\s*to note that the supplemental information report was not conducive/i
  ];
  const TRUSTED_ACTION_TYPES = new Set([
    "adverse_action", "warning", "probation", "show_cause", "removed", "notice"
  ]);

  function isTrackedAction(action) {
    const type = normalizeActionText(action.action_type);
    const accreditor = String(action.accreditor || "").toUpperCase();
    const label = normalizeActionText(action.action_label || action.action_label_raw);
    const notes = normalizeActionText(action.notes);
    const haystack = `${type} ${label} ${notes}`;
    const contentOnly = `${label} ${notes}`;

    // Phase 1: MSCHE per-institution rows that classify as "monitoring"
    // are routine "acknowledged the monitoring report" administrative
    // updates -- not the warnings/probation/closure scope this page
    // advertises in its intro text. Drop them from BOTH the global
    // recent-actions table and the per-school detail view to avoid
    // burying the meaningful sanction rows. HLC and SACSCOC monitoring
    // signals are kept since they represent more substantive monitoring
    // statuses, not routine receipt acknowledgements.
    if (accreditor === "MSCHE" && type === "monitoring") return false;

    // Phase 4: drop procedural MSCHE rows (request-for-report,
    // requirement-to-submit-teach-out-plan, follow-up-visit notes,
    // candidate-assessment paperwork) from the global table. These
    // are administrative procedure, not sanctions or status changes.
    if (accreditor === "MSCHE") {
      // Check action_label_short FIRST: R-side helper has stripped the
      // "Staff acted on behalf of the Commission" preamble there, so
      // anchored-at-start patterns match cleanly. Fall through to
      // action_label / action_label_raw as a safety net if the helper
      // output is missing or unexpected.
      const candidateLabels = [
        action.action_label_short,
        action.action_label,
        action.action_label_raw
      ].filter(function (s) { return typeof s === "string" && s.length > 0; });
      for (const pat of MSCHE_PROCEDURAL_DROP_PATTERNS) {
        if (candidateLabels.some(function (label) { return pat.test(label); })) {
          return false;
        }
      }
    }

    const excludedPattern = /substantive change|program addition/;
    if (excludedPattern.test(haystack) && !TRUSTED_ACTION_TYPES.has(type)) return false;

    const statusActionPattern = /warning|probation|formal notice of concern|notice of concern|\bmonitoring\b|removed from (warning|probation|formal notice of concern|notice of concern|notice|monitoring)|removed from membership|placed on probation|issue a notice of concern|continue a warning|continued on warning|continued on probation|denied reaffirmation/;
    const closureActionPattern = /accepted notification of institutional closure|accept(?:ed)? teach-?out plan|teach out plan|teach-out plan|removed from membership/;
    const requiredReportPattern = /require (?:the institution to provide )?(?:an )?(?:interim|progress|follow-?up|monitoring) report/;
    const standaloneLowSignalPattern = /^(special visit|interim report|progress report|accepted progress report|accepted interim report|follow-?up report|monitoring report|second monitoring report|third monitoring report)$/;
    const hasSpecialVisit = /special visit/.test(haystack);
    const hasSanctionDecision = statusActionPattern.test(contentOnly) || closureActionPattern.test(contentOnly) || requiredReportPattern.test(contentOnly);

    if (hasSpecialVisit && !hasSanctionDecision) return false;

    if (statusActionPattern.test(contentOnly) || closureActionPattern.test(contentOnly) || requiredReportPattern.test(contentOnly)) {
      return true;
    }
    if (standaloneLowSignalPattern.test(label)) return false;

    return ["warning", "probation", "monitoring", "notice"].includes(type) ||
      /removed from membership|teach-?out|institutional closure/.test(haystack);
  }

  function isRecentTrackedAction(action) {
    const year = getActionYear(action);
    return isTrackedAction(action) && !Number.isNaN(year) && year >= MIN_ACTION_YEAR && hasOccurred(action);
  }

  function formatActionDate(action) {
    return action.action_date || action.action_year || "";
  }

  function getActionLink(action) {
    return action.source_url || action.source_page_url || "";
  }

  function getActionYear(action) {
    const explicitYear = Number(action.action_year || "");
    if (!Number.isNaN(explicitYear) && explicitYear > 0) return explicitYear;
    const dateText = String(action.action_date || "");
    const match = dateText.match(/\b(19|20)\d{2}\b/);
    return match ? Number(match[0]) : NaN;
  }

  // Extracts state from notes field (e.g., ", TX" -> "Texas")
  function inferStateFromNotes(notes) {
    const match = String(notes || "").match(/,\s*([A-Z]{2})\b/);
    if (!match) return "";
    return STATE_ABBR_TO_NAME[match[1]] || "";
  }

  function isRecentDisplayAction(action) {
    // isRecentTrackedAction already checks year validity, MIN_ACTION_YEAR floor,
    // and hasOccurred — no need to re-check them here.
    return isDisplayAction(action) && isRecentTrackedAction(action);
  }

  // Deduplication key: accreditor + action label + date + URL
  function dedupeActions(actions) {
    const seen = new Set();
    return (actions || []).filter((action) => {
      const key = [
        action.accreditor || "",
        action.action_label || action.action_label_raw || action.action_type || "",
        action.action_date || action.action_year || "",
        getActionLink(action) || ""
      ].join("||");
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }

  // Per-accreditor de-duplication for institutions that appear in BOTH a
  // by-status snapshot row and a per-institution detail row.
  //
  // HLC's snapshot labels are concise current-status summaries ("On
  // Notice", "On Probation") that read better than the detail page's
  // verbose restatement ("Placed on Probation. The institution was
  // notified of this action on November 11, 2025. Information was
  // posted for the public on November 12, 2025..."). When both
  // exist, prefer the snapshot and drop the detail.
  //
  // MSCHE's snapshot labels are bare category strings ("Non-Compliance
  // Probation") with no action context; the per-institution detail rows
  // carry the actual board action sentence. When both exist, prefer
  // the detail and drop the snapshot.
  //
  // For institutions that only have one shape of row, nothing is
  // dropped -- the lone row surfaces regardless of accreditor.
  const HLC_SNAPSHOT_LABELS = new Set([
    "On Notice",
    "On Probation",
    "Removal of Sanction",
    "Withdrawal of Accreditation"
  ]);
  const MSCHE_SNAPSHOT_LABELS = new Set([
    "Non-Compliance Warning",
    "Non-Compliance Probation",
    "Non-Compliance Show Cause",
    "Adverse Action"
  ]);

  function getEffectiveActions(school) {
    const actions = dedupeActions(Array.isArray(school?.actions) ? school.actions : []);
    if (actions.length === 0) return actions;

    const isHlcSnapshot = (a) =>
      String(a.accreditor || "").toUpperCase() === "HLC" &&
      HLC_SNAPSHOT_LABELS.has(a.action_label || a.action_label_raw);
    const isMscheSnapshot = (a) =>
      String(a.accreditor || "").toUpperCase() === "MSCHE" &&
      MSCHE_SNAPSHOT_LABELS.has(a.action_label || a.action_label_raw);

    const hlcHasSnapshot = actions.some(isHlcSnapshot);
    const hlcHasDetail = actions.some((a) =>
      String(a.accreditor || "").toUpperCase() === "HLC" && !isHlcSnapshot(a)
    );
    const mscheHasSnapshot = actions.some(isMscheSnapshot);
    const mscheHasDetail = actions.some((a) =>
      String(a.accreditor || "").toUpperCase() === "MSCHE" && !isMscheSnapshot(a)
    );

    return actions.filter((action) => {
      const acc = String(action.accreditor || "").toUpperCase();
      if (acc === "HLC" && hlcHasSnapshot && hlcHasDetail && !isHlcSnapshot(action)) {
        return false;
      }
      if (acc === "MSCHE" && mscheHasSnapshot && mscheHasDetail && isMscheSnapshot(action)) {
        return false;
      }
      return true;
    });
  }

  // ------ Table Rendering ------
  function sortAccreditationActions(items, sortState) {
    const sorted = (items || []).slice();
    const direction = sortState?.direction === "asc" ? 1 : -1;
    sorted.sort((a, b) => {
      if (sortState?.key === "institution_name") {
        const primary = compareText(a.institution_name, b.institution_name) * direction;
        if (primary !== 0) return primary;
        return compareDateDesc(a.action_date || a.action_year, b.action_date || b.action_year);
      }
      if (sortState?.key === "accreditor") {
        const primary = compareText(expandAccreditors(a.accreditor || ""), expandAccreditors(b.accreditor || "")) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "action_label") {
        const primary = compareText(actionSortLabel(a), actionSortLabel(b)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "state") {
        const primary = compareText(a.state, b.state) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "control_label") {
        const primary = compareText(a.control_label, b.control_label) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "action_date") {
        const primary = compareDateDesc(a.action_date || a.action_year, b.action_date || b.action_year) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      return compareDateDesc(a.action_date || a.action_year, b.action_date || b.action_year);
    });
    return sorted;
  }

function renderSchoolActions(actions, school, sortState, relatedIndexes) {
    const filtered = (actions || []).filter(isRecentDisplayAction);
    if (!filtered.length) return renderEmpty("No accreditation actions found.");
    const schoolName = resolveInstitutionName(school?.institution_name, filtered[0]);
    const detailRows = filtered.map((action) => ({
      ...action,
      institution_name: schoolName,
      state: school?.state || inferStateFromNotes(action.notes),
      control_label: school?.control_label || ""
    }));
    const rows = sortAccreditationActions(detailRows, sortState)
      .map((action) => {
        // Per-school detail must show the FULL board-action language, not
        // the compact bucket label used in the global table. Explicit
        // reference to action_label_raw documents the intent for future
        // maintainers; action_label_short is intentionally NOT consulted.
        const fullLabel = action.action_label
          || action.action_label_raw
          || action.action_type
          || "";
        const scope = String(action.action_scope || "").trim();
        const labelCell = scope
          ? (fullLabel ? `${fullLabel} (${scope})` : `(${scope})`)
          : fullLabel;
        return [
          expandAccreditors(action.accreditor || ""),
          labelCell,
          action.state || "",
          action.control_label || "",
          formatActionDate(action),
          renderExternalLinkCell(getActionLink(action), "Source link")
        ];
      });
    return `${renderHistoryTable({
      ariaLabel: "Accreditation actions for this institution",
      headers: [
        renderSortableHeader("accreditor", sortState, "Accreditor"),
        renderSortableHeader("action_label", sortState, "Action"),
        renderSortableHeader("state", sortState, "State"),
        renderSortableHeader("control_label", sortState, "Sector"),
        renderSortableHeader("action_date", sortState, "Date"),
        "<th>Link</th>"
      ],
      rows
    })}${renderRelatedInstitutionLinks({
      unitid: school?.unitid,
      financialUnitid: school?.financial_unitid,
      current: "accreditation",
      relatedIndexes
    })}`;
  }

  function buildDefaultActionRows(data) {
    const schools = data?.schools || data || {};
    return Object.values(schools)
      .flatMap((school) => {
        const actionRows = Array.isArray(school.landing_actions)
          ? school.landing_actions
          : getEffectiveActions(school).filter(isRecentDisplayAction);
        return actionRows.map((action) => ({
          unitid: school.unitid,
          institution_name: resolveInstitutionName(school.institution_name, action),
          city: school.city || "",
          state: school.state || inferStateFromNotes(action.notes),
          control_label: school.control_label || "",
          category: school.category || "",
          is_primary_tracker: school.is_primary_tracker === true,
          accreditor: action.accreditor || "",
          action_label: action.action_label || action.action_label_raw || action.action_type || "",
          action_label_short: action.action_label_short || "",
          action_scope: action.action_scope || "",
          action_type: action.action_type || "",
          action_date: action.action_date || "",
          action_year: action.action_year || "",
          display_action: action.display_action !== false,
          source_url: getActionLink(action)
        }));
      })
      .sort((a, b) => {
        const dateCompare = String(b.action_date || b.action_year || "").localeCompare(String(a.action_date || a.action_year || ""));
        if (dateCompare !== 0) return dateCompare;
        const nameCompare = String(a.institution_name || "").localeCompare(String(b.institution_name || ""));
        if (nameCompare !== 0) return nameCompare;
        return expandAccreditors(a.accreditor || "").localeCompare(expandAccreditors(b.accreditor || ""));
      });
  }

  function renderActionTablePage(actions, page, pageSize, emptyMessage, linkNames = true, sortState = { key: "action_date", direction: "desc" }) {
    const { totalPages, currentPage, pageItems } = paginateItems(actions, page, pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    const rows = pageItems
      .map((action) => [
        linkNames ? renderSchoolLinkCell(action.unitid, action.institution_name, "accreditation.html") : action.institution_name || "",
        expandAccreditors(action.accreditor || ""),
        actionLabelCell(action),
        action.state || "",
        action.control_label || "",
        action.action_date || action.action_year || "",
        renderExternalLinkCell(action.source_url, "Source link")
      ]);

    return `
      ${renderHistoryTable({
        ariaLabel: linkNames ? "Recent accreditation actions by institution" : "Recent accreditation actions",
        headers: [
          renderSortableHeader("institution_name", sortState, "Institution"),
          renderSortableHeader("accreditor", sortState, "Accreditor"),
          renderSortableHeader("action_label", sortState, "Action"),
          renderSortableHeader("state", sortState, "State"),
          renderSortableHeader("control_label", sortState, "Sector"),
          renderSortableHeader("action_date", sortState, "Date"),
          "<th>Link</th>"
        ],
        rows
      })}
      <div class="pagination" aria-label="Accreditation actions pages">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function renderOtherActionTablePage(actions, page, pageSize, emptyMessage, linkNames = true, sortState = { key: "action_date", direction: "desc" }) {
    const { totalPages, currentPage, pageItems } = paginateItems(actions, page, pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    const rows = pageItems
      .map((action) => [
        linkNames ? renderSchoolLinkCell(action.unitid, action.institution_name, "accreditation.html") : action.institution_name || "",
        actionLabelCell(action),
        action.state || "",
        action.action_date || action.action_year || "",
        renderExternalLinkCell(action.source_url, "Source link")
      ]);

    return `
      ${renderHistoryTable({
        ariaLabel: linkNames ? "Recent accreditation actions at other institutions" : "Recent accreditation actions",
        headers: [
          renderSortableHeader("institution_name", sortState, "Institution"),
          renderSortableHeader("action_label", sortState, "Action"),
          renderSortableHeader("state", sortState, "State"),
          renderSortableHeader("action_date", sortState, "Date"),
          "<th>Link</th>"
        ],
        rows
      })}
      <div class="pagination" aria-label="Accreditation actions pages">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  // Options bag (replaces 8-positional-arg signature). All keys optional except container + actions.
  // Shared keys with cuts.js / research.js so a future shared extraction is trivial.
  function setupPagination({
    container,
    actions,
    pageSize = PAGE_SIZE,
    emptyMessage = "No accreditation actions found.",
    downloadButtonId = null,
    downloadFilename = "accreditation-actions.csv",
    searchInput = null,
    linkNames = true
  }) {
    makeTableController({
      container,
      items: actions,
      pageSize,
      searchInput,
      initialSortState: { key: "action_date", direction: "desc" },
      sortItems: sortAccreditationActions,
      renderPage: (filteredActions, currentPage, size, sortState) => renderActionTablePage(filteredActions, currentPage, size, emptyMessage, linkNames, sortState),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "Accreditor", "Action", "Scope", "State", "Sector", "Date", "Source"],
      downloadRow: (action) => [
        action.institution_name || "",
        expandAccreditors(action.accreditor || ""),
        action.action_label || action.action_type || "",
        action.action_scope || "",
        action.state || "",
        action.control_label || "",
        action.action_date || action.action_year || "",
        action.source_url || ""
      ]
    });
  }

  function setupOtherPagination({
    container,
    actions,
    pageSize = PAGE_SIZE,
    emptyMessage = "No accreditation actions found.",
    downloadButtonId = null,
    downloadFilename = "accreditation-actions.csv",
    searchInput = null,
    linkNames = true
  }) {
    makeTableController({
      container,
      items: actions,
      pageSize,
      searchInput,
      initialSortState: { key: "action_date", direction: "desc" },
      sortItems: sortAccreditationActions,
      renderPage: (filteredActions, currentPage, size, sortState) => renderOtherActionTablePage(filteredActions, currentPage, size, emptyMessage, linkNames, sortState),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "Action", "Scope", "State", "Date", "Source"],
      downloadRow: (action) => [
        action.institution_name || "",
        action.action_label || action.action_type || "",
        action.action_scope || "",
        action.state || "",
        action.action_date || action.action_year || "",
        action.source_url || ""
      ]
    });
  }

  function renderLimitations(data) {
    const representedStates = [...new Set(Object.values(data.schools || {}).map((school) => school.state).filter(Boolean))].sort();

    // Collect accreditor codes from the JSON (covered_accreditors is the canonical list).
    const accreditorCodes = new Set();
    (data.covered_accreditors || []).forEach((value) => {
      String(value || "")
        .split(/[;,]/)
        .map((part) => part.trim())
        .filter(Boolean)
        .forEach((part) => accreditorCodes.add(part));
    });

    // Build a linked list of tracked accreditors, sorted alphabetically by name.
    const trackedLinks = [...accreditorCodes]
      .filter((code) => ACCREDITOR_NAMES[code])
      .sort((a, b) => (ACCREDITOR_NAMES[a] || a).localeCompare(ACCREDITOR_NAMES[b] || b))
      .map((code) => {
        const name = ACCREDITOR_NAMES[code] || code;
        const url = ACCREDITOR_URLS[code];
        const states = ACCREDITOR_STATES[code];
        const codeLabel = ACCREDITOR_NAMES[code] ? `(${code})` : "";
        const rawLinkLabel = `${name} ${codeLabel}`.trim();
        const safeStatesNote = states ? ` (${escapeHtml(states)})` : "";
        if (url) {
          const linkHtml = window.TrackerApp.renderExternalLink(url, rawLinkLabel)
            || escapeHtml(rawLinkLabel);
          return `<li>${linkHtml}${safeStatesNote}</li>`;
        }
        return `<li>${escapeHtml(rawLinkLabel)}${safeStatesNote}</li>`;
      });

    const covered = trackedLinks.length
      ? `<p><strong>Currently tracked accreditors:</strong></p><ul class="link-list">${trackedLinks.join("")}</ul>`
      : "";

    const helpText = `<p>The tables above show universities that have received — or corrected — notices of warning, monitoring, probation, and other commission actions that require follow-up reports from 2019 to the present. The tables also show closure and teach-out decisions. A college can still be accredited while also being under warning, notice, monitoring, probation, or another follow-up action.</p>
        <p>This page currently tracks accreditation actions by the accreditors listed above dating back to 2019. The main table focuses on 4-year, primarily bachelor's-degree-granting institutions in the financial tracker, while the second table focuses on other institutions.</p>
        <p>A missing result here does not mean an institution has no accreditation history or current accreditation.</p>
        <p>If your college is not included here, students should check the website of their institution or the appropriate accreditor.</p>`;
    return `${covered}${helpText}`;
  }

  // ------ Initialization ------

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid, { active: "accreditation" });

    if (!unitid) {
      const [accreditationIndex, metadata] = await Promise.all([
        loadJson("data/accreditation_index.json"),
        loadJson("data/metadata.json")
      ]);
      renderDataAsOf("accreditation-data-as-of", metadata?.generated_at);
      document.getElementById("accreditation-limitations").innerHTML = renderLimitations({
        covered_accreditors: Object.keys(ACCREDITOR_NAMES),
        schools: {}
      });
      // Landing page: retain a real document heading for screen-reader users
      // but keep it visually hidden so the existing banner layout is unchanged.
      const landingHeading = document.getElementById("accreditation-school-name");
      landingHeading.textContent = "Accreditation actions";
      landingHeading.classList.add("sr-only");
      landingHeading.classList.remove("is-hidden");
      const primaryFilter = document.getElementById("accreditation-filter");
      const primaryFilterLabel = document.querySelector('label[for="accreditation-filter"]');
      if (primaryFilter) primaryFilter.classList.remove("is-hidden");
      if (primaryFilterLabel) primaryFilterLabel.classList.remove("is-hidden");
      const allActions = buildDefaultActionRows(accreditationIndex);
      const primaryActions = allActions.filter(isPrimaryBachelorsInstitution);
      const otherActions = allActions.filter((action) => !isPrimaryBachelorsInstitution(action));
      setDataCardVisible("accreditation-other-status", true);
      const otherFilter = document.getElementById("accreditation-other-filter");
      setupPagination({
        container: document.getElementById("accreditation-status"),
        actions: primaryActions,
        emptyMessage: "No accreditation actions from 2019 to the present are available for 4-year, primarily bachelor's-degree-granting institutions.",
        downloadButtonId: "accreditation-table-download",
        downloadFilename: "accreditation-primary.csv",
        searchInput: primaryFilter
      });
      setupOtherPagination({
        container: document.getElementById("accreditation-other-status"),
        actions: otherActions,
        pageSize: OTHER_PAGE_SIZE,
        emptyMessage: "No accreditation actions from 2019 to the present are available for other institutions.",
        downloadButtonId: "accreditation-other-download",
        downloadFilename: "accreditation-other.csv",
        searchInput: otherFilter,
        linkNames: false
      });
      return;
    }

    const [accreditationIndex, cutsIndex, researchIndex, metadata] = await Promise.all([
      loadJson("data/accreditation_index.json"),
      loadJson("data/college_cuts_index.json"),
      loadJson("data/research_funding_index.json"),
      loadJson("data/metadata.json")
    ]);
    renderDataAsOf("accreditation-data-as-of", metadata?.generated_at);
    const limitations = document.getElementById("accreditation-limitations");
    if (limitations) {
      limitations.innerHTML = renderLimitations({
        covered_accreditors: Object.keys(ACCREDITOR_NAMES),
        schools: {}
      });
    }
    const relatedIndexes = {
      cuts: cutsIndex,
      accreditation: accreditationIndex,
      research: researchIndex
    };
    const indexedSchool = findRelatedIndexRecord(accreditationIndex, unitid, "action_count");
    let data = null;
    let school = null;

    if (indexedSchool || !isNumericUnitid(unitid)) {
      data = await loadJson("data/accreditation.json");
      renderDataAsOf("accreditation-data-as-of", data?.generated_at);
      if (limitations) limitations.innerHTML = renderLimitations(data);
      school = data.schools?.[indexedSchool?.unitid || unitid];
    }

    // School-specific view
    if (!school) {
      const missingHeading = document.getElementById("accreditation-school-name");
      missingHeading.textContent = "No tracked accreditation record found";
      missingHeading.classList.remove("is-hidden");
      missingHeading.classList.remove("sr-only");
      document.getElementById("accreditation-status").innerHTML = renderEmpty("No accreditation actions found.");
      return;
    }

    const schoolHeading = document.getElementById("accreditation-school-name");
    const schoolActions = getEffectiveActions(school).filter(isRecentDisplayAction);
    const schoolName = resolveInstitutionName(school.institution_name, schoolActions[0]);
    schoolHeading.textContent = schoolName || "Accreditation";
    syncTabs(unitid, { active: "accreditation", financialUnitid: school.financial_unitid });
    schoolHeading.classList.remove("is-hidden");
    schoolHeading.classList.remove("sr-only");
    const accreditationOverview = document.getElementById("accreditation-overview");
    if (accreditationOverview) {
      accreditationOverview.classList.add("is-hidden");
      accreditationOverview.textContent = "";
    }
    setDataCardVisible("accreditation-other-status", false);
    const otherStatus = document.getElementById("accreditation-other-status");
    const otherTitle = document.getElementById("accreditation-other-title");
    const mainDownload = document.getElementById("accreditation-table-download");
    const primaryFilter = document.getElementById("accreditation-filter");
    const primaryFilterLabel = document.querySelector('label[for="accreditation-filter"]');
    if (primaryFilter) primaryFilter.classList.add("is-hidden");
    if (primaryFilterLabel) primaryFilterLabel.classList.add("is-hidden");
    if (otherStatus) otherStatus.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";
    let detailSortState = { key: "action_date", direction: "desc" };
    const renderDetailTable = () => {
      document.getElementById("accreditation-status").innerHTML = renderSchoolActions(
        schoolActions,
        { ...school, institution_name: schoolName },
        detailSortState,
        relatedIndexes
      );
      bindSortControls(
        document.getElementById("accreditation-status"),
        detailSortState,
        { key: "action_date", direction: "desc" },
        (nextSortState) => {
          detailSortState = nextSortState;
          renderDetailTable();
        }
      );
    };
    renderDetailTable();
    if (mainDownload) {
      mainDownload.classList.toggle("is-hidden", schoolActions.length === 0);
      mainDownload.onclick = () => downloadRowsCsv(
        `${String(schoolName || "accreditation").toLowerCase().replace(/[^a-z0-9]+/g, "-")}-accreditation.csv`,
        ["Accreditor", "Action", "Scope", "State", "Sector", "Date", "Source"],
        schoolActions.map((action) => [
          expandAccreditors(action.accreditor || ""),
          action.action_label || action.action_label_raw || action.action_type || "",
          action.action_scope || "",
          school.state || "",
          school.control_label || "",
          formatActionDate(action),
          getActionLink(action)
        ])
      );
    }
  }

  function showLoadError(error) {
    console.error(error);
    const heading = document.getElementById("accreditation-school-name");
    const mainStatus = document.getElementById("accreditation-status");
    const otherStatus = document.getElementById("accreditation-other-status");
    const limitations = document.getElementById("accreditation-limitations");
    const mainDownload = document.getElementById("accreditation-table-download");
    const otherDownload = document.getElementById("accreditation-other-download");

    if (heading) {
      heading.textContent = "Accreditation data unavailable";
      heading.classList.remove("is-hidden");
      heading.classList.remove("sr-only");
    }
    if (mainStatus) {
      mainStatus.innerHTML = renderEmpty("Accreditation actions could not be loaded. Please try again later.");
    }
    if (otherStatus) {
      otherStatus.innerHTML = "";
    }
    if (limitations) {
      limitations.innerHTML = renderEmpty("Methodology details could not be loaded with the accreditation data.");
    }
    if (mainDownload) mainDownload.classList.add("is-hidden");
    if (otherDownload) otherDownload.classList.add("is-hidden");
    setDataCardVisible("accreditation-other-status", false);
  }

  init().catch(showLoadError);
})();
