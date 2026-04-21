/**
 * FILE: accreditation.js
 * Displays accreditation actions and regulatory history for colleges.
 * Shows all recent actions (paginated) or school-specific detail view via ?unitid= URL param.
 * Tracks warnings, probations, monitoring, closures from 2019 onward.
 */

(function () {
  const {
    loadJson,
    schoolUrl,
    renderExternalLink,
    renderSchoolLink,
    renderPaginationButtons,
    paginateItems,
    setupPaginatedTable
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

  const ACCREDITOR_URLS = {
    HLC:     "https://www.hlcommission.org/",
    MSCHE:   "https://www.msche.org/",
    SACSCOC: "https://sacscoc.org/",
    NECHE:   "https://www.neche.org/",
    NWCCU:   "https://nwccu.org/",
    WSCUC:   "https://www.wscuc.org/",
    ACCJC:   "https://accjc.org/"
  };

  const OTHER_ACCREDITORS = [
    { short: "ACCJC", name: "Accrediting Commission for Community and Junior Colleges", url: "https://accjc.org/" }
  ];

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

  function getParam(name) {
    const params = new URLSearchParams(window.location.search);
    return params.get(name);
  }

  function setText(id, value) {
    const el = document.getElementById(id);
    if (el) el.textContent = value || "";
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

  function isPrimaryBachelorsInstitution(record) {
    const category = String(record?.category || "");
    return /primarily baccalaureate or above/i.test(category) && !/not primarily baccalaureate or above/i.test(category);
  }

  function renderEmpty(message) {
    return `<div class="empty-state"><p>${escapeHtml(message)}</p></div>`;
  }

  function normalizeQuery(value) {
    return String(value || "").trim().toLowerCase();
  }

  function filterByInstitution(items, query) {
    const normalized = normalizeQuery(query);
    if (!normalized) return items || [];
    return (items || []).filter((item) => String(item.institution_name || "").toLowerCase().includes(normalized));
  }

  function setSectionVisible(id, show) {
    const node = document.getElementById(id);
    const section = node ? node.closest(".data-card") : null;
    if (section) {
      section.classList.toggle("is-hidden", !show);
      if (show) {
        section.removeAttribute("aria-hidden");
      } else {
        section.setAttribute("aria-hidden", "true");
      }
    }
  }

  // CSV escaping: quotes values containing commas, quotes, or newlines
  function csvEscape(value) {
    const text = String(value ?? "");
    return /[",\n]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
  }

  function downloadRowsCsv(filename, headers, rows) {
    const csv = [headers, ...rows]
      .map((row) => row.map(csvEscape).join(","))
      .join("\n");
    const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    anchor.remove();
    URL.revokeObjectURL(url);
  }

  function escapeHtml(value) {
    return String(value ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
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

  function normalizeActionText(value) {
    return String(value || "").toLowerCase();
  }

  // Handles both YYYY-MM-DD and YYYY-MM formats
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
    return null;
  }

  function hasOccurred(action) {
    const actionDate = parseActionDate(action);
    if (actionDate) return actionDate.getTime() <= TODAY.getTime();
    const year = getActionYear(action);
    return !Number.isNaN(year) && year >= MIN_ACTION_YEAR && year <= TODAY.getFullYear();
  }

  // Complex regex patterns to identify significant actions (warnings, probations, monitoring, etc.)
  // while filtering out minor actions like substantive changes or program additions
  function isTrackedAction(action) {
    const type = normalizeActionText(action.action_type);
    const label = normalizeActionText(action.action_label || action.action_label_raw);
    const notes = normalizeActionText(action.notes);
    const haystack = `${type} ${label} ${notes}`;
    const contentOnly = `${label} ${notes}`;
    const excludedPattern = /substantive change|program addition/;
    if (excludedPattern.test(haystack)) return false;

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

  function isRecentTrackedAction(action) {
    const year = getActionYear(action);
    return isTrackedAction(action) && !Number.isNaN(year) && year >= MIN_ACTION_YEAR && hasOccurred(action);
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

  function getEffectiveActions(school) {
    return dedupeActions(Array.isArray(school?.actions) ? school.actions : []);
  }

  function renderInstitutionLinks(unitid, financialUnitid) {
    if (!unitid) return "";
    const financeLink = financialUnitid
      ? `<li>${renderSchoolLink(financialUnitid, "Finances", "school.html")}</li>`
      : "";
    return `
      <div class="related-links">
        <p><strong>Explore this institution:</strong></p>
        <ul class="link-list">
          ${financeLink}
          <li>${renderSchoolLink(unitid, "College Cuts", "cuts.html")}</li>
          <li>${renderSchoolLink(financialUnitid || unitid, "Research Funding Cuts", "research.html")}</li>
        </ul>
      </div>
    `;
  }

  // ------ Table Rendering ------

  function renderSchoolActions(actions, unitid, state, controlLabel, financialUnitid) {
    const filtered = (actions || []).filter(isRecentTrackedAction);
    if (!filtered.length) return renderEmpty("No accreditation actions found.");
    const rows = filtered
      .slice()
      .sort((a, b) => String(formatActionDate(b)).localeCompare(String(formatActionDate(a))))
      .map((action) => `
        <tr>
          <td>${escapeHtml(expandAccreditors(action.accreditor || ""))}</td>
          <td>${escapeHtml(action.action_label || action.action_label_raw || action.action_type || "")}</td>
          <td>${escapeHtml(state || "")}</td>
          <td>${escapeHtml(controlLabel || "")}</td>
          <td>${escapeHtml(formatActionDate(action))}</td>
          <td>${renderExternalLink(getActionLink(action), "Source link")}</td>
        </tr>
      `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>Accreditor</th>
              <th>Action</th>
              <th>State</th>
              <th>Sector</th>
              <th>Date</th>
              <th>Link</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>${renderInstitutionLinks(unitid, financialUnitid)}
    `;
  }

  function buildDefaultActionRows(data) {
    return Object.values(data.schools || {})
      .flatMap((school) =>
        getEffectiveActions(school).filter(isRecentTrackedAction).map((action) => ({
          unitid: school.unitid,
          institution_name: school.institution_name || "",
          city: school.city || "",
          state: school.state || inferStateFromNotes(action.notes),
          control_label: school.control_label || "",
          category: school.category || "",
          accreditor: action.accreditor || "",
          action_label: action.action_label || action.action_label_raw || action.action_type || "",
          action_type: action.action_type || "",
          action_date: action.action_date || "",
          action_year: action.action_year || "",
          source_url: getActionLink(action)
        }))
      )
      .sort((a, b) => {
        const dateCompare = String(b.action_date || b.action_year || "").localeCompare(String(a.action_date || a.action_year || ""));
        if (dateCompare !== 0) return dateCompare;
        const nameCompare = String(a.institution_name || "").localeCompare(String(b.institution_name || ""));
        if (nameCompare !== 0) return nameCompare;
        return expandAccreditors(a.accreditor || "").localeCompare(expandAccreditors(b.accreditor || ""));
      });
  }

  function renderActionTablePage(actions, page, pageSize, emptyMessage, linkNames = true) {
    const { totalPages, currentPage, pageItems } = paginateItems(actions, page, pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    const rows = pageItems
      .map((action) => `
        <tr>
          <td>${linkNames ? renderSchoolLink(action.unitid, action.institution_name, "school.html") : escapeHtml(action.institution_name || "")}</td>
          <td>${escapeHtml(expandAccreditors(action.accreditor || ""))}</td>
          <td>${escapeHtml(action.action_label || action.action_type || "")}</td>
          <td>${escapeHtml(action.state || "")}</td>
          <td>${escapeHtml(action.control_label || "")}</td>
          <td>${escapeHtml(action.action_date || action.action_year || "")}</td>
          <td>${renderExternalLink(action.source_url, "Source link")}</td>
        </tr>
      `)
      .join("");

    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>Institution</th>
              <th>Accreditor</th>
              <th>Action</th>
              <th>State</th>
              <th>Sector</th>
              <th>Date</th>
              <th>Link</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
      <div class="pagination" aria-label="Accreditation actions pages">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function setupPagination(container, actions, pageSize = PAGE_SIZE, emptyMessage = "No accreditation actions found.", downloadButtonId = null, downloadFilename = "accreditation-actions.csv", searchInput = null, linkNames = true) {
    if (!container) return;
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;
    setupPaginatedTable({
      container,
      items: actions,
      pageSize,
      searchInput,
      filterItems: filterByInstitution,
      renderPage: (filteredActions, currentPage, size) => renderActionTablePage(filteredActions, currentPage, size, emptyMessage, linkNames),
      downloadButton,
      downloadRows: (pageItems) => downloadRowsCsv(
          downloadFilename,
          ["Institution", "Accreditor", "Action", "State", "Sector", "Date", "Source"],
        pageItems.map((action) => [
            action.institution_name || "",
            expandAccreditors(action.accreditor || ""),
            action.action_label || action.action_type || "",
            action.state || "",
            action.control_label || "",
            action.action_date || action.action_year || "",
            action.source_url || ""
          ])
      )
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
        return url
          ? `<li><a href="${url}" target="_blank" rel="noopener">${name}</a></li>`
          : `<li>${name}</li>`;
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
    syncTabs(unitid);

    const data = await loadJson("data/accreditation.json");
    document.getElementById("accreditation-limitations").innerHTML = renderLimitations(data);

    if (!unitid) {
      // Landing page: show all recent actions split by institution type
      document.getElementById("accreditation-school-name").textContent = "Accreditation actions";
      document.getElementById("accreditation-school-name").classList.add("is-hidden");
      const allActions = buildDefaultActionRows(data);
      const primaryActions = allActions.filter(isPrimaryBachelorsInstitution);
      const otherActions = allActions.filter((action) => !isPrimaryBachelorsInstitution(action));
      setSectionVisible("accreditation-other-status", true);
      const primaryFilter = document.getElementById("accreditation-filter");
      const otherFilter = document.getElementById("accreditation-other-filter");
      setupPagination(
        document.getElementById("accreditation-status"),
        primaryActions,
        PAGE_SIZE,
        "No accreditation actions from 2019 to the present are available for 4-year, primarily bachelor's-degree-granting institutions.",
        "accreditation-table-download",
        "accreditation-primary.csv",
        primaryFilter
      );
      setupPagination(
        document.getElementById("accreditation-other-status"),
        otherActions,
        OTHER_PAGE_SIZE,
        "No accreditation actions from 2019 to the present are available for other institutions.",
        "accreditation-other-download",
        "accreditation-other.csv",
        otherFilter,
        false  // institution names in the "other" table are plain text, not links
      );
      return;
    }

    // School-specific view
    const school = data.schools?.[unitid];
    if (!school) {
      document.getElementById("accreditation-school-name").textContent = "No tracked accreditation record found";
      document.getElementById("accreditation-school-name").classList.remove("is-hidden");
      document.getElementById("accreditation-status").innerHTML = renderEmpty("No accreditation actions found.");
      return;
    }

    document.getElementById("accreditation-school-name").textContent = school.institution_name || "Accreditation";
    document.getElementById("accreditation-school-name").classList.remove("is-hidden");
    setText("accreditation-school-location", [school.city, school.state].filter(Boolean).join(", "));
    setText("accreditation-school-control", school.control_label || "");
    setText("accreditation-school-category", school.category || "");
    const accreditationOverview = document.getElementById("accreditation-overview");
    const scopeText = school.is_primary_tracker
      ? "This institution appears in the main tracker universe because it is a 4-year, primarily bachelor's-degree-granting school with finance data in this project."
      : "This institution appears here because it has matched accreditation actions data, even though it falls outside the main 4-year financial tracker universe.";
    if (accreditationOverview) {
      accreditationOverview.classList.remove("is-hidden");
      accreditationOverview.innerHTML = `<p>${scopeText}</p>`;
    }
    setSectionVisible("accreditation-other-status", false);
    const otherStatus = document.getElementById("accreditation-other-status");
    const otherTitle = document.getElementById("accreditation-other-title");
    const mainDownload = document.getElementById("accreditation-table-download");
    const otherDownload = document.getElementById("accreditation-other-download");
    if (mainDownload) mainDownload.classList.add("is-hidden");
    if (otherDownload) otherDownload.classList.add("is-hidden");
    if (otherStatus) otherStatus.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";
    document.getElementById("accreditation-status").innerHTML = renderSchoolActions(getEffectiveActions(school), school.unitid, school.state, school.control_label, school.financial_unitid);
  }

  init().catch((error) => {
    console.error(error);
  });
})();
