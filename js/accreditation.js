/**
 * FILE: accreditation.js
 * PURPOSE: Displays accreditation actions and regulatory history for colleges
 * PAGE: accreditation.html
 * JSON DATA SOURCES:
 *   - data/accreditation.json - Main data file with schools and their accreditation actions
 *   - Contains unitid, accreditor codes, action dates, status, links to source documents
 *
 * KEY CONCEPTS:
 *   - unitid = unique federal college ID; URL param ?unitid=12345 loads school-specific view
 *   - If no unitid: shows tables of all recent accreditation actions (paginated)
 *   - If unitid provided: shows detailed history for that specific school
 *   - Accreditors = regional bodies (HLC, MSCHE, SACSCOC, etc.) that oversee college quality
 *   - Tracked actions: warning, probation, notice of concern, monitoring, removed from probation, etc.
 *   - MIN_ACTION_YEAR = 2019 (only shows actions from 2019 forward)
 */

(function () {
  // Import shared utilities from global TrackerApp object
  const { loadJson, schoolUrl } = window.TrackerApp;

  // Pagination settings
  const PAGE_SIZE = 20; // Main table (4-year primary institutions)
  const OTHER_PAGE_SIZE = 5; // Secondary table (other institutions)
  const TODAY = new Date();

  // Accreditor code-to-full-name lookup table
  const ACCREDITOR_NAMES = {
    HLC: "Higher Learning Commission",
    MSCHE: "Middle States Commission on Higher Education",
    SACSCOC: "Southern Association of Colleges and Schools Commission on Colleges",
    NECHE: "New England Commission of Higher Education",
    NWCCU: "Northwest Commission on Colleges and Universities",
    WSCUC: "WASC Senior College and University Commission",
    ACCJC: "Accrediting Commission for Community and Junior Colleges"
  };

  // List of other accreditor bodies to link to
  const OTHER_ACCREDITORS = [
    { short: "ACCJC", name: "Accrediting Commission for Community and Junior Colleges", url: "https://accjc.org/" }
  ];

  // Only show accreditation actions from 2019 onward
  const MIN_ACTION_YEAR = 2019;

  // US state abbreviation to full name lookup
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

  /**
   * Extracts URL query parameter value
   * @description Parses window.location.search for the named parameter
   * @param {string} name - Parameter name (e.g., "unitid")
   * @returns {string|null} Parameter value or null if not found
   */
  function getParam(name) {
    const params = new URLSearchParams(window.location.search);
    return params.get(name);
  }

  /**
   * Sets the text content of an element by ID
   * @description Safely updates a DOM element's textContent, handling missing elements gracefully
   * @param {string} id - Element ID to update
   * @param {string} value - Text to display
   */
  function setText(id, value) {
    const el = document.getElementById(id);
    if (el) el.textContent = value || "";
  }

  /**
   * Updates navigation tab links for this school
   * @description Synchronizes links in the navigation tabs to point to the current school's data
   * across different pages (finances, accreditation, cuts, research)
   * @param {string} unitid - The school's unitid to link to
   */
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

  /**
   * Determines if a school is a 4-year primarily baccalaureate institution
   * @description Checks the category field to identify schools in the main tracker universe
   * (4-year institutions granting primarily bachelor degrees)
   * @param {Object} record - School record with category field
   * @returns {boolean} True if school is primary bachelor-granting institution
   */
  function isPrimaryBachelorsInstitution(record) {
    const category = String(record?.category || "");
    return /primarily baccalaureate or above/i.test(category) && !/not primarily baccalaureate or above/i.test(category);
  }

  /**
   * Renders an empty state message
   * @description Returns HTML for a "no data" placeholder
   * @param {string} message - Message to display
   * @returns {string} HTML string for empty state
   */
  function renderEmpty(message) {
    return `<div class="empty-state"><p>${message}</p></div>`;
  }

  /**
   * Creates a link to the financial page for a school
   * @description Generates an anchor tag linking to school.html for a given unitid
   * @param {string} unitid - The school's unitid
   * @param {string} label - Link text
   * @returns {string} HTML anchor tag or plain label if no unitid
   */
  function financePageLink(unitid, label) {
    return unitid
      ? `<a href="${schoolUrl(unitid, "school.html")}">${label || ""}</a>`
      : (label || "");
  }

  /**
   * Normalizes a search query string
   * @description Converts to lowercase and trims whitespace for consistent matching
   * @param {string} value - Raw query string
   * @returns {string} Normalized query
   */
  function normalizeQuery(value) {
    return String(value || "").trim().toLowerCase();
  }

  /**
   * Filters an array of items by institution name
   * @description Performs case-insensitive substring matching on institution_name
   * @param {Array<Object>} items - Array of action/institution objects
   * @param {string} query - Search query string
   * @returns {Array<Object>} Filtered array matching the query
   */
  function filterByInstitution(items, query) {
    const normalized = normalizeQuery(query);
    if (!normalized) return items || [];
    return (items || []).filter((item) => String(item.institution_name || "").toLowerCase().includes(normalized));
  }

  /**
   * Shows or hides a DOM section and its containing data-card
   * @description Toggles the is-hidden class on the closest .data-card parent
   * @param {string} id - Element ID to show/hide
   * @param {boolean} show - True to show, false to hide
   */
  function setSectionVisible(id, show) {
    const node = document.getElementById(id);
    const section = node ? node.closest(".data-card") : null;
    if (section) {
      section.classList.toggle("is-hidden", !show);
    }
  }

  /**
   * Escapes a value for CSV export
   * @description Properly quotes and escapes values containing commas, quotes, or newlines
   * @param {*} value - Value to escape
   * @returns {string} CSV-safe string
   */
  function csvEscape(value) {
    const text = String(value ?? "");
    return /[",\n]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
  }

  /**
   * Triggers a CSV file download with the given data
   * @description Creates a Blob with CSV content and triggers browser download
   * @param {string} filename - Name for the downloaded file
   * @param {Array<string>} headers - CSV column headers
   * @param {Array<Array>} rows - 2D array of row data
   */
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

  /**
   * Escapes HTML special characters to prevent XSS
   * @description Converts &, <, >, ", ' to HTML entities
   * @param {*} value - Value to escape
   * @returns {string} Safe HTML string
   */
  function escapeHtml(value) {
    return String(value ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  /**
   * Expands accreditor code abbreviations to full names
   * @description Converts "HLC,MSCHE" to "Higher Learning Commission, Middle States Commission..."
   * @param {string} value - Comma-separated accreditor codes
   * @returns {string} Comma-separated full accreditor names
   */
  function expandAccreditors(value) {
    if (!value) return "";
    return String(value)
      .split(",")
      .map((part) => part.trim())
      .filter(Boolean)
      .map((part) => ACCREDITOR_NAMES[part] || part)
      .join(", ");
  }

  /**
   * Normalizes action text to lowercase for matching
   * @description Converts action_type/label to lowercase
   * @param {string} value - Raw action text
   * @returns {string} Lowercase text
   */
  function normalizeActionText(value) {
    return String(value || "").toLowerCase();
  }

  /**
   * Parses an accreditation action date into a Date object
   * @description Handles both YYYY-MM-DD and YYYY-MM formats
   * @param {Object} action - Action object with action_date field
   * @param {string} action.action_date - Date string to parse
   * @returns {Date|null} Parsed date or null if parsing fails
   */
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

  /**
   * Checks if an action date has already occurred (is in the past)
   * @description Compares action_date to TODAY to include only past events
   * @param {Object} action - Action with action_date or action_year
   * @returns {boolean} True if action date is in the past
   */
  function hasOccurred(action) {
    const actionDate = parseActionDate(action);
    if (actionDate) return actionDate.getTime() <= TODAY.getTime();
    const year = getActionYear(action);
    return !Number.isNaN(year) && year >= MIN_ACTION_YEAR && year <= TODAY.getFullYear();
  }

  /**
   * Determines if an action should be tracked/displayed
   * @description Uses regex patterns to identify significant accreditation actions
   * (warnings, probations, monitoring, etc.) while filtering out minor actions
   * like substantive changes or program additions
   * @param {Object} action - Action with type, label, notes fields
   * @returns {boolean} True if action meets tracking criteria
   */
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

  /**
   * Formats an action date for display
   * @description Returns action_date if available, falls back to action_year
   * @param {Object} action - Action object
   * @returns {string} Formatted date string
   */
  function formatActionDate(action) {
    return action.action_date || action.action_year || "";
  }

  /**
   * Extracts the source URL from an action record
   * @description Prefers source_url, falls back to source_page_url
   * @param {Object} action - Action object
   * @returns {string} URL string or empty string
   */
  function getActionLink(action) {
    return action.source_url || action.source_page_url || "";
  }

  /**
   * Extracts the year from an action date
   * @description Gets explicit action_year if present, otherwise parses from action_date
   * @param {Object} action - Action with action_year or action_date fields
   * @returns {number} Year as number, or NaN if not found
   */
  function getActionYear(action) {
    const explicitYear = Number(action.action_year || "");
    if (!Number.isNaN(explicitYear) && explicitYear > 0) return explicitYear;
    const dateText = String(action.action_date || "");
    const match = dateText.match(/\b(19|20)\d{2}\b/);
    return match ? Number(match[0]) : NaN;
  }

  /**
   * Extracts state from action notes field
   * @description Looks for state abbreviation in notes (e.g., ", TX" -> "Texas")
   * @param {string} notes - Notes field containing possible state info
   * @returns {string} Full state name or empty string
   */
  function inferStateFromNotes(notes) {
    const match = String(notes || "").match(/,\s*([A-Z]{2})\b/);
    if (!match) return "";
    return STATE_ABBR_TO_NAME[match[1]] || "";
  }

  /**
   * Checks if an action is recent and should be tracked
   * @description Combination of isTrackedAction, hasOccurred, and MIN_ACTION_YEAR check
   * @param {Object} action - Action to evaluate
   * @returns {boolean} True if action is recent, occurred, and tracked
   */
  function isRecentTrackedAction(action) {
    const year = getActionYear(action);
    return isTrackedAction(action) && !Number.isNaN(year) && year >= MIN_ACTION_YEAR && hasOccurred(action);
  }

  /**
   * Removes duplicate actions from an array
   * @description Uses accreditor, action label, date, and URL as dedup key
   * @param {Array<Object>} actions - Array of action objects
   * @returns {Array<Object>} De-duplicated array
   */
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

  /**
   * Gets effective (de-duplicated) actions for a school
   * @description Safely extracts and de-dupes actions array
   * @param {Object} school - School object with actions array
   * @returns {Array<Object>} De-duplicated actions
   */
  function getEffectiveActions(school) {
    return dedupeActions(Array.isArray(school?.actions) ? school.actions : []);
  }

  /**
   * Renders related links to other pages for a school
   * @description Creates sidebar with links to financial, accreditation, and research pages
   * @param {string} unitid - School's unitid
   * @param {string} financialUnitid - Financial data unitid (may differ for some schools)
   * @returns {string} HTML for related links section
   */
  function renderInstitutionLinks(unitid, financialUnitid) {
    if (!unitid) return "";
    const financeLink = financialUnitid
      ? `<li><a href="${schoolUrl(financialUnitid, "school.html")}">Finances</a></li>`
      : "";
    return `
      <div class="related-links">
        <p><strong>Explore this institution:</strong></p>
        <ul class="link-list">
          ${financeLink}
          <li><a href="${schoolUrl(unitid, "cuts.html")}">College Cuts</a></li>
          <li><a href="${schoolUrl(financialUnitid || unitid, "research.html")}">Research Funding Cuts</a></li>
        </ul>
      </div>
    `;
  }

  /**
   * Renders accreditation actions table for a single school
   * @description Creates a formatted table of recent accreditation actions for school-specific view
   * @param {Array<Object>} actions - Array of action objects
   * @param {string} unitid - School's unitid
   * @param {string} state - School's state
   * @param {string} controlLabel - School's sector (public/private/etc)
   * @param {string} financialUnitid - Financial data unitid
   * @returns {string} HTML table or empty state
   */
  function renderSchoolActions(actions, unitid, state, controlLabel, financialUnitid) {
    const filtered = (actions || []).filter(isRecentTrackedAction);
    if (!filtered.length) return renderEmpty("No accreditation actions found.");
    const rows = filtered
      .slice()
      .sort((a, b) => String(formatActionDate(b)).localeCompare(String(formatActionDate(a))))
      .map((action) => `
        <tr>
          <td>${expandAccreditors(action.accreditor || "")}</td>
          <td>${action.action_label || action.action_label_raw || action.action_type || ""}</td>
          <td>${escapeHtml(state || "")}</td>
          <td>${escapeHtml(controlLabel || "")}</td>
          <td>${formatActionDate(action)}</td>
          <td>${getActionLink(action) ? `<a href="${getActionLink(action)}" target="_blank" rel="noopener">Source link</a>` : ""}</td>
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

  /**
   * Builds array of all recent tracked actions across all schools
   * @description Flattens school objects into individual action rows with school metadata
   * Sorts by date descending. Used for default landing page view.
   * @param {Object} data - Data object with schools keyed by unitid
   * @returns {Array<Object>} Array of action rows with school info included
   */
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

  /**
   * Renders a single page of the actions table with pagination
   * @description Slices actions array and renders HTML table with pagination buttons
   * @param {Array<Object>} actions - Full array of actions
   * @param {number} page - Current page number (1-indexed)
   * @param {number} pageSize - Rows per page
   * @param {string} emptyMessage - Message if no rows on this page
   * @returns {string} HTML table with pagination controls
   */
  function renderActionTablePage(actions, page, pageSize, emptyMessage) {
    const totalPages = Math.max(1, Math.ceil(actions.length / pageSize));
    const safePage = Math.min(Math.max(1, page), totalPages);
    const start = (safePage - 1) * pageSize;
    const pageRows = actions.slice(start, start + pageSize);

    if (!pageRows.length) {
      return renderEmpty(emptyMessage);
    }

    const rows = pageRows
      .map((action) => `
        <tr>
          <td>${financePageLink(action.unitid, escapeHtml(action.institution_name))}</td>
          <td>${escapeHtml(expandAccreditors(action.accreditor || ""))}</td>
          <td>${escapeHtml(action.action_label || action.action_type || "")}</td>
          <td>${escapeHtml(action.state || "")}</td>
          <td>${escapeHtml(action.control_label || "")}</td>
          <td>${escapeHtml(action.action_date || action.action_year || "")}</td>
          <td>${action.source_url ? `<a href="${action.source_url}" target="_blank" rel="noopener">Source link</a>` : ""}</td>
        </tr>
      `)
      .join("");

    const pagination = Array.from({ length: totalPages }, (_, idx) => idx + 1)
      .map((pageNumber) => {
        const currentAttr = pageNumber === safePage ? ' aria-current="page"' : "";
        return `<button type="button" class="pagination-button${pageNumber === safePage ? " is-active" : ""}" data-page="${pageNumber}" aria-label="Page ${pageNumber}"${currentAttr}>${pageNumber}</button>`;
      })
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
        ${pagination}
      </div>
    `;
  }

  /**
   * Sets up pagination and filtering for an actions table
   * @description Manages page/sort state and re-renders on page change or search filter.
   * Attaches event listeners to pagination buttons and search input.
   * @param {HTMLElement} container - DOM element to render table into
   * @param {Array<Object>} actions - Full array of actions to paginate
   * @param {number} pageSize - Rows per page
   * @param {string} emptyMessage - "No results" message
   * @param {string} downloadButtonId - Optional ID of download CSV button
   * @param {string} downloadFilename - Filename for CSV export
   * @param {HTMLInputElement} searchInput - Optional search input for filtering by school name
   */
  function setupPagination(container, actions, pageSize = PAGE_SIZE, emptyMessage = "No accreditation actions found.", downloadButtonId = null, downloadFilename = "accreditation-actions.csv", searchInput = null) {
    if (!container) return;
    let currentPage = 1;
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      const filteredActions = filterByInstitution(actions, searchInput?.value || "");
      container.innerHTML = renderActionTablePage(filteredActions, currentPage, pageSize, emptyMessage);
      // Move focus to the pagination region so screen readers announce the updated content
      setTimeout(() => {
        const pagination = container.querySelector(".pagination");
        if (pagination) pagination.focus();
      }, 0);
      const totalPages = Math.max(1, Math.ceil(filteredActions.length / pageSize));
      const safePage = Math.min(Math.max(1, currentPage), totalPages);
      const start = (safePage - 1) * pageSize;
      const pageRows = filteredActions.slice(start, start + pageSize);
      if (downloadButton) {
        downloadButton.classList.toggle("is-hidden", pageRows.length === 0);
        downloadButton.onclick = () => downloadRowsCsv(
          downloadFilename,
          ["Institution", "Accreditor", "Action", "State", "Sector", "Date", "Source"],
          pageRows.map((action) => [
            action.institution_name || "",
            expandAccreditors(action.accreditor || ""),
            action.action_label || action.action_type || "",
            action.state || "",
            action.control_label || "",
            action.action_date || action.action_year || "",
            action.source_url || ""
          ])
        );
      }
      container.querySelectorAll(".pagination-button").forEach((button) => {
        button.addEventListener("click", () => {
          const nextPage = Number(button.dataset.page || "1");
          if (!Number.isNaN(nextPage) && nextPage !== currentPage) {
            currentPage = nextPage;
            render();
          }
        });
      });
    };

    if (searchInput && !searchInput.dataset.boundFilter) {
      searchInput.addEventListener("input", () => {
        currentPage = 1;
        render();
      });
      searchInput.dataset.boundFilter = "true";
    }

    render();
  }

  /**
   * Renders the limitations section explaining data coverage and caveats
   * @description Builds HTML explaining which accreditors are tracked, which states are represented,
   * which action types are included, and links to other accreditor resources
   * @param {Object} data - Data object with coverage information
   * @returns {string} HTML for limitations section
   */
  function renderLimitations(data) {
    const representedStates = [...new Set(Object.values(data.schools || {}).map((school) => school.state).filter(Boolean))].sort();
    const accreditorCodes = new Set();
    (data.covered_accreditors || []).forEach((value) => {
      String(value || "")
        .split(/[;,]/)
        .map((part) => part.trim())
        .filter(Boolean)
        .forEach((part) => accreditorCodes.add(part));
    });
    (data.source_coverage || []).forEach((row) => {
      const code = String(row.accreditor || "").trim();
      if (code) accreditorCodes.add(code);
    });
    const trackedNames = [...accreditorCodes]
      .map((code) => ACCREDITOR_NAMES[code] || code)
      .sort((a, b) => a.localeCompare(b));
    const sortedCoverage = [...(data.source_coverage || [])]
      .filter((row) => isTrackedAction(row))
      .sort((a, b) => {
        const accreditorCompare = expandAccreditors(a.accreditor || "").localeCompare(expandAccreditors(b.accreditor || ""));
        if (accreditorCompare !== 0) return accreditorCompare;
        const actionCompare = String(a.action_type || "").localeCompare(String(b.action_type || ""));
        if (actionCompare !== 0) return actionCompare;
        return String(a.n || "").localeCompare(String(b.n || ""));
      });
    const covered = trackedNames.length
      ? `<p><strong>Currently tracked accreditors:</strong> ${trackedNames.join(", ")}</p>`
      : "";
    const accreditedNote = `<p>Currently, a college can still be accredited while also being under warning, notice, monitoring, probation, or another public follow-up action. That is why this page emphasizes recent actions instead of a simple accredited / not accredited label.</p>`;
    const stateNote = representedStates.length
      ? `<p>In the current version of this page, matched institutions appear in these states: ${representedStates.join(", ")}.</p>`
      : "";
    const coverageRows = sortedCoverage.length
      ? `<div class="history-table-wrap"><table class="history-table"><thead><tr><th>Accreditor</th><th>Action type</th></tr></thead><tbody>${
          sortedCoverage.map((row) => `<tr><td>${expandAccreditors(row.accreditor || "")}</td><td>${row.action_type || ""}</td></tr>`).join("")
        }</tbody></table></div>`
      : "";
    const otherBodies = OTHER_ACCREDITORS.length
      ? `<p><strong>Other institutional accreditors:</strong></p><ul class="link-list">${
          OTHER_ACCREDITORS.map((row) => `<li><a href="${row.url}" target="_blank" rel="noopener">${row.name}</a></li>`).join("")
        }</ul>`
      : "";
    const helpText = `<p>This page currently reflects only the accreditors and action types already collected in this project. The main table focuses on 4-year, primarily bachelor's-degree-granting institutions in the financial tracker, with a smaller secondary table for other institutions. The display is limited to warning, notice, monitoring, probation, removal-from-those actions, closure or teach-out decisions, and commission actions that explicitly require additional reports from 2019 to the present.</p>
        <p>A missing result here does not mean an institution has no accreditation history or current accreditation.</p>
        <p>If your college is not included here, students should check the institution's own accreditation webpage, the accreditor's institution directory or public actions page, and the institution's consumer-information or disclosure page.</p>`;
    return `${covered}${accreditedNote}${stateNote}${helpText}${coverageRows}${otherBodies}`;
  }

  /**
   * Main initialization function
   * @description Loads accreditation data, determines if showing school-specific or landing view,
   * and sets up tables and pagination accordingly
   */
  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid);

    const data = await loadJson("data/accreditation.json");
    document.getElementById("accreditation-limitations").innerHTML = renderLimitations(data);
    const topIntro = document.getElementById("accreditation-intro-top");
    const bottomIntro = document.getElementById("accreditation-intro-bottom");
    if (topIntro) {
      topIntro.innerHTML = "Accreditating bodies oversee whether colleges are following educational standards. Explore whether an institution has received a warning, been put on probation, sought approval for closure or exited probation.<br><br>The first table below shows accreditation actions from 2019 to the present at 4-year, primarily bachelor's-degree-granting institutions. Scroll further down the page to see recent accreditation actions at other institutions.";
    }
    if (bottomIntro) {
      bottomIntro.textContent = "";
    }

    if (!unitid) {
      // Landing page: show all recent accreditation actions, split by institution type
      document.getElementById("accreditation-school-name").textContent = "";
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
        otherFilter
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

  // Run init on page load
  init().catch((error) => {
    console.error(error);
  });
})();
