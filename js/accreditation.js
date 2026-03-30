(function () {
  const { loadJson, schoolUrl } = window.TrackerApp;
  const PAGE_SIZE = 20;
  const OTHER_PAGE_SIZE = 5;
  const TODAY = new Date();
  const ACCREDITOR_NAMES = {
    HLC: "Higher Learning Commission",
    MSCHE: "Middle States Commission on Higher Education",
    SACSCOC: "Southern Association of Colleges and Schools Commission on Colleges",
    NECHE: "New England Commission of Higher Education",
    NWCCU: "Northwest Commission on Colleges and Universities",
    WSCUC: "WASC Senior College and University Commission",
    ACCJC: "Accrediting Commission for Community and Junior Colleges"
  };
  const OTHER_ACCREDITORS = [
    { short: "ACCJC", name: "Accrediting Commission for Community and Junior Colleges", url: "https://accjc.org/" }
  ];
  const MIN_ACTION_YEAR = 2019;
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
  }

  function isPrimaryBachelorsInstitution(record) {
    const category = String(record?.category || "");
    return /primarily baccalaureate or above/i.test(category) && !/not primarily baccalaureate or above/i.test(category);
  }

  function renderEmpty(message) {
    return `<div class="empty-state"><p>${message}</p></div>`;
  }

  function setSectionVisible(id, show) {
    const node = document.getElementById(id);
    const section = node ? node.closest(".data-card") : null;
    if (section) {
      section.classList.toggle("is-hidden", !show);
    }
  }

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

  function inferStateFromNotes(notes) {
    const match = String(notes || "").match(/,\s*([A-Z]{2})\b/);
    if (!match) return "";
    return STATE_ABBR_TO_NAME[match[1]] || "";
  }

  function isRecentTrackedAction(action) {
    const year = getActionYear(action);
    return isTrackedAction(action) && !Number.isNaN(year) && year >= MIN_ACTION_YEAR && hasOccurred(action);
  }

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
      ? `<li><a href="${schoolUrl(financialUnitid, "school.html")}">Finances</a></li>`
      : "";
    return `
      <div class="related-links">
        <p><strong>Explore this institution:</strong></p>
        <ul class="link-list">
          ${financeLink}
          <li><a href="${schoolUrl(unitid, "cuts.html")}">College Cuts</a></li>
        </ul>
      </div>
    `;
  }

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
          <td><a href="${schoolUrl(action.unitid, "accreditation.html")}">${escapeHtml(action.institution_name)}</a></td>
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
      .map((pageNumber) => `
        <button
          type="button"
          class="pagination-button${pageNumber === safePage ? " is-active" : ""}"
          data-page="${pageNumber}"
          aria-label="Page ${pageNumber}"
          aria-current="${pageNumber === safePage ? "page" : "false"}"
        >${pageNumber}</button>
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
        ${pagination}
      </div>
    `;
  }

  function setupPagination(container, actions, pageSize = PAGE_SIZE, emptyMessage = "No accreditation actions found.", downloadButtonId = null, downloadFilename = "accreditation-actions.csv") {
    if (!container) return;
    let currentPage = 1;
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      container.innerHTML = renderActionTablePage(actions, currentPage, pageSize, emptyMessage);
      const totalPages = Math.max(1, Math.ceil(actions.length / pageSize));
      const safePage = Math.min(Math.max(1, currentPage), totalPages);
      const start = (safePage - 1) * pageSize;
      const pageRows = actions.slice(start, start + pageSize);
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

    render();
  }

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

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid);

    const data = await loadJson("data/accreditation.json");
    document.getElementById("accreditation-limitations").innerHTML = renderLimitations(data);
    const topIntro = document.getElementById("accreditation-intro-top");
    const bottomIntro = document.getElementById("accreditation-intro-bottom");
    if (topIntro) {
      topIntro.textContent = "Currently, this page focuses on accreditation actions from 2019 to the present at 4-year, primarily bachelor's-degree-granting institutions in the financial tracker. Scroll further down the page to see recent accreditation actions at other institutions.";
    }
    if (bottomIntro) {
      bottomIntro.textContent = "";
    }

    if (!unitid) {
      const allActions = buildDefaultActionRows(data);
      const primaryActions = allActions.filter(isPrimaryBachelorsInstitution);
      const otherActions = allActions.filter((action) => !isPrimaryBachelorsInstitution(action));
      setSectionVisible("accreditation-other-status", true);
      setupPagination(
        document.getElementById("accreditation-status"),
        primaryActions,
        PAGE_SIZE,
        "No accreditation actions from 2019 to the present are available for 4-year, primarily bachelor's-degree-granting institutions.",
        "accreditation-table-download",
        "accreditation-primary.csv"
      );
      setupPagination(
        document.getElementById("accreditation-other-status"),
        otherActions,
        OTHER_PAGE_SIZE,
        "No accreditation actions from 2019 to the present are available for other institutions.",
        "accreditation-other-download",
        "accreditation-other.csv"
      );
      return;
    }

    const school = data.schools?.[unitid];
    if (!school) {
      document.getElementById("accreditation-school-name").textContent = "No tracked accreditation record found";
      document.getElementById("accreditation-status").innerHTML = renderEmpty("No accreditation actions found.");
      return;
    }

    document.getElementById("accreditation-school-name").textContent = school.institution_name || "Accreditation";
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
    document.getElementById("accreditation-status").innerHTML = renderEmpty("The accreditation data could not be loaded.");
  });
})();
