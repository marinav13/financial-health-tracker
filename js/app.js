/**
 * FILE: app.js
 * Global search functionality for cross-page school/institution search.
 * Loads index from data/ folder based on page context (financial, accreditation, cuts, or research).
 * Uses unitid (federal college ID) to navigate to specific school pages.
 */

// ------ Shared Utilities ------

function escapeHtml(value) {
  return String(value ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

// ------ Search Index Loading ------

// Loads JSON index file from data/ folder
async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Failed to load ${path}`);
  return response.json();
}

// Determines target page for search navigation
function getSearchTargetPage() {
  return document.body.dataset.searchPage || "school.html";
}

// Loads appropriate index file based on page context
function getSearchSourcePath() {
  const source = document.body.dataset.searchSource || "all";
  if (source === "accreditation") return "data/accreditation_index.json";
  if (source === "cuts") return "data/college_cuts_index.json";
  if (source === "research") return "data/research_funding_index.json";
  return "data/schools_index.json";
}

function getSearchSourceKind() {
  return document.body.dataset.searchSource || "all";
}

// ------ Search URL Building ------

// Builds navigation URL with unitid query param
function schoolUrl(unitid, page = getSearchTargetPage()) {
  return `${page}?unitid=${encodeURIComponent(unitid)}`;
}

function isNumericUnitid(value) {
  return /^[0-9]+$/.test(String(value || ""));
}

function relatedPageUnitid(unitid, financialUnitid) {
  if (isNumericUnitid(unitid)) return String(unitid);
  if (isNumericUnitid(financialUnitid)) return String(financialUnitid);
  return "";
}

function isPrimaryTrackerInstitution(record) {
  return record?.is_primary_tracker === true;
}

function syncTabs(unitid = "", options = {}) {
  const active = options.active || document.body.dataset.activeTab || (
    document.body.dataset.searchSource || "finances"
  );
  const tabs = {
    finances: document.getElementById("tab-finances"),
    cuts: document.getElementById("tab-cuts"),
    accreditation: document.getElementById("tab-accreditation"),
    research: document.getElementById("tab-research")
  };

  // The top nav is site-level navigation: every tab always points at the
  // section's main landing page, never at a school-specific URL. Deep-linking
  // the nav to the current school was surfacing empty "No X found" pages for
  // schools not tracked in the destination dataset (the majority of schools
  // aren't in cuts/accreditation/research), which is a dead-end UX.
  //
  // Per-school navigation lives in the in-body "Explore this institution"
  // block (see renderRelatedInstitutionLinks and school.js's
  // renderSchoolRelatedPages), which only links to sections where the
  // school actually has data.
  //
  // The `unitid` / `financialUnitid` args are retained for call-site
  // backward compatibility and future use, but no longer influence hrefs.
  if (tabs.finances) tabs.finances.href = "index.html";
  if (tabs.cuts) tabs.cuts.href = "cuts.html";
  if (tabs.accreditation) tabs.accreditation.href = "accreditation.html";
  if (tabs.research) tabs.research.href = "research.html";

  Object.entries(tabs).forEach(([name, tab]) => {
    if (!tab) return;
    const isActive = name === active;
    tab.classList.toggle("is-active", isActive);
    if (isActive) {
      tab.setAttribute("aria-current", "page");
    } else {
      tab.removeAttribute("aria-current");
    }
  });
}

function renderRelatedInstitutionLinks(options = {}) {
  const {
    unitid = "",
    financialUnitid = "",
    current = "",
    include = ["finances", "cuts", "accreditation", "research"]
  } = options;
  const financeUnitid = isNumericUnitid(financialUnitid) ? financialUnitid : (isNumericUnitid(unitid) ? unitid : "");
  const pageUnitid = relatedPageUnitid(unitid, financialUnitid);
  const links = [];

  if (include.includes("finances") && current !== "finances" && financeUnitid) {
    links.push(window.TrackerApp.renderSchoolLink(financeUnitid, "Finances", "school.html"));
  }
  if (include.includes("cuts") && current !== "cuts" && pageUnitid) {
    links.push(window.TrackerApp.renderSchoolLink(pageUnitid, "College Cuts", "cuts.html"));
  }
  if (include.includes("accreditation") && current !== "accreditation" && pageUnitid) {
    links.push(window.TrackerApp.renderSchoolLink(pageUnitid, "Accreditation", "accreditation.html"));
  }
  if (include.includes("research") && current !== "research" && pageUnitid) {
    links.push(window.TrackerApp.renderSchoolLink(pageUnitid, "Research Funding Cuts", "research.html"));
  }

  if (!links.length) return "";
  return `
    <div class="related-links">
      <p><strong>Explore this institution:</strong></p>
      <ul class="link-list">${links.map((link) => `<li>${link}</li>`).join("")}</ul>
    </div>
  `;
}

// ------ Search Tokenization & Matching ------

function normalizeSearchText(value) {
  return String(value || "")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "");
}

// Splits query into searchable tokens (alphanumeric only, lowercase)
function tokenizeSearch(value) {
  return normalizeSearchText(value)
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

// Pre-computes combined searchable string for performance
function buildSearchHaystack(row) {
  return [
    row.institution_name,
    row.institution_unique_name,
    row.city,
    row.state
  ]
    .filter(Boolean)
    .join(" ")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "");
}

// ------ Search Initialization & Rendering ------

async function initSearch() {
  const raw = await loadJson(getSearchSourcePath());

  const schools = (Array.isArray(raw) ? raw : Object.values(raw || {})).slice().map((row) => ({
    ...row,
    _searchHaystack: buildSearchHaystack(row)
  })).sort((a, b) =>
    String(a.institution_unique_name || a.institution_name || "").localeCompare(
      String(b.institution_unique_name || b.institution_name || "")
    )
  );

  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");
  const datalist = document.getElementById("school-options");
  if (!input || !results) return;

  // Mark the results container as a listbox so screen readers announce
  // individual items (role="option") as selectable choices.
  results.setAttribute("role", "listbox");
  if (!results.id) results.id = "search-results";
  input.setAttribute("role", "combobox");
  input.setAttribute("aria-autocomplete", "list");
  input.setAttribute("aria-controls", results.id);
  input.setAttribute("aria-expanded", "false");

  const page = getSearchTargetPage();
  const sourceKind = getSearchSourceKind();

  let activeIndex = -1;

  // Populate datalist for browser autocomplete
  if (datalist) {
    datalist.innerHTML = "";
    schools.forEach((item) => {
      const option = document.createElement("option");
      option.value = item.institution_unique_name || item.institution_name;
      datalist.appendChild(option);
    });
  }

  function clearResults() {
    results.innerHTML = "";
    activeIndex = -1;
    input.setAttribute("aria-expanded", "false");
    input.removeAttribute("aria-activedescendant");
  }

  function getMatchText(row) {
    return row.institution_unique_name || row.institution_name || "";
  }

  // Shows context-specific badge (accreditation action, college cut, etc.)
  function getResultBadge(row) {
    function trimBadge(text) {
      const value = String(text || "");
      return value.length > 120 ? `${value.slice(0, 117)}...` : value;
    }
    if (sourceKind === "cuts" && row.latest_cut_label) {
      const date = row.latest_cut_date || "";
      return `Latest cut${date ? ` (${date})` : ""}: ${trimBadge(row.latest_cut_label)}`;
    }
    if (sourceKind === "accreditation" && row.latest_action_label) {
      const date = row.latest_action_date || "";
      return `Latest action${date ? ` (${date})` : ""}: ${trimBadge(row.latest_action_label)}`;
    }
    return "";
  }

  function getAllResultButtons() {
    return Array.from(results.querySelectorAll(".result-item[data-unitid]"));
  }

  function setActiveOption(newIndex) {
    const buttons = getAllResultButtons();
    if (!buttons.length) return;
    activeIndex = Math.max(0, Math.min(newIndex, buttons.length - 1));
    buttons.forEach((btn, i) => {
      btn.setAttribute("tabindex", "-1");
      btn.setAttribute("aria-selected", i === activeIndex ? "true" : "false");
    });
    input.setAttribute("aria-activedescendant", buttons[activeIndex].id);
  }

  function navigateToActive() {
    const buttons = getAllResultButtons();
    if (buttons[activeIndex]) {
      window.location.href = schoolUrl(buttons[activeIndex].dataset.unitid, page);
    }
  }

  // Multi-token matching: all tokens must appear in haystack
  // Results sorted by relevance (name starts with query = higher priority)
  function renderMatches(query) {
    const q = normalizeSearchText(query).trim();
    const tokens = tokenizeSearch(query);
    if (!q || !tokens.length) {
      clearResults();
      return;
    }

    const matches = schools
      .filter((row) => tokens.every((token) => row._searchHaystack.includes(token)))
      .sort((a, b) => {
        const aName = normalizeSearchText(a.institution_name || "");
        const bName = normalizeSearchText(b.institution_name || "");
        const aStarts = aName.startsWith(q) ? 1 : 0;
        const bStarts = bName.startsWith(q) ? 1 : 0;
        if (aStarts !== bStarts) return bStarts - aStarts;

        const aUniqueStarts = normalizeSearchText(a.institution_unique_name || "").startsWith(q) ? 1 : 0;
        const bUniqueStarts = normalizeSearchText(b.institution_unique_name || "").startsWith(q) ? 1 : 0;
        if (aUniqueStarts !== bUniqueStarts) return bUniqueStarts - aUniqueStarts;

        return String(a.institution_unique_name || a.institution_name || "").localeCompare(
          String(b.institution_unique_name || b.institution_name || "")
        );
      })
      .slice(0, 8);

    if (!matches.length) {
      clearResults();
      input.setAttribute("aria-expanded", "true");
      results.innerHTML = `<div id="${results.id}-empty" class="result-item is-empty" role="option" tabindex="-1">No matching institutions found.</div>`;
      return;
    }

    activeIndex = -1;
    input.setAttribute("aria-expanded", "true");
    input.removeAttribute("aria-activedescendant");
    results.setAttribute("aria-label", `${matches.length} search result${matches.length !== 1 ? "s" : ""}`);
    results.innerHTML = matches.map((row, idx) => `
      <button type="button" id="${results.id}-option-${idx}" class="result-item" role="option" data-unitid="${escapeHtml(row.unitid)}" tabindex="-1" aria-selected="false">
        <span>${escapeHtml(getMatchText(row))}</span>
        ${getResultBadge(row) ? `<small class="small-meta">${escapeHtml(getResultBadge(row))}</small>` : ""}
      </button>
    `).join("");

    results.querySelectorAll("[data-unitid]").forEach((button, i) => {
      button.addEventListener("click", () => {
        window.location.href = schoolUrl(button.dataset.unitid, page);
      });

      button.addEventListener("keydown", (e) => {
        if (e.key === "ArrowDown") {
          e.preventDefault();
          setActiveOption(i + 1);
          input.focus();
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          setActiveOption(i - 1);
          input.focus();
        } else if (e.key === "Enter" || e.key === " ") {
          window.location.href = schoolUrl(button.dataset.unitid, page);
        }
      });
    });
  }

  input.addEventListener("input", (e) => {
    renderMatches(e.target.value);
  });

  // Arrow keys begin from the input; result buttons continue the same pattern.
  input.addEventListener("keydown", (e) => {
    if (e.key === "ArrowDown") {
      e.preventDefault();
      if (!getAllResultButtons().length) renderMatches(input.value);
      setActiveOption(activeIndex + 1);
    } else if (e.key === "ArrowUp") {
      e.preventDefault();
      if (!getAllResultButtons().length) renderMatches(input.value);
      setActiveOption(activeIndex <= 0 ? getAllResultButtons().length - 1 : activeIndex - 1);
    } else if (e.key === "Enter") {
      navigateToActive();
    } else if (e.key === "Escape") {
      clearResults();
      input.focus();
    }
  });
}

initSearch().catch((error) => {
  console.error("Search initialization failed:", error);
  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");
  if (input) {
    input.disabled = true;
    input.setAttribute("aria-disabled", "true");
  }
  if (results) {
    results.setAttribute("role", "status");
    results.setAttribute("aria-live", "polite");
    results.innerHTML = '<div class="result-item is-empty">Search is temporarily unavailable.</div>';
  }
});

// Renders a "Data as of <date>" line into a placeholder element.
// Accepts an ISO YYYY-MM-DD string from the pipeline's `generated_at`. Leaves
// the element hidden if the value is missing or cannot be parsed, so the UI
// never shows "Invalid Date" or a stale placeholder.
function renderDataAsOf(elementId, generatedAt) {
  const el = document.getElementById(elementId);
  if (!el) return;
  if (typeof generatedAt !== "string" || !generatedAt) {
    el.hidden = true;
    el.textContent = "";
    return;
  }
  const match = /^(\d{4})-(\d{2})-(\d{2})/.exec(generatedAt);
  if (!match) {
    el.hidden = true;
    el.textContent = "";
    return;
  }
  const year = Number(match[1]);
  const month = Number(match[2]) - 1;
  const day = Number(match[3]);
  const date = new Date(Date.UTC(year, month, day));
  if (Number.isNaN(date.getTime())) {
    el.hidden = true;
    el.textContent = "";
    return;
  }
  const formatted = date.toLocaleDateString("en-US", {
    year: "numeric",
    month: "long",
    day: "numeric",
    timeZone: "UTC"
  });
  el.textContent = `Data as of ${formatted}.`;
  el.hidden = false;
}

window.TrackerApp = window.TrackerApp || {};
window.TrackerApp.loadJson = loadJson;
window.TrackerApp.renderDataAsOf = renderDataAsOf;
window.TrackerApp.schoolUrl = schoolUrl;
window.TrackerApp.isNumericUnitid = isNumericUnitid;
window.TrackerApp.isPrimaryTrackerInstitution = isPrimaryTrackerInstitution;
window.TrackerApp.syncTabs = syncTabs;
window.TrackerApp.renderRelatedInstitutionLinks = renderRelatedInstitutionLinks;

window.TrackerApp.escapeHtml = escapeHtml;
window.TrackerApp.normalizeSearchText = normalizeSearchText;
window.TrackerApp.tokenizeSearch = tokenizeSearch;

window.TrackerApp.normalizeQuery = function normalizeQuery(value) {
  return normalizeSearchText(value).trim();
};

window.TrackerApp.filterByInstitution = function filterByInstitution(items, query) {
  const normalized = window.TrackerApp.normalizeQuery(query);
  if (!normalized) return items || [];
  return (items || []).filter((item) => normalizeSearchText(item.institution_name || "").includes(normalized));
};

window.TrackerApp.setDataCardVisible = function setDataCardVisible(id, show) {
  const node = document.getElementById(id);
  const section = node ? node.closest(".data-card") : null;
  if (!section) return;
  section.classList.toggle("is-hidden", !show);
  if (show) {
    section.removeAttribute("aria-hidden");
  } else {
    section.setAttribute("aria-hidden", "true");
  }
};

window.TrackerApp.getParam = function getParam(name) {
  return new URLSearchParams(window.location.search).get(name);
};

window.TrackerApp.renderEmpty = function renderEmpty(message) {
  return `<div class="empty-state"><p>${escapeHtml(message)}</p></div>`;
};

window.TrackerApp.csvEscape = function csvEscape(value) {
  const text = String(value ?? "");
  return /[",\n]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
};

window.TrackerApp.downloadRowsCsv = function downloadRowsCsv(filename, headers, rows) {
  const csv = [headers, ...rows]
    .map((row) => row.map(window.TrackerApp.csvEscape).join(","))
    .join("\n");
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = filename;
  document.body.appendChild(anchor);
  anchor.click();
  anchor.remove();
  // Defer revocation so the click-triggered download has time to read the
  // blob URL. Current Chromium doesn't cancel an in-flight download when
  // the URL is revoked synchronously, but Safari and Firefox have shipped
  // regressions here; a zero-delay setTimeout is the defensive choice.
  setTimeout(() => URL.revokeObjectURL(url), 0);
};

window.TrackerApp.compareText = function compareText(a, b) {
  return String(a || "").localeCompare(String(b || ""), undefined, { sensitivity: "base" });
};

window.TrackerApp.compareDateDesc = function compareDateDesc(a, b) {
  return String(b || "").localeCompare(String(a || ""));
};

window.TrackerApp.renderTextCell = function renderTextCell(value) {
  return { __trackerCell: "text", value };
};

window.TrackerApp.renderSchoolLinkCell = function renderSchoolLinkCell(unitid, label, page = "school.html") {
  return { __trackerCell: "school-link", unitid, label, page };
};

window.TrackerApp.renderExternalLinkCell = function renderExternalLinkCell(url, label = "Source") {
  return { __trackerCell: "external-link", url, label };
};

function renderStructuredCell(cell) {
  if (!cell || typeof cell !== "object" || !cell.__trackerCell) return null;
  if (cell.__trackerCell === "text") return escapeHtml(cell.value);
  if (cell.__trackerCell === "school-link") {
    return window.TrackerApp.renderSchoolLink(cell.unitid, cell.label, cell.page);
  }
  if (cell.__trackerCell === "external-link") {
    return window.TrackerApp.renderExternalLink(cell.url, cell.label);
  }
  return escapeHtml(cell.value);
}

window.TrackerApp.renderHistoryTable = function renderHistoryTable(options = {}) {
  const {
    headers = [],
    rows = [],
    caption = "",
    ariaLabel = "",
    tableClass = "history-table"
  } = options;
  const captionHtml = caption ? `<caption>${escapeHtml(caption)}</caption>` : "";
  const ariaLabelAttr = ariaLabel ? ` aria-label="${escapeHtml(ariaLabel)}"` : "";
  const headerHtml = (headers || []).map((header) => String(header || "")).join("");
  const rowHtml = (rows || []).map((row) => {
    if (Array.isArray(row)) {
      return `<tr>${row.map((cell) => {
        const structuredCell = renderStructuredCell(cell);
        const cellHtml = structuredCell !== null
          ? structuredCell
          : escapeHtml(cell);
        return `<td>${cellHtml}</td>`;
      }).join("")}</tr>`;
    }
    return String(row || "");
  }).join("");

  return `
    <div class="history-table-wrap">
      <table class="${escapeHtml(tableClass)}"${ariaLabelAttr}>
        ${captionHtml}
        <thead><tr>${headerHtml}</tr></thead>
        <tbody>${rowHtml}</tbody>
      </table>
    </div>
  `;
};

window.TrackerApp.safeExternalUrl = function safeExternalUrl(url) {
  const value = String(url ?? "").trim();
  if (!value) return "";
  if (!/^https?:\/\//i.test(value)) return "";
  try {
    const parsed = new URL(value);
    return ["http:", "https:"].includes(parsed.protocol) ? parsed.href : "";
  } catch (_) {
    return "";
  }
};

window.TrackerApp.safeUrl = window.TrackerApp.safeExternalUrl;

function renderAnchorHtml(attrs, label) {
  const cleanAttrs = Object.entries(attrs || {})
    .filter(([, value]) => value !== null && value !== undefined && value !== "");

  if (typeof document !== "undefined" && document.createElement) {
    const anchor = document.createElement("a");
    cleanAttrs.forEach(([name, value]) => {
      anchor.setAttribute(name, String(value));
    });
    anchor.textContent = label ?? "";
    if (typeof anchor.outerHTML === "string") return anchor.outerHTML;
  }

  const attrText = cleanAttrs
    .map(([name, value]) => ` ${name}="${escapeHtml(value)}"`)
    .join("");
  return `<a${attrText}>${escapeHtml(label ?? "")}</a>`;
}

window.TrackerApp.renderExternalLink = function renderExternalLink(url, label = "Source") {
  const href = window.TrackerApp.safeExternalUrl(url);
  if (!href) return "";
  return renderAnchorHtml(
    { href, target: "_blank", rel: "noopener noreferrer" },
    label
  );
};

window.TrackerApp.renderSchoolLink = function renderSchoolLink(unitid, label, page = "school.html") {
  if (!unitid) return escapeHtml(label || "");
  return renderAnchorHtml({ href: schoolUrl(unitid, page) }, label || "");
};

window.TrackerApp.renderPaginationButtons = function renderPaginationButtons({ currentPage, totalPages }) {
  const pageCount = Math.max(1, Number(totalPages) || 1);
  const safePage = Math.min(Math.max(1, Number(currentPage) || 1), pageCount);
  return Array.from({ length: pageCount }, (_, idx) => {
    const pageNumber = idx + 1;
    const isCurrent = pageNumber === safePage;
    const currentAttr = isCurrent ? ' aria-current="page"' : "";
    const ariaLabel = isCurrent ? `Current page, page ${pageNumber}` : `Go to page ${pageNumber}`;
    return `<button type="button" class="pagination-button${isCurrent ? " is-active" : ""}" data-page="${pageNumber}" aria-label="${escapeHtml(ariaLabel)}"${currentAttr}>${pageNumber}</button>`;
  }).join("");
};

window.TrackerApp.paginateItems = function paginateItems(items, page, pageSize) {
  const rows = Array.isArray(items) ? items : [];
  const size = Math.max(1, Number(pageSize) || rows.length || 1);
  const totalPages = Math.max(1, Math.ceil(rows.length / size));
  const currentPage = Math.min(Math.max(1, Number(page) || 1), totalPages);
  const start = (currentPage - 1) * size;
  return {
    totalPages,
    currentPage,
    start,
    pageItems: rows.slice(start, start + size)
  };
};

window.TrackerApp.focusAfterRender = function focusAfterRender(container, selector) {
  setTimeout(() => {
    const node = container?.querySelector(selector);
    if (!node) return;
    if (!node.hasAttribute("tabindex")) node.setAttribute("tabindex", "-1");
    node.focus();
  }, 0);
};

window.TrackerApp.bindPaginationControls = function bindPaginationControls(container, currentPage, onPageChange) {
  container?.querySelectorAll(".pagination-button").forEach((button) => {
    button.addEventListener("click", () => {
      const nextPage = Number(button.dataset.page || "1");
      if (!Number.isNaN(nextPage) && nextPage !== currentPage) {
        onPageChange(nextPage);
      }
    });
  });
};

window.TrackerApp.bindSortControls = function bindSortControls(container, sortState, fallback, onSortChange) {
  const fallbackKey = typeof fallback === "string" ? fallback : fallback?.key;
  const fallbackDirection = typeof fallback === "string" ? "desc" : (fallback?.direction || "desc");
  container?.querySelectorAll(".sort-button").forEach((button) => {
    button.addEventListener("click", () => {
      const key = button.dataset.sortKey || fallbackKey;
      const direction = button.dataset.sortDirection || fallbackDirection;
      if (!key || (sortState?.key === key && sortState?.direction === direction)) return;
      onSortChange({ key, direction });
    });
  });
};

window.TrackerApp.setupPaginatedTable = function setupPaginatedTable(options) {
  const {
    container,
    items,
    pageSize,
    searchInput = null,
    filterItems = (rows) => rows,
    sortItems = (rows) => rows,
    renderPage,
    initialSortState = null,
    defaultSortState = initialSortState,
    downloadButton = null,
    downloadRows = null,
    focusSelector = '.pagination-button[aria-current="page"]'
  } = options || {};

  if (!container || typeof renderPage !== "function") return null;

  let currentPage = 1;
  let sortState = initialSortState ? { ...initialSortState } : null;
  const sourceItems = Array.isArray(items) ? items : [];
  let shouldFocusAfterRender = false;

  const render = () => {
    const filteredItems = filterItems(sourceItems, searchInput?.value || "");
    const sortedItems = sortItems(filteredItems, sortState);
    container.innerHTML = renderPage(sortedItems, currentPage, pageSize, sortState);
    if (shouldFocusAfterRender) {
      window.TrackerApp.focusAfterRender(container, focusSelector);
      shouldFocusAfterRender = false;
    }

    const pageState = window.TrackerApp.paginateItems(sortedItems, currentPage, pageSize);
    currentPage = pageState.currentPage;

    if (downloadButton && typeof downloadRows === "function") {
      downloadButton.classList.toggle("is-hidden", pageState.pageItems.length === 0);
      downloadButton.onclick = () => downloadRows(pageState.pageItems);
    }

    window.TrackerApp.bindPaginationControls(container, currentPage, (nextPage) => {
      currentPage = nextPage;
      shouldFocusAfterRender = true;
      render();
    });

    if (sortState) {
      window.TrackerApp.bindSortControls(container, sortState, defaultSortState, (nextSortState) => {
        sortState = nextSortState;
        currentPage = 1;
        shouldFocusAfterRender = true;
        render();
      });
    }
  };

  if (searchInput && !searchInput.dataset.boundPaginatedTable) {
    searchInput.addEventListener("input", () => {
      currentPage = 1;
      render();
    });
    searchInput.dataset.boundPaginatedTable = "true";
  }

  render();
  return { render };
};

// Convenience factory that wraps setupPaginatedTable with the boilerplate
// shared by every table page (cuts, research, accreditation):
//   - Resolves `downloadButton` from either an Element or an element-id string.
//   - Defaults `filterItems` to the shared filterByInstitution behavior.
//   - Builds the CSV `downloadRows` function from a headers array + row mapper
//     when the caller supplies those instead of a full rows function.
//
// Use this instead of calling setupPaginatedTable directly from page scripts;
// keeping the shared helper means filter/paginate/download/sort behavior stays
// consistent and can't drift between cuts.js, research.js, and accreditation.js.
window.TrackerApp.makeTableController = function makeTableController(options) {
  if (!options) return null;
  const container = typeof options.container === "string"
    ? document.getElementById(options.container)
    : options.container;
  if (!container) return null;

  const downloadButton = typeof options.downloadButton === "string"
    ? document.getElementById(options.downloadButton)
    : options.downloadButton || null;

  let downloadRows = options.downloadRows;
  if (!downloadRows
      && options.downloadFilename
      && Array.isArray(options.downloadHeaders)
      && typeof options.downloadRow === "function") {
    const filename = options.downloadFilename;
    const headers = options.downloadHeaders;
    const rowFn = options.downloadRow;
    downloadRows = (pageItems) => window.TrackerApp.downloadRowsCsv(
      filename,
      headers,
      pageItems.map(rowFn)
    );
  }

  return window.TrackerApp.setupPaginatedTable({
    container,
    items: options.items,
    pageSize: options.pageSize,
    searchInput: options.searchInput || null,
    filterItems: options.filterItems || window.TrackerApp.filterByInstitution,
    sortItems: options.sortItems,
    renderPage: options.renderPage,
    initialSortState: options.initialSortState || null,
    defaultSortState: options.defaultSortState || options.initialSortState || null,
    downloadButton,
    downloadRows,
    focusSelector: options.focusSelector
  });
};

window.TrackerApp.renderSortableHeader = function renderSortableHeader(key, sortState, label) {
  const safeKey = escapeHtml(key || "");
  const safeLabel = escapeHtml(label || "");
  const activeKey = sortState?.key || "";
  const activeDirection = activeKey === key ? sortState.direction : "";
  const ariaSort = activeDirection === "asc"
    ? ' aria-sort="ascending"'
    : activeDirection === "desc"
      ? ' aria-sort="descending"'
      : "";
  const upClass = activeDirection === "asc" ? " is-active" : "";
  const downClass = activeDirection === "desc" ? " is-active" : "";
  return `
    <th${ariaSort}>
      <span class="sort-header-label">${safeLabel}</span>
      <span class="sort-controls" aria-label="Sort ${safeLabel}">
        <button type="button" class="sort-button${upClass}" data-sort-key="${safeKey}" data-sort-direction="asc" aria-label="Sort ${safeLabel} ascending">▲</button>
        <button type="button" class="sort-button${downClass}" data-sort-key="${safeKey}" data-sort-direction="desc" aria-label="Sort ${safeLabel} descending">▼</button>
      </span>
    </th>
  `;
};
