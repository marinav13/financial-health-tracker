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

// ------ Search Tokenization & Matching ------

// Splits query into searchable tokens (alphanumeric only, lowercase)
function tokenizeSearch(value) {
  return String(value || "")
    .toLowerCase()
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
    .toLowerCase();
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

  // Roving tabindex pattern: only active button has tabindex="0"
  function setActiveButton(newIndex) {
    const buttons = getAllResultButtons();
    if (!buttons.length) return;
    activeIndex = Math.max(0, Math.min(newIndex, buttons.length - 1));
    buttons.forEach((btn, i) => {
      btn.setAttribute("tabindex", i === activeIndex ? "0" : "-1");
    });
    buttons[activeIndex].focus();
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
    const q = query.trim().toLowerCase();
    const tokens = tokenizeSearch(query);
    if (!q || !tokens.length) {
      clearResults();
      return;
    }

    const matches = schools
      .filter((row) => tokens.every((token) => row._searchHaystack.includes(token)))
      .sort((a, b) => {
        const aName = String(a.institution_name || "").toLowerCase();
        const bName = String(b.institution_name || "").toLowerCase();
        const aStarts = aName.startsWith(q) ? 1 : 0;
        const bStarts = bName.startsWith(q) ? 1 : 0;
        if (aStarts !== bStarts) return bStarts - aStarts;

        const aUniqueStarts = String(a.institution_unique_name || "").toLowerCase().startsWith(q) ? 1 : 0;
        const bUniqueStarts = String(b.institution_unique_name || "").toLowerCase().startsWith(q) ? 1 : 0;
        if (aUniqueStarts !== bUniqueStarts) return bUniqueStarts - aUniqueStarts;

        return String(a.institution_unique_name || a.institution_name || "").localeCompare(
          String(b.institution_unique_name || b.institution_name || "")
        );
      })
      .slice(0, 8);

    if (!matches.length) {
      clearResults();
      results.innerHTML = `<div class="result-item is-empty" role="option" tabindex="-1">No matching institutions found.</div>`;
      return;
    }

    activeIndex = -1;
    results.setAttribute("aria-label", `${matches.length} search result${matches.length !== 1 ? "s" : ""}`);
    results.innerHTML = matches.map((row) => `
      <button type="button" class="result-item" role="option" data-unitid="${row.unitid}" tabindex="-1">
        <span>${escapeHtml(getMatchText(row))}</span>
        ${getResultBadge(row) ? `<small class="small-meta">${escapeHtml(getResultBadge(row))}</small>` : ""}
      </button>
    `).join("");

    results.querySelectorAll("[data-unitid]").forEach((button, i) => {
      button.addEventListener("click", () => {
        window.location.href = schoolUrl(button.dataset.unitid, page);
      });

      // Arrow key navigation with roving tabindex
      button.addEventListener("keydown", (e) => {
        if (e.key === "ArrowDown") {
          e.preventDefault();
          setActiveButton(i + 1);
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          setActiveButton(i - 1);
        } else if (e.key === "Enter" || e.key === " ") {
          window.location.href = schoolUrl(button.dataset.unitid, page);
        }
      });
    });
  }

  input.addEventListener("input", (e) => {
    renderMatches(e.target.value);
  });

  // Escape key dismisses the results dropdown and returns focus to the input.
  input.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      clearResults();
      input.focus();
    }
  });
}

initSearch().catch((error) => {
  console.error("Search initialization failed:", error);
});

window.TrackerApp = window.TrackerApp || {};
window.TrackerApp.loadJson = loadJson;
window.TrackerApp.schoolUrl = schoolUrl;

window.TrackerApp.escapeHtml = escapeHtml;

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
