/**
 * FILE: app.js
 * PURPOSE: Global search functionality for the Financial Health Tracker web app
 * FEATURE: Enables cross-page school/institution search from any page in the tracker
 * JSON DATA SOURCE: Loads from either:
 *   - data/schools_index.json (default: financial tracker schools)
 *   - data/accreditation_index.json (when searchSource="accreditation")
 *   - data/college_cuts_index.json (when searchSource="cuts")
 *   - data/research_funding_index.json (when searchSource="research")
 *
 * KEY CONCEPT: unitid (Unit ID) = unique federal college identifier from IPEDS database
 * This file provides the core search infrastructure that other pages (school.html, cuts.html, etc.) use
 */

/**
 * Fetches JSON data from a given path
 * @description Async function that loads JSON files from the server using fetch API.
 * Essential for loading pre-built index and school data files.
 * @param {string} path - File path relative to app root (e.g., "data/schools_index.json")
 * @returns {Promise<Object>} Parsed JSON object from the file
 * @throws {Error} If fetch fails or response is not ok
 */
async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Failed to load ${path}`);
  return response.json();
}

/**
 * Gets the target page for search navigation
 * @description Reads data-search-page attribute from body element to determine
 * which page to navigate to when a school is selected from search results
 * @returns {string} Page filename (e.g., "school.html", "cuts.html")
 */
function getSearchTargetPage() {
  return document.body.dataset.searchPage || "school.html";
}

/**
 * Determines which JSON index file to load based on search context
 * @description Reads data-search-source attribute from body to load the appropriate
 * index file for the current page context (financial data, accreditation, cuts, or research)
 * @returns {string} Path to index JSON file in data/ folder
 */
function getSearchSourcePath() {
  const source = document.body.dataset.searchSource || "all";
  if (source === "accreditation") return "data/accreditation_index.json";
  if (source === "cuts") return "data/college_cuts_index.json";
  if (source === "research") return "data/research_funding_index.json";
  return "data/schools_index.json";
}

/**
 * Gets the type of search index being used
 * @description Returns the search source kind for context-aware badge display
 * (e.g., show latest accreditation action vs. latest college cut)
 * @returns {string} Search source identifier ("accreditation", "cuts", "research", or "all")
 */
function getSearchSourceKind() {
  return document.body.dataset.searchSource || "all";
}

/**
 * Creates a URL to navigate to a specific school's page with URL parameter
 * @description Builds a navigation URL using the unitid as a query parameter.
 * Example: schoolUrl(12345, "school.html") returns "school.html?unitid=12345"
 * This allows the target page to fetch the specific school's JSON data via data/schools/12345.json
 * @param {string|number} unitid - The Unit ID (unique college identifier)
 * @param {string} page - Optional page name; defaults to result of getSearchTargetPage()
 * @returns {string} URL with query parameter for the school ID
 */
function schoolUrl(unitid, page = getSearchTargetPage()) {
  return `${page}?unitid=${encodeURIComponent(unitid)}`;
}

/**
 * Splits a search query into searchable tokens
 * @description Converts user input to lowercase and splits on non-alphanumeric characters
 * to create an array of tokens that can be matched against school data
 * Example: "New York State" -> ["new", "york", "state"]
 * @param {string} value - Raw search query text from user input
 * @returns {Array<string>} Array of lowercase tokens for matching
 */
function tokenizeSearch(value) {
  return String(value || "")
    .toLowerCase()
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

/**
 * Builds a combined searchable string ("haystack") from school fields
 * @description Creates a single string of concatenated school info that will be
 * searched against. This is pre-computed when loading the index for performance.
 * @param {Object} row - School data object from the index JSON
 * @param {string} row.institution_name - Official school name
 * @param {string} row.institution_unique_name - Display name (often includes campus)
 * @param {string} row.city - City where school is located
 * @param {string} row.state - State abbreviation or full name
 * @returns {string} Lowercase combined text for search matching
 */
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

/**
 * Initializes the search functionality on page load
 * @description Main entry point for search system. Loads the appropriate index JSON,
 * populates the datalist for autocomplete, sets up event listeners for keyboard/mouse navigation,
 * and manages the search results dropdown. Implements keyboard accessibility with arrow key support
 * and roving tabindex pattern for screen readers.
 * @returns {Promise<void>} Completes when search is initialized and ready
 */
async function initSearch() {
  // Load the appropriate index file based on page context
  const raw = await loadJson(getSearchSourcePath());

  // Convert to array if object, pre-compute search haystack for each school, and sort alphabetically
  const schools = (Array.isArray(raw) ? raw : Object.values(raw || {})).slice().map((row) => ({
    ...row,
    _searchHaystack: buildSearchHaystack(row)
  })).sort((a, b) =>
    String(a.institution_unique_name || a.institution_name || "").localeCompare(
      String(b.institution_unique_name || b.institution_name || "")
    )
  );

  // Get DOM references for search input, results container, and datalist
  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");
  const datalist = document.getElementById("school-options");
  if (!input || !results) return;

  // Get navigation context from page
  const page = getSearchTargetPage();
  const sourceKind = getSearchSourceKind();

  // Roving tabindex state - tracks which result is keyboard-focused
  let activeIndex = -1;

  // Populate HTML datalist for browser autocomplete
  if (datalist) {
    datalist.innerHTML = "";
    schools.forEach((item) => {
      const option = document.createElement("option");
      option.value = item.institution_unique_name || item.institution_name;
      datalist.appendChild(option);
    });
  }

  /**
   * Clears the search results dropdown
   * @description Empties the results container and resets the active index
   */
  function clearResults() {
    results.innerHTML = "";
    activeIndex = -1;
  }

  /**
   * Gets the display text for a school search result
   * @description Returns the unique name if available, falls back to institution name
   * @param {Object} row - School object from index
   * @returns {string} Name to display in search results
   */
  function getMatchText(row) {
    return row.institution_unique_name || row.institution_name || "";
  }

  /**
   * Generates a secondary badge showing latest action for a school
   * @description Displays context-specific info like latest accreditation action or college cut.
   * Trims long text to prevent layout overflow.
   * @param {Object} row - School object from index
   * @returns {string} HTML/text for badge, empty string if not applicable to search type
   */
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

  /**
   * Gets all result button elements from the current results dropdown
   * @description Helper to query all active result buttons for keyboard navigation
   * @returns {Array<HTMLElement>} Array of result button DOM elements
   */
  function getAllResultButtons() {
    return Array.from(results.querySelectorAll(".result-item[data-unitid]"));
  }

  /**
   * Sets which result button is keyboard-focused using roving tabindex pattern
   * @description Updates the activeIndex and manages tabindex attributes on all buttons.
   * Only the active button has tabindex="0" (focusable), others have tabindex="-1" (not in tab order).
   * @param {number} newIndex - Index of result to activate (auto-clamped to valid range)
   */
  function setActiveButton(newIndex) {
    const buttons = getAllResultButtons();
    if (!buttons.length) return;
    // Clamp to valid range [0, length-1]
    activeIndex = Math.max(0, Math.min(newIndex, buttons.length - 1));
    buttons.forEach((btn, i) => {
      btn.setAttribute("tabindex", i === activeIndex ? "0" : "-1");
    });
    buttons[activeIndex].focus();
  }

  /**
   * Navigates to the school page for the currently keyboard-selected result
   * @description Called when user presses Enter on focused result
   */
  function navigateToActive() {
    const buttons = getAllResultButtons();
    if (buttons[activeIndex]) {
      window.location.href = schoolUrl(buttons[activeIndex].dataset.unitid, page);
    }
  }

  /**
   * Filters and renders search results based on current query
   * @description Implements multi-token matching: all query tokens must appear in the haystack.
   * Results are sorted by relevance (name starts with query gets priority).
   * Limited to 8 results for performance. Shows "no results" message if empty.
   * @param {string} query - Current search input value
   */
  function renderMatches(query) {
    const q = query.trim().toLowerCase();
    const tokens = tokenizeSearch(query);
    if (!q || !tokens.length) {
      clearResults();
      return;
    }

    // Filter schools: ALL tokens must appear in the haystack
    const matches = schools
      .filter((row) => tokens.every((token) => row._searchHaystack.includes(token)))
      .sort((a, b) => {
        // Sort by relevance: schools whose name STARTS with query come first
        const aName = String(a.institution_name || "").toLowerCase();
        const bName = String(b.institution_name || "").toLowerCase();
        const aStarts = aName.startsWith(q) ? 1 : 0;
        const bStarts = bName.startsWith(q) ? 1 : 0;
        if (aStarts !== bStarts) return bStarts - aStarts;

        // Secondary sort: unique name starting with query
        const aUniqueStarts = String(a.institution_unique_name || "").toLowerCase().startsWith(q) ? 1 : 0;
        const bUniqueStarts = String(b.institution_unique_name || "").toLowerCase().startsWith(q) ? 1 : 0;
        if (aUniqueStarts !== bUniqueStarts) return bUniqueStarts - aUniqueStarts;

        // Final sort: alphabetical by unique name
        return String(a.institution_unique_name || a.institution_name || "").localeCompare(
          String(b.institution_unique_name || b.institution_name || "")
        );
      })
      .slice(0, 8); // Limit to 8 results

    if (!matches.length) {
      clearResults();
      results.innerHTML = `<div class="result-item is-empty" role="option" tabindex="-1">No matching institutions found.</div>`;
      return;
    }

    // Reset active index and render result buttons
    activeIndex = -1;
    results.setAttribute("aria-label", `${matches.length} search result${matches.length !== 1 ? "s" : ""}`);
    results.innerHTML = matches.map((row) => `
      <button type="button" class="result-item" role="option" data-unitid="${row.unitid}" tabindex="-1">
        <span>${getMatchText(row)}</span>
        ${getResultBadge(row) ? `<small class="small-meta">${getResultBadge(row)}</small>` : ""}
      </button>
    `).join("");

    // Attach event listeners to each result button
    results.querySelectorAll("[data-unitid]").forEach((button, i) => {
      // Click to navigate
      button.addEventListener("click", () => {
        window.location.href = schoolUrl(button.dataset.unitid, page);
      });

      // Arrow key navigation on each result button (implements roving tabindex)
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

  // Attach event listener to search input
  input.addEventListener("input", (e) => {
    renderMatches(e.target.value);
  });
}

// Initialize search on page load
initSearch().catch((error) => {
  console.error("Search initialization failed:", error);
});

// Export functions to window for use by other scripts
window.TrackerApp = window.TrackerApp || {};
window.TrackerApp.loadJson = loadJson;
window.TrackerApp.schoolUrl = schoolUrl;
