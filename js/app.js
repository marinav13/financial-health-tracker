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

function isPrimaryTrackerInstitution(record) {
  return record?.is_primary_tracker === true;
}

function hasIndexedRelatedRecord(record, countField) {
  if (!record) return false;
  const count = Number(record[countField]);
  return Number.isFinite(count) ? count > 0 : true;
}

function findRelatedIndexRecord(index, unitid, countField) {
  const normalizedUnitid = String(unitid || "");
  if (!normalizedUnitid) return null;
  const direct = index?.[normalizedUnitid];
  if (hasIndexedRelatedRecord(direct, countField)) return direct;
  return Object.values(index || {}).find((record) =>
    String(record?.financial_unitid || "") === normalizedUnitid &&
    hasIndexedRelatedRecord(record, countField)
  ) || null;
}

function syncTabs(unitid = "", options = {}) {
  const active = options.active || document.body.dataset.activeTab || (
    document.body.dataset.searchSource || "finances"
  );
  const tabs = {
    home: document.getElementById("tab-home"),
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
  if (tabs.home) tabs.home.href = "index.html";
  if (tabs.finances) tabs.finances.href = "school.html";
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
    hasFinancialProfile = false,
    current = "",
    include = ["finances", "cuts", "accreditation", "research"],
    relatedIndexes = {}
  } = options;
  // Only render the Finances link when the caller passes a numeric
  // financialUnitid *and* the current side-dataset record is part of the
  // primary tracker universe. Numeric IDs alone are not sufficient:
  // closed or otherwise excluded schools can still carry a financial_unitid
  // in cuts/accreditation/research, but there is no underlying
  // data/schools/<id>.json profile to open from the Finances page.
  const financeUnitid = isNumericUnitid(financialUnitid) ? String(financialUnitid) : "";
  const links = [];
  const cutsRecord = findRelatedIndexRecord(relatedIndexes.cuts, unitid, "cut_count")
    || findRelatedIndexRecord(relatedIndexes.cuts, financialUnitid, "cut_count");
  const accreditationRecord = findRelatedIndexRecord(relatedIndexes.accreditation, unitid, "action_count")
    || findRelatedIndexRecord(relatedIndexes.accreditation, financialUnitid, "action_count");
  const researchRecord = findRelatedIndexRecord(relatedIndexes.research, unitid, "total_disrupted_grants")
    || findRelatedIndexRecord(relatedIndexes.research, financialUnitid, "total_disrupted_grants");

  if (include.includes("finances") && current !== "finances" && financeUnitid && hasFinancialProfile) {
    links.push(window.TrackerApp.renderSchoolLink(financeUnitid, "Finances", "school.html"));
  }
  if (include.includes("cuts") && current !== "cuts" && cutsRecord?.unitid) {
    links.push(window.TrackerApp.renderSchoolLink(cutsRecord.unitid, "College Cuts", "cuts.html"));
  }
  if (include.includes("accreditation") && current !== "accreditation" && accreditationRecord?.unitid) {
    links.push(window.TrackerApp.renderSchoolLink(accreditationRecord.unitid, "Accreditation", "accreditation.html"));
  }
  if (include.includes("research") && current !== "research" && researchRecord?.unitid) {
    links.push(window.TrackerApp.renderSchoolLink(researchRecord.unitid, "Research Funding Cuts", "research.html"));
  }

  if (!links.length) return "";
  return `
    <div class="related-links">
      <p><strong>Explore this institution further by looking at its:</strong></p>
      <ul class="link-list">${links.map((link) => `<li>${link}</li>`).join("")}</ul>
    </div>
  `;
}

function upgradeSiteFooter() {
  if (typeof document === "undefined" || typeof document.querySelector !== "function") return;
  const footer = document.querySelector(".pg-foot");
  if (!footer) return;

  footer.replaceChildren();

  const wrapper = document.createElement("div");
  wrapper.className = "ftr-bottom";

  const content = document.createElement("div");
  content.className = "site-nav-copyright";

  const message = document.createElement("p");
  message.className = "ftr-info-msg";
  message.textContent = "The Hechinger Report covers inequality and innovation in education with in-depth journalism that uses research, data and stories from classrooms and campuses to show the public how education can be improved and why it matters.";

  const nav = document.createElement("nav");
  nav.className = "ftr-site-nav";
  nav.setAttribute("aria-label", "Footer");

  const list = document.createElement("ul");
  list.className = "ftr-nav-items";

  [
    ["Our Work", "https://hechingerreport.org/"],
    ["Our Mission", "https://hechingerreport.org/our-mission/"],
    ["Contact Us", "https://hechingerreport.org/contact/"]
  ].forEach(([label, href]) => {
    const item = document.createElement("li");
    item.className = "ftr-nav-item";

    const link = document.createElement("a");
    link.className = "ftr-nav-link";
    link.href = href;
    link.textContent = label;

    item.appendChild(link);
    list.appendChild(item);
  });

  nav.appendChild(list);
  content.append(message, nav);
  wrapper.appendChild(content);
  footer.appendChild(wrapper);
}

// ------ Search Tokenization & Matching ------

function normalizeSearchText(value) {
  return String(value || "")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "");
}

const AFFECTED_PARENS_RE =
  /\s*\(\s*[\d,]+\s+(?:students?|faculty|staff|positions?|employees?|people)?\s*affected\s*\)\s*$/i;

function cleanCutLabel(value) {
  return String(value || "").replace(AFFECTED_PARENS_RE, "").trim();
}

const US_STATE_ABBREVIATIONS = Object.freeze({
  "alabama": "AL",
  "alaska": "AK",
  "arizona": "AZ",
  "arkansas": "AR",
  "california": "CA",
  "colorado": "CO",
  "connecticut": "CT",
  "delaware": "DE",
  "district of columbia": "DC",
  "florida": "FL",
  "georgia": "GA",
  "hawaii": "HI",
  "idaho": "ID",
  "illinois": "IL",
  "indiana": "IN",
  "iowa": "IA",
  "kansas": "KS",
  "kentucky": "KY",
  "louisiana": "LA",
  "maine": "ME",
  "maryland": "MD",
  "massachusetts": "MA",
  "michigan": "MI",
  "minnesota": "MN",
  "mississippi": "MS",
  "missouri": "MO",
  "montana": "MT",
  "nebraska": "NE",
  "nevada": "NV",
  "new hampshire": "NH",
  "new jersey": "NJ",
  "new mexico": "NM",
  "new york": "NY",
  "north carolina": "NC",
  "north dakota": "ND",
  "ohio": "OH",
  "oklahoma": "OK",
  "oregon": "OR",
  "pennsylvania": "PA",
  "rhode island": "RI",
  "south carolina": "SC",
  "south dakota": "SD",
  "tennessee": "TN",
  "texas": "TX",
  "utah": "UT",
  "vermont": "VT",
  "virginia": "VA",
  "washington": "WA",
  "west virginia": "WV",
  "wisconsin": "WI",
  "wyoming": "WY"
});

// Splits query into searchable tokens (alphanumeric only, lowercase)
function tokenizeSearch(value) {
  return normalizeSearchText(value)
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

function stateAbbreviationForName(stateName) {
  return US_STATE_ABBREVIATIONS[normalizeSearchText(stateName).trim()] || "";
}

function splitSearchAliasValue(value) {
  const text = String(value ?? "").trim();
  if (!text) return [];
  return text
    .replace(/\|{2,}/g, "|")
    .split(/\||;|\s+\/\s+/)
    .map((part) => part.trim())
    .filter(Boolean);
}
function collectSearchAliases(row) {
  const rawValues = [];
  ["institution_alias", "institution_aliases", "alias", "aliases"].forEach((key) => {
    const value = row?.[key];
    if (Array.isArray(value)) {
      rawValues.push(...value);
    } else {
      rawValues.push(value);
    }
  });
  const seen = new Set();
  return rawValues.reduce((aliases, value) => {
    splitSearchAliasValue(value).forEach((text) => {
      if (seen.has(text)) return;
      seen.add(text);
      aliases.push(text);
    });
    return aliases;
  }, []);
}

// Pre-computes combined searchable string for performance
function buildSearchHaystack(row, aliases = []) {
  return [
    row.institution_name,
    row.institution_unique_name,
    row.city,
    row.state,
    stateAbbreviationForName(row.state),
    row.unitid,
    ...aliases
  ]
    .filter(Boolean)
    .join(" ")
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "");
}

function buildStateCatalog(rows) {
  const states = new Map();
  rows.forEach((row) => {
    const display = String(row?.state || "").trim();
    if (!display) return;
    const norm = normalizeSearchText(display).trim();
    if (!norm) return;
    const existing = states.get(norm);
    if (existing) {
      existing.count += 1;
      return;
    }
    const abbr = stateAbbreviationForName(display);
    states.set(norm, {
      display,
      norm,
      abbr,
      abbrNorm: normalizeSearchText(abbr).trim(),
      count: 1
    });
  });
  return Array.from(states.values()).sort((a, b) => a.display.localeCompare(b.display));
}

function getExactStateMatch(stateCatalog, queryNorm) {
  if (!queryNorm) return null;
  return stateCatalog.find((state) =>
    state.norm === queryNorm ||
    (state.abbrNorm && state.abbrNorm === queryNorm)
  ) || null;
}

function getSearchInstances() {
  if (typeof document === "undefined") return [];
  const canQueryAll = typeof document.querySelectorAll === "function";
  const canGetById = typeof document.getElementById === "function";
  const explicitInstances = (canQueryAll
    ? Array.from(document.querySelectorAll("[data-school-search-instance]"))
    : []
  ).map((container, index) => {
    const input = container.querySelector(".search");
    const results = container.querySelector(".results");
    if (!input || !results) return null;
    return { container, input, results, index };
  }).filter(Boolean);

  if (explicitInstances.length) return explicitInstances;

  if (!canGetById) return [];
  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");
  if (!input || !results) return [];
  const container = typeof input.closest === "function"
    ? (input.closest(".search-panel") || document.body)
    : document.body;
  return [{ container, input, results, index: 0 }];
}

// ------ Search Initialization & Rendering ------

async function initSearch() {
  const raw = await loadJson(getSearchSourcePath());

  const schools = (Array.isArray(raw) ? raw : Object.values(raw || {})).slice().map((row) => {
    const searchAliases = collectSearchAliases(row);
    return {
      ...row,
      _searchAliases: searchAliases,
      _stateNorm: normalizeSearchText(row.state || "").trim(),
      _searchHaystack: buildSearchHaystack(row, searchAliases)
    };
  }).sort((a, b) =>
    String(a.institution_unique_name || a.institution_name || "").localeCompare(
      String(b.institution_unique_name || b.institution_name || "")
    )
  );
  const stateCatalog = buildStateCatalog(schools);

  const page = getSearchTargetPage();
  const sourceKind = getSearchSourceKind();
  const instances = getSearchInstances();
  if (!instances.length) return;

  // Populate datalist for browser autocomplete
  const datalist = document.getElementById("school-options");
  if (datalist) {
    datalist.innerHTML = "";
    schools.forEach((item) => {
      const option = document.createElement("option");
      option.value = item.institution_unique_name || item.institution_name;
      datalist.appendChild(option);
    });
  }

  function getMatchText(row) {
    return row.institution_name || row.institution_unique_name || "";
  }

  function getResultMeta(row) {
    return [row.city, row.state].filter(Boolean).join(" | ");
  }

  // Shows context-specific badge (accreditation action, college cut, etc.)
  function getResultBadge(row) {
    function trimBadge(text) {
      const value = String(text || "");
      return value.length > 120 ? `${value.slice(0, 117)}...` : value;
    }
    if (sourceKind === "cuts" && row.latest_cut_label) {
      const date = row.latest_cut_date || "";
      return `Latest cut${date ? ` (${date})` : ""}: ${trimBadge(cleanCutLabel(row.latest_cut_label))}`;
    }
    if (sourceKind === "accreditation" && row.latest_action_label) {
      const date = row.latest_action_date || "";
      return `Latest action${date ? ` (${date})` : ""}: ${trimBadge(row.latest_action_label)}`;
    }
    return "";
  }

  instances.forEach(({ input, results, index }) => {
    let activeIndex = -1;

    // Mark the results container as a listbox so screen readers announce
    // individual items (role="option") as selectable choices.
    results.setAttribute("role", "listbox");
    if (!results.id) results.id = index === 0 ? "search-results" : `search-results-${index + 1}`;
    input.setAttribute("role", "combobox");
    input.setAttribute("aria-autocomplete", "list");
    input.setAttribute("aria-controls", results.id);
    input.setAttribute("aria-expanded", "false");

    function clearResults() {
      results.innerHTML = "";
      results.classList.remove("has-results");
      results.removeAttribute("aria-label");
      activeIndex = -1;
      input.setAttribute("aria-expanded", "false");
      input.removeAttribute("aria-activedescendant");
    }

    function getAllResultButtons() {
      return Array.from(results.querySelectorAll('.result-item[role="option"]'));
    }

    function setActiveOption(newIndex) {
      const buttons = getAllResultButtons();
      if (!buttons.length) return;
      activeIndex = Math.max(0, Math.min(newIndex, buttons.length - 1));
      buttons.forEach((btn, buttonIndex) => {
        btn.setAttribute("tabindex", "-1");
        btn.setAttribute("aria-selected", buttonIndex === activeIndex ? "true" : "false");
      });
      const activeButton = buttons[activeIndex];
      input.setAttribute("aria-activedescendant", activeButton.id);
      activeButton.scrollIntoView({ block: "nearest" });
    }

    function commitOption(button) {
      if (!button) return;
      const unitid = String(button.dataset.unitid || "").trim();
      if (unitid) {
        window.location.href = schoolUrl(unitid, page);
        return;
      }
      const state = String(button.dataset.state || "").trim();
      if (!state) return;
      input.value = state;
      renderMatches(state);
      input.focus();
      if (typeof input.setSelectionRange === "function") {
        const end = input.value.length;
        input.setSelectionRange(end, end);
      }
    }

    function navigateToActive() {
      const buttons = getAllResultButtons();
      if (buttons[activeIndex]) {
        commitOption(buttons[activeIndex]);
        return;
      }
      if (buttons.length === 1) {
        commitOption(buttons[0]);
      }
    }

    function renderMatches(query) {
      const q = normalizeSearchText(query).trim();
      const tokens = tokenizeSearch(query);
      if (!q || !tokens.length) {
        clearResults();
        return;
      }

      const exactState = getExactStateMatch(stateCatalog, q);
      const matches = (exactState
        ? schools.filter((row) => row._stateNorm === exactState.norm)
        : schools.filter((row) => tokens.every((token) => row._searchHaystack.includes(token)))
      ).sort((a, b) => {
          if (exactState) {
            return String(a.institution_unique_name || a.institution_name || "").localeCompare(
              String(b.institution_unique_name || b.institution_name || "")
            );
          }
          const aName = normalizeSearchText(a.institution_name || "");
          const bName = normalizeSearchText(b.institution_name || "");
          const aStarts = aName.startsWith(q) ? 1 : 0;
          const bStarts = bName.startsWith(q) ? 1 : 0;
          if (aStarts !== bStarts) return bStarts - aStarts;

          const aUniqueStarts = normalizeSearchText(a.institution_unique_name || "").startsWith(q) ? 1 : 0;
          const bUniqueStarts = normalizeSearchText(b.institution_unique_name || "").startsWith(q) ? 1 : 0;
          if (aUniqueStarts !== bUniqueStarts) return bUniqueStarts - aUniqueStarts;

          const aAliasStarts = (a._searchAliases || []).some((alias) => normalizeSearchText(alias).startsWith(q)) ? 1 : 0;
          const bAliasStarts = (b._searchAliases || []).some((alias) => normalizeSearchText(alias).startsWith(q)) ? 1 : 0;
          if (aAliasStarts !== bAliasStarts) return bAliasStarts - aAliasStarts;

          const aStateStarts = normalizeSearchText(a.state || "").startsWith(q) ? 1 : 0;
          const bStateStarts = normalizeSearchText(b.state || "").startsWith(q) ? 1 : 0;
          if (aStateStarts !== bStateStarts) return bStateStarts - aStateStarts;

          return String(a.institution_unique_name || a.institution_name || "").localeCompare(
            String(b.institution_unique_name || b.institution_name || "")
          );
        });

      if (!matches.length) {
        results.classList.add("has-results");
        results.setAttribute("aria-label", "No search results");
        input.setAttribute("aria-expanded", "true");
        input.removeAttribute("aria-activedescendant");
        activeIndex = -1;
        results.innerHTML = `<div id="${results.id}-empty" class="result-item is-empty">No matching institutions found.</div>`;
        return;
      }

      activeIndex = -1;
      results.classList.add("has-results");
      results.scrollTop = 0;
      input.setAttribute("aria-expanded", "true");
      input.removeAttribute("aria-activedescendant");
      results.setAttribute(
        "aria-label",
        `${matches.length} search result${matches.length !== 1 ? "s" : ""}`
      );
      let optionIndex = 0;
      const renderSchoolOption = (row) => {
        const id = `${results.id}-option-${optionIndex++}`;
        const metaParts = [getResultMeta(row), getResultBadge(row)].filter(Boolean);
        return `
          <button type="button" id="${id}" class="result-item" role="option" data-unitid="${escapeHtml(row.unitid)}" tabindex="-1" aria-selected="false">
            <span class="result-item-label">${escapeHtml(getMatchText(row))}</span>
            ${metaParts.length ? `<span class="result-item-meta">${escapeHtml(metaParts.join(" | "))}</span>` : ""}
          </button>
        `;
      };
      const schoolCountLabel = `${matches.length} tracked institution${matches.length !== 1 ? "s" : ""}`;
      results.innerHTML = `
        <div class="result-section" role="group" aria-label="Schools">
          <div class="result-section-title">Schools (${escapeHtml(schoolCountLabel)})</div>
          ${matches.map(renderSchoolOption).join("")}
        </div>
      `;
    }

    results.addEventListener("click", (e) => {
      const option = e.target.closest('.result-item[role="option"]');
      if (!option) return;
      commitOption(option);
    });

    results.addEventListener("mousedown", (e) => {
      const option = e.target.closest('.result-item[role="option"]');
      if (!option) return;
      e.preventDefault();
    });

    input.addEventListener("input", (e) => {
      renderMatches(e.target.value);
    });

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
  });
}

upgradeSiteFooter();

async function initMethodologyInstitutionCount() {
  if (typeof document === "undefined" || typeof document.querySelectorAll !== "function") return;
  const targets = Array.from(document.querySelectorAll("#methodology-institution-count, #methodology-institution-count-lower"));
  if (!targets.length) return;
  const schools = await loadJson("data/schools_index.json");
  const count = Array.isArray(schools) ? schools.length : 0;
  if (!Number.isFinite(count) || count <= 0) return;
  const formatted = new Intl.NumberFormat("en-US").format(count);
  targets.forEach((target) => {
    target.textContent = formatted;
  });
}

initMethodologyInstitutionCount().catch((error) => {
  console.error("Methodology institution count failed:", error);
});

initSearch().catch((error) => {
  console.error("Search initialization failed:", error);
  getSearchInstances().forEach(({ input, results }) => {
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
window.TrackerApp.findRelatedIndexRecord = findRelatedIndexRecord;
window.TrackerApp.syncTabs = syncTabs;
window.TrackerApp.renderRelatedInstitutionLinks = renderRelatedInstitutionLinks;

window.TrackerApp.escapeHtml = escapeHtml;
window.TrackerApp.normalizeSearchText = normalizeSearchText;
window.TrackerApp.cleanCutLabel = cleanCutLabel;
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

// Sets textContent on a DOM node by id; falls back to "No data" when value is
// nullish so callers can pass through pipeline values without inline guards.
window.TrackerApp.setText = function setText(id, value) {
  const node = document.getElementById(id);
  if (node) node.textContent = value ?? "No data";
};

window.TrackerApp.csvEscape = function csvEscape(value) {
  const text = String(value ?? "");
  const sanitized = /^[=+\-@]/.test(text) ? `'${text}` : text;
  return /[",\n]/.test(sanitized) ? `"${sanitized.replace(/"/g, '""')}"` : sanitized;
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

function parseComparableDate(value) {
  const text = String(value || "").trim();
  if (!text) return null;
  if (/^\d{4}-\d{2}-\d{2}$/.test(text)) {
    const parsed = new Date(`${text}T00:00:00Z`);
    return Number.isNaN(parsed.getTime()) ? null : parsed.getTime();
  }
  if (/^\d{4}-\d{2}$/.test(text)) {
    const parsed = new Date(`${text}-01T00:00:00Z`);
    return Number.isNaN(parsed.getTime()) ? null : parsed.getTime();
  }
  if (/^\d{4}$/.test(text)) {
    const parsed = new Date(`${text}-01-01T00:00:00Z`);
    return Number.isNaN(parsed.getTime()) ? null : parsed.getTime();
  }
  const parsed = new Date(text);
  return Number.isNaN(parsed.getTime()) ? null : parsed.getTime();
}

window.TrackerApp.compareDateDesc = function compareDateDesc(a, b) {
  const aTime = parseComparableDate(a);
  const bTime = parseComparableDate(b);
  const aHasDate = Number.isFinite(aTime);
  const bHasDate = Number.isFinite(bTime);

  if (aHasDate && bHasDate && aTime !== bTime) {
    return bTime - aTime;
  }
  if (aHasDate !== bHasDate) {
    return aHasDate ? -1 : 1;
  }
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

// Windowed pagination: First | Prev | 1 ... [window around current] ... last | Next | Last.
// Replaces the prior "render every page button from 1..N" approach which produced
// 100+ buttons under tall tables (e.g. the 111-page "Recent accreditation actions
// at other institutions" table). For totalPages <= 7 we still render every page
// (no ellipsis needed); for larger sets we show the first/last anchors plus a
// 3-page window around currentPage with ellipsis fillers. Buttons all carry
// data-page so existing click handlers continue to work; ellipsis fillers are
// non-interactive <span>s.
window.TrackerApp.renderPaginationButtons = function renderPaginationButtons({ currentPage, totalPages }) {
  const pageCount = Math.max(1, Number(totalPages) || 1);
  const safePage = Math.min(Math.max(1, Number(currentPage) || 1), pageCount);

  const renderPage = (n) => {
    const isCurrent = n === safePage;
    const currentAttr = isCurrent ? ' aria-current="page"' : "";
    const ariaLabel = isCurrent ? `Current page, page ${n}` : `Go to page ${n}`;
    return `<button type="button" class="pagination-button${isCurrent ? " is-active" : ""}" data-page="${n}" aria-label="${escapeHtml(ariaLabel)}"${currentAttr}>${n}</button>`;
  };
  const renderEllipsis = () => `<span class="pagination-ellipsis">…</span>`;
  const renderNav = (pageTarget, label, ariaLabel, disabled) => {
    const disabledAttr = disabled ? " disabled" : "";
    const disabledClass = disabled ? " is-disabled" : "";
    return `<button type="button" class="pagination-button pagination-nav${disabledClass}" data-page="${pageTarget}" aria-label="${escapeHtml(ariaLabel)}"${disabledAttr}>${label}</button>`;
  };

  const parts = [];
  parts.push(renderNav(1, "« First", "Go to first page", safePage === 1));
  parts.push(renderNav(Math.max(1, safePage - 1), "‹ Prev", "Go to previous page", safePage === 1));

  if (pageCount <= 7) {
    for (let n = 1; n <= pageCount; n++) parts.push(renderPage(n));
  } else if (safePage <= 4) {
    for (let n = 1; n <= 5; n++) parts.push(renderPage(n));
    parts.push(renderEllipsis());
    parts.push(renderPage(pageCount));
  } else if (safePage >= pageCount - 3) {
    parts.push(renderPage(1));
    parts.push(renderEllipsis());
    for (let n = pageCount - 4; n <= pageCount; n++) parts.push(renderPage(n));
  } else {
    parts.push(renderPage(1));
    parts.push(renderEllipsis());
    for (let n = safePage - 1; n <= safePage + 1; n++) parts.push(renderPage(n));
    parts.push(renderEllipsis());
    parts.push(renderPage(pageCount));
  }

  parts.push(renderNav(Math.min(pageCount, safePage + 1), "Next ›", "Go to next page", safePage === pageCount));
  parts.push(renderNav(pageCount, "Last »", "Go to last page", safePage === pageCount));

  return parts.join("");
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
  const focusNode = () => {
    const node = container?.querySelector(selector);
    if (!node) return false;
    if (!node.hasAttribute("tabindex")) node.setAttribute("tabindex", "-1");
    node.focus({ preventScroll: true });
    return document.activeElement === node;
  };

  if (focusNode()) return;
  setTimeout(focusNode, 0);
};

window.TrackerApp.bindPaginationControls = function bindPaginationControls(container, currentPage, onPageChange) {
  container?.querySelectorAll(".pagination-button").forEach((button) => {
    button.addEventListener("click", () => {
      const nextPage = Number(button.dataset.page || "1");
      if (!Number.isNaN(nextPage) && nextPage !== currentPage) {
        onPageChange(nextPage, `.pagination-button[data-page="${nextPage}"]`);
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
      onSortChange(
        { key, direction },
        `.sort-button[data-sort-key="${key}"][data-sort-direction="${direction}"]`
      );
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
  let pendingFocusSelector = focusSelector;

  const render = () => {
    const filteredItems = filterItems(sourceItems, searchInput?.value || "");
    const sortedItems = sortItems(filteredItems, sortState);
    container.innerHTML = renderPage(sortedItems, currentPage, pageSize, sortState);
    if (shouldFocusAfterRender) {
      window.TrackerApp.focusAfterRender(container, pendingFocusSelector || focusSelector);
      shouldFocusAfterRender = false;
      pendingFocusSelector = focusSelector;
    }

    const pageState = window.TrackerApp.paginateItems(sortedItems, currentPage, pageSize);
    currentPage = pageState.currentPage;

    if (downloadButton && typeof downloadRows === "function") {
      // Download exports the full filtered + sorted list, not just the
      // current page. Hidden when the filtered list itself is empty so
      // an "empty" filter doesn't surface a zero-row CSV.
      downloadButton.classList.toggle("is-hidden", sortedItems.length === 0);
      downloadButton.onclick = () => downloadRows(sortedItems);
    }

    window.TrackerApp.bindPaginationControls(container, currentPage, (nextPage, nextFocusSelector) => {
      currentPage = nextPage;
      shouldFocusAfterRender = true;
      pendingFocusSelector = nextFocusSelector || focusSelector;
      render();
    });

    if (sortState) {
      window.TrackerApp.bindSortControls(container, sortState, defaultSortState, (nextSortState, nextFocusSelector) => {
        sortState = nextSortState;
        currentPage = 1;
        shouldFocusAfterRender = true;
        pendingFocusSelector = nextFocusSelector || focusSelector;
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
