async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Failed to load ${path}`);
  return response.json();
}

function getSearchTargetPage() {
  return document.body.dataset.searchPage || "school.html";
}

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

function schoolUrl(unitid, page = getSearchTargetPage()) {
  return `${page}?unitid=${encodeURIComponent(unitid)}`;
}

function tokenizeSearch(value) {
  return String(value || "")
    .toLowerCase()
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

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
  const page = getSearchTargetPage();
  const sourceKind = getSearchSourceKind();

  // Roving tabindex state
  let activeIndex = -1;

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

  function setActiveButton(newIndex) {
    const buttons = getAllResultButtons();
    if (!buttons.length) return;
    // Clamp to valid range
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
        <span>${getMatchText(row)}</span>
        ${getResultBadge(row) ? `<small class="small-meta">${getResultBadge(row)}</small>` : ""}
      </button>
    `).join("");

    results.querySelectorAll("[data-unitid]").forEach((button, i) => {
      button.addEventListener("click", () => {
        window.location.href = schoolUrl(button.dataset.unitid, page);
      });
      // Arrow key navigation on each result button
      button.addEventListener("keydown", (e) => {
        if (e.key === "ArrowDown") {
          e.preventDefault();
          setActiveButton(i + 1);
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          setActiveButton(i - 1);
        } else if (e.key === "Enter" || e.key === " ") {
          window.location.href = schoolUrl(button.dataset.unitid, page);
        } else if (e.key === "Escape") {
          clearResults();
          input.focus();
        }
      });
    });
  }

  function navigateFromValue(value) {
    const q = value.trim().toLowerCase();
    if (!q) return false;
    const match = schools.find((row) =>
      (row.institution_unique_name || "").toLowerCase() === q ||
      (row.institution_name || "").toLowerCase() === q
    );
    if (match) {
      window.location.href = schoolUrl(match.unitid, page);
      return true;
    }
    return false;
  }

  results.setAttribute("role", "listbox");

  input.addEventListener("input", () => {
    const q = input.value.trim().toLowerCase();
    if (!q) {
      clearResults();
      return;
    }
    if (navigateFromValue(input.value)) return;
    renderMatches(input.value);
  });

  input.addEventListener("change", () => {
    if (navigateFromValue(input.value)) return;
    renderMatches(input.value);
  });

  input.addEventListener("keydown", (event) => {
    const buttons = getAllResultButtons();

    if (event.key === "Enter") {
      if (navigateFromValue(input.value)) {
        event.preventDefault();
        return;
      }
      if (buttons.length) {
        event.preventDefault();
        // If a result is already focused (activeIndex >= 0), navigate there;
        // otherwise focus and navigate to the first result.
        if (activeIndex >= 0 && document.activeElement && document.activeElement.dataset.unitid) {
          navigateToActive();
        } else {
          setActiveButton(0);
        }
      }
    } else if (event.key === "ArrowDown") {
      if (!buttons.length) return;
      event.preventDefault();
      setActiveButton(activeIndex < 0 ? 0 : activeIndex + 1);
    } else if (event.key === "ArrowUp") {
      if (!buttons.length) return;
      event.preventDefault();
      if (activeIndex <= 0) {
        clearResults();
        input.focus();
      } else {
        setActiveButton(activeIndex - 1);
      }
    } else if (event.key === "Escape") {
      clearResults();
      input.focus();
    }
  });
}

window.TrackerApp = {
  loadJson,
  schoolUrl
};

initSearch().catch((error) => {
  console.error(error);
  const results = document.getElementById("search-results");
  if (results) results.textContent = "The search data could not be loaded.";
});
