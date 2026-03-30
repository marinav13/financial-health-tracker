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
  return "data/schools_index.json";
}

function getSearchSourceKind() {
  return document.body.dataset.searchSource || "all";
}

function schoolUrl(unitid, page = getSearchTargetPage()) {
  return `${page}?unitid=${encodeURIComponent(unitid)}`;
}

async function initSearch() {
  const raw = await loadJson(getSearchSourcePath());
  const schools = (Array.isArray(raw) ? raw : Object.values(raw || {})).slice().sort((a, b) =>
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

  function renderMatches(query) {
    const q = query.trim().toLowerCase();
    if (!q) {
      clearResults();
      return;
    }
    const matches = schools.filter((row) => {
      const uniqueName = String(row.institution_unique_name || "").toLowerCase();
      const institutionName = String(row.institution_name || "").toLowerCase();
      return uniqueName.includes(q) || institutionName.includes(q);
    }).slice(0, 8);

    if (!matches.length) {
      results.innerHTML = `<div class="result-item is-empty">No matching institutions found.</div>`;
      return;
    }

    results.innerHTML = matches.map((row) => `
      <button type="button" class="result-item" data-unitid="${row.unitid}">
        <span>${getMatchText(row)}</span>
        ${getResultBadge(row) ? `<small class="small-meta">${getResultBadge(row)}</small>` : ""}
      </button>
    `).join("");

    results.querySelectorAll("[data-unitid]").forEach((button) => {
      button.addEventListener("click", () => {
        window.location.href = schoolUrl(button.dataset.unitid, page);
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
    if (event.key === "Enter") {
      if (navigateFromValue(input.value)) {
        event.preventDefault();
        return;
      }
      const firstMatch = results.querySelector("[data-unitid]");
      if (firstMatch) {
        event.preventDefault();
        window.location.href = schoolUrl(firstMatch.dataset.unitid, page);
      }
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
