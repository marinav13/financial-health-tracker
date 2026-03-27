async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Failed to load ${path}`);
  return response.json();
}

function schoolUrl(unitid) {
  return `school.html?unitid=${encodeURIComponent(unitid)}`;
}

function renderResults(items, container) {
  container.innerHTML = "";
  items.forEach((item) => {
    const link = document.createElement("a");
    link.className = "result-item";
    link.href = schoolUrl(item.unitid);
    link.innerHTML = `<strong>${item.institution_name}</strong><br>${item.city}, ${item.state} | ${item.control_label}`;
    container.appendChild(link);
  });
}

async function initSearch() {
  const schools = await loadJson("data/schools_index.json");
  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");
  if (!input || !results) return;

  input.addEventListener("input", () => {
    const q = input.value.trim().toLowerCase();
    if (!q) {
      results.innerHTML = "";
      return;
    }
    const matched = schools
      .filter((row) =>
        row.institution_unique_name.toLowerCase().includes(q) ||
        row.institution_name.toLowerCase().includes(q)
      )
      .slice(0, 10);
    renderResults(matched, results);
  });
}

initSearch().catch((error) => {
  console.error(error);
  const results = document.getElementById("search-results");
  if (results) results.textContent = "The search data could not be loaded.";
});
