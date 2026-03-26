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
    link.innerHTML = `<strong>${item.institution_name}</strong><br>${item.city}, ${item.state} · ${item.control_label}`;
    container.appendChild(link);
  });
}

function renderRanking(list, container, formatter) {
  container.innerHTML = "";
  list.forEach((item) => {
    const li = document.createElement("li");
    const link = document.createElement("a");
    link.className = "rank-link";
    link.href = schoolUrl(item.unitid);
    link.textContent = `${item.institution_name} (${formatter(item.value)})`;
    li.appendChild(link);
    container.appendChild(li);
  });
}

async function init() {
  const [schools, rankings] = await Promise.all([
    loadJson("data/schools_index.json"),
    loadJson("data/rankings.json")
  ]);

  const input = document.getElementById("school-search");
  const results = document.getElementById("search-results");

  input.addEventListener("input", () => {
    const q = input.value.trim().toLowerCase();
    if (!q) {
      results.innerHTML = "";
      return;
    }
    const matched = schools
      .filter((row) => row.institution_unique_name.toLowerCase().includes(q) || row.institution_name.toLowerCase().includes(q))
      .slice(0, 12);
    renderResults(matched, results);
  });

  renderRanking(rankings.lists.enrollment_decline_5yr, document.getElementById("rank-enrollment"), (v) => `${v}%`);
  renderRanking(rankings.lists.staff_cuts_5yr, document.getElementById("rank-staff"), (v) => `${v}%`);
  renderRanking(rankings.lists.federal_dependence, document.getElementById("rank-federal"), (v) => `${v}%`);
  renderRanking(rankings.lists.private_closure_risk, document.getElementById("rank-private-risk"), (v) => `${v} loss years`);
}

init().catch((error) => {
  console.error(error);
  document.getElementById("search-results").textContent = "The search data could not be loaded.";
});
