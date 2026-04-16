(function () {
  const { loadJson, schoolUrl } = window.TrackerApp;
  const PAGE_SIZE = 20;
  const OTHER_PAGE_SIZE = 8;

  function getParam(name) {
    return new URLSearchParams(window.location.search).get(name);
  }

  function setText(id, value) {
    const el = document.getElementById(id);
    if (el) el.textContent = value || "";
  }

  function syncTabs(unitid, financialUnitid) {
    const finances = document.getElementById("tab-finances");
    if (finances) {
      finances.href = financialUnitid
        ? `school.html?unitid=${encodeURIComponent(financialUnitid)}`
        : "index.html";
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

  function renderEmpty(message) {
    return `<div class="empty-state"><p>${message}</p></div>`;
  }

  function setSectionVisible(id, show) {
    const node = document.getElementById(id);
    const section = node ? node.closest(".data-card") : null;
    if (section) section.classList.toggle("is-hidden", !show);
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

  function compareText(a, b) {
    return String(a || "").localeCompare(String(b || ""), undefined, { sensitivity: "base" });
  }

  function compareDateDesc(a, b) {
    return String(b || "").localeCompare(String(a || ""));
  }

  function researchPageLink(unitid, label) {
    return unitid
      ? `<a href="${schoolUrl(unitid, "research.html")}">${label || ""}</a>`
      : (label || "");
  }

  function normalizeQuery(value) {
    return String(value || "").trim().toLowerCase();
  }

  function filterByInstitution(items, query) {
    const normalized = normalizeQuery(query);
    if (!normalized) return items || [];
    return (items || []).filter((item) => String(item.institution_name || "").toLowerCase().includes(normalized));
  }

  function hasPositiveFunding(value) {
    const amount = Number(value);
    return Number.isFinite(amount) && amount > 0;
  }

  function filterPositiveFundingInstitutions(items) {
    return (items || []).filter((item) => hasPositiveFunding(item.total_disrupted_award_remaining));
  }

  function filterPositiveFundingGrants(grants) {
    return (grants || []).filter((grant) => hasPositiveFunding(grant.award_remaining));
  }

  function renderSortControls(key, sortState, label) {
    const activeKey = sortState?.key || "";
    const activeDirection = activeKey === key ? sortState.direction : "";
    const upClass = activeDirection === "asc" ? " is-active" : "";
    const downClass = activeDirection === "desc" ? " is-active" : "";
    return `
      <span class="sort-header-label">${label}</span>
      <span class="sort-controls" aria-label="Sort ${label}">
        <button type="button" class="sort-button${upClass}" data-sort-key="${key}" data-sort-direction="asc" aria-label="Sort ${label} ascending">▲</button>
        <button type="button" class="sort-button${downClass}" data-sort-key="${key}" data-sort-direction="desc" aria-label="Sort ${label} descending">▼</button>
      </span>
    `;
  }

  function formatCurrency(value) {
    const amount = Number(value);
    if (!Number.isFinite(amount)) return "No data";
    return new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    }).format(amount);
  }

  function formatDate(value) {
    return value || "";
  }

  function agencyLabel(value) {
    const labels = { nih: "NIH", nsf: "NSF", epa: "EPA", samhsa: "SAMHSA", cdc: "CDC" };
    return labels[value] || String(value || "").toUpperCase();
  }

  function isPrimaryBachelorsInstitution(record) {
    const category = String(record?.category || "");
    return /primarily baccalaureate or above/i.test(category) && !/not primarily baccalaureate or above/i.test(category);
  }

  function sortByAmountThenName(items) {
    return (items || []).slice().sort((a, b) => {
      const amountDiff = Number(b.total_disrupted_award_remaining || 0) - Number(a.total_disrupted_award_remaining || 0);
      if (amountDiff !== 0) return amountDiff;
      return String(a.institution_name || "").localeCompare(String(b.institution_name || ""));
    });
  }

  function renderInstitutionLinks(unitid, financialUnitid) {
    const financeLink = financialUnitid
      ? `<li><a href="${schoolUrl(financialUnitid, "school.html")}">Finances</a></li>`
      : "";
    const cutsLink = (!String(unitid).startsWith("research-") && unitid)
      ? `<li><a href="${schoolUrl(unitid, "cuts.html")}">College Cuts</a></li>`
      : "";
    const list = [financeLink, cutsLink].filter(Boolean).join("");
    if (!list) return "";
    return `
      <div class="related-links">
        <p><strong>Explore this institution:</strong></p>
        <ul class="link-list">${list}</ul>
      </div>
    `;
  }

  function renderSummaryGrid(school) {
    if (!school) return "";
    const visibleGrants = filterPositiveFundingGrants(school.grants || []);
    const visibleAgencySummary = (school.agency_summary || [])
      .filter((item) => Number(item.disrupted_grants || 0) > 0 && hasPositiveFunding(item.disrupted_award_remaining));
    const totalFunding = visibleAgencySummary.length
      ? visibleAgencySummary.reduce((sum, item) => sum + Number(item.disrupted_award_remaining || 0), 0)
      : (visibleGrants.length
        ? visibleGrants.reduce((sum, item) => sum + Number(item.award_remaining || 0), 0)
        : Number(school.total_disrupted_award_remaining || 0));
    const total = `
      <article class="metric-strip neutral">
        <div class="metric-question">Current disrupted funding</div>
        <div class="metric-answer">${formatCurrency(totalFunding)}</div>
      </article>
    `;
    const grants = `
      <article class="metric-strip neutral">
        <div class="metric-question">Currently disrupted grants</div>
        <div class="metric-answer">${visibleGrants.length}</div>
      </article>
    `;
    const agencies = visibleAgencySummary
      .map((item) => `
          <article class="metric-strip neutral">
            <div class="metric-question">${item.agency_label}</div>
          <div class="metric-statement">${Number(item.disrupted_grants || 0)} grants<br>${formatCurrency(item.disrupted_award_remaining)}</div>
        </article>
      `)
      .join("");
    return total + grants + agencies;
  }

  function sumValues(items, field) {
    return (items || []).reduce((sum, item) => sum + Number(item?.[field] || 0), 0);
  }

  function renderLandingSummaryGrid(items) {
    const funding = sumValues(items, "total_disrupted_award_remaining");
    const institutions = (items || []).length;
    return `
      <article class="metric-strip neutral">
        <div class="metric-statement">Since January 2025, the federal government has cut at least <strong>${formatCurrency(funding)}</strong> in research grants that were supposed to go to at least <strong>${institutions.toLocaleString("en-US")}</strong> colleges and universities nationwide.</div>
      </article>
    `;
  }

  function normalizeSector(value) {
    const text = String(value || "").toLowerCase();
    if (text.includes("public")) return "public";
    if (text.includes("private")) return "private";
    return "other";
  }

  function filterBySector(items, sector) {
    if (sector === "both") return items;
    return (items || []).filter((item) => normalizeSector(item.control_label) === sector);
  }

  function sortGrants(grants, sortState) {
    const sorted = (grants || []).slice();
    const direction = sortState?.direction === "desc" ? -1 : 1;
    sorted.sort((a, b) => {
      if (sortState?.key === "termination_date") {
        const primary = compareDateDesc(a.termination_date, b.termination_date) * direction;
        if (primary !== 0) return primary;
        return compareText(a.project_title, b.project_title);
      }
      return compareDateDesc(a.termination_date, b.termination_date);
    });
    return sorted;
  }

  function renderGrantTable(grants, sortState = { key: "termination_date", direction: "desc" }) {
    const visibleGrants = filterPositiveFundingGrants(grants);
    if (!visibleGrants.length) return renderEmpty("No currently disrupted grants are available.");
    const rows = sortGrants(visibleGrants, sortState).map((grant) => `
      <tr>
        <td>${agencyLabel(grant.agency)}</td>
        <td>${grant.project_title || ""}</td>
        <td>${grant.grant_id || ""}</td>
        <td>${formatCurrency(grant.award_remaining)}</td>
        <td>${formatDate(grant.termination_date)}</td>
        <td>${grant.source_url ? `<a href="${grant.source_url}" target="_blank" rel="noopener">Source</a>` : ""}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>Agency</th>
              <th>Grant</th>
              <th>Grant ID</th>
              <th>Funding still disrupted</th>
              <th>${renderSortControls("termination_date", sortState, "Termination date")}</th>
              <th>Source</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  function sortInstitutionRows(items, sortState) {
    const sorted = (items || []).slice();
    const direction = sortState?.direction === "desc" ? -1 : 1;
    sorted.sort((a, b) => {
      if (sortState?.key === "institution_name") {
        const primary = compareText(a.institution_name, b.institution_name) * direction;
        if (primary !== 0) return primary;
        return compareText(a.state, b.state);
      }
      if (sortState?.key === "state") {
        const primary = compareText(a.state, b.state) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "sector") {
        const primary = compareText(a.control_label, b.control_label) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "disrupted_grants") {
        const primary = (Number(a.total_disrupted_grants || 0) - Number(b.total_disrupted_grants || 0)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "funding") {
        const primary = (Number(a.total_disrupted_award_remaining || 0) - Number(b.total_disrupted_award_remaining || 0)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      const amountDiff = Number(b.total_disrupted_award_remaining || 0) - Number(a.total_disrupted_award_remaining || 0);
      if (amountDiff !== 0) return amountDiff;
      return compareText(a.institution_name, b.institution_name);
    });
    return sorted;
  }

  function buildStateSummaryRows(items) {
    const summary = new Map();
    (items || []).forEach((item) => {
      const state = item.state || "Unknown";
      if (!summary.has(state)) {
        summary.set(state, {
          state,
          publicFunding: 0,
          privateFunding: 0,
          totalFunding: 0
        });
      }
      const row = summary.get(state);
      const amount = Number(item.total_disrupted_award_remaining || 0);
      row.totalFunding += amount;
      const sector = normalizeSector(item.control_label);
      if (sector === "public") {
        row.publicFunding += amount;
      } else if (sector === "private") {
        row.privateFunding += amount;
      }
    });
    return Array.from(summary.values());
  }

  function sortStateSummaryRows(items, sortState) {
    const sorted = (items || []).slice();
    const direction = sortState?.direction === "desc" ? -1 : 1;
    sorted.sort((a, b) => {
      if (sortState?.key === "state") {
        return compareText(a.state, b.state) * direction;
      }
      if (sortState?.key === "public_funding") {
        const primary = (Number(a.publicFunding || 0) - Number(b.publicFunding || 0)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.state, b.state);
      }
      if (sortState?.key === "private_funding") {
        const primary = (Number(a.privateFunding || 0) - Number(b.privateFunding || 0)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.state, b.state);
      }
      if (sortState?.key === "total_funding") {
        const primary = (Number(a.totalFunding || 0) - Number(b.totalFunding || 0)) * direction;
        if (primary !== 0) return primary;
        return compareText(a.state, b.state);
      }
      const totalDiff = Number(b.totalFunding || 0) - Number(a.totalFunding || 0);
      if (totalDiff !== 0) return totalDiff;
      return compareText(a.state, b.state);
    });
    return sorted;
  }

  function renderStateSummaryTable(items, sortState = { key: "public_funding", direction: "desc" }) {
    if (!items || !items.length) return renderEmpty("No state summary is available.");
    const rows = sortStateSummaryRows(items, sortState).map((item) => `
      <tr>
        <td>${item.state || ""}</td>
        <td>${formatCurrency(item.publicFunding)}</td>
        <td>${formatCurrency(item.privateFunding)}</td>
        <td>${formatCurrency(item.totalFunding)}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>${renderSortControls("state", sortState, "State")}</th>
              <th>${renderSortControls("public_funding", sortState, "Public cuts")}</th>
              <th>${renderSortControls("private_funding", sortState, "Private cuts")}</th>
              <th>${renderSortControls("total_funding", sortState, "Total cuts")}</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  function setupStateSummary(container, items) {
    if (!container) return;
    let sortState = { key: "public_funding", direction: "desc" };
    const render = () => {
      const stateRows = buildStateSummaryRows(items);
      container.innerHTML = renderStateSummaryTable(stateRows, sortState);
      // Move focus to the table so screen readers announce the updated sort
      setTimeout(() => {
        const table = container.querySelector(".history-table");
        if (table) table.focus();
      }, 0);
      container.querySelectorAll(".sort-button").forEach((button) => {
        button.addEventListener("click", () => {
          const key = button.dataset.sortKey || "public_funding";
          const direction = button.dataset.sortDirection || "desc";
          if (sortState.key === key && sortState.direction === direction) return;
          sortState = { key, direction };
          render();
        });
      });
    };
    render();
  }

  function renderDefaultTable(items, sortState) {
    if (!items || !items.length) return renderEmpty("No research funding cuts are available.");
    const rows = items.map((item) => `
      <tr>
        <td>${researchPageLink(item.unitid, item.institution_name || "")}</td>
        <td>${item.state || ""}</td>
        <td>${item.control_label || ""}</td>
        <td>${Number(item.total_disrupted_grants || 0)}</td>
        <td>${formatCurrency(item.total_disrupted_award_remaining)}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>${renderSortControls("institution_name", sortState, "Institution")}</th>
              <th>${renderSortControls("state", sortState, "State")}</th>
              <th>${renderSortControls("sector", sortState, "Sector")}</th>
              <th>${renderSortControls("disrupted_grants", sortState, "Disrupted grants")}</th>
              <th>${renderSortControls("funding", sortState, "Funding cut or frozen")}</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  function renderTablePage(items, page, pageSize, emptyMessage, ariaLabel, sortState) {
    const totalPages = Math.max(1, Math.ceil(items.length / pageSize));
    const safePage = Math.min(Math.max(1, page), totalPages);
    const start = (safePage - 1) * pageSize;
    const pageItems = items.slice(start, start + pageSize);
    if (!pageItems.length) return renderEmpty(emptyMessage);

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
      ${renderDefaultTable(pageItems, sortState)}
      <div class="pagination" aria-label="${ariaLabel}">
        ${pagination}
      </div>
    `;
  }

  function setupPagination(container, items, pageSize, emptyMessage, downloadButtonId, downloadFilename, ariaLabel, searchInput = null) {
    if (!container) return;
    let currentPage = 1;
    let sortState = { key: "funding", direction: "desc" };
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      const filteredItems = filterByInstitution(items, searchInput?.value || "");
      const sortedItems = sortInstitutionRows(filteredItems, sortState);
      container.innerHTML = renderTablePage(sortedItems, currentPage, pageSize, emptyMessage, ariaLabel, sortState);
      // Move focus to the pagination region so screen readers announce the updated content
      setTimeout(() => {
        const pagination = container.querySelector(".pagination");
        if (pagination) pagination.focus();
      }, 0);
      const totalPages = Math.max(1, Math.ceil(sortedItems.length / pageSize));
      const safePage = Math.min(Math.max(1, currentPage), totalPages);
      const start = (safePage - 1) * pageSize;
      const pageItems = sortedItems.slice(start, start + pageSize);
      if (downloadButton) {
        downloadButton.classList.toggle("is-hidden", pageItems.length === 0);
        downloadButton.onclick = () => downloadRowsCsv(
          downloadFilename,
          ["Institution", "State", "Sector", "Disrupted grants", "Funding still disrupted"],
          pageItems.map((item) => [
            item.institution_name || "",
            item.state || "",
            item.control_label || "",
            Number(item.total_disrupted_grants || 0),
            item.total_disrupted_award_remaining || ""
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
      container.querySelectorAll(".sort-button").forEach((button) => {
        button.addEventListener("click", () => {
          const key = button.dataset.sortKey || "funding";
          const direction = button.dataset.sortDirection || "desc";
          if (sortState.key === key && sortState.direction === direction) return;
          sortState = { key, direction };
          currentPage = 1;
          render();
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

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid);

      const data = await loadJson("data/research_funding.json");
      const schools = filterPositiveFundingInstitutions(Object.values(data.schools || {}));
    const container = document.getElementById("research-list");
    const otherContainer = document.getElementById("research-other-list");
    const stateSummaryContainer = document.getElementById("research-state-summary");
    const stateSummaryCard = document.getElementById("research-state-summary-card");
    const summaryGrid = document.getElementById("research-summary-grid");
    const title = document.getElementById("research-section-title");
    const otherTitle = document.getElementById("research-other-section-title");
    const filterWrap = document.getElementById("research-sector-filter");
    const searchInput = document.getElementById("research-filter");

    if (!unitid) {
      document.getElementById("research-school-name").textContent = "";
      document.getElementById("research-school-name").classList.add("is-hidden");
      const ranked = sortByAmountThenName(schools);
      const renderLanding = () => {
        const filtered = sortByAmountThenName(ranked);
        if (summaryGrid) summaryGrid.innerHTML = renderLandingSummaryGrid(filtered);
        if (title) {
          title.textContent = "";
          title.classList.add("is-hidden");
        }
        setupPagination(
          container,
          filtered,
          PAGE_SIZE,
          "No currently disrupted research grants are available for this sector filter.",
          "research-table-download",
          "research-funding.csv",
          "Research funding pages",
          searchInput
        );
      };
      setSectionVisible("research-other-list", false);
      if (otherContainer) otherContainer.innerHTML = "";
      if (otherTitle) otherTitle.textContent = "";
      if (stateSummaryCard) stateSummaryCard.classList.remove("is-hidden");
      setupStateSummary(stateSummaryContainer, ranked);
      if (filterWrap) filterWrap.classList.add("is-hidden");
      renderLanding();
      return;
    }

      const school = schools.find((item) => String(item.unitid) === String(unitid));
    if (!school) {
      document.getElementById("research-school-name").textContent = "No tracked research funding cuts found";
      document.getElementById("research-school-name").classList.remove("is-hidden");
      if (summaryGrid) summaryGrid.innerHTML = renderEmpty("No current research funding cuts were found for this institution.");
      if (container) container.innerHTML = renderEmpty("No current research funding cuts were found for this institution.");
      if (stateSummaryCard) stateSummaryCard.classList.add("is-hidden");
      return;
    }

    syncTabs(unitid, school.financial_unitid);

    document.getElementById("research-school-name").textContent = school.institution_name || "Research Funding Cuts";
    document.getElementById("research-school-name").classList.remove("is-hidden");
    setText("research-school-location", [school.city, school.state].filter(Boolean).join(", "));
    setText("research-school-control", school.control_label || "");
    setText("research-school-category", school.category || "");
    if (summaryGrid) summaryGrid.innerHTML = renderSummaryGrid(school);

    const overview = document.getElementById("research-overview");
    if (overview) {
      overview.classList.remove("is-hidden");
      overview.innerHTML = "";
    }

    title.textContent = `Currently disrupted grants (${filterPositiveFundingGrants(school.grants || []).length})`;
    if (title) title.classList.remove("is-hidden");
    if (filterWrap) filterWrap.classList.add("is-hidden");
    if (stateSummaryCard) stateSummaryCard.classList.add("is-hidden");
    setSectionVisible("research-other-list", false);
    const mainDownload = document.getElementById("research-table-download");
    const otherDownload = document.getElementById("research-other-download");
      if (mainDownload) {
        mainDownload.classList.remove("is-hidden");
        mainDownload.onclick = () => downloadRowsCsv(
          `${String(school.institution_name || "research-funding").toLowerCase().replace(/[^a-z0-9]+/g, "-")}-research-funding.csv`,
          ["Agency", "Grant", "Grant ID", "Funding still disrupted", "Termination date", "Source"],
          filterPositiveFundingGrants(school.grants || []).map((grant) => [
            agencyLabel(grant.agency),
            grant.project_title || "",
            grant.grant_id || "",
          grant.award_remaining || "",
          grant.termination_date || "",
          grant.source_url || ""
        ])
      );
    }
    if (otherDownload) otherDownload.classList.add("is-hidden");
    if (otherContainer) otherContainer.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";

    let grantSortState = { key: "termination_date", direction: "desc" };
    const renderDetailTable = () => {
      container.innerHTML = renderGrantTable(school.grants || [], grantSortState) + renderInstitutionLinks(school.unitid, school.financial_unitid);
      container.querySelectorAll(".sort-button").forEach((button) => {
        button.addEventListener("click", () => {
          const key = button.dataset.sortKey || "termination_date";
          const direction = button.dataset.sortDirection || "desc";
          if (grantSortState.key === key && grantSortState.direction === direction) return;
          grantSortState = { key, direction };
          renderDetailTable();
        });
      });
    };
    renderDetailTable();
  }

  init().catch((error) => {
    console.error(error);
    const container = document.getElementById("research-list");
    if (container) container.innerHTML = renderEmpty("The research funding cuts data could not be loaded.");
  });
})();
