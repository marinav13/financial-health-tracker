(function () {
  const {
    loadJson,
    schoolUrl,
    escapeHtml,
    renderExternalLink,
    renderSchoolLink,
    renderPaginationButtons,
    renderSortableHeader,
    paginateItems,
    focusAfterRender,
    bindSortControls,
    setupPaginatedTable,
    filterByInstitution,
    setDataCardVisible,
    downloadRowsCsv
  } = window.TrackerApp;
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
    return `<div class="empty-state"><p>${escapeHtml(message)}</p></div>`;
  }

  function setSectionVisible(id, show) {
    setDataCardVisible(id, show);
  }

  function compareText(a, b) {
    return String(a || "").localeCompare(String(b || ""), undefined, { sensitivity: "base" });
  }

  function compareDateDesc(a, b) {
    return String(b || "").localeCompare(String(a || ""));
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
      ? `<li>${renderSchoolLink(financialUnitid, "Finances", "school.html")}</li>`
      : "";
    const cutsLink = (!String(unitid).startsWith("research-") && unitid)
      ? `<li>${renderSchoolLink(unitid, "College Cuts", "cuts.html")}</li>`
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
            <div class="metric-question">${escapeHtml(item.agency_label)}</div>
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
        <td>${escapeHtml(agencyLabel(grant.agency))}</td>
        <td>${escapeHtml(grant.project_title)}</td>
        <td>${escapeHtml(grant.grant_id)}</td>
        <td>${formatCurrency(grant.award_remaining)}</td>
        <td>${formatDate(grant.termination_date)}</td>
        <td>${renderExternalLink(grant.source_url, "Source")}</td>
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
              ${renderSortableHeader("termination_date", sortState, "Termination date")}
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
        <td>${escapeHtml(item.state || "")}</td>
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
              ${renderSortableHeader("state", sortState, "State")}
              ${renderSortableHeader("public_funding", sortState, "Public cuts")}
              ${renderSortableHeader("private_funding", sortState, "Private cuts")}
              ${renderSortableHeader("total_funding", sortState, "Total cuts")}
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
      focusAfterRender(container, ".history-table");
      bindSortControls(container, sortState, { key: "public_funding", direction: "desc" }, (nextSortState) => {
        sortState = nextSortState;
        render();
      });
    };
    render();
  }

  function renderDefaultTable(items, sortState) {
    if (!items || !items.length) return renderEmpty("No research funding cuts are available.");
    const rows = items.map((item) => `
      <tr>
        <td>${renderSchoolLink(item.unitid, item.institution_name, "research.html")}</td>
        <td>${escapeHtml(item.state || "")}</td>
        <td>${escapeHtml(item.control_label || "")}</td>
        <td>${Number(item.total_disrupted_grants || 0)}</td>
        <td>${formatCurrency(item.total_disrupted_award_remaining)}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              ${renderSortableHeader("institution_name", sortState, "Institution")}
              ${renderSortableHeader("state", sortState, "State")}
              ${renderSortableHeader("sector", sortState, "Sector")}
              ${renderSortableHeader("disrupted_grants", sortState, "Disrupted grants")}
              ${renderSortableHeader("funding", sortState, "Funding cut or frozen")}
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  function renderTablePage(items, page, pageSize, emptyMessage, ariaLabel, sortState) {
    const { totalPages, currentPage, pageItems } = paginateItems(items, page, pageSize);
    if (!pageItems.length) return renderEmpty(emptyMessage);

    return `
      ${renderDefaultTable(pageItems, sortState)}
      <div class="pagination" aria-label="${ariaLabel}">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function setupPagination(container, items, pageSize, emptyMessage, downloadButtonId, downloadFilename, ariaLabel, searchInput = null) {
    if (!container) return;
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;
    setupPaginatedTable({
      container,
      items,
      pageSize,
      searchInput,
      initialSortState: { key: "funding", direction: "desc" },
      defaultSortState: { key: "funding", direction: "desc" },
      filterItems: filterByInstitution,
      sortItems: sortInstitutionRows,
      renderPage: (sortedItems, currentPage, size, sortState) => renderTablePage(sortedItems, currentPage, size, emptyMessage, ariaLabel, sortState),
      downloadButton,
      downloadRows: (pageItems) => downloadRowsCsv(
          downloadFilename,
          ["Institution", "State", "Sector", "Disrupted grants", "Funding still disrupted"],
        pageItems.map((item) => [
            item.institution_name || "",
            item.state || "",
            item.control_label || "",
            Number(item.total_disrupted_grants || 0),
            item.total_disrupted_award_remaining || ""
          ])
      )
    });
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
    const searchInput = document.getElementById("research-filter");

    if (!unitid) {
      document.getElementById("research-school-name").textContent = "Research funding cuts";
      document.getElementById("research-school-name").classList.add("is-hidden");
      const ranked = sortByAmountThenName(schools);
      const renderLanding = () => {
        const filtered = sortByAmountThenName(ranked);
        if (summaryGrid) summaryGrid.innerHTML = renderLandingSummaryGrid(filtered);
        if (title) {
          title.textContent = "Research funding cuts by institution";
          title.classList.add("is-hidden");
        }
        setupPagination(
          container,
          filtered,
          PAGE_SIZE,
          "No currently disrupted research grants are available.",
          "research-table-download",
          "research-funding.csv",
          "Research funding pages",
          searchInput
        );
      };
      setSectionVisible("research-other-list", false);
      if (otherContainer) otherContainer.innerHTML = "";
      if (otherTitle) otherTitle.textContent = "Research funding cuts at other higher-ed institutions";
      if (stateSummaryCard) stateSummaryCard.classList.remove("is-hidden");
      setupStateSummary(stateSummaryContainer, ranked);
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
      bindSortControls(container, grantSortState, { key: "termination_date", direction: "desc" }, (nextSortState) => {
        grantSortState = nextSortState;
        renderDetailTable();
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
