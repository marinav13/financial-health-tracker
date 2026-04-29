(function () {
  const {
    loadJson,
    escapeHtml,
    getParam,
    renderEmpty,
    renderPaginationButtons,
    renderSortableHeader,
    paginateItems,
    focusAfterRender,
    bindSortControls,
    setDataCardVisible,
    downloadRowsCsv,
    compareText,
    compareDateDesc,
    renderHistoryTable,
    renderSchoolLinkCell,
    renderExternalLinkCell,
    findRelatedIndexRecord,
    isNumericUnitid,
    isPrimaryTrackerInstitution,
    syncTabs,
    renderRelatedInstitutionLinks,
    renderDataAsOf,
    makeTableController
  } = window.TrackerApp;
  const PAGE_SIZE = 20;
  const OTHER_PAGE_SIZE = 8;
  const MIN_PUBLIC_AWARD_REMAINING = 100;

  function hasPositiveFunding(value) {
    const amount = Number(value);
    return Number.isFinite(amount) && amount >= MIN_PUBLIC_AWARD_REMAINING;
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
    return isPrimaryTrackerInstitution(record);
  }

  function sortByAmountThenName(items) {
    return (items || []).slice().sort((a, b) => {
      const amountDiff = Number(b.total_disrupted_award_remaining || 0) - Number(a.total_disrupted_award_remaining || 0);
      if (amountDiff !== 0) return amountDiff;
      return String(a.institution_name || "").localeCompare(String(b.institution_name || ""));
    });
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
    const rows = sortGrants(visibleGrants, sortState).map((grant) => [
      agencyLabel(grant.agency),
      grant.project_title,
      grant.grant_id,
      formatCurrency(grant.award_remaining),
      formatDate(grant.termination_date),
      renderExternalLinkCell(grant.source_url, "Source")
    ]);
    return renderHistoryTable({
      ariaLabel: "Currently disrupted grants for this institution",
      headers: [
        "<th>Agency</th>",
        "<th>Grant</th>",
        "<th>Grant ID</th>",
        "<th>Funding still disrupted</th>",
        renderSortableHeader("termination_date", sortState, "Termination date"),
        "<th>Source</th>"
      ],
      rows
    });
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
    const rows = sortStateSummaryRows(items, sortState).map((item) => [
      item.state || "",
      formatCurrency(item.publicFunding),
      formatCurrency(item.privateFunding),
      formatCurrency(item.totalFunding)
    ]);
    return renderHistoryTable({
      ariaLabel: "Research funding cuts by state",
      headers: [
        renderSortableHeader("state", sortState, "State"),
        renderSortableHeader("public_funding", sortState, "Public cuts"),
        renderSortableHeader("private_funding", sortState, "Private cuts"),
        renderSortableHeader("total_funding", sortState, "Total cuts")
      ],
      rows
    });
  }

  function setupStateSummary(container, items) {
    if (!container) return;
    let sortState = { key: "public_funding", direction: "desc" };
    let shouldFocusAfterRender = false;
    const render = () => {
      const stateRows = buildStateSummaryRows(items);
      container.innerHTML = renderStateSummaryTable(stateRows, sortState);
      if (shouldFocusAfterRender) {
        focusAfterRender(container, ".history-table");
        shouldFocusAfterRender = false;
      }
      bindSortControls(container, sortState, { key: "public_funding", direction: "desc" }, (nextSortState) => {
        sortState = nextSortState;
        shouldFocusAfterRender = true;
        render();
      });
    };
    render();
  }

  function renderDefaultTable(items, sortState, ariaLabel = "Research funding cuts by institution") {
    if (!items || !items.length) return renderEmpty("No research funding cuts are available.");
    const rows = items.map((item) => [
      renderSchoolLinkCell(item.unitid, item.institution_name, "research.html"),
      formatCurrency(item.total_disrupted_award_remaining),
      Number(item.total_disrupted_grants || 0),
      item.state || "",
      item.control_label || ""
    ]);
    return renderHistoryTable({
      ariaLabel,
      headers: [
        renderSortableHeader("institution_name", sortState, "Institution"),
        renderSortableHeader("funding", sortState, "Funding cut or frozen"),
        renderSortableHeader("disrupted_grants", sortState, "Disrupted grants"),
        renderSortableHeader("state", sortState, "State"),
        renderSortableHeader("sector", sortState, "Sector")
      ],
      rows
    });
  }

  function renderOtherInstitutionsTable(items, sortState, ariaLabel = "Research funding cuts at other institutions") {
    if (!items || !items.length) return renderEmpty("No research funding cuts are available.");
    const rows = items.map((item) => [
      renderSchoolLinkCell(item.unitid, item.institution_name, "research.html"),
      formatCurrency(item.total_disrupted_award_remaining),
      Number(item.total_disrupted_grants || 0),
      item.state || ""
    ]);
    return renderHistoryTable({
      ariaLabel,
      headers: [
        renderSortableHeader("institution_name", sortState, "Institution"),
        renderSortableHeader("funding", sortState, "Funding cut or frozen"),
        renderSortableHeader("disrupted_grants", sortState, "Disrupted grants"),
        renderSortableHeader("state", sortState, "State")
      ],
      rows
    });
  }

  function renderTablePage(items, page, pageSize, emptyMessage, paginationLabel, sortState, tableLabel = "Research funding cuts by institution") {
    const { totalPages, currentPage, pageItems } = paginateItems(items, page, pageSize);
    if (!pageItems.length) return renderEmpty(emptyMessage);

    return `
      ${renderDefaultTable(pageItems, sortState, tableLabel)}
      <div class="pagination" aria-label="${paginationLabel}">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function renderOtherTablePage(items, page, pageSize, emptyMessage, paginationLabel, sortState, tableLabel = "Research funding cuts at other institutions") {
    const { totalPages, currentPage, pageItems } = paginateItems(items, page, pageSize);
    if (!pageItems.length) return renderEmpty(emptyMessage);

    return `
      ${renderOtherInstitutionsTable(pageItems, sortState, tableLabel)}
      <div class="pagination" aria-label="${paginationLabel}">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  // Options bag (replaces 9-positional-arg signature). All keys optional except container + items.
  // Shared keys with accreditation.js / cuts.js so future shared extraction is trivial.
  // paginationLabel and tableLabel are research-specific; the others are common.
  function setupPagination({
    container,
    items,
    pageSize,
    emptyMessage,
    downloadButtonId,
    downloadFilename,
    paginationLabel,
    searchInput = null,
    tableLabel = "Research funding cuts by institution"
  }) {
    return makeTableController({
      container,
      items,
      pageSize,
      searchInput,
      initialSortState: { key: "funding", direction: "desc" },
      sortItems: sortInstitutionRows,
      renderPage: (sortedItems, currentPage, size, sortState) => renderTablePage(sortedItems, currentPage, size, emptyMessage, paginationLabel, sortState, tableLabel),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "Funding cut or frozen", "Disrupted grants", "State", "Sector"],
      downloadRow: (item) => [
        item.institution_name || "",
        item.total_disrupted_award_remaining || "",
        Number(item.total_disrupted_grants || 0),
        item.state || "",
        item.control_label || ""
      ]
    });
  }

  function resetLandingScrollPosition() {
    if (window.location.hash) return;
    // history.scrollRestoration is set to "manual" before data loads,
    // so the browser won't restore a saved position on reload or
    // back-nav. Deferring the explicit scroll to rAF ensures it runs
    // after the freshly rendered table has been laid out, which is the
    // only moment where scroll could otherwise drift from 0.
    window.requestAnimationFrame(() => {
      window.scrollTo({ top: 0, left: 0, behavior: "auto" });
    });
  }

  function setupOtherPagination({
    container,
    items,
    pageSize,
    emptyMessage,
    downloadButtonId,
    downloadFilename,
    paginationLabel,
    searchInput = null,
    tableLabel = "Research funding cuts at other institutions"
  }) {
    return makeTableController({
      container,
      items,
      pageSize,
      searchInput,
      initialSortState: { key: "funding", direction: "desc" },
      sortItems: sortInstitutionRows,
      renderPage: (sortedItems, currentPage, size, sortState) => renderOtherTablePage(sortedItems, currentPage, size, emptyMessage, paginationLabel, sortState, tableLabel),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "Funding cut or frozen", "Disrupted grants", "State"],
      downloadRow: (item) => [
        item.institution_name || "",
        item.total_disrupted_award_remaining || "",
        Number(item.total_disrupted_grants || 0),
        item.state || ""
      ]
    });
  }

  async function init() {
    const unitid = getParam("unitid");
    if (!unitid && "scrollRestoration" in window.history) {
      window.history.scrollRestoration = "manual";
    }
    syncTabs(unitid, { active: "research" });

    const [researchIndex, metadata] = await Promise.all([
      loadJson("data/research_funding_index.json"),
      loadJson("data/metadata.json")
    ]);
    renderDataAsOf("research-data-as-of", metadata?.generated_at);
    const schools = filterPositiveFundingInstitutions(Object.values(researchIndex || {}));
    const container = document.getElementById("research-list");
    const otherContainer = document.getElementById("research-other-list");
    const stateSummaryContainer = document.getElementById("research-state-summary");
    const stateSummaryCard = document.getElementById("research-state-summary-card");
    const summaryGrid = document.getElementById("research-summary-grid");
    const title = document.getElementById("research-section-title");
    const otherTitle = document.getElementById("research-other-section-title");
    const searchInput = document.getElementById("research-filter");
    const otherSearchInput = document.getElementById("research-other-filter");
    const searchLabel = document.querySelector('label[for="research-filter"]');
    if (searchLabel) searchLabel.classList.toggle("is-hidden", !!unitid);
    if (searchInput) searchInput.classList.toggle("is-hidden", !!unitid);

    if (!unitid) {
      // Landing view: the h1#research-school-name ships with class="sr-only"
      // in research.html, so the heading is already correct for screen-reader
      // users without being visually present. The h2#research-section-title
      // ships with class="is-hidden" and stays hidden on the landing view.
      const ranked = sortByAmountThenName(schools);
      const primary = ranked.filter(isPrimaryBachelorsInstitution);
      const other = ranked.filter((school) => !isPrimaryBachelorsInstitution(school));
      const renderLanding = () => {
        if (summaryGrid) summaryGrid.innerHTML = renderLandingSummaryGrid(ranked);
        setupPagination({
          container,
          items: primary,
          pageSize: PAGE_SIZE,
          emptyMessage: "No currently disrupted research grants are available for 4-year, primarily bachelor's-degree-granting institutions.",
          downloadButtonId: "research-table-download",
          downloadFilename: "research-funding-primary.csv",
          paginationLabel: "Research funding pages",
          searchInput,
          tableLabel: "Research funding cuts at 4-year institutions"
        });
        setupOtherPagination({
          container: otherContainer,
          items: other,
          pageSize: OTHER_PAGE_SIZE,
          emptyMessage: "No currently disrupted research grants are available for other higher-ed institutions.",
          downloadButtonId: "research-other-download",
          downloadFilename: "research-funding-other.csv",
          paginationLabel: "Research funding pages for other higher-ed institutions",
          searchInput: otherSearchInput,
          tableLabel: "Research funding cuts at other higher-ed institutions"
        });
      };
      setDataCardVisible("research-other-list", other.length > 0);
      if (otherTitle) otherTitle.textContent = "Research funding cuts at other higher-ed institutions";
      if (stateSummaryCard) stateSummaryCard.classList.remove("is-hidden");
      setupStateSummary(stateSummaryContainer, ranked);
      renderLanding();
      resetLandingScrollPosition();
      return;
    }

    const [cutsIndex, accreditationIndex] = await Promise.all([
      loadJson("data/college_cuts_index.json"),
      loadJson("data/accreditation_index.json")
    ]);
    const relatedIndexes = {
      cuts: cutsIndex,
      accreditation: accreditationIndex,
      research: researchIndex
    };
    const indexedSchool = findRelatedIndexRecord(researchIndex, unitid, "total_disrupted_grants");
    let school = null;
    if (indexedSchool || !isNumericUnitid(unitid)) {
      const data = await loadJson("data/research_funding.json");
      renderDataAsOf("research-data-as-of", data?.generated_at);
      school = data.schools?.[indexedSchool?.unitid || unitid] || null;
    }

    if (!school) {
      const missingHeading = document.getElementById("research-school-name");
      missingHeading.textContent = "No tracked research funding cuts found";
      missingHeading.classList.remove("is-hidden");
      missingHeading.classList.remove("sr-only");
      if (summaryGrid) summaryGrid.innerHTML = renderEmpty("No current research funding cuts were found for this institution.");
      if (container) container.innerHTML = renderEmpty("No current research funding cuts were found for this institution.");
      if (stateSummaryCard) stateSummaryCard.classList.add("is-hidden");
      return;
    }

    syncTabs(unitid, { active: "research", financialUnitid: school.financial_unitid });

    const schoolHeading = document.getElementById("research-school-name");
    schoolHeading.textContent = school.institution_name || "Research Funding Cuts";
    schoolHeading.classList.remove("is-hidden");
    schoolHeading.classList.remove("sr-only");
    if (summaryGrid) summaryGrid.innerHTML = renderSummaryGrid(school);

    title.textContent = `Currently disrupted grants (${filterPositiveFundingGrants(school.grants || []).length})`;
    if (title) title.classList.remove("is-hidden");
    if (stateSummaryCard) stateSummaryCard.classList.add("is-hidden");
    setDataCardVisible("research-other-list", false);
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
      container.innerHTML = renderGrantTable(school.grants || [], grantSortState) + renderRelatedInstitutionLinks({
        unitid: school.unitid,
        financialUnitid: school.financial_unitid,
        current: "research",
        relatedIndexes
      });
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
