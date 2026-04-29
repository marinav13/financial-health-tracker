(function () {
  const {
    loadJson,
    escapeHtml,
    getParam,
    renderEmpty,
    renderExternalLink,
    renderPaginationButtons,
    renderSortableHeader,
    paginateItems,
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
  const PAGE_SIZE = 25;
  const OTHER_PAGE_SIZE = 5;
  const MIN_DEFAULT_YEAR = 2024;

  function isPrimaryBachelorsInstitution(record) {
    return isPrimaryTrackerInstitution(record);
  }

  function renderCutItem(cut) {
    const date = cut.announcement_date || cut.announcement_year || "";
    const term = cut.effective_term ? `<p class="small-meta">Effective term: ${escapeHtml(cut.effective_term)}</p>` : "";
    const sourceLink = renderExternalLink(cut.source_url, "Source");
    const source = sourceLink
      ? `<p class="small-meta">${sourceLink}${cut.source_publication ? ` | ${escapeHtml(cut.source_publication)}` : ""}</p>`
      : "";
    return `
      <article class="data-card">
        <h3>${escapeHtml(cut.program_name || "Unnamed cut")}</h3>
        ${date ? `<p class="small-meta">Date: ${escapeHtml(date)}</p>` : ""}
        ${term}
        ${cut.notes ? `<p>${escapeHtml(cut.notes)}</p>` : ""}
        ${source}
      </article>
    `;
  }

  function formatAffectedCount(cut) {
    const affected = Number(cut.positions_affected || cut.faculty_affected);
    if (!Number.isFinite(affected) || affected <= 0) return "";
    // Skip if program_name already contains the count (e.g. "Staff layoff (20 positions affected)")
    const label = cut.program_name || "";
    if (label.includes("positions affected") || label.includes("students affected")) return "";
    return ` (${affected} affected)`;
  }

  function sortCuts(items, sortState) {
    const sorted = (items || []).slice();
    const direction = sortState?.direction === "desc" ? -1 : 1;
    sorted.sort((a, b) => {
      if (sortState?.key === "institution_name") {
        const primary = compareText(a.institution_name, b.institution_name) * direction;
        if (primary !== 0) return primary;
        return compareDateDesc(a.announcement_date || a.announcement_year, b.announcement_date || b.announcement_year);
      }
      if (sortState?.key === "state") {
        const primary = compareText(a.state, b.state) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "announcement_date") {
        const primary = sortState.direction === "asc"
          ? compareDateDesc(b.announcement_date || b.announcement_year, a.announcement_date || a.announcement_year)
          : compareDateDesc(a.announcement_date || a.announcement_year, b.announcement_date || b.announcement_year);
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      return compareDateDesc(a.announcement_date || a.announcement_year, b.announcement_date || b.announcement_year);
    });
    return sorted;
  }

  function renderCutsTable(items, sortState) {
    if (!items || !items.length) return renderEmpty("No matched cuts are available.");
    const rows = items.map((cut) => [
      renderSchoolLinkCell(cut.financial_unitid, cut.institution_name, "cuts.html"),
      (cut.program_name || "") + formatAffectedCount(cut),
      cut.state,
      cut.control_label,
      cut.announcement_date || cut.announcement_year || ""
    ]);
    return renderHistoryTable({
      ariaLabel: "College cuts by institution",
      headers: [
        renderSortableHeader("institution_name", sortState, "Institution"),
        "<th>College program or staffing cut</th>",
        renderSortableHeader("state", sortState, "State"),
        "<th>Sector</th>",
        renderSortableHeader("announcement_date", sortState, "Date")
      ],
      rows
    });
  }

  function getAnnouncementYear(cut) {
    const explicitYear = Number(cut.announcement_year || "");
    if (!Number.isNaN(explicitYear) && explicitYear > 0) return explicitYear;
    const dateText = String(cut.announcement_date || "");
    const match = dateText.match(/\b(19|20)\d{2}\b/);
    return match ? Number(match[0]) : NaN;
  }

  function buildRecentCuts(cutsData) {
    return Object.values(cutsData.schools || {})
      .flatMap((school) =>
        (school.cuts || []).map((cut) => ({
          ...cut,
          institution_name: school.institution_name || cut.institution_name || "",
          state: school.state || cut.state || "",
          control_label: school.control_label || cut.control_label || "",
          category: school.category || cut.category || "",
          unitid: school.unitid || cut.unitid || "",
          financial_unitid: school.financial_unitid || null,
          is_primary_tracker: school.is_primary_tracker
        }))
      )
      .filter((cut) => {
        const year = getAnnouncementYear(cut);
        return !Number.isNaN(year) && year >= MIN_DEFAULT_YEAR;
      })
      .sort((a, b) =>
        String(b.announcement_date || b.announcement_year || "").localeCompare(
          String(a.announcement_date || a.announcement_year || "")
        )
      );
  }

  function renderCutsTablePage(items, page, pageSize, emptyMessage, sortState) {
    const { totalPages, currentPage, pageItems } = paginateItems(items, page, pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    return `
      ${renderCutsTable(pageItems, sortState)}
      <div class="pagination" aria-label="College cuts pages">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function renderDetailDownloadToolbar() {
    return `
      <div class="table-toolbar detail-download-toolbar">
        <div class="table-toolbar-actions">
          <button id="cuts-detail-download" class="download-button" type="button">Download Displayed Table</button>
        </div>
      </div>
    `;
  }

  // Options bag (replaces 7-positional-arg signature). All keys optional except container + items.
  // Shared keys with accreditation.js / research.js so future shared extraction is trivial.
  function setupPagination({
    container,
    items,
    pageSize = PAGE_SIZE,
    emptyMessage = `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available.`,
    downloadButtonId = null,
    downloadFilename = "college-cuts.csv",
    searchInput = null
  }) {
    return makeTableController({
      container,
      items,
      pageSize,
      searchInput,
      initialSortState: { key: "announcement_date", direction: "desc" },
      sortItems: sortCuts,
      renderPage: (sortedItems, currentPage, size, sortState) => renderCutsTablePage(sortedItems, currentPage, size, emptyMessage, sortState),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "State", "Sector", "Cut", "Date", "Source"],
      downloadRow: (cut) => [
        cut.institution_name || "",
        cut.state || "",
        cut.control_label || "",
        (cut.program_name || "") + formatAffectedCount(cut),
        cut.announcement_date || cut.announcement_year || "",
        cut.source_url || ""
      ]
    });
  }

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid, { active: "cuts" });
    const container = document.getElementById("cuts-list");
    const otherContainer = document.getElementById("cuts-other-list");
    const title = document.getElementById("cuts-section-title");
    const otherTitle = document.getElementById("cuts-other-section-title");
    const mainToolbar = document.getElementById("cuts-table-download")?.closest(".table-toolbar");

    if (!unitid) {
      // Landing view: keep a real document heading for screen-reader navigation,
      // but hide it visually so the existing masthead/banner layout is unchanged.
      const landingHeading = document.getElementById("cuts-school-name");
      landingHeading.textContent = "College cuts";
      landingHeading.classList.add("sr-only");
      landingHeading.classList.remove("is-hidden");
      if (mainToolbar) mainToolbar.classList.remove("is-hidden");
      const cutsData = await loadJson("data/college_cuts.json");
      renderDataAsOf("cuts-data-as-of", cutsData?.generated_at);
      const recent = buildRecentCuts(cutsData);
      const primary = recent.filter(isPrimaryBachelorsInstitution);
      const other = recent.filter((cut) => !isPrimaryBachelorsInstitution(cut));
      setDataCardVisible("cuts-other-list", true);
      title.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at 4-year institutions that primarily grant bachelors degrees`;
      if (otherTitle) otherTitle.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at other institutions`;
      const primaryFilter = document.getElementById("cuts-filter");
      const otherFilter = document.getElementById("cuts-other-filter");
      setupPagination({
        container,
        items: primary,
        emptyMessage: `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for 4-year, primarily bachelor's-degree-granting institutions.`,
        downloadButtonId: "cuts-table-download",
        downloadFilename: "cuts-primary.csv",
        searchInput: primaryFilter
      });
      setupPagination({
        container: otherContainer,
        items: other,
        pageSize: OTHER_PAGE_SIZE,
        emptyMessage: `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for other institutions.`,
        downloadButtonId: "cuts-other-download",
        downloadFilename: "cuts-other.csv",
        searchInput: otherFilter
      });
      return;
    }

    const [cutsIndex, accreditationIndex, researchIndex, metadata] = await Promise.all([
      loadJson("data/college_cuts_index.json"),
      loadJson("data/accreditation_index.json"),
      loadJson("data/research_funding_index.json"),
      loadJson("data/metadata.json")
    ]);
    renderDataAsOf("cuts-data-as-of", metadata?.generated_at);
    const relatedIndexes = {
      cuts: cutsIndex,
      accreditation: accreditationIndex,
      research: researchIndex
    };
    const indexedSchool = findRelatedIndexRecord(cutsIndex, unitid, "cut_count");
    let school = null;

    if (indexedSchool || !isNumericUnitid(unitid)) {
      const cutsData = await loadJson("data/college_cuts.json");
      renderDataAsOf("cuts-data-as-of", cutsData?.generated_at);
      school = cutsData.schools?.[indexedSchool?.unitid || unitid];
    }

    if (!school) {
      const missingHeading = document.getElementById("cuts-school-name");
      missingHeading.textContent = "No matched cuts found";
      missingHeading.classList.remove("is-hidden");
      missingHeading.classList.remove("sr-only");
      container.innerHTML = renderEmpty("No matched college cuts were found for this institution in the current dataset.");
      title.textContent = "Cuts";
      return;
    }

    const schoolHeading = document.getElementById("cuts-school-name");
    schoolHeading.textContent = school.institution_name || "College Cuts";
    schoolHeading.classList.remove("is-hidden");
    schoolHeading.classList.remove("sr-only");
    if (mainToolbar) mainToolbar.classList.add("is-hidden");
    syncTabs(unitid, { active: "cuts", financialUnitid: school.financial_unitid });
    const relatedLinks = renderRelatedInstitutionLinks({
      unitid: school.unitid,
      financialUnitid: school.financial_unitid,
      current: "cuts",
      relatedIndexes
    });
    const cutCount = school.cut_count ?? 0;
    title.textContent = cutCount === 1 ? "College program or staffing cut" : `College program or staffing cuts (${cutCount})`;
    setDataCardVisible("cuts-other-list", false);
    if (otherContainer) otherContainer.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";
    if (!(school.cuts || []).length) {
      container.innerHTML = renderEmpty("No matched cuts were found for this institution.");
      return;
    }

    let detailSortState = { key: "announcement_date", direction: "desc" };
    const detailRows = (school.cuts || []).map((cut) => ({
      ...cut,
      institution_name: school.institution_name,
      state: school.state,
      control_label: school.control_label,
      unitid: school.unitid,
      financial_unitid: school.financial_unitid
    }));
    const renderDetailTable = () => {
      container.innerHTML = school.cuts.map(renderCutItem).join("") +
        renderDetailDownloadToolbar() +
        renderCutsTable(detailRows, detailSortState) +
        relatedLinks;
      const detailDownload = document.getElementById("cuts-detail-download");
      if (detailDownload) {
        detailDownload.onclick = () => downloadRowsCsv(
          `${String(school.institution_name || "college-cuts").toLowerCase().replace(/[^a-z0-9]+/g, "-")}-cuts.csv`,
          ["Institution", "State", "Sector", "Cut", "Date", "Source"],
          detailRows.map((cut) => [
            cut.institution_name || "",
            cut.state || "",
            cut.control_label || "",
            (cut.program_name || "") + formatAffectedCount(cut),
            cut.announcement_date || cut.announcement_year || "",
            cut.source_url || ""
          ])
        );
      }
      bindSortControls(container, detailSortState, { key: "announcement_date", direction: "desc" }, (nextSortState) => {
        detailSortState = nextSortState;
        renderDetailTable();
      });
    };
    renderDetailTable();
  }

  init().catch((error) => {
    console.error(error);
    const container = document.getElementById("cuts-list");
    if (container) container.innerHTML = renderEmpty("The college cuts data could not be loaded.");
  });
})();
