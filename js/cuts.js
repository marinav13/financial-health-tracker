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
    makeTableController,
    cleanCutLabel,
    getCommittedSearchValue
  } = window.TrackerApp;
  const PAGE_SIZE = 25;
  const MIN_DEFAULT_YEAR = 2024;
  const ALL_CATEGORIES_VALUE = "__all__";
  const CUT_CATEGORY_ORDER = [
    "Institution closures/absorptions",
    "Campus closures",
    "Athletics cuts",
    "Staff layoffs / furloughs",
    "Academic program cuts / admissions pauses",
    "Student support closures",
    "Research center cuts",
    "Multiple cut types"
  ];

  function isPrimaryBachelorsInstitution(record) {
    return isPrimaryTrackerInstitution(record);
  }

  function renderCutItem(cut) {
    const label = cleanCutLabel(cut.cut_label_public || cut.program_name || resolveDisplayCategory(cut));
    const category = resolveDisplayCategory(cut);
    const date = cut.announcement_date || cut.announcement_year || "";
    const term = cut.effective_term ? `<p class="small-meta">Effective term: ${escapeHtml(cut.effective_term)}</p>` : "";
    const sourceLink = renderExternalLink(cut.source_url, "Source");
    const source = sourceLink
      ? `<p class="small-meta">${sourceLink}${cut.source_publication ? ` | ${escapeHtml(cut.source_publication)}` : ""}</p>`
      : "";
    const categoryMeta = category && category !== label
      ? `<p class="small-meta">Category: ${escapeHtml(category)}</p>`
      : "";
    return `
      <article class="data-card data-card--cut">
        <h3>${escapeHtml(label)}</h3>
        ${categoryMeta}
        ${date ? `<p class="small-meta">Date: ${escapeHtml(date)}</p>` : ""}
        ${term}
        ${source}
      </article>
    `;
  }

  function normalizeDisplayCategory(value) {
    const category = String(value || "").trim();
    if (category === "Research centers") return "Research center cuts";
    return category;
  }

  function resolveDisplayCategory(cut) {
    if (cut?.display_category) return normalizeDisplayCategory(cut.display_category);
    switch (String(cut?.cut_type || "").trim().toLowerCase()) {
      case "institution_closure":
        return "Institution closures/absorptions";
      case "campus_closure":
        return "Campus closures";
      case "staff_layoff":
      case "faculty_layoff":
      case "hiring_freeze":
        return "Staff layoffs / furloughs";
      case "program_suspension":
        return "Academic program cuts / admissions pauses";
      case "department_closure":
        return "Research center cuts";
      default:
        break;
    }
    return "Academic program cuts / admissions pauses";
  }

  function buildCategoryOptions(items) {
    const present = new Set((items || []).map((item) => resolveDisplayCategory(item)).filter(Boolean));
    const ordered = CUT_CATEGORY_ORDER.filter((category) => present.has(category));
    const extras = Array.from(present).filter((category) => !CUT_CATEGORY_ORDER.includes(category)).sort((a, b) => a.localeCompare(b));
    return [...ordered, ...extras];
  }

  function getSelectedCategories(filterRoot) {
    if (!filterRoot) return [];
    const selected = Array.from(filterRoot.querySelectorAll('input[type="checkbox"][data-category-value]:checked'))
      .map((input) => String(input.getAttribute("data-category-value") || "").trim())
      .filter(Boolean);
    return selected.includes(ALL_CATEGORIES_VALUE) ? [] : selected;
  }

  function updateCategoryFilterSummary(filterRoot) {
    if (!filterRoot) return;
    const summary = filterRoot.querySelector(".table-filter-select");
    if (!summary) return;
    const selected = getSelectedCategories(filterRoot);
    if (!selected.length) {
      summary.textContent = "All categories";
      return;
    }
    if (selected.length === 1) {
      [summary.textContent] = selected;
      return;
    }
    summary.textContent = `${selected.length} categories selected`;
  }

  function normalizeCategoryFilterState(filterRoot, changedValue = "") {
    if (!filterRoot) return;
    const checkboxes = Array.from(filterRoot.querySelectorAll('input[type="checkbox"][data-category-value]'));
    const allCheckbox = checkboxes.find((input) => String(input.getAttribute("data-category-value")) === ALL_CATEGORIES_VALUE);
    const categoryCheckboxes = checkboxes.filter((input) => input !== allCheckbox);
    if (!allCheckbox) return;

    if (changedValue === ALL_CATEGORIES_VALUE && allCheckbox.checked) {
      categoryCheckboxes.forEach((input) => {
        input.checked = false;
      });
      updateCategoryFilterSummary(filterRoot);
      return;
    }

    const checkedCategories = categoryCheckboxes.filter((input) => input.checked);
    if (!checkedCategories.length || checkedCategories.length === categoryCheckboxes.length) {
      allCheckbox.checked = true;
      categoryCheckboxes.forEach((input) => {
        input.checked = false;
      });
    } else {
      allCheckbox.checked = false;
    }

    updateCategoryFilterSummary(filterRoot);
  }

  function populateCategoryFilter(filterRoot, items) {
    if (!filterRoot) return;
    const optionsContainer = filterRoot.querySelector("#cuts-category-filter-options");
    if (!optionsContainer) return;
    const previouslySelected = getSelectedCategories(filterRoot);
    const options = buildCategoryOptions(items);
    const selectedValues = new Set(previouslySelected.filter((value) => options.includes(value)));
    const showAll = !selectedValues.size;
    optionsContainer.innerHTML = [
      `<label class="table-filter-option"><input type="checkbox" data-category-value="${ALL_CATEGORIES_VALUE}"${showAll ? " checked" : ""}> <span>All categories</span></label>`,
      ...options.map((value) => {
        const checked = selectedValues.has(value);
        return `<label class="table-filter-option"><input type="checkbox" data-category-value="${escapeHtml(value)}"${checked ? " checked" : ""}> <span>${escapeHtml(value)}</span></label>`;
      })
    ].join("");
    updateCategoryFilterSummary(filterRoot);
  }

  function initCategoryFilterDismissal(filterRoot) {
    if (!filterRoot || filterRoot.dataset.dismissalBound === "true") return;
    filterRoot.dataset.dismissalBound = "true";

    document.addEventListener("click", (event) => {
      if (!filterRoot.open) return;
      if (filterRoot.contains(event.target)) return;
      filterRoot.open = false;
    });

    filterRoot.addEventListener("keydown", (event) => {
      if (event.key !== "Escape") return;
      filterRoot.open = false;
      filterRoot.querySelector(".table-filter-select")?.focus();
    });

    filterRoot.addEventListener("focusout", () => {
      window.setTimeout(() => {
        if (!filterRoot.open) return;
        const active = document.activeElement;
        if (active && filterRoot.contains(active)) return;
        filterRoot.open = false;
      }, 0);
    });
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
      if (sortState?.key === "control_label") {
        const primary = compareText(a.control_label, b.control_label) * direction;
        if (primary !== 0) return primary;
        const secondary = compareText(a.institution_name, b.institution_name);
        if (secondary !== 0) return secondary;
        return compareDateDesc(a.announcement_date || a.announcement_year, b.announcement_date || b.announcement_year);
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

  function renderCutsTable(items, sortState, options = {}) {
    const { landingMode = false } = options;
    if (!items || !items.length) return renderEmpty("No matched cuts are available.");
    const rows = landingMode
      ? items.map((cut) => [
        renderSchoolLinkCell(cut.financial_unitid, cut.institution_name, "cuts.html"),
        resolveDisplayCategory(cut),
        cut.state,
        cut.control_label,
        cut.announcement_date || cut.announcement_year || ""
      ])
      : items.map((cut) => [
        renderSchoolLinkCell(cut.financial_unitid, cut.institution_name, "cuts.html"),
        cleanCutLabel(cut.cut_label_public || cut.program_name),
        cut.state,
        cut.control_label,
        cut.announcement_date || cut.announcement_year || ""
      ]);
    return renderHistoryTable({
      ariaLabel: "College cuts by institution",
      headers: landingMode
        ? [
          renderSortableHeader("institution_name", sortState, "Institution"),
          "<th>TYPE OF CUTS</th>",
          renderSortableHeader("state", sortState, "State"),
          renderSortableHeader("control_label", sortState, "Sector"),
          renderSortableHeader("announcement_date", sortState, "Date")
        ]
        : [
          renderSortableHeader("institution_name", sortState, "Institution"),
          "<th>College program or staffing cut</th>",
          renderSortableHeader("state", sortState, "State"),
          renderSortableHeader("control_label", sortState, "Sector"),
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

  function buildRecentCuts(cutsIndex) {
    return Object.values(cutsIndex || {})
      .flatMap((school) => {
        return (school.landing_cuts || []).map((cut) => {
          return {
            ...cut,
            institution_name: school.institution_name || cut.institution_name || "",
            state: school.state || cut.state || "",
            control_label: school.control_label || cut.control_label || "",
            category: school.category || cut.category || "",
            unitid: school.unitid || cut.unitid || "",
            financial_unitid: school.financial_unitid || null,
            is_primary_tracker: school.is_primary_tracker,
            display_category:
              cut.display_category || ""
          };
        });
      })
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

  function renderCutsTablePage(items, page, pageSize, emptyMessage, sortState, options = {}) {
    const { totalPages, currentPage, pageItems } = paginateItems(items, page, pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    return `
      ${renderCutsTable(pageItems, sortState, options)}
      <div class="pagination" aria-label="College cuts pages">
        ${renderPaginationButtons({ currentPage, totalPages })}
      </div>
    `;
  }

  function renderDetailDownloadToolbar() {
    return `
      <div class="table-toolbar detail-download-toolbar">
        <div class="table-toolbar-actions">
          <button id="cuts-detail-download" class="download-button" type="button">Download college cuts data</button>
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
    searchInput = null,
    filterOnInput = true,
    searchValueResolver = null,
    landingMode = false,
    categoryFilter = null
  }) {
    const controller = makeTableController({
      container,
      items,
      pageSize,
      searchInput,
      filterOnInput,
      searchValueResolver,
      filterItems: (rows, query) => {
        const institutionMatches = window.TrackerApp.filterByInstitution(rows, query);
        const selectedCategories = getSelectedCategories(categoryFilter);
        if (!selectedCategories.length) return institutionMatches;
        return institutionMatches.filter((row) => selectedCategories.includes(resolveDisplayCategory(row)));
      },
      initialSortState: { key: "announcement_date", direction: "desc" },
      sortItems: sortCuts,
      renderPage: (sortedItems, currentPage, size, sortState) => renderCutsTablePage(sortedItems, currentPage, size, emptyMessage, sortState, { landingMode }),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "State", "Sector", "Category", "Cut", "Date", "Source"],
      downloadRow: (cut) => [
        cut.institution_name || "",
        cut.state || "",
        cut.control_label || "",
        resolveDisplayCategory(cut),
        cleanCutLabel(cut.cut_label_public || cut.program_name),
        cut.announcement_date || cut.announcement_year || "",
        cut.source_url || ""
      ]
    });
    if (categoryFilter) {
      categoryFilter.addEventListener("change", (event) => {
        const target = event.target;
        if (!(target instanceof HTMLInputElement) || target.type !== "checkbox") return;
        normalizeCategoryFilterState(categoryFilter, String(target.getAttribute("data-category-value") || ""));
        controller?.reset?.();
      });
    }
    return controller;
  }

  async function init() {
    const unitid = getParam("unitid");
    syncTabs({ active: "cuts" });
    // Editorial Calm: swap landing-mode hero (visible H1 + lede on landing
    // pages) for the institution-mode quad-banner + school-mast block when
    // ?unitid is set. Two siblings, exactly one visible at a time.
    const landingHero = document.getElementById("cuts-landing-hero");
    if (landingHero) landingHero.classList.toggle("is-hidden", Boolean(unitid));
    const institutionMast = document.getElementById("cuts-institution-mast");
    if (institutionMast) institutionMast.classList.toggle("is-hidden", !unitid);
    const container = document.getElementById("cuts-list");
    const otherContainer = document.getElementById("cuts-other-list");
    const title = document.getElementById("cuts-section-title");
    const tableIntro = document.getElementById("cuts-table-intro");
    const otherTitle = document.getElementById("cuts-other-section-title");
    const mainToolbar = document.getElementById("cuts-table-download")?.closest(".table-toolbar");
    if (tableIntro) tableIntro.classList.toggle("is-hidden", Boolean(unitid));

    if (!unitid) {
      // Landing view: keep a real document heading for screen-reader navigation,
      // but hide it visually so the existing masthead/banner layout is unchanged.
      const landingHeading = document.getElementById("cuts-school-name");
      landingHeading.textContent = "College cuts";
      landingHeading.classList.add("sr-only");
      landingHeading.classList.remove("is-hidden");
      if (mainToolbar) mainToolbar.classList.remove("is-hidden");
      const [cutsIndex, metadata] = await Promise.all([
        loadJson("data/college_cuts_index.json"),
        loadJson("data/metadata.json")
      ]);
      renderDataAsOf("cuts-data-as-of", metadata?.generated_at);
      const recent = buildRecentCuts(cutsIndex);
      const primary = recent.filter(isPrimaryBachelorsInstitution);
      setDataCardVisible("cuts-other-list", true);
      title.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at four-year degree-granting institutions`;
      const primaryFilter = document.getElementById("cuts-filter");
      const categoryFilter = document.getElementById("cuts-category-filter");
      populateCategoryFilter(categoryFilter, primary);
      initCategoryFilterDismissal(categoryFilter);
      setupPagination({
        container,
        items: primary,
        emptyMessage: `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for 4-year, primarily bachelor's-degree-granting institutions.`,
        downloadButtonId: "cuts-table-download",
        downloadFilename: "cuts-primary.csv",
        searchInput: primaryFilter,
        filterOnInput: false,
        searchValueResolver: getCommittedSearchValue,
        landingMode: true,
        categoryFilter
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
    // Editorial Calm: italic meta line under the H1 — "City, State · Sector".
    // Built with <span class="sep">·</span> separators so the dot picks up
    // its own muted color and spacing from CSS instead of being a plain
    // text character inside the italic run.
    const schoolMeta = document.getElementById("cuts-school-meta");
    if (schoolMeta) {
      const parts = [
        [school.city, school.state].filter(Boolean).join(", "),
        school.control_label || ""
      ].filter(Boolean);
      // Empty <span class="sep"> nodes — the middle-dot glyph is added
      // via CSS ::after so the span has no real text content. That
      // keeps the Playwright aria-hidden-sync helper happy and lets
      // screen readers skip the dot entirely (no aria-hidden needed
      // since pseudo-content is invisible to AT).
      schoolMeta.innerHTML = parts
        .map((part) => escapeHtml(part))
        .join('<span class="sep"></span>');
    }
    if (mainToolbar) mainToolbar.classList.add("is-hidden");
    syncTabs({ active: "cuts" });
    const relatedLinks = renderRelatedInstitutionLinks({
      unitid: school.unitid,
      financialUnitid: school.financial_unitid,
      hasFinancialProfile: indexedSchool?.is_primary_tracker === true || school.is_primary_tracker === true,
      current: "cuts",
      relatedIndexes
    });
    const cutCount = school.cut_count ?? 0;
    title.textContent = `College program or staffing cuts (${cutCount})`;
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
          ["Institution", "State", "Sector", "Category", "Cut", "Date", "Source"],
          detailRows.map((cut) => [
            cut.institution_name || "",
            cut.state || "",
            cut.control_label || "",
            resolveDisplayCategory(cut),
            cleanCutLabel(cut.cut_label_public || cut.program_name),
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
