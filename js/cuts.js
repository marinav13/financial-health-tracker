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
    "Student support cuts",
    "Research center cuts",
    "Community program cuts",
    "Academic program cuts",
    "Staff layoffs / furloughs"
  ];
  const DISPLAY_CATEGORY_ALIASES = new Map([
    ["Institution closures", "Institution closures/absorptions"],
    ["Research centers", "Research center cuts"],
    ["Academic program cuts / admissions pauses", "Academic program cuts"],
    ["Student support closures", "Student support cuts"]
  ]);

  function isPrimaryBachelorsInstitution(record) {
    return isPrimaryTrackerInstitution(record);
  }

  function renderCutItem(cut) {
    const label = cleanCutLabel(cut.cut_label_public || cut.program_name || resolveDisplayCategory(cut));
    const categories = resolveDisplayCategories(cut);
    const date = cut.announcement_date || cut.announcement_year || "";
    const term = cut.effective_term ? `<p class="small-meta">Effective term: ${escapeHtml(cut.effective_term)}</p>` : "";
    const sourceLink = renderExternalLink(cut.source_url, "Source");
    const source = sourceLink
      ? `<p class="small-meta">${sourceLink}${cut.source_publication ? ` | ${escapeHtml(cut.source_publication)}` : ""}</p>`
      : "";
    return `
      <article class="data-card data-card--cut">
        <h3>${escapeHtml(label)}</h3>
        ${renderCategoryTags(categories)}
        ${date ? `<p class="small-meta">Date: ${escapeHtml(date)}</p>` : ""}
        ${term}
        ${source}
      </article>
    `;
  }

  function normalizeDisplayCategory(value) {
    const category = String(value || "").trim();
    if (!category || category === "Multiple cut types") return "";
    return DISPLAY_CATEGORY_ALIASES.get(category) || category;
  }

  function normalizeDisplayCategories(values) {
    let categories = [];
    if (Array.isArray(values)) {
      categories = values;
    } else if (typeof values === "string") {
      categories = values.trim() ? [values] : [];
    } else if (values != null) {
      categories = [values];
    }
    return categories
      .map((value) => normalizeDisplayCategory(value))
      .filter(Boolean)
      .filter((value, index, list) => list.indexOf(value) === index);
  }

  function normalizeCategoryText(...parts) {
    return parts
      .flat()
      .map((value) => String(value || "").trim())
      .filter(Boolean)
      .join(" ")
      .toLowerCase()
      .replace(/[\r\n\t]+/g, " ")
      .replace(/[‘’]/g, "'")
      .replace(/[“”]/g, '"')
      .replace(/[^a-z0-9'./& -]+/g, " ")
      .replace(/\s{2,}/g, " ")
      .trim();
  }

  function matchesCategory(text, patterns) {
    if (!text || !patterns.length) return false;
    return patterns.some((pattern) => pattern.test(text));
  }

  function inferDisplayCategories(cut) {
    const normalizedType = String(cut?.cut_type || "").trim().toLowerCase();
    const subjectText = normalizeCategoryText(cut?.program_name, cut?.cut_label_public);
    const text = normalizeCategoryText(subjectText, cut?.cut_summary_public, cut?.notes);
    const unitActionPatterns = [
      /\bclos(?:e|ed|es)\b/i,
      /\bclosing\b/i,
      /\bclosure\b/i,
      /\bshut(?:ting)? down\b/i,
      /\beliminat(?:e|ed|ing)\b/i,
      /\bdiscontinu(?:e|ed|ing)\b/i,
      /\bsuspend(?:ed|ing)?\b/i,
      /\bsunset(?:ted|ting)?\b/i,
      /\bphase(?:d)? out\b/i,
      /\bdrop(?:ped|ping)?\b/i,
      /\bpaus(?:e|ed|ing)\b/i,
      /\bterminat(?:e|ed|ing)\b/i
    ];
    const unitClosurePatterns = [
      /\bclos(?:e|ed|es)\b/i,
      /\bclosing\b/i,
      /\bclosure\b/i,
      /\bshut(?:ting)? down\b/i,
      /\bdiscontinu(?:e|ed|ing)\b/i,
      /\bsuspend(?:ed|ing)?\b/i,
      /\bsunset(?:ted|ting)?\b/i,
      /\bphase(?:d)? out\b/i,
      /\bdrop(?:ped|ping)?\b/i,
      /\bpaus(?:e|ed|ing)\b/i,
      /\bterminat(?:e|ed|ing)\b/i
    ];
    const institutionSignal = normalizedType === "institution_closure" || matchesCategory(text, [
      /\binstitution closure\b/i,
      /\bclosing as an accredited institution\b/i,
      /\bceasing all (?:academic |mba )?operations\b/i,
      /\bpermanently closing\b/i,
      /\bclosing permanently\b/i,
      /\b(?:college|university|institution|school) (?:is |will be |will )?clos(?:e|ing|ed)\b/i
    ]);
    if (institutionSignal) return ["Institution closures/absorptions"];

    const campusSignal = normalizedType === "campus_closure" || matchesCategory(text, [
      /\bcampus closure\b/i,
      /\bcampus (?:is |will be |will )?clos(?:e|ing|ed)\b/i,
      /\bsatellite campus(?:es)?\b/i,
      /\bdowntown center campus\b/i
    ]);
    if (campusSignal) return ["Campus closures"];

    const athleticsAction = matchesCategory(subjectText, [
      /\bathletics\b/i,
      /\bathletic department\b/i,
      /\bncaa\b/i,
      /\bstudent-?athletes?\b/i,
      /\bfootball\b/i,
      /\bbasketball\b/i,
      /\bbaseball\b/i,
      /\bsoftball\b/i,
      /\bsoccer\b/i,
      /\bvolleyball\b/i,
      /\bwrestling\b/i,
      /\bswimming\b/i,
      /\bdiving\b/i,
      /\bgolf\b/i,
      /\btennis\b/i,
      /\btrack\b/i,
      /\bcross country\b/i,
      /\bcoach(?:ing)?\b/i,
      /\bathletic scholarships?\b/i
    ]) || matchesCategory(text, [/\bathletic department\b/i, /\bathletics\b/i]);

    const staffPatterns = [
      /\blayoff(?:s)?\b/i,
      /\blaid off\b/i,
      /\bfurlough(?:ed|s)?\b/i,
      /\breduction in force\b/i,
      /\brif\b/i,
      /\bpositions? eliminated\b/i,
      /\bjob cuts?\b/i,
      /\bworkforce reduction\b/i,
      /\bvoluntary separations?\b/i,
      /\bnon-?renew(?:al|ed)?\b/i,
      /\bhiring freeze\b/i
    ];
    const staffPrimaryAction = ["staff_layoff", "faculty_layoff", "hiring_freeze"].includes(normalizedType) ||
      matchesCategory(subjectText, staffPatterns);
    const staffAction = staffPrimaryAction || matchesCategory(text, staffPatterns);

    const studentSupportSubject = matchesCategory(subjectText, [
      /\bstudent support\b/i,
      /\bstudent success\b/i,
      /\bwelcome center\b/i,
      /\boutreach\b/i,
      /\badvis(?:ing|or)\b/i,
      /\bretention\b/i,
      /\bpre-?college\b/i,
      /\bfirst-?year\b/i,
      /\borientation\b/i,
      /\bscholar(?:s|ship)?\b/i,
      /\bdiversity\b/i,
      /\bequity\b/i,
      /\binclusion\b/i,
      /\bsocial justice\b/i,
      /\blgbtq?\b/i,
      /\bgender and sexuality\b/i,
      /\bmulticultural\b/i,
      /\bcampus ministry\b/i,
      /\bstudent life\b/i,
      /\bstudent culture\b/i,
      /\bcollege to career\b/i
    ]) || matchesCategory(text, [
      /\bgender and sexuality center\b/i,
      /\boffice of diversity\b/i,
      /\boffice of social justice\b/i
    ]);
    const studentSupportAction = studentSupportSubject && (
      ["program_suspension", "department_closure"].includes(normalizedType) ||
      matchesCategory(subjectText, unitClosurePatterns) ||
      matchesCategory(text, [/\bshut(?:ting)? down\b/i, /\bclosing\b/i, /\bclosed\b/i, /\bdiscontinu(?:e|ed|ing)\b/i, /\bpaus(?:e|ed|ing)\b/i])
    );

    const researchCenterSubject = matchesCategory(subjectText, [
      /\bcenter\b/i,
      /\bcentre\b/i,
      /\binstitute\b/i,
      /\bresearch lab\b/i,
      /\bresearch laboratory\b/i,
      /\bobservatory\b/i,
      /\bmuseum\b/i,
      /\boffice\b/i
    ]) && !studentSupportSubject;
    const researchCenterAction = researchCenterSubject && (
      normalizedType === "department_closure" ||
      matchesCategory(subjectText, unitClosurePatterns) ||
      matchesCategory(text, [/\bshut(?:ting)? down\b/i, /\bclosing\b/i, /\bclosed\b/i, /\bdiscontinu(?:e|ed|ing)\b/i])
    );

    const communityProgramSubject = matchesCategory(subjectText, [
      /\bcommunity program(?:s)?\b/i,
      /\bcommunity music school\b/i,
      /\bmusic school\b/i,
      /\bextension\b/i,
      /\boutreach program(?:s)?\b/i,
      /\bpublic service\b/i,
      /\bcontinuing education\b/i,
      /\bsnap-?ed\b/i,
      /\bnutrition education\b/i,
      /\bcommunity education\b/i,
      /\bcommunity engagement\b/i
    ]) || matchesCategory(text, [
      /\bserved [0-9, ]+ people annually\b/i,
      /\bserved communities? across\b/i,
      /\bcommunity members?\b/i
    ]);
    const communityProgramAction = communityProgramSubject && (
      ["program_suspension", "department_closure"].includes(normalizedType) ||
      matchesCategory(subjectText, unitActionPatterns) ||
      matchesCategory(text, [/\bfunding (?:eliminated|ended|cut)\b/i, /\bgrant funding (?:cut|eliminated)\b/i])
    );

    let academicAction = matchesCategory(subjectText, [
      /\badmissions? paus(?:e|ed|ing)\b/i,
      /\bmajor(?:s)?\b/i,
      /\bminor(?:s)?\b/i,
      /\bdegree(?:s)?\b/i,
      /\bcertificate(?:s)?\b/i,
      /\bconcentration(?:s)?\b/i,
      /\bph\.?d\.?\b/i,
      /\bmaster'?s\b/i,
      /\bbachelor'?s\b/i,
      /\bgraduate programs?\b/i,
      /\bacademic programs?\b/i
    ]);
    if (!academicAction) {
      academicAction = matchesCategory(subjectText, [
        /\bclasses?\b.{0,24}\bcut/i,
        /\bcut(?:ting)?\b.{0,24}\bclasses?\b/i,
        /\bcourses?\b.{0,24}\bcut/i,
        /\bcut(?:ting)?\b.{0,24}\bcourses?\b/i
      ]);
    }
    if (!academicAction && normalizedType === "program_suspension" &&
      !athleticsAction && !studentSupportAction && !researchCenterAction && !communityProgramAction) {
      academicAction = true;
    }
    if (!academicAction && normalizedType === "department_closure" &&
      !athleticsAction && !studentSupportAction && !researchCenterAction && !communityProgramAction) {
      academicAction = true;
    }
    if (!academicAction &&
      matchesCategory(subjectText, [/\bdepartment\b/i]) &&
      (matchesCategory(subjectText, unitActionPatterns) || normalizedType === "department_closure") &&
      !athleticsAction && !studentSupportAction && !researchCenterAction && !communityProgramAction) {
      academicAction = true;
    }

    const categories = [];
    const addCategory = (value) => {
      if (value && !categories.includes(value)) categories.push(value);
    };
    if (athleticsAction) addCategory("Athletics cuts");
    if (studentSupportAction) addCategory("Student support cuts");
    if (researchCenterAction) addCategory("Research center cuts");
    if (communityProgramAction) addCategory("Community program cuts");
    if (academicAction) addCategory("Academic program cuts");
    if (staffAction) addCategory("Staff layoffs / furloughs");
    if (categories.length) return categories;
    if (["staff_layoff", "faculty_layoff", "hiring_freeze"].includes(normalizedType)) return ["Staff layoffs / furloughs"];
    if (normalizedType === "program_suspension") return ["Academic program cuts"];
    if (normalizedType === "department_closure") {
      if (researchCenterSubject) return ["Research center cuts"];
      if (communityProgramSubject) return ["Community program cuts"];
      return ["Academic program cuts"];
    }
    if (researchCenterSubject) return ["Research center cuts"];
    if (communityProgramSubject) return ["Community program cuts"];
    return ["Academic program cuts"];
  }

  function resolveDisplayCategories(cut) {
    const explicit = normalizeDisplayCategories(cut?.display_categories);
    if (explicit.length) return explicit;
    const inferred = inferDisplayCategories(cut);
    if (inferred.length) return inferred;
    const hinted = normalizeDisplayCategories([cut?.primary_display_category, cut?.display_category]);
    return hinted.length ? hinted : ["Academic program cuts"];
  }

  function resolveDisplayCategory(cut) {
    const [primary = "Academic program cuts"] = resolveDisplayCategories(cut);
    return primary;
  }

  function renderCategoryTags(categories, className = "") {
    const tags = normalizeDisplayCategories(categories);
    if (!tags.length) return "";
    const classAttr = ["cut-tag-list", className].filter(Boolean).join(" ");
    return `<div class="${classAttr}" aria-label="Type of cuts">${tags.map((tag) => `<span class="cut-tag">${escapeHtml(tag)}</span>`).join("")}</div>`;
  }

  function formatCategoriesForCsv(cut) {
    return resolveDisplayCategories(cut).join("; ");
  }

  function buildCategoryOptions(items) {
    const present = new Set((items || []).flatMap((item) => resolveDisplayCategories(item)).filter(Boolean));
    const ordered = CUT_CATEGORY_ORDER.filter((category) => present.has(category));
    const extras = Array.from(present).filter((category) => !CUT_CATEGORY_ORDER.includes(category)).sort((a, b) => a.localeCompare(b));
    return [...ordered, ...extras];
  }

  function normalizeSelectedCategoryState(selectedCategories, changedValue, isChecked, allOptions) {
    if (changedValue === ALL_CATEGORIES_VALUE) return [];
    const next = new Set((selectedCategories || []).filter((value) => (allOptions || []).includes(value)));
    if (isChecked) {
      next.add(changedValue);
    } else {
      next.delete(changedValue);
    }
    if (!next.size || next.size === (allOptions || []).length) return [];
    return Array.from(next);
  }

  function renderCategoryFilterOptions(options, selectedCategories) {
    const selected = new Set((selectedCategories || []).filter((value) => (options || []).includes(value)));
    const showAll = selected.size === 0;
    return [
      `<label class="table-filter-option"><input type="checkbox" data-category-value="${ALL_CATEGORIES_VALUE}"${showAll ? " checked" : ""}> <span>All categories</span></label>`,
      ...(options || []).map((value) => `<label class="table-filter-option"><input type="checkbox" data-category-value="${escapeHtml(value)}"${selected.has(value) ? " checked" : ""}> <span>${escapeHtml(value)}</span></label>`)
    ].join("");
  }

  function renderCutsCategoryHeader(filterState = {}) {
    const selectedCount = (filterState.selectedCategories || []).length;
    const buttonClasses = ["table-header-filter-trigger", "filter-button"];
    if (selectedCount > 0) buttonClasses.push("is-active");
    return `
      <th class="history-table-filter-col">
        <div class="table-header-filter-wrap">
          <span>TYPE OF CUTS</span>
          <button
            id="cuts-category-filter-button"
            class="${buttonClasses.join(" ")}"
            type="button"
            aria-label="Filter by type of cuts"
            aria-controls="cuts-category-filter-menu"
            aria-expanded="${filterState.isOpen ? "true" : "false"}"
          >
            <img class="filter-icon" src="assets/cuts-filter-icon.png" alt="" aria-hidden="true">
          </button>
          <div
            id="cuts-category-filter-menu"
            class="table-filter-menu-options table-filter-menu-options--header"
            aria-label="Filter by type of cuts"
            ${filterState.isOpen ? "" : "hidden"}
          >
            ${renderCategoryFilterOptions(filterState.options, filterState.selectedCategories)}
          </div>
        </div>
      </th>
    `;
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
    const {
      landingMode = false,
      categoryFilterState = null,
      preserveHeaderOnEmpty = false,
      emptyMessage = "No matched cuts are available."
    } = options;
    if ((!items || !items.length) && !(landingMode && preserveHeaderOnEmpty)) {
      return renderEmpty(emptyMessage);
    }
    const rows = landingMode
      ? ((items && items.length)
        ? items.map((cut) => `
        <tr>
          <td>${window.TrackerApp.renderSchoolLink(cut.financial_unitid, cut.institution_name, "cuts.html")}</td>
          <td>${renderCategoryTags(resolveDisplayCategories(cut), "history-table-cut-tags")}</td>
          <td>${escapeHtml(cut.state || "")}</td>
          <td>${escapeHtml(cut.control_label || "")}</td>
          <td>${escapeHtml(cut.announcement_date || cut.announcement_year || "")}</td>
        </tr>
      `)
        : [`<tr><td colspan="5" class="history-table-empty-cell">${escapeHtml(emptyMessage)}</td></tr>`])
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
          renderCutsCategoryHeader(categoryFilterState || {}),
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
            primary_display_category: cut.primary_display_category || cut.display_category || "",
            display_category: cut.display_category || "",
            display_categories: normalizeDisplayCategories(cut.display_categories)
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
      if (!options.landingMode) return renderEmpty(emptyMessage);
      return renderCutsTable([], sortState, {
        ...options,
        preserveHeaderOnEmpty: true,
        emptyMessage
      });
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
    landingMode = false
  }) {
    let selectedCategories = [];
    let isCategoryFilterOpen = false;
    const categoryOptions = landingMode ? buildCategoryOptions(items) : [];
    const controller = makeTableController({
      container,
      items,
      pageSize,
      searchInput,
      filterOnInput,
      searchValueResolver,
      filterItems: (rows, query) => {
        const institutionMatches = window.TrackerApp.filterByInstitution(rows, query);
        if (!selectedCategories.length) return institutionMatches;
        return institutionMatches.filter((row) => resolveDisplayCategories(row).some((category) => selectedCategories.includes(category)));
      },
      initialSortState: { key: "announcement_date", direction: "desc" },
      sortItems: sortCuts,
      renderPage: (sortedItems, currentPage, size, sortState) => renderCutsTablePage(sortedItems, currentPage, size, emptyMessage, sortState, {
        landingMode,
        categoryFilterState: landingMode ? {
          isOpen: isCategoryFilterOpen,
          options: categoryOptions,
          selectedCategories
        } : null
      }),
      downloadButton: downloadButtonId,
      downloadFilename,
      downloadHeaders: ["Institution", "State", "Sector", "Type of cuts", "Cut", "Date", "Source"],
      downloadRow: (cut) => [
        cut.institution_name || "",
        cut.state || "",
        cut.control_label || "",
        formatCategoriesForCsv(cut),
        cleanCutLabel(cut.cut_label_public || cut.program_name),
        cut.announcement_date || cut.announcement_year || "",
        cut.source_url || ""
      ]
    });

    if (landingMode && container && !container.dataset.boundCutsCategoryFilter) {
      container.dataset.boundCutsCategoryFilter = "true";

      container.addEventListener("click", (event) => {
        const target = event.target;
        if (!(target instanceof Element)) return;
        const trigger = target.closest("#cuts-category-filter-button");
        if (!trigger) return;
        event.preventDefault();
        event.stopPropagation();
        isCategoryFilterOpen = !isCategoryFilterOpen;
        controller?.render?.();
      });

      container.addEventListener("change", (event) => {
        const target = event.target;
        if (!(target instanceof HTMLInputElement) || target.type !== "checkbox") return;
        const changedValue = String(target.getAttribute("data-category-value") || "").trim();
        if (!changedValue) return;
        selectedCategories = normalizeSelectedCategoryState(selectedCategories, changedValue, target.checked, categoryOptions);
        isCategoryFilterOpen = true;
        controller?.reset?.();
      });

      container.addEventListener("keydown", (event) => {
        if (event.key !== "Escape" || !isCategoryFilterOpen) return;
        isCategoryFilterOpen = false;
        controller?.render?.();
      });

      document.addEventListener("click", (event) => {
        if (!isCategoryFilterOpen) return;
        if (container.contains(event.target)) return;
        isCategoryFilterOpen = false;
        controller?.render?.();
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
      setupPagination({
        container,
        items: primary,
        emptyMessage: `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for 4-year, primarily bachelor's-degree-granting institutions.`,
        downloadButtonId: "cuts-table-download",
        downloadFilename: "cuts-primary.csv",
        searchInput: primaryFilter,
        filterOnInput: false,
        searchValueResolver: getCommittedSearchValue,
        landingMode: true
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
          ["Institution", "State", "Sector", "Type of cuts", "Cut", "Date", "Source"],
          detailRows.map((cut) => [
            cut.institution_name || "",
            cut.state || "",
            cut.control_label || "",
            formatCategoriesForCsv(cut),
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
