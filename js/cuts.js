(function () {
  const { loadJson, schoolUrl, escapeHtml, safeUrl } = window.TrackerApp;
  const PAGE_SIZE = 25;
  const OTHER_PAGE_SIZE = 5;
  const CLOSURE_PAGE_SIZE = 10;
  const MIN_DEFAULT_YEAR = 2024;

  function getParam(name) {
    const params = new URLSearchParams(window.location.search);
    return params.get(name);
  }

  function textOrEmpty(id, value) {
    const el = document.getElementById(id);
    if (el) el.textContent = value || "";
  }

  function syncTabs(unitid) {
    const finances = document.getElementById("tab-finances");
    if (finances) {
      finances.href = unitid ? `school.html?unitid=${encodeURIComponent(unitid)}` : "index.html";
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

  function isPrimaryBachelorsInstitution(record) {
    return record?.is_primary_tracker === true;
  }

  function renderCutItem(cut) {
    const metaParts = [cut.announcement_date || cut.announcement_year, cut.cut_type, cut.status].filter(Boolean);
    const term = cut.effective_term ? `<p class="small-meta">Effective term: ${cut.effective_term}</p>` : "";
    const source = safeUrl(cut.source_url)
      ? `<p class="small-meta"><a href="${safeUrl(cut.source_url)}" target="_blank" rel="noopener">Source</a>${cut.source_publication ? ` | ${escapeHtml(cut.source_publication)}` : ""}</p>`
      : "";
    return `
      <article class="data-card">
        <h3>${cut.program_name || "Unnamed cut"}</h3>
        <p class="small-meta">${metaParts.join(" | ")}</p>
        ${term}
        ${cut.notes ? `<p>${cut.notes}</p>` : ""}
        ${source}
      </article>
    `;
  }

  function renderInstitutionLinks(unitid, financialUnitid) {
    if (!unitid) return "";
    const financeLink = financialUnitid
      ? `<li><a href="${schoolUrl(financialUnitid, "school.html")}">Finances</a></li>`
      : "";
    return `
      <div class="related-links">
        <p><strong>Explore this institution:</strong></p>
        <ul class="link-list">
          ${financeLink}
          <li><a href="${schoolUrl(unitid, "accreditation.html")}">Accreditation</a></li>
          <li><a href="${schoolUrl(financialUnitid || unitid, "research.html")}">Research Funding Cuts</a></li>
        </ul>
      </div>
    `;
  }

  function formatAffectedCount(cut) {
    const affected = Number(cut.positions_affected || cut.faculty_affected);
    if (!Number.isFinite(affected) || affected <= 0) return "";
    // Skip if program_name already contains the count (e.g. "Staff layoff (20 positions affected)")
    const label = cut.program_name || "";
    if (label.includes("positions affected") || label.includes("students affected")) return "";
    return ` (${affected} positions affected)`;
  }

  function renderEmpty(message) {
    return `<div class="empty-state"><p>${message}</p></div>`;
  }

  function setSectionVisible(id, show) {
    const node = document.getElementById(id);
    const section = node ? node.closest(".data-card") : null;
    if (section) {
      section.classList.toggle("is-hidden", !show);
      section.setAttribute("aria-hidden", show ? "false" : "true");
    }
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

  function financePageLink(unitid, label) {
    return unitid
      ? `<a href="${schoolUrl(unitid, "school.html")}">${label || ""}</a>`
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

  function renderCutsTable(items, sortState) {
    if (!items || !items.length) return renderEmpty("No matched cuts are available.");
    const rows = items.map((cut) => `
      <tr>
        <td>${financePageLink(cut.financial_unitid, escapeHtml(cut.institution_name))}</td>
        <td>${escapeHtml(cut.state)}</td>
        <td>${escapeHtml(cut.control_label)}</td>
        <td>${escapeHtml(cut.program_name || "") + formatAffectedCount(cut)}</td>
        <td>${cut.announcement_date || cut.announcement_year || ""}</td>
        <td>${safeUrl(cut.source_url) ? `<a href="${safeUrl(cut.source_url)}" target="_blank" rel="noopener">Source</a>` : ""}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>${renderSortControls("institution_name", sortState, "Institution")}</th>
              <th>${renderSortControls("state", sortState, "State")}</th>
              <th>Sector</th>
              <th>Cut</th>
              <th>${renderSortControls("announcement_date", sortState, "Date")}</th>
              <th>Source</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  function sortClosures(items, sortState) {
    const sorted = (items || []).slice();
    const direction = sortState?.direction === "desc" ? -1 : 1;
    sorted.sort((a, b) => {
      if (sortState?.key === "institution_name") {
        const primary = compareText(a.institution_name, b.institution_name) * direction;
        if (primary !== 0) return primary;
        return compareText(a.close_date, b.close_date);
      }
      if (sortState?.key === "state") {
        const primary = compareText(a.state, b.state) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      if (sortState?.key === "close_date") {
        const primary = compareText(a.close_date, b.close_date) * direction;
        if (primary !== 0) return primary;
        return compareText(a.institution_name, b.institution_name);
      }
      return compareText(a.close_date, b.close_date);
    });
    return sorted;
  }

  function renderClosuresTable(items, sortState) {
    if (!items || !items.length) return renderEmpty("No matched closures are available.");
    const rows = items.map((closure) => `
      <tr>
        <td>${financePageLink(closure.unitid, escapeHtml(closure.institution_name))}</td>
        <td>${escapeHtml(closure.state)}</td>
        <td>${escapeHtml(closure.control_label)}</td>
        <td>${closure.close_date_display || closure.close_date || ""}</td>
        <td>Federal data</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>${renderSortControls("institution_name", sortState, "Institution")}</th>
              <th>${renderSortControls("state", sortState, "State")}</th>
              <th>Sector</th>
              <th>${renderSortControls("close_date", sortState, "Closure date")}</th>
              <th>Source</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
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
    const totalPages = Math.max(1, Math.ceil(items.length / pageSize));
    const safePage = Math.min(Math.max(1, page), totalPages);
    const start = (safePage - 1) * pageSize;
    const pageItems = items.slice(start, start + pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    const pagination = Array.from({ length: totalPages }, (_, idx) => idx + 1)
      .map((pageNumber) => {
        const currentAttr = pageNumber === safePage ? ' aria-current="page"' : "";
        return `<button type="button" class="pagination-button${pageNumber === safePage ? " is-active" : ""}" data-page="${pageNumber}" aria-label="Page ${pageNumber}"${currentAttr}>${pageNumber}</button>`;
      })
      .join("");

    return `
      ${renderCutsTable(pageItems, sortState)}
      <div class="pagination" aria-label="College cuts pages">
        ${pagination}
      </div>
    `;
  }

  function renderClosuresTablePage(items, page, pageSize, emptyMessage, sortState) {
    const totalPages = Math.max(1, Math.ceil(items.length / pageSize));
    const safePage = Math.min(Math.max(1, page), totalPages);
    const start = (safePage - 1) * pageSize;
    const pageItems = items.slice(start, start + pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

    const pagination = Array.from({ length: totalPages }, (_, idx) => idx + 1)
      .map((pageNumber) => {
        const currentAttr = pageNumber === safePage ? ' aria-current="page"' : "";
        return `<button type="button" class="pagination-button${pageNumber === safePage ? " is-active" : ""}" data-page="${pageNumber}" aria-label="Page ${pageNumber}"${currentAttr}>${pageNumber}</button>`;
      })
      .join("");

    return `
      ${renderClosuresTable(pageItems, sortState)}
      <div class="pagination" aria-label="Closure pages">
        ${pagination}
      </div>
    `;
  }

  function setupPagination(container, items, pageSize = PAGE_SIZE, emptyMessage = `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available.`, downloadButtonId = null, downloadFilename = "college-cuts.csv", searchInput = null) {
    if (!container) return;
    let currentPage = 1;
    let sortState = { key: "announcement_date", direction: "desc" };
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      const filteredItems = filterByInstitution(items, searchInput?.value || "");
      const sortedItems = sortCuts(filteredItems, sortState);
      container.innerHTML = renderCutsTablePage(sortedItems, currentPage, pageSize, emptyMessage, sortState);
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
        downloadButton.setAttribute("aria-hidden", pageItems.length === 0 ? "true" : "false");
        downloadButton.onclick = () => downloadRowsCsv(
          downloadFilename,
          ["Institution", "State", "Sector", "Cut", "Date", "Source"],
          pageItems.map((cut) => [
            cut.institution_name || "",
            cut.state || "",
            cut.control_label || "",
            (cut.program_name || "") + formatAffectedCount(cut),
            cut.announcement_date || cut.announcement_year || "",
            cut.source_url || ""
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
          const key = button.dataset.sortKey || "announcement_date";
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

  function setupClosurePagination(container, items, pageSize = CLOSURE_PAGE_SIZE, emptyMessage = "No tracker-universe closures from 2024-2026 are available.", downloadButtonId = null, downloadFilename = "college-closures.csv", searchInput = null) {
    if (!container) return;
    let currentPage = 1;
    let sortState = { key: "close_date", direction: "asc" };
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      const filteredItems = filterByInstitution(items, searchInput?.value || "");
      const sortedItems = sortClosures(filteredItems, sortState);
      container.innerHTML = renderClosuresTablePage(sortedItems, currentPage, pageSize, emptyMessage, sortState);
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
        downloadButton.setAttribute("aria-hidden", pageItems.length === 0 ? "true" : "false");
        downloadButton.onclick = () => downloadRowsCsv(
          downloadFilename,
          ["Institution", "State", "Sector", "Closure date", "Source"],
          pageItems.map((closure) => [
            closure.institution_name || "",
            closure.state || "",
            closure.control_label || "",
            closure.close_date_display || closure.close_date || "",
            "Federal data"
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
          const key = button.dataset.sortKey || "close_date";
          const direction = button.dataset.sortDirection || "asc";
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

  function formatDateLong(dateText) {
    if (!dateText) return "";
    const parsed = new Date(`${dateText}T00:00:00`);
    if (Number.isNaN(parsed.getTime())) return dateText;
    return parsed.toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" });
  }

  function isFourYearInstitution(record) {
    const category = String(record?.category || "");
    return /4-year or above/i.test(category) || /primarily baccalaureate or above/i.test(category);
  }

  function buildRecentClosures(closureData, schoolsIndex) {
    const schoolLookup = new Map((schoolsIndex || []).map((school) => [String(school.unitid || ""), school]));
    return Object.values(closureData?.schools || {})
      .map((closure) => {
        const school = schoolLookup.get(String(closure.unitid || "")) || {};
        return {
          ...closure,
          institution_name: school.institution_name || closure.institution_name || "",
          state: school.state || closure.state || "",
          control_label: school.control_label || "",
          category: school.category || "",
          close_date_display: formatDateLong(closure.close_date || "")
        };
      })
      .filter((closure) => {
        const year = Number(closure.close_year || "");
        return Number.isFinite(year) && year >= 2024 && year <= 2026 && isFourYearInstitution(closure);
      });
  }

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid);

    const cutsData = await loadJson("data/college_cuts.json");
    const closureData = await loadJson("data/closure_status_by_unitid.json");
    const schoolsIndex = await loadJson("data/schools_index.json");
    const container = document.getElementById("cuts-list");
    const otherContainer = document.getElementById("cuts-other-list");
    const closuresContainer = document.getElementById("cuts-closures-list");
    const title = document.getElementById("cuts-section-title");
    const otherTitle = document.getElementById("cuts-other-section-title");
    const closuresTitle = document.getElementById("cuts-closures-section-title");

    if (!unitid) {
      document.getElementById("cuts-school-name").textContent = "";
      document.getElementById("cuts-school-name").classList.add("is-hidden");
      document.getElementById("cuts-school-name").setAttribute("aria-hidden", "true");
      const recent = buildRecentCuts(cutsData);
      const primary = recent.filter(isPrimaryBachelorsInstitution);
      const other = recent.filter((cut) => !isPrimaryBachelorsInstitution(cut));
      const closures = buildRecentClosures(closureData, schoolsIndex);
      setSectionVisible("cuts-other-list", true);
      setSectionVisible("cuts-closures-list", false);
      title.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at 4-year institutions that primarily grant bachelors degrees`;
      if (otherTitle) otherTitle.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at other institutions`;
      if (closuresTitle) closuresTitle.textContent = "Closures at 4-year institutions, 2024-2026";
      const primaryFilter = document.getElementById("cuts-filter");
      const otherFilter = document.getElementById("cuts-other-filter");
      const closuresFilter = document.getElementById("cuts-closures-filter");
      setupPagination(container, primary, PAGE_SIZE, `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for 4-year, primarily bachelor's-degree-granting institutions.`, "cuts-table-download", "cuts-primary.csv", primaryFilter);
      setupPagination(otherContainer, other, OTHER_PAGE_SIZE, `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for other institutions.`, "cuts-other-download", "cuts-other.csv", otherFilter);
      setupClosurePagination(closuresContainer, closures, CLOSURE_PAGE_SIZE, "No tracker-universe closures from 2024-2026 are available.", "cuts-closures-download", "cuts-closures.csv", closuresFilter);
      return;
    }

    const school = cutsData.schools?.[unitid];
    if (!school) {
      document.getElementById("cuts-school-name").textContent = "No matched cuts found";
      document.getElementById("cuts-school-name").classList.remove("is-hidden");
      document.getElementById("cuts-school-name").setAttribute("aria-hidden", "false");
      container.innerHTML = renderEmpty("No matched college cuts were found for this institution in the current dataset.");
      title.textContent = "Cuts";
      return;
    }

    document.getElementById("cuts-school-name").textContent = school.institution_name || "College Cuts";
    document.getElementById("cuts-school-name").classList.remove("is-hidden");
      document.getElementById("cuts-school-name").setAttribute("aria-hidden", "false");
    textOrEmpty("cuts-school-location", [school.city, school.state].filter(Boolean).join(", "));
    textOrEmpty("cuts-school-control", school.control_label || "");
    textOrEmpty("cuts-school-category", school.category || "");
    const scopeText = school.is_primary_tracker
      ? "This institution appears in the main tracker universe because it is a 4-year, primarily bachelor's-degree-granting school with finance data in this project."
      : "This institution appears here because it has matched college cuts data, even though it falls outside the main 4-year financial tracker universe.";
    const financeLinkText = school.financial_unitid
      ? `You can switch back to <a href="${schoolUrl(school.financial_unitid, "school.html")}">financial trends</a>, open <a href="${schoolUrl(unitid, "accreditation.html")}">accreditation</a>, or view <a href="${schoolUrl(school.financial_unitid, "research.html")}">research funding cuts</a>.`
      : `You can also open <a href="${schoolUrl(unitid, "accreditation.html")}">accreditation</a> for this institution.`;
    const overview = document.getElementById("cuts-overview");
    if (overview) {
      overview.classList.remove("is-hidden");
      overview.setAttribute("aria-hidden", "false");
      overview.innerHTML = `<p>${scopeText}</p><p>This page shows the latest matched college cuts for ${escapeHtml(school.institution_name)}. ${financeLinkText}</p>`;
    }
    title.textContent = school.cut_count === 1 ? "Cut" : `Cuts (${school.cut_count})`;
    setSectionVisible("cuts-other-list", false);
    setSectionVisible("cuts-closures-list", false);
    const mainDownload = document.getElementById("cuts-table-download");
    const otherDownload = document.getElementById("cuts-other-download");
    const closuresDownload = document.getElementById("cuts-closures-download");
    if (mainDownload) {
      mainDownload.classList.add("is-hidden");
      mainDownload.setAttribute("aria-hidden", "true");
    }
    if (otherDownload) {
      otherDownload.classList.add("is-hidden");
      otherDownload.setAttribute("aria-hidden", "true");
    }
    if (closuresDownload) {
      closuresDownload.classList.add("is-hidden");
      closuresDownload.setAttribute("aria-hidden", "true");
    }
    if (otherContainer) otherContainer.innerHTML = "";
    if (closuresContainer) closuresContainer.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";
    if (closuresTitle) closuresTitle.textContent = "";
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
      unitid: school.unitid
    }));
    const renderDetailTable = () => {
      container.innerHTML = school.cuts.map(renderCutItem).join("") +
        renderCutsTable(detailRows, detailSortState) +
        renderInstitutionLinks(school.unitid, school.financial_unitid);
      container.querySelectorAll(".sort-button").forEach((button) => {
        button.addEventListener("click", () => {
          const key = button.dataset.sortKey || "announcement_date";
          const direction = button.dataset.sortDirection || "desc";
          if (detailSortState.key === key && detailSortState.direction === direction) return;
          detailSortState = { key, direction };
          renderDetailTable();
        });
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
