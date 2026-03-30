(function () {
  const { loadJson, schoolUrl } = window.TrackerApp;
  const PAGE_SIZE = 25;
  const OTHER_PAGE_SIZE = 5;
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
  }

  function isPrimaryBachelorsInstitution(record) {
    const category = String(record?.category || "");
    return /primarily baccalaureate or above/i.test(category) && !/not primarily baccalaureate or above/i.test(category);
  }

  function renderCutItem(cut) {
    const metaParts = [cut.announcement_date || cut.announcement_year, cut.cut_type, cut.status].filter(Boolean);
    const term = cut.effective_term ? `<p class="small-meta">Effective term: ${cut.effective_term}</p>` : "";
    const source = cut.source_url
      ? `<p class="small-meta"><a href="${cut.source_url}" target="_blank" rel="noopener">Source</a>${cut.source_publication ? ` | ${cut.source_publication}` : ""}</p>`
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
        </ul>
      </div>
    `;
  }

  function formatAffectedCount(cut) {
    const affected = Number(cut.positions_affected || cut.faculty_affected);
    if (!Number.isFinite(affected) || affected <= 0) return "";
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

  function renderCutsTable(items) {
    if (!items || !items.length) return renderEmpty("No matched cuts are available.");
    const rows = items.map((cut) => `
      <tr>
        <td>${cut.unitid ? `<a href="${schoolUrl(cut.unitid, "cuts.html")}">${cut.institution_name || ""}</a>` : (cut.institution_name || "")}</td>
        <td>${cut.state || ""}</td>
        <td>${cut.control_label || ""}</td>
        <td>${(cut.program_name || "") + formatAffectedCount(cut)}</td>
        <td>${cut.announcement_date || cut.announcement_year || ""}</td>
        <td>${cut.source_url ? `<a href="${cut.source_url}" target="_blank" rel="noopener">Source</a>` : ""}</td>
      </tr>
    `).join("");
    return `
      <div class="history-table-wrap">
        <table class="history-table">
          <thead>
            <tr>
              <th>Institution</th>
              <th>State</th>
              <th>Sector</th>
              <th>Cut</th>
              <th>Date</th>
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
          unitid: school.unitid || cut.unitid || ""
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

  function renderCutsTablePage(items, page, pageSize, emptyMessage) {
    const totalPages = Math.max(1, Math.ceil(items.length / pageSize));
    const safePage = Math.min(Math.max(1, page), totalPages);
    const start = (safePage - 1) * pageSize;
    const pageItems = items.slice(start, start + pageSize);

    if (!pageItems.length) {
      return renderEmpty(emptyMessage);
    }

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
      ${renderCutsTable(pageItems)}
      <div class="pagination" aria-label="College cuts pages">
        ${pagination}
      </div>
    `;
  }

  function setupPagination(container, items, pageSize = PAGE_SIZE, emptyMessage = `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available.`, downloadButtonId = null, downloadFilename = "college-cuts.csv") {
    if (!container) return;
    let currentPage = 1;
    const downloadButton = downloadButtonId ? document.getElementById(downloadButtonId) : null;

    const render = () => {
      container.innerHTML = renderCutsTablePage(items, currentPage, pageSize, emptyMessage);
      const totalPages = Math.max(1, Math.ceil(items.length / pageSize));
      const safePage = Math.min(Math.max(1, currentPage), totalPages);
      const start = (safePage - 1) * pageSize;
      const pageItems = items.slice(start, start + pageSize);
      if (downloadButton) {
        downloadButton.classList.toggle("is-hidden", pageItems.length === 0);
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
    };

    render();
  }

  async function init() {
    const unitid = getParam("unitid");
    syncTabs(unitid);

    const cutsData = await loadJson("data/college_cuts.json");
    const container = document.getElementById("cuts-list");
    const otherContainer = document.getElementById("cuts-other-list");
    const title = document.getElementById("cuts-section-title");
    const otherTitle = document.getElementById("cuts-other-section-title");

    if (!unitid) {
      const recent = buildRecentCuts(cutsData);
      const primary = recent.filter(isPrimaryBachelorsInstitution);
      const other = recent.filter((cut) => !isPrimaryBachelorsInstitution(cut));
      setSectionVisible("cuts-other-list", true);
      title.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at 4-Year, Primarily Bachelor's-Degree-Granting Institutions`;
      if (otherTitle) otherTitle.textContent = `Cuts since ${MIN_DEFAULT_YEAR} at other institutions`;
      setupPagination(container, primary, PAGE_SIZE, `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for 4-year, primarily bachelor's-degree-granting institutions.`, "cuts-table-download", "cuts-primary.csv");
      setupPagination(otherContainer, other, OTHER_PAGE_SIZE, `No matched cuts from ${MIN_DEFAULT_YEAR} to the present are available for other institutions.`, "cuts-other-download", "cuts-other.csv");
      return;
    }

    const school = cutsData.schools?.[unitid];
    if (!school) {
      document.getElementById("cuts-school-name").textContent = "No matched cuts found";
      container.innerHTML = renderEmpty("No matched college cuts were found for this institution in the current dataset.");
      title.textContent = "Cuts";
      return;
    }

    document.getElementById("cuts-school-name").textContent = school.institution_name || "College Cuts";
    textOrEmpty("cuts-school-location", [school.city, school.state].filter(Boolean).join(", "));
    textOrEmpty("cuts-school-control", school.control_label || "");
    textOrEmpty("cuts-school-category", school.category || "");
    const scopeText = school.is_primary_tracker
      ? "This institution appears in the main tracker universe because it is a 4-year, primarily bachelor's-degree-granting school with finance data in this project."
      : "This institution appears here because it has matched college cuts data, even though it falls outside the main 4-year financial tracker universe.";
    const financeLinkText = school.financial_unitid
      ? `You can switch back to <a href="${schoolUrl(school.financial_unitid, "school.html")}">financial trends</a> or open <a href="${schoolUrl(unitid, "accreditation.html")}">accreditation</a>.`
      : `You can also open <a href="${schoolUrl(unitid, "accreditation.html")}">accreditation</a> for this institution.`;
    const overview = document.getElementById("cuts-overview");
    if (overview) {
      overview.classList.remove("is-hidden");
      overview.innerHTML = `<p>${scopeText}</p><p>This page shows the latest matched college cuts for ${school.institution_name}. ${financeLinkText}</p>`;
    }
    title.textContent = school.cut_count === 1 ? "Cut" : `Cuts (${school.cut_count})`;
    setSectionVisible("cuts-other-list", false);
    const mainDownload = document.getElementById("cuts-table-download");
    const otherDownload = document.getElementById("cuts-other-download");
    if (mainDownload) mainDownload.classList.add("is-hidden");
    if (otherDownload) otherDownload.classList.add("is-hidden");
    if (otherContainer) otherContainer.innerHTML = "";
    if (otherTitle) otherTitle.textContent = "";
    container.innerHTML = (school.cuts || []).length
      ? school.cuts.map(renderCutItem).join("") +
        renderCutsTable((school.cuts || []).map((cut) => ({ ...cut, institution_name: school.institution_name, state: school.state, control_label: school.control_label, unitid: school.unitid }))) +
        renderInstitutionLinks(school.unitid, school.financial_unitid)
      : renderEmpty("No matched cuts were found for this institution.");
  }

  init().catch((error) => {
    console.error(error);
    const container = document.getElementById("cuts-list");
    if (container) container.innerHTML = renderEmpty("The college cuts data could not be loaded.");
  });
})();
