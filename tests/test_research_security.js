/**
 * Malicious fixture tests for research-page rendering.
 *
 * This executes the shipped research.js controller against a small in-memory
 * fixture so JSON-backed agency labels stay escaped in summary and table HTML.
 */

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const ROOT = path.resolve(__dirname, "..");

function escapeHtml(value) {
  return String(value ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function makeElement(id) {
  const attributes = new Map();
  return {
    id,
    innerHTML: "",
    textContent: "",
    onclick: null,
    dataset: {},
    classList: {
      add() {},
      remove() {},
      toggle() {}
    },
    setAttribute(name, value) {
      attributes.set(name, String(value));
    },
    removeAttribute(name) {
      attributes.delete(name);
    },
    closest() {
      return this;
    }
  };
}

function makeDocument(ids) {
  const elements = new Map(ids.map((id) => [id, makeElement(id)]));
  return {
    elements,
    getElementById(id) {
      if (!elements.has(id)) elements.set(id, makeElement(id));
      return elements.get(id);
    }
  };
}

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

let passed = 0;
let failed = 0;

async function run(name, fn) {
  try {
    await fn();
    console.log(`  PASS: ${name}`);
    passed++;
  } catch (error) {
    console.log(`  FAIL: ${name}: ${error.message}`);
    failed++;
  }
}

async function renderResearchFixture() {
  const document = makeDocument([
    "research-summary-grid",
    "research-list",
    "research-state-summary",
    "research-state-summary-card",
    "research-section-title",
    "research-other-list",
    "research-other-section-title",
    "research-filter",
    "research-school-name",
    "research-school-location",
    "research-school-control",
    "research-school-category",
    "research-overview",
    "research-table-download",
    "research-other-download",
    "tab-finances",
    "tab-cuts",
    "tab-accreditation",
    "tab-research"
  ]);

  const fixture = {
    schools: {
      "malicious-1": {
        unitid: "malicious-1",
        financial_unitid: "123456",
        institution_name: "Malicious Fixture University",
        state: "NY",
        control_label: "Private nonprofit",
        category: "Primarily baccalaureate or above",
        total_disrupted_award_remaining: 1000,
        total_disrupted_grants: 1,
        agency_summary: [
          {
            agency_label: '<img src=x onerror="alert(1)">',
            disrupted_grants: 1,
            disrupted_award_remaining: 1000
          }
        ],
        grants: [
          {
            agency: 'nih"><svg onload="alert(1)">',
            project_title: "<b>Injected project</b>",
            grant_id: 'grant" onclick="alert(1)',
            award_remaining: 1000,
            termination_date: "2025-01-01",
            source_url: "https://example.edu/source"
          }
        ]
      }
    }
  };

  const context = {
    console,
    URLSearchParams,
    document,
    window: {
      location: { search: "?unitid=malicious-1" },
      TrackerApp: {
        loadJson: async () => fixture,
        schoolUrl: (unitid, page = "school.html") => `${page}?unitid=${encodeURIComponent(unitid)}`,
        escapeHtml,
        renderExternalLink: (url, label = "Source") => `<a href="${escapeHtml(url)}">${escapeHtml(label)}</a>`,
        renderSchoolLink: (unitid, label, page = "school.html") => `<a href="${page}?unitid=${encodeURIComponent(unitid)}">${escapeHtml(label)}</a>`,
        renderPaginationButtons: () => "",
        renderSortableHeader: (key, sortState, label) => `<th>${escapeHtml(label)}</th>`,
        compareText: (a, b) => String(a || "").localeCompare(String(b || ""), undefined, { sensitivity: "base" }),
        compareDateDesc: (a, b) => String(b || "").localeCompare(String(a || "")),
        renderHtmlCell: (html) => ({ __trackerHtml: String(html ?? "") }),
        renderTextCell: (value) => ({ __trackerCell: "text", value }),
        renderSchoolLinkCell: (unitid, label, page = "school.html") => ({ __trackerCell: "school-link", unitid, label, page }),
        renderExternalLinkCell: (url, label = "Source") => ({ __trackerCell: "external-link", url, label }),
        isPrimaryTrackerInstitution: (record) => record?.is_primary_tracker === true,
        renderHistoryTable: ({ headers = [], rows = [] } = {}) => `
          <div class="history-table-wrap">
            <table class="history-table">
              <thead><tr>${headers.join("")}</tr></thead>
              <tbody>${rows.map((row) => `<tr>${row.map((cell) => {
                let cellHtml = "";
                if (cell && typeof cell === "object" && Object.prototype.hasOwnProperty.call(cell, "__trackerHtml")) {
                  cellHtml = cell.__trackerHtml;
                } else if (cell && typeof cell === "object" && cell.__trackerCell === "text") {
                  cellHtml = escapeHtml(cell.value);
                } else if (cell && typeof cell === "object" && cell.__trackerCell === "school-link") {
                  cellHtml = context.window.TrackerApp.renderSchoolLink(cell.unitid, cell.label, cell.page);
                } else if (cell && typeof cell === "object" && cell.__trackerCell === "external-link") {
                  cellHtml = context.window.TrackerApp.renderExternalLink(cell.url, cell.label);
                } else {
                  cellHtml = escapeHtml(cell);
                }
                return `<td>${cellHtml}</td>`;
              }).join("")}</tr>`).join("")}</tbody>
            </table>
          </div>
        `,
        paginateItems: (items) => ({ totalPages: 1, currentPage: 1, pageItems: items || [] }),
        focusAfterRender: () => {},
        bindSortControls: () => {},
        setupPaginatedTable: ({ container, items, renderPage, pageSize, initialSortState }) => {
          container.innerHTML = renderPage(items, 1, pageSize, initialSortState);
          return { render() {} };
        },
        filterByInstitution: (items) => items,
        setDataCardVisible: () => {},
        downloadRowsCsv: () => {},
        syncTabs: () => {},
        renderRelatedInstitutionLinks: () => ""
      }
    }
  };
  context.global = context;

  const source = fs.readFileSync(path.join(ROOT, "js", "research.js"), "utf8");
  vm.runInNewContext(source, context, { filename: "js/research.js" });
  await new Promise((resolve) => setImmediate(resolve));

  return document;
}

console.log("\n=== Research Rendering Security Tests ===\n");

run("research summary and grant rows escape malicious agency labels", async () => {
  const document = await renderResearchFixture();
  const summaryHtml = document.getElementById("research-summary-grid").innerHTML;
  const tableHtml = document.getElementById("research-list").innerHTML;

  assert(!summaryHtml.includes("<img"), "Summary should not contain raw img tags");
  assert(!summaryHtml.includes('onerror="'), "Summary should not contain raw malicious event-handler attributes");
  assert(summaryHtml.includes("&lt;img"), "Summary should preserve agency label as escaped text");

  assert(!tableHtml.includes("<svg"), "Grant table should not contain raw SVG tags from agency values");
  assert(!tableHtml.includes('onload="'), "Grant table should not contain raw malicious event-handler attributes");
  assert(tableHtml.includes("&lt;SVG"), "Grant table should preserve unknown agency code as escaped text");
});

process.on("beforeExit", () => {
  console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
  if (failed > 0) process.exitCode = 1;
});
