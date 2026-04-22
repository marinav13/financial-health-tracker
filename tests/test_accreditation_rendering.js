/**
 * Accreditation rendering smoke tests.
 *
 * These execute the shipped accreditation.js controller against a fixture so
 * the browser honors export-level display decisions instead of reclassifying
 * scraped action phrasing with a second regex.
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
  return {
    id,
    innerHTML: "",
    textContent: "",
    dataset: {},
    classList: {
      add() {},
      remove() {}
    },
    setAttribute() {},
    removeAttribute() {},
    closest() {
      return this;
    }
  };
}

function makeDocument(ids) {
  const elements = new Map(ids.map((id) => [id, makeElement(id)]));
  return {
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

async function renderAccreditationFixture() {
  const document = makeDocument([
    "accreditation-limitations",
    "accreditation-school-name",
    "accreditation-school-location",
    "accreditation-school-control",
    "accreditation-school-category",
    "accreditation-overview",
    "accreditation-status",
    "accreditation-other-status",
    "accreditation-other-title",
    "accreditation-table-download",
    "accreditation-other-download",
    "tab-finances",
    "tab-cuts",
    "tab-accreditation",
    "tab-research"
  ]);

  const fixture = {
    covered_accreditors: ["MSCHE"],
    schools: {
      "100": {
        unitid: "100",
        financial_unitid: "100",
        has_financial_profile: true,
        is_primary_tracker: true,
        institution_name: "Example University",
        city: "Boston",
        state: "Massachusetts",
        control_label: "Public",
        category: "Degree-granting, primarily baccalaureate or above",
        actions: [
          {
            accreditor: "MSCHE",
            action_type: "warning",
            action_label: "Warning",
            action_status: "active",
            action_date: "January 2020",
            notes: "Public warning issued",
            source_url: "https://example.org/accreditation",
            display_action: true
          },
          {
            accreditor: "MSCHE",
            action_type: "warning",
            action_label: "Hidden exported action",
            action_status: "active",
            action_date: "2020-01-01",
            notes: "This would pass the old browser regex but should stay hidden.",
            source_url: "https://example.org/hidden",
            display_action: false
          }
        ]
      }
    }
  };

  const trackerApp = {
    loadJson: async () => fixture,
    escapeHtml,
    renderPaginationButtons: () => "",
    paginateItems: (items) => ({ totalPages: 1, currentPage: 1, pageItems: items || [] }),
    setupPaginatedTable: () => {},
    filterByInstitution: (items) => items,
    setDataCardVisible: () => {},
    downloadRowsCsv: () => {},
    isPrimaryTrackerInstitution: (record) => record?.is_primary_tracker === true,
    syncTabs: () => {},
    renderRelatedInstitutionLinks: () => "",
    renderSchoolLinkCell: (unitid, label, page = "school.html") => ({ __trackerCell: "school-link", unitid, label, page }),
    renderExternalLinkCell: (url, label = "Source") => ({ __trackerCell: "external-link", url, label }),
    renderSchoolLink: (unitid, label, page = "school.html") => `<a href="${page}?unitid=${encodeURIComponent(unitid)}">${escapeHtml(label)}</a>`,
    renderExternalLink: (url, label = "Source") => `<a href="${escapeHtml(url)}">${escapeHtml(label)}</a>`,
    renderHistoryTable: ({ headers = [], rows = [] } = {}) => `
      <table>
        <thead><tr>${headers.join("")}</tr></thead>
        <tbody>${rows.map((row) => `<tr>${row.map((cell) => {
          if (cell && typeof cell === "object" && cell.__trackerCell === "school-link") {
            return `<td>${trackerApp.renderSchoolLink(cell.unitid, cell.label, cell.page)}</td>`;
          }
          if (cell && typeof cell === "object" && cell.__trackerCell === "external-link") {
            return `<td>${trackerApp.renderExternalLink(cell.url, cell.label)}</td>`;
          }
          return `<td>${escapeHtml(cell)}</td>`;
        }).join("")}</tr>`).join("")}</tbody>
      </table>
    `
  };

  const context = {
    console,
    Date,
    URLSearchParams,
    document,
    window: {
      location: { search: "?unitid=100" },
      TrackerApp: trackerApp
    }
  };
  context.global = context;

  const source = fs.readFileSync(path.join(ROOT, "js", "accreditation.js"), "utf8");
  vm.runInNewContext(source, context, { filename: "js/accreditation.js" });
  await new Promise((resolve) => setImmediate(resolve));

  return document;
}

console.log("\n=== Accreditation Rendering Tests ===\n");

run("honors display_action=false from export", async () => {
  const document = await renderAccreditationFixture();
  const html = document.getElementById("accreditation-status").innerHTML;
  assert(html.includes("Warning"), "Expected visible warning action to render.");
  assert(!html.includes("Hidden exported action"), "display_action=false action should not render.");
});

run("accepts month-name accreditation dates", async () => {
  const document = await renderAccreditationFixture();
  const html = document.getElementById("accreditation-status").innerHTML;
  assert(html.includes("January 2020"), "Expected month-name date action to render as recent.");
});

process.on("exit", () => {
  console.log(`\nPassed: ${passed}`);
  console.log(`Failed: ${failed}`);
  if (failed > 0) process.exitCode = 1;
});
