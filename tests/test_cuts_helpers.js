/**
 * Narrow regression tests for cuts.js category-shape handling.
 *
 * Run with: node tests/test_cuts_helpers.js
 */

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const ROOT = path.resolve(__dirname, "..");
const CUTS_SRC = fs.readFileSync(path.join(ROOT, "js", "cuts.js"), "utf8");

function loadCutsHelpers() {
  const helperSource = `${CUTS_SRC.split("async function init()")[0]}
  window.__cutsTest = {
    normalizeDisplayCategories,
    resolveDisplayCategories,
    buildRecentCuts
  };
})();`;

  const noop = () => {};
  const context = {
    console: { error() {} },
    document: {
      getElementById: () => null
    },
    window: {
      TrackerApp: {
        loadJson: async () => ({}),
        escapeHtml: (value) => String(value ?? ""),
        getParam: () => "",
        renderEmpty: () => "",
        renderExternalLink: () => "",
        renderPaginationButtons: () => "",
        renderSortableHeader: () => "",
        paginateItems: () => ({ pageItems: [], currentPage: 1, totalPages: 1 }),
        bindSortControls: noop,
        setDataCardVisible: noop,
        downloadRowsCsv: noop,
        compareText: () => 0,
        compareDateDesc: () => 0,
        renderHistoryTable: () => "",
        renderSchoolLinkCell: () => "",
        renderExternalLinkCell: () => "",
        findRelatedIndexRecord: () => null,
        isNumericUnitid: (value) => /^[0-9]+$/.test(String(value || "")),
        isPrimaryTrackerInstitution: (record) => record?.is_primary_tracker === true,
        syncTabs: noop,
        renderRelatedInstitutionLinks: () => "",
        renderDataAsOf: noop,
        makeTableController: () => null,
        cleanCutLabel: (value) => String(value ?? ""),
        getCommittedSearchValue: () => ""
      }
    }
  };
  context.global = context;
  vm.runInNewContext(helperSource, context, { filename: "js/cuts.js" });
  return context.window.__cutsTest;
}

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

let passed = 0;
let failed = 0;

function run(name, fn) {
  try {
    fn();
    console.log(`  PASS: ${name}`);
    passed++;
  } catch (error) {
    console.log(`  FAIL: ${name}: ${error.message}`);
    failed++;
  }
}

console.log("\n=== Cuts Helper Tests ===\n");

const cuts = loadCutsHelpers();

run("resolveDisplayCategories keeps explicit string display_categories ahead of inference", () => {
  const resolved = cuts.resolveDisplayCategories({
    display_categories: "Staff layoffs / furloughs",
    cut_type: "program_suspension",
    program_name: "History major suspended"
  });

  assert(Array.isArray(resolved), "Expected resolved categories array");
  assert(resolved.length === 1, `Expected one explicit category, got ${JSON.stringify(resolved)}`);
  assert(resolved[0] === "Staff layoffs / furloughs", `Expected explicit category to win, got ${JSON.stringify(resolved)}`);
});

run("buildRecentCuts preserves string-shaped landing display_categories as an explicit array", () => {
  const recent = cuts.buildRecentCuts({
    "123456": {
      unitid: "123456",
      institution_name: "Example University",
      state: "Example State",
      control_label: "Public",
      is_primary_tracker: true,
      landing_cuts: [{
        announcement_date: "2026-07-01",
        announcement_year: 2026,
        program_name: "History major suspended",
        cut_type: "program_suspension",
        display_categories: "Staff layoffs / furloughs"
      }]
    }
  });

  assert(recent.length === 1, `Expected one recent cut row, got ${recent.length}`);
  assert(Array.isArray(recent[0].display_categories), "Expected recent cut display_categories array");
  assert(
    JSON.stringify(recent[0].display_categories) === JSON.stringify(["Staff layoffs / furloughs"]),
    `Expected explicit landing category array, got ${JSON.stringify(recent[0].display_categories)}`
  );
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
