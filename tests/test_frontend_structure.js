/**
 * Frontend structure smoke tests.
 * Checks that each HTML file contains the required structural elements
 * (skip links, main landmark, masthead, nav tabs, data containers).
 *
 * Run with: node tests/test_frontend_structure.js
 */

const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..");

const PAGES = [
  {
    name: "Homepage (index.html)",
    file: "index.html",
    checks: [
      { pattern: /<a[^>]+href="#main"[^>]*class="skip-link"/, message: 'Skip-to-main link targeting "#main"' },
      { pattern: /<main[^>]+id="main"/, message: '<main id="main">' },
      { pattern: /class="masthead"/, message: "Masthead element" },
      { pattern: /class="top-tabs"[^>]*aria-label="Sections"/, message: 'Top nav with aria-label="Sections"' },
      { pattern: /<a[^>]+href="index\.html"[^>]*aria-current="page"[^>]*>Finances<\/a>/, message: "Finances nav tab with aria-current" },
      { pattern: /<a[^>]+href="cuts\.html"[^>]*>College Cuts<\/a>/, message: "College Cuts nav tab" },
      { pattern: /<a[^>]+href="accreditation\.html"[^>]*>Accreditation<\/a>/, message: "Accreditation nav tab" },
      { pattern: /<a[^>]+href="research\.html"[^>]*>Research Funding Cuts<\/a>/, message: "Research nav tab" },
      { pattern: /class="masthead-title"[^>]*>College Financial Health Explorer<\/div>/, message: "Masthead title" },
      { pattern: /class="search-panel"/, message: "Search panel" },
      { pattern: /id="school-search"/, message: "Search input" },
      { pattern: /id="search-results"/, message: "Search results container" },
      { pattern: /role="listbox"/, message: 'Search results role="listbox"' },
    ],
  },
  {
    name: "College Cuts (cuts.html)",
    file: "cuts.html",
    checks: [
      { pattern: /<a[^>]+href="#main"[^>]*class="skip-link"/, message: 'Skip-to-main link targeting "#main"' },
      { pattern: /<main[^>]+id="main"/, message: '<main id="main">' },
      { pattern: /class="masthead"/, message: "Masthead element" },
      { pattern: /class="top-tabs"[^>]*aria-label="Sections"/, message: 'Top nav with aria-label="Sections"' },
      { pattern: /<a[^>]+class="top-tab is-active"[^>]+href="cuts\.html"[^>]+aria-current="page"[^>]*>College Cuts<\/a>/, message: "Active College Cuts tab with aria-current" },
      { pattern: /id="cuts-list"[^>]*aria-live="polite"/, message: 'cuts-list with aria-live="polite"' },
      { pattern: /id="cuts-other-list"[^>]*aria-live="polite"/, message: 'cuts-other-list with aria-live="polite"' },
      { pattern: /id="cuts-filter"/, message: "Primary filter input" },
      { pattern: /id="cuts-other-filter"/, message: "Other filter input" },
      { pattern: /class="table-filter-label"[^>]+for="cuts-filter"/, message: 'Visible label for cuts-filter' },
      { pattern: /class="table-filter-label"[^>]+for="cuts-other-filter"/, message: 'Visible label for cuts-other-filter' },
      { pattern: /class="masthead-title"[^>]*>College Financial Health Explorer<\/div>/, message: "Masthead title" },
    ],
  },
  {
    name: "Research Funding (research.html)",
    file: "research.html",
    checks: [
      { pattern: /<a[^>]+href="#main"[^>]*class="skip-link"/, message: 'Skip-to-main link targeting "#main"' },
      { pattern: /<main[^>]+id="main"/, message: '<main id="main">' },
      { pattern: /class="masthead"/, message: "Masthead element" },
      { pattern: /class="top-tabs"[^>]*aria-label="Sections"/, message: 'Top nav with aria-label="Sections"' },
      { pattern: /<a[^>]+class="top-tab is-active"[^>]+href="research\.html"[^>]+aria-current="page"[^>]*>Research Funding Cuts<\/a>/, message: "Active Research nav tab with aria-current" },
      { pattern: /id="research-list"[^>]*aria-live="polite"/, message: 'research-list with aria-live="polite"' },
      { pattern: /id="research-other-list"[^>]*aria-live="polite"/, message: 'research-other-list with aria-live="polite"' },
      { pattern: /id="research-state-summary"[^>]*aria-live="polite"/, message: 'research-state-summary with aria-live="polite"' },
      { pattern: /id="research-filter"/, message: "Research filter input" },
      { pattern: /class="table-filter-label"[^>]+for="research-filter"/, message: 'Visible label for research-filter' },
      { pattern: /class="masthead-title"[^>]*>College Financial Health Explorer<\/div>/, message: "Masthead title" },
    ],
  },
  {
    name: "Accreditation (accreditation.html)",
    file: "accreditation.html",
    checks: [
      { pattern: /<a[^>]+href="#main"[^>]*class="skip-link"/, message: 'Skip-to-main link targeting "#main"' },
      { pattern: /<main[^>]+id="main"/, message: '<main id="main">' },
      { pattern: /class="masthead"/, message: "Masthead element" },
      { pattern: /class="top-tabs"[^>]*aria-label="Sections"/, message: 'Top nav with aria-label="Sections"' },
      { pattern: /<a[^>]+class="top-tab is-active"[^>]+href="accreditation\.html"[^>]+aria-current="page"[^>]*>Accreditation<\/a>/, message: "Active Accreditation tab with aria-current" },
      { pattern: /id="accreditation-status"[^>]*aria-live="polite"/, message: 'accreditation-status with aria-live="polite"' },
      { pattern: /id="accreditation-other-status"[^>]*aria-live="polite"/, message: 'accreditation-other-status with aria-live="polite"' },
      { pattern: /id="accreditation-filter"/, message: "Primary accreditation filter input" },
      { pattern: /id="accreditation-other-filter"/, message: "Other accreditation filter input" },
      { pattern: /class="table-filter-label"[^>]+for="accreditation-filter"/, message: 'Visible label for accreditation-filter' },
      { pattern: /class="table-filter-label"[^>]+for="accreditation-other-filter"/, message: 'Visible label for accreditation-other-filter' },
      { pattern: /class="masthead-title"[^>]*>College Financial Health Explorer<\/div>/, message: "Masthead title" },
    ],
  },
  {
    name: "School Detail (school.html)",
    file: "school.html",
    checks: [
      { pattern: /<a[^>]+href="#main"[^>]*class="skip-link"/, message: 'Skip-to-main link targeting "#main"' },
      { pattern: /<main[^>]+id="main"/, message: '<main id="main">' },
      { pattern: /class="masthead"/, message: "Masthead element" },
      { pattern: /class="top-tabs"[^>]*aria-label="Sections"/, message: 'Top nav with aria-label="Sections"' },
      { pattern: /<a[^>]+href="school\.html"[^>]+aria-current="page"[^>]*>Finances<\/a>/, message: "Finances nav tab with aria-current" },
      { pattern: /role="listbox"/, message: 'Search results role="listbox"' },
      { pattern: /class="masthead-title"[^>]*>College Financial Health Explorer<\/div>/, message: "Masthead title" },
      { pattern: /<h2[^>]+class="section-title"[^>]*>Financial Trends<\/h2>/, message: "Financial Trends h2" },
      { pattern: /<h2[^>]+class="section-title"[^>]*>Enrollment<\/h2>/, message: "Enrollment h2" },
      { pattern: /<h2[^>]+class="section-title"[^>]*>Staffing<\/h2>/, message: "Staffing h2" },
      { pattern: /<h2[^>]+class="section-title"[^>]*>Endowment<\/h2>/, message: "Endowment h2" },
      { pattern: /<h2[^>]+class="section-title"[^>]*>Federal And State Aid<\/h2>/, message: "Federal And State Aid h2" },
    ],
  },
];

// CSS smoke tests: check styles.css contains required patterns
const CSS_CHECKS = [
  { pattern: /\.sr-only/, message: ".sr-only utility class" },
  { pattern: /\.table-filter-label/, message: ".table-filter-label class" },
  { pattern: /\.top-tab:focus-visible/, message: ".top-tab:focus-visible" },
  { pattern: /\.result-item:focus-visible/, message: ".result-item:focus-visible" },
  { pattern: /\.sort-button:focus-visible/, message: ".sort-button:focus-visible" },
  { pattern: /\.filter-button:focus-visible/, message: ".filter-button:focus-visible" },
  { pattern: /\.skip-link/, message: ".skip-link class" },
  { pattern: /--status-blue-dark:/, message: "--status-blue-dark CSS variable" },
  { pattern: /\.search-panel:focus-within/, message: ".search-panel:focus-within box-shadow" },
];

let passed = 0;
let failed = 0;
const failures = [];

function check(name, pattern, content, message) {
  const passedCheck = typeof pattern === "function" ? pattern(content) : pattern.test(content);
  if (passedCheck) {
    console.log(`  PASS: ${message}`);
    passed++;
  } else {
    console.log(`  FAIL: ${message}`);
    failures.push(`${name}: ${message}`);
    failed++;
  }
}

console.log("=== Frontend Structure Smoke Tests ===\n");

// HTML page tests
for (const page of PAGES) {
  console.log(`\n${page.name}:`);
  const filePath = path.join(ROOT, page.file);
  if (!fs.existsSync(filePath)) {
    console.log(`  FAIL: File not found: ${page.file}`);
    failures.push(`${page.name}: File not found`);
    failed++;
    continue;
  }
  const content = fs.readFileSync(filePath, "utf8");
  for (const { pattern, message } of page.checks) {
    check(page.name, pattern, content, message);
  }
}

// CSS tests
console.log("\n\nstyles.css:");
const cssPath = path.join(ROOT, "styles.css");
if (!fs.existsSync(cssPath)) {
  console.log("  FAIL: styles.css not found");
  failures.push("styles.css: File not found");
  failed++;
} else {
  const cssContent = fs.readFileSync(cssPath, "utf8");
  for (const { pattern, message } of CSS_CHECKS) {
    check("styles.css", pattern, cssContent, message);
  }
}

// JS smoke: ensure key functions exist in app.js
console.log("\n\njs/app.js:");
const appJsPath = path.join(ROOT, "js", "app.js");
if (fs.existsSync(appJsPath)) {
  const appJs = fs.readFileSync(appJsPath, "utf8");
  check("app.js", /setAttribute\("role", "listbox"\)/, appJs, 'Search results role="listbox" set via setAttribute');
  check("app.js", /setAttribute\("role", "combobox"\)/, appJs, 'Search input role="combobox" set via setAttribute');
  check("app.js", /aria-expanded/, appJs, "search input aria-expanded state");
  check("app.js", /aria-activedescendant/, appJs, "search input aria-activedescendant state");
  check("app.js", /syncTabs/, appJs, "shared tab synchronization helper");
  check("app.js", /renderRelatedInstitutionLinks/, appJs, "shared related institution links helper");
  check("app.js", /renderSchoolLinkCell/, appJs, "structured school link table cell helper");
  check("app.js", /renderExternalLinkCell/, appJs, "structured external link table cell helper");
  check("app.js", /setActiveButton/, appJs, "roving tabindex: setActiveButton function");
  check("app.js", /ArrowDown/, appJs, "arrow key navigation: ArrowDown handler");
  check("app.js", /Escape/, appJs, "Escape key closes results");
  check("app.js", /aria-label.*search result/, appJs, "aria-label on search results container");
  check("app.js", /Search is temporarily unavailable/, appJs, "user-visible search load failure message");
  check("app.js", /downloadRowsCsv/, appJs, "shared CSV download helper");
  check("app.js", /setDataCardVisible/, appJs, "shared data-card visibility helper");
  check("app.js", /initSearch/, appJs, "initSearch function");
} else {
  console.log("  FAIL: js/app.js not found");
  failures.push("app.js: File not found");
  failed++;
}

console.log("\n\njs/accreditation.js:");
const accreditationJsPath = path.join(ROOT, "js", "accreditation.js");
if (fs.existsSync(accreditationJsPath)) {
  const accreditationJs = fs.readFileSync(accreditationJsPath, "utf8");
  check("accreditation.js", /function showLoadError/, accreditationJs, "user-visible accreditation load failure handler");
  check("accreditation.js", /Accreditation actions could not be loaded/, accreditationJs, "accreditation load failure message");
  check("accreditation.js", /init\(\)\.catch\(showLoadError\)/, accreditationJs, "accreditation init uses visible error handler");
} else {
  console.log("  FAIL: js/accreditation.js not found");
  failures.push("accreditation.js: File not found");
  failed++;
}

// Summary
console.log(`\n\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) {
  console.log("\nFailures:");
  for (const f of failures) console.log(`  - ${f}`);
  process.exit(1);
}
