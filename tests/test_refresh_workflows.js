/**
 * Static regression tests for refresh workflow failure-mode handling.
 *
 * These do not execute external scrapers. They protect the workflow contracts
 * that make fragile data sources fail loudly, time out, or degrade intentionally.
 */

const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..");
const WEEKLY = fs.readFileSync(path.join(ROOT, ".github", "workflows", "refresh-ipeds-site-data.yml"), "utf8");
const FULL = fs.readFileSync(path.join(ROOT, ".github", "workflows", "refresh-ipeds-full.yml"), "utf8");

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

function stepBlock(workflow, name) {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const match = workflow.match(new RegExp(`- name: ${escaped}[\\s\\S]*?(?=\\n\\s*- name:|\\n\\s*Commit and push|$)`));
  return match ? match[0] : "";
}

function stepBlockContaining(workflow, text) {
  const blocks = workflow.split(/\n\s*- name: /).slice(1).map((block) => `- name: ${block}`);
  return blocks.find((block) => block.includes(text)) || "";
}

console.log("\n=== Refresh Workflow Failure-Mode Tests ===\n");

run("weekly refresh verifies Supabase mapping output before downstream joins", () => {
  const block = stepBlock(WEEKLY, "Verify Supabase mapping exists");
  assert(block.includes("test -s data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv"), "Expected non-empty mapping file check");
  assert(block.includes("Missing Supabase mapping CSV"), "Expected explicit failure message for missing mapping");
  assert(block.includes("exit 1"), "Expected missing mapping to fail the workflow");
});

run("weekly refresh treats closure sheet import as optional external input", () => {
  const block = stepBlock(WEEKLY, "Import closure outputs from published Google Sheet");
  assert(block.includes("continue-on-error: true"), "Expected closure sheet import to be allowed to fail without killing source refresh");
  assert(block.includes("--sheet"), "Expected workflow to pass explicit sheet URL");
});

run("weekly external-data steps have bounded timeouts", () => {
  const names = [
    "Refresh accreditation actions with cache fallback",
    "Sync Supabase",
    "Refresh college cuts from public API",
    "Build unfiltered research join for USAspending analysis",
    "Refresh USAspending reinstatement analysis",
    "Refresh research cuts with Proposal G filter",
    "Import closure outputs from published Google Sheet",
    "Rebuild static web exports"
  ];
  names.forEach((name) => {
    const block = stepBlockContaining(WEEKLY, name);
    assert(block.includes("timeout-minutes:"), `Expected timeout-minutes for step: ${name}`);
  });
});

run("weekly refresh caches scraper and API responses for fallback/retry workflows", () => {
  assert(WEEKLY.includes("Cache accreditation scraped HTML"), "Expected accreditation HTML cache step");
  assert(WEEKLY.includes("data_pipelines/accreditation/cache"), "Expected accreditation cache path");
  assert(WEEKLY.includes("Cache Grant Witness downloads and USAspending responses"), "Expected Grant Witness/USAspending cache step");
  assert(WEEKLY.includes("data_pipelines/grant_witness/cache"), "Expected Grant Witness cache path");
});

run("weekly refresh runs R smoke tests through activated renv library", () => {
  const block = stepBlock(WEEKLY, "Run shared helper smoke tests");
  assert(block.includes("Rscript ./tests/run_shared_helper_smoke_tests.R"), "Expected R smoke tests to run");
  assert(!block.includes("--vanilla"), "Expected weekly smoke tests not to bypass renv activation with --vanilla");
});

run("full refresh explicitly installs packages used by --vanilla R scripts", () => {
  const dependencyBlock = stepBlock(FULL, "Set up R dependencies");
  ["dplyr", "httr2", "jsonlite", "openxlsx", "purrr", "readr", "readxl", "stringr", "tidyr", "xml2"].forEach((pkg) => {
    assert(dependencyBlock.includes(`any::${pkg}`), `Expected setup-r-dependencies to install ${pkg}`);
  });

  const smokeBlock = stepBlock(FULL, "Run shared helper smoke tests");
  assert(smokeBlock.includes("Rscript --vanilla ./tests/run_shared_helper_smoke_tests.R"), "Expected full refresh smoke tests to preserve --vanilla coverage");
});

run("full refresh restores IPEDS downloads from cache before external collection", () => {
  const block = stepBlock(FULL, "Restore IPEDS download cache");
  assert(block.includes("ipeds/cache/downloads"), "Expected IPEDS download cache path");
  assert(block.includes("restore-keys:"), "Expected restore key fallback for missing exact cache");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
