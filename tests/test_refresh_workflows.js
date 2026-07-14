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
const TESTS = fs.readFileSync(path.join(ROOT, ".github", "workflows", "tests.yml"), "utf8");
const PUBLISH = fs.readFileSync(path.join(ROOT, ".github", "workflows", "publish-editorial-overrides.yml"), "utf8");
const ACCESSIBILITY = fs.readFileSync(path.join(ROOT, ".github", "workflows", "accessibility.yml"), "utf8");
const PAGES_PARITY = fs.readFileSync(path.join(ROOT, ".github", "workflows", "pages-parity.yml"), "utf8");
const PACKAGE_JSON = JSON.parse(fs.readFileSync(path.join(ROOT, "package.json"), "utf8"));

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

run("weekly refresh stages review queues before pulling sheet decisions", () => {
  const stageAccreditationIndex = WEEKLY.indexOf("- name: Stage accreditation review queue");
  const pullAccreditationIndex = WEEKLY.indexOf("- name: Pull accreditation review decisions");
  const stageCutsIndex = WEEKLY.indexOf("- name: Stage college cuts review queue");
  const pullCutsIndex = WEEKLY.indexOf("- name: Pull college cuts review decisions");
  const pullClosureFlagsIndex = WEEKLY.indexOf("- name: Pull closure flag review decisions");
  assert(stageAccreditationIndex >= 0, "Expected accreditation stage step");
  assert(pullAccreditationIndex > stageAccreditationIndex, "Expected accreditation sheet pull after stage");
  assert(stageCutsIndex >= 0, "Expected college cuts stage step");
  assert(pullCutsIndex > stageCutsIndex, "Expected college cuts sheet pull after stage");
  assert(pullClosureFlagsIndex > pullCutsIndex, "Expected closure flag sheet pull after the main cuts sheet pull");
});

run("weekly refresh fails loudly when closure sheet import breaks", () => {
  // Requirement change: the closure-sheet step previously carried
  // `continue-on-error: true`, which silently masked import failures and let
  // stale or empty closure data ship to production. The workflow now fails
  // the run if the import step fails, so users see the error instead of
  // silently degraded data.
  const block = stepBlock(WEEKLY, "Import closure outputs from published Google Sheet");
  assert(!block.includes("continue-on-error: true"), "Closure sheet import must not be marked continue-on-error; failures should surface");
  assert(block.includes("--sheet"), "Expected workflow to pass explicit sheet URL");
});

run("full refresh fails loudly when closure sheet import breaks", () => {
  // See note above: closure-sheet failures must surface rather than be
  // swallowed. The committed fallback is still validated after rebuild.
  const block = stepBlock(FULL, "Import closure outputs from published Google Sheet");
  assert(!block.includes("continue-on-error: true"), "Closure sheet import must not be marked continue-on-error in full refresh");
  assert(block.includes("--sheet"), "Expected workflow to pass explicit sheet URL");
  const validateBlock = stepBlock(FULL, "Validate rebuilt artifacts");
  assert(validateBlock.includes("node ./tests/test_data_exports.js"), "Expected rebuilt closure data to be validated before commit");
});

run("weekly external-data steps have bounded timeouts", () => {
  const names = [
    "Refresh accreditation actions with cache fallback",
    "Sync Supabase",
    "Build college cuts watchlist",
    "Discover college cuts",
    "Refresh college cuts with cache fallback",
    "Refresh research cuts from Grant Witness",
    "Import closure outputs from published Google Sheet",
    "Report college cuts discovery precision",
    "Rebuild static web exports"
  ];
  names.forEach((name) => {
    const block = stepBlockContaining(WEEKLY, name);
    assert(block.includes("timeout-minutes:"), `Expected timeout-minutes for step: ${name}`);
  });
});

run("full refresh external and build steps have bounded timeouts", () => {
  const names = [
    "Run shared helper smoke tests",
    "Rebuild canonical IPEDS dataset",
    "Rebuild Scorecard and graduation-rate joins",
    "Refresh accreditation actions with cache fallback",
    "Build college cuts watchlist",
    "Discover college cuts",
    "Refresh college cuts with cache fallback",
    "Refresh research cuts from Grant Witness",
    "Import closure outputs from published Google Sheet",
    "Rebuild HCM lookup",
    "Rebuild federal composite score lookup",
    "Rebuild static web exports",
    "Validate rebuilt artifacts",
    "Commit and push updated data"
  ];
  names.forEach((name) => {
    const block = stepBlockContaining(FULL, name);
    assert(block.includes("timeout-minutes:"), `Expected timeout-minutes for full refresh step: ${name}`);
  });
});

run("weekly refresh caches scraper and API responses for fallback/retry workflows", () => {
  assert(WEEKLY.includes("Cache accreditation scraped HTML"), "Expected accreditation HTML cache step");
  assert(WEEKLY.includes("data_pipelines/accreditation/cache"), "Expected accreditation cache path");
  assert(WEEKLY.includes("Cache Grant Witness downloads and USAspending responses"), "Expected Grant Witness/USAspending cache step");
  assert(WEEKLY.includes("data_pipelines/grant_witness/cache"), "Expected Grant Witness cache path");
  assert(WEEKLY.includes("Cache college cuts discovery fetches"), "Expected college cuts discovery cache step");
  assert(WEEKLY.includes("data_pipelines/college_cuts/discovery/cache"), "Expected college cuts discovery cache path");
});

run("refresh workflows install Python packages before discovery-time Python scripts", () => {
  const weeklySetupIndex = WEEKLY.indexOf("- name: Set up Python");
  const weeklyInstallIndex = WEEKLY.indexOf("- name: Install Python packages");
  const weeklyNodeIndex = WEEKLY.indexOf("- name: Set up Node.js");
  assert(weeklySetupIndex >= 0, "Expected weekly Python setup step");
  assert(weeklyInstallIndex > weeklySetupIndex, "Expected weekly pip install after Python setup");
  assert(weeklyNodeIndex > weeklyInstallIndex, "Expected weekly pip install before Node setup");
  const weeklyBlock = stepBlock(WEEKLY, "Install Python packages");
  assert(weeklyBlock.includes("pip install -r requirements.txt"), "Expected weekly workflow to install requirements.txt");

  const fullSetupIndex = FULL.indexOf("- name: Set up Python");
  const fullInstallIndex = FULL.indexOf("- name: Install Python packages");
  const fullNodeIndex = FULL.indexOf("- name: Set up Node.js");
  assert(fullSetupIndex >= 0, "Expected full refresh Python setup step");
  assert(fullInstallIndex > fullSetupIndex, "Expected full refresh pip install after Python setup");
  assert(fullNodeIndex > fullInstallIndex, "Expected full refresh pip install before Node setup");
  const fullBlock = stepBlock(FULL, "Install Python packages");
  assert(fullBlock.includes("pip install -r requirements.txt"), "Expected full refresh workflow to install requirements.txt");
});

run("weekly refresh builds the cuts watchlist before discovery and warn-gates failures", () => {
  const watchlistIndex = WEEKLY.indexOf("- name: Build college cuts watchlist");
  const discoveryIndex = WEEKLY.indexOf("- name: Discover college cuts");
  const cutsJoinIndex = WEEKLY.indexOf("- name: Refresh college cuts with cache fallback");
  assert(watchlistIndex >= 0, "Expected weekly watchlist build step");
  assert(discoveryIndex > watchlistIndex, "Expected weekly watchlist build before discovery");
  assert(discoveryIndex >= 0, "Expected weekly college cuts discovery step");
  assert(cutsJoinIndex > discoveryIndex, "Expected weekly discovery before the college cuts join");
  const watchlistBlock = stepBlock(WEEKLY, "Build college cuts watchlist");
  assert(watchlistBlock.includes("build_watchlist.py"), "Expected weekly watchlist builder");
  assert(watchlistBlock.includes("timeout-minutes: 10"), "Expected bounded timeout for weekly watchlist build");
  const block = stepBlock(WEEKLY, "Discover college cuts");
  assert(block.includes("timeout-minutes: 20"), "Expected bounded timeout for weekly discovery");
  assert(block.includes("ANTHROPIC_API_KEY"), "Expected weekly discovery to pass ANTHROPIC_API_KEY");
  assert(block.includes("CUTS_NEWS_WINDOW"), "Expected weekly discovery to pass CUTS_NEWS_WINDOW");
  assert(block.includes("github.event.inputs.news_window"), "Expected weekly discovery to honor workflow_dispatch news_window overrides");
  assert(block.includes("run_discovery.py"), "Expected weekly discovery runner");
  assert(block.includes('|| echo "::warning::cuts discovery failed; staging continues without new discovered candidates."'), "Expected weekly discovery failure to warn and continue");
});

run("weekly refresh exposes a manual news_window override for discovery backfills", () => {
  assert(WEEKLY.includes("workflow_dispatch:"), "Expected manual weekly workflow trigger");
  assert(WEEKLY.includes("news_window:"), "Expected workflow_dispatch news_window input");
  assert(WEEKLY.includes("Leave blank for the normal 14d window"), "Expected documented default news window behavior");
});

run("weekly refresh reports cuts discovery precision after pulling the cuts review sheet", () => {
  const pullIndex = WEEKLY.indexOf("- name: Pull closure flag review decisions");
  const precisionIndex = WEEKLY.indexOf("- name: Report college cuts discovery precision");
  assert(pullIndex >= 0, "Expected weekly closure flag review pull step");
  assert(precisionIndex > pullIndex, "Expected precision report after closure flag review pull");
  const block = stepBlock(WEEKLY, "Report college cuts discovery precision");
  assert(block.includes("report_precision.py"), "Expected precision telemetry script");
  assert(block.includes("timeout-minutes: 5"), "Expected bounded timeout for precision telemetry");
});

run("weekly refresh runs R smoke tests through activated renv library", () => {
  const sysreqBlock = stepBlock(WEEKLY, "Install system dependencies");
  [
    "cmake",
    "libcurl4-openssl-dev",
    "libjpeg-dev",
    "libpoppler-cpp-dev",
    "libx11-dev",
    "pandoc"
  ].forEach((pkg) => {
    assert(sysreqBlock.includes(pkg), `Expected weekly refresh to install ${pkg} before renv restore`);
  });
  const restoreBlock = stepBlock(WEEKLY, "Restore R packages");
  assert(restoreBlock.includes("renv::restore()"), "Expected weekly refresh to restore the renv library before smoke tests");
  const block = stepBlock(WEEKLY, "Run shared helper smoke tests");
  assert(block.includes("Rscript ./tests/run_shared_helper_smoke_tests.R"), "Expected R smoke tests to run");
  assert(!block.includes("--vanilla"), "Expected weekly smoke tests not to bypass renv activation with --vanilla");
});

run("R-bearing workflows pin Ubuntu 24.04, align to lockfile R, and disable symlinked renv caches", () => {
  [TESTS, WEEKLY, FULL, PUBLISH].forEach((workflow) => {
    assert(workflow.includes("runs-on: ubuntu-24.04"), "Expected Ubuntu 24.04 pin on R-bearing workflow");
    assert(workflow.includes('r-version: "4.5.3"'), "Expected R version to match the lockfile");
    assert(workflow.includes('RENV_CONFIG_CACHE_SYMLINKS: "FALSE"'), "Expected symlink-free renv cache configuration");
  });
  assert(!TESTS.includes("use-public-rspm: true"), "Expected tests workflow to stop forcing public RSPM");
});

run("R-bearing workflows cache the same pinned renv library path and cache key", () => {
  const weeklyBlock = stepBlock(WEEKLY, "Cache R packages");
  const testsBlock = stepBlock(TESTS, "Cache R packages");
  const fullBlock = stepBlock(FULL, "Cache R packages");
  const publishBlock = stepBlock(PUBLISH, "Cache R packages");
  [weeklyBlock, testsBlock, fullBlock, publishBlock].forEach((block) => {
    assert(block.includes("path: renv/library"), "Expected renv/library cache path");
    assert(block.includes("key: r-ubuntu-24.04-${{ hashFiles('renv.lock') }}"), "Expected image-pinned renv cache key");
    assert(block.includes("r-ubuntu-24.04-"), "Expected image-pinned renv restore key prefix");
    assert(!block.includes("R_LIBS_USER"), "Expected no divergent R_LIBS_USER cache path");
  });
});

run("full refresh restores renv and installs system dependencies before --vanilla R scripts", () => {
  const cacheBlock = stepBlock(FULL, "Cache R packages");
  assert(cacheBlock.includes("path: renv/library"), "Expected full refresh to cache renv/library");
  const sysreqBlock = stepBlock(FULL, "Install system dependencies");
  [
    "cmake",
    "libcurl4-openssl-dev",
    "libjpeg-dev",
    "libpoppler-cpp-dev",
    "libx11-dev",
    "pandoc"
  ].forEach((pkg) => {
    assert(sysreqBlock.includes(pkg), `Expected full refresh to install ${pkg} before renv restore`);
  });
  const restoreBlock = stepBlock(FULL, "Restore R packages");
  assert(restoreBlock.includes("renv::restore()"), "Expected full refresh to restore packages from renv.lock");
  const smokeBlock = stepBlock(FULL, "Run shared helper smoke tests");
  assert(smokeBlock.includes("Rscript --vanilla ./tests/run_shared_helper_smoke_tests.R"), "Expected full refresh smoke tests to preserve --vanilla coverage");
});

run("publish workflow restores renv with the same hardened system dependency set", () => {
  const cacheBlock = stepBlock(PUBLISH, "Cache R packages");
  assert(cacheBlock.includes("path: renv/library"), "Expected publish workflow to cache renv/library");
  const sysreqBlock = stepBlock(PUBLISH, "Install system dependencies");
  [
    "cmake",
    "libcurl4-openssl-dev",
    "libjpeg-dev",
    "libpoppler-cpp-dev",
    "libx11-dev",
    "pandoc"
  ].forEach((pkg) => {
    assert(sysreqBlock.includes(pkg), `Expected publish workflow to install ${pkg} before renv restore`);
  });
  const restoreBlock = stepBlock(PUBLISH, "Restore R packages");
  assert(restoreBlock.includes("renv::restore()"), "Expected publish workflow to restore packages from renv.lock");
});

run("full refresh restores IPEDS downloads from cache before external collection", () => {
  const block = stepBlock(FULL, "Restore IPEDS download cache");
  assert(block.includes("ipeds/cache/downloads"), "Expected IPEDS download cache path");
  assert(block.includes("restore-keys:"), "Expected restore key fallback for missing exact cache");
});

run("refresh workflows validate rebuilt artifacts before committing", () => {
  [WEEKLY, FULL].forEach((workflow) => {
    const rebuildIndex = workflow.indexOf("- name: Rebuild static web exports");
    const validateIndex = workflow.indexOf("- name: Validate rebuilt artifacts");
    const commitIndex = workflow.indexOf("- name: Commit and push updated data");
    assert(rebuildIndex >= 0, "Expected rebuild step");
    assert(validateIndex > rebuildIndex, "Expected validation after rebuild");
    assert(commitIndex > validateIndex, "Expected validation before commit");
    const block = stepBlock(workflow, "Validate rebuilt artifacts");
    assert(block.includes("node ./tests/test_data_exports.js"), "Expected static data export validation");
    assert(block.includes("python ./tests/test_import_supabase.py"), "Expected Supabase mapping validation");
  });
});

run("full refresh stages every tracked pipeline artifact it rebuilds before publish", () => {
  const block = stepBlock(FULL, "Commit and push updated data");
  [
    "set -e",
    "git add -f \\",
    "data/ \\",
    "data_pipelines/accreditation/accreditation_review_candidates.csv \\",
    "data_pipelines/accreditation/accreditation_tracker_actions_joined.csv \\",
    "data_pipelines/accreditation/accreditation_tracker_institution_summary.csv \\",
    "data_pipelines/accreditation/dapip_action_rows_filtered.csv \\",
    "data_pipelines/accreditation/dapip_code_coverage.csv \\",
    "data_pipelines/accreditation/dapip_public_table_policy_counts.csv \\",
    "data_pipelines/accreditation/dapip_public_table_policy_family_counts.csv \\",
    "data_pipelines/accreditation/dapip_vs_scraper_audit.csv \\",
    "data_pipelines/college_cuts/college_cuts_review_candidates.csv \\",
    "data_pipelines/college_cuts/closure_flags_review.csv \\",
    "data_pipelines/college_cuts/closure_flags_review_candidates.csv \\",
    "data_pipelines/college_cuts/college_cuts_financial_tracker_cut_level_joined.csv \\",
    "data_pipelines/college_cuts/college_cuts_financial_tracker_unmatched_for_review.csv \\",
    "data_pipelines/college_cuts/discovered_cut_candidates.csv \\",
    "data_pipelines/college_cuts/discovery/leads.csv \\",
    "data_pipelines/college_cuts/discovery/classifications.csv \\",
    "data_pipelines/college_cuts/discovery/watchlist.csv \\",
    "data_pipelines/scorecard/tracker_outcomes_joined.csv",
    "if git diff --staged --quiet; then"
  ].forEach((needle) => {
    assert(block.includes(needle), `Expected full refresh commit block to include: ${needle}`);
  });
  assert(!block.includes("git add data/"), "Expected full refresh to avoid bare git add data/");
});

run("weekly refresh stages discovery artifacts and tracks cuts discovery drift", () => {
  const commitBlock = stepBlock(WEEKLY, "Commit and push updated data");
  [
    "--add data_pipelines/college_cuts/closure_flags_review.csv \\",
    "--add data_pipelines/college_cuts/closure_flags_review_candidates.csv \\",
    "--add data_pipelines/college_cuts/college_cuts_financial_tracker_unmatched_for_review.csv \\",
    "--add data_pipelines/college_cuts/discovered_cut_candidates.csv \\",
    "--add data_pipelines/college_cuts/discovery/leads.csv \\",
    "--add data_pipelines/college_cuts/discovery/classifications.csv \\",
    "--add data_pipelines/college_cuts/discovery/watchlist.csv \\",
    "--conflict-path data_pipelines/college_cuts/closure_flags_review.csv \\",
    "--conflict-path data_pipelines/college_cuts/closure_flags_review_candidates.csv \\",
    "--conflict-path data_pipelines/college_cuts/college_cuts_financial_tracker_unmatched_for_review.csv \\",
    "--conflict-path data_pipelines/college_cuts/discovered_cut_candidates.csv \\",
    "--conflict-path data_pipelines/college_cuts/discovery/leads.csv \\",
    "--conflict-path data_pipelines/college_cuts/discovery/classifications.csv \\",
    "--conflict-path data_pipelines/college_cuts/discovery/watchlist.csv \\"
  ].forEach((needle) => {
    assert(commitBlock.includes(needle), `Expected weekly refresh commit block to include: ${needle}`);
  });
  const driftBlock = stepBlock(WEEKLY, "Report scraper drift warnings");
  assert(driftBlock.includes('PIPELINE DRIFT WARNING: cuts_discovery'), "Expected weekly drift scan to include cuts_discovery");
  assert(driftBlock.includes('DRIFTED="$DRIFTED,cuts_discovery"'), "Expected weekly drift state tracking for cuts_discovery");
});

run("weekly refresh appends unmatched discovered cuts to the dedicated triage tab after main cuts staging", () => {
  assert(WEEKLY.includes('COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_TAB: "cuts_unmatched_review"'), "Expected unmatched cuts review tab env");
  assert(WEEKLY.includes('COLLEGE_CUTS_CLOSURE_FLAGS_REVIEW_SHEET_TAB: "closure_flags_review"'), "Expected closure flags review tab env");
  const block = stepBlock(WEEKLY, "Append new review rows to Google Sheet (post-commit)");
  assert(block.includes("sync_review_sheet_appends.R"), "Expected main review sheet append script");
  assert(block.includes("sync_cuts_unmatched_review_sheet_appends.R"), "Expected unmatched cuts review append script");
  assert(block.includes("sync_closure_flags_review_sheet_appends.R"), "Expected closure flags review append script");
  assert(block.includes("${COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_TAB}"), "Expected unmatched cuts tab env to be passed");
  assert(block.includes("${COLLEGE_CUTS_CLOSURE_FLAGS_REVIEW_SHEET_TAB}"), "Expected closure flags tab env to be passed");
  assert(block.includes("College cuts unmatched review sheet append failed"), "Expected unmatched tab append failures to warn and continue");
  assert(block.includes("Closure flags review sheet append failed"), "Expected closure flags append failures to warn and continue");
});

run("full refresh rebuilds side data lookups before static web exports", () => {
  const rebuildIndex = FULL.indexOf("- name: Rebuild static web exports");
  [
    "Import closure outputs from published Google Sheet",
    "Rebuild HCM lookup",
    "Rebuild federal composite score lookup"
  ].forEach((name) => {
    const stepIndex = FULL.indexOf(`- name: ${name}`);
    assert(stepIndex >= 0, `Expected full refresh step: ${name}`);
    assert(stepIndex < rebuildIndex, `Expected ${name} before static web exports`);
    const block = stepBlock(FULL, name);
    assert(block.includes("timeout-minutes:"), `Expected timeout-minutes for ${name}`);
  });
  assert(FULL.includes("pip install -r requirements.txt"), "Expected Python dependencies before HCM/composite rebuilds");
});

run("full refresh IPEDS year range is workflow-configurable", () => {
  assert(FULL.includes('IPEDS_START_YEAR: "2014"'), "Expected configured IPEDS start year");
  assert(FULL.includes('IPEDS_END_YEAR: "2024"'), "Expected configured IPEDS end year");
  const block = stepBlock(FULL, "Rebuild canonical IPEDS dataset");
  assert(block.includes('--start-year "${IPEDS_START_YEAR}"'), "Expected start year to come from workflow env");
  assert(block.includes('--end-year "${IPEDS_END_YEAR}"'), "Expected end year to come from workflow env");
  assert(!block.includes("--end-year 2024"), "Expected no hard-coded end year in command");
});

run("Node CI workflows use deterministic npm ci installs", () => {
  [TESTS, ACCESSIBILITY, PAGES_PARITY].forEach((workflow) => {
    assert(workflow.includes("npm ci"), "Expected npm ci in Node workflow");
    assert(!workflow.includes("npm install"), "Expected npm install to be absent from Node workflow");
  });
});

run("JS smoke tests include lightweight static analysis", () => {
  assert(PACKAGE_JSON.scripts["test:lint"] === "node tests/test_static_analysis.js", "Expected test:lint script");
  assert(PACKAGE_JSON.scripts["test:smoke"].startsWith("npm run test:lint &&"), "Expected test:smoke to run static analysis first");
});

run("Playwright e2e tests run as a dedicated CI job separate from smoke", () => {
  // Guard against accidental removal or merger of the browser-test job. The
  // smoke/e2e split is deliberate: smoke must stay fast and dependency-light
  // (no browser download, no server boot), and e2e must stay an independent
  // signal that can fail without blocking the quick JS/static checks.
  assert(PACKAGE_JSON.scripts["test:e2e"] === "playwright test", "Expected test:e2e script to invoke playwright");
  assert(!PACKAGE_JSON.scripts["test:smoke"].includes("test:e2e"), "test:smoke must not chain into test:e2e — keep browser tests in their own CI job");
  assert(!PACKAGE_JSON.scripts["test:smoke"].includes("playwright"), "test:smoke must not invoke Playwright directly");

  const e2eBlock = stepBlockContaining(TESTS, "Run Playwright e2e tests");
  assert(e2eBlock.includes("npm run test:e2e"), "Expected CI to run npm run test:e2e");
  assert(TESTS.includes("    container:"), "Expected Playwright e2e job to run in a dedicated container");
  assert(TESTS.includes("      image: mcr.microsoft.com/playwright:v1.59.1-noble"), "Expected official Playwright container image");
  // The e2e job must be a top-level job, not a step inside js-tests. Match
  // the header at the same 2-space indent as other jobs; multiline mode is
  // needed because the workflow file may use CRLF line endings.
  assert(/^  e2e-tests:\s*$/m.test(TESTS), "Expected a top-level e2e-tests job separate from js-tests");
  assert(/^  js-tests:\s*$/m.test(TESTS), "Expected js-tests to remain as its own job (sanity check for the split)");
});

run("Playwright e2e workflow uses official container and avoids browser provisioning", () => {
  const e2eJobMatch = TESTS.match(/  e2e-tests:\s*[\s\S]*?timeout-minutes:\s*30/);
  assert(Boolean(e2eJobMatch), "Expected a bounded timeout on the Playwright job");
  assert(/container:\s*\n\s*image:\s*mcr\.microsoft\.com\/playwright:v1\.59\.1-noble/m.test(TESTS), "Expected official Playwright container image");
  assert(/options:\s*--ipc=host/m.test(TESTS), "Expected Playwright container to enable shared IPC");
  assert(/env:\s*\n\s*HOME:\s*\/root/m.test(TESTS), "Expected Playwright container job to set HOME=/root for Firefox");
  assert(/DOCKER_CONFIG:\s*\/tmp\/\.docker/m.test(TESTS), "Expected Playwright container job to route Docker config into writable /tmp/.docker");
  assert(TESTS.includes("Prepare writable Docker config"), "Expected e2e workflow to create writable Docker config directory");
  assert(TESTS.includes("mkdir -p /tmp/.docker"), "Expected e2e workflow to pre-create /tmp/.docker");

  assert(!TESTS.includes("Cache Playwright browsers"), "Expected browser cache step to be absent when using the official Playwright container");
  assert(!TESTS.includes("Install Playwright system dependencies"), "Expected install-deps step to be absent when using the official Playwright container");
  assert(!TESTS.includes("Install Playwright Chromium browser"), "Expected Chromium browser install step to be absent when using the official Playwright container");
  assert(!TESTS.includes("Install Playwright Firefox browser"), "Expected Firefox browser install step to be absent when using the official Playwright container");
  assert(!TESTS.includes("playwright install chromium"), "Expected no explicit Chromium browser install in the e2e workflow");
  assert(!TESTS.includes("playwright install firefox"), "Expected no explicit Firefox browser install in the e2e workflow");
});

run("deployed Pages parity workflow compares live site to committed artifacts", () => {
  assert(PAGES_PARITY.includes('workflows: ["pages build and deployment"]'), "Expected workflow_run trigger after Pages deployment");
  assert(PAGES_PARITY.includes("workflow_dispatch:"), "Expected manual parity trigger");
  assert(PAGES_PARITY.includes("npm run test:pages"), "Expected deployed parity npm script");
  assert(PAGES_PARITY.includes("PAGES_BASE_URL: https://financialtracker.hechingerreport.org"), "Expected explicit GitHub Pages URL");
  assert(PACKAGE_JSON.scripts["test:pages"] === "node tests/test_deployed_pages_parity.js", "Expected test:pages script");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
