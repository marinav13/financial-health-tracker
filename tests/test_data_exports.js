/**
 * Smoke tests for committed static data exports.
 *
 * These catch stale generated JSON that can pass code-level tests but still
 * break the public interactive pages.
 */

const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..");
const SCHOOLS_DIR = path.join(ROOT, "data", "schools");
const METADATA = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "metadata.json"), "utf8"));
const ACCREDITATION = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "accreditation.json"), "utf8"));
const RESEARCH_FUNDING = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "research_funding.json"), "utf8"));
const RESEARCH_FUNDING_INDEX = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "research_funding_index.json"), "utf8"));
const CLOSURE_STATUS = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "closure_status_by_unitid.json"), "utf8"));
const HCM_STATUS = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "hcm2_by_unitid.json"), "utf8"));
const FEDERAL_COMPOSITE = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "federal_composite_scores_by_unitid.json"), "utf8"));
const MIN_RESEARCH_AWARD_REMAINING = 100;

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

function numericSeries(school, key) {
  return (school.series?.[key] || [])
    .filter((point) => point && point.year != null && point.value != null)
    .map((point) => ({ year: Number(point.year), value: Number(point.value) }))
    .filter((point) => Number.isFinite(point.year) && Number.isFinite(point.value));
}

console.log("\n=== Static Data Export Smoke Tests ===\n");

run("school JSON files keep multi-year revenue series when five-year summaries exist", () => {
  const files = fs.readdirSync(SCHOOLS_DIR).filter((file) => file.endsWith(".json"));
  const staleFiles = [];

  for (const file of files) {
    const school = JSON.parse(fs.readFileSync(path.join(SCHOOLS_DIR, file), "utf8"));
    if (school.summary?.revenue_pct_change_5yr == null) continue;
    const revenueSeries = numericSeries(school, "revenue_total_adjusted");
    if (revenueSeries.length < 2) {
      staleFiles.push(`${file} (${school.profile?.institution_name || school.unitid})`);
    }
  }

  assert(
    staleFiles.length === 0,
    `Expected multi-year revenue series; one-year exports found in: ${staleFiles.slice(0, 10).join(", ")}`
  );
});

run("sample school chart series spans the committed IPEDS range", () => {
  const sample = JSON.parse(fs.readFileSync(path.join(SCHOOLS_DIR, "101709.json"), "utf8"));
  const years = numericSeries(sample, "revenue_total_adjusted").map((point) => point.year);
  const latestYear = Number(METADATA.latest_year || years[years.length - 1]);
  assert(years.length >= 10, `Expected at least 10 revenue years, got ${years.length}`);
  assert(years[0] < latestYear, `Expected first revenue year to predate latest year ${latestYear}, got ${years[0]}`);
  assert(years[years.length - 1] === latestYear, `Expected latest revenue year to be ${latestYear}, got ${years[years.length - 1]}`);
});

run("closure status export is not a fixture artifact", () => {
  const source = String(CLOSURE_STATUS.source_file || CLOSURE_STATUS.source || "");
  const schools = CLOSURE_STATUS.schools || {};
  const names = Object.values(schools).map((school) => String(school.institution_name || ""));
  assert(!/closure-fixture|\/tmp|\\tmp/i.test(source), `Closure status source looks like a fixture path: ${source}`);
  assert(Object.keys(schools).length > 2, `Closure status export is suspiciously small: ${Object.keys(schools).length} rows`);
  assert(Number.isFinite(Number(CLOSURE_STATUS.min_close_year)), "Closure status export is missing min_close_year");
  assert(Number.isFinite(Number(CLOSURE_STATUS.max_close_year)), "Closure status export is missing max_close_year");
  assert(!names.includes("Example State University"), "Closure status export contains fixture school Example State University");
  assert(!names.includes("Sample College"), "Closure status export contains fixture school Sample College");
});

run("source-versioned side exports include source version labels", () => {
  assert(HCM_STATUS.source_version_label, "HCM export is missing source_version_label");
  assert(FEDERAL_COMPOSITE.source_version_label, "Federal composite export is missing source_version_label");
});

run("matched research and accreditation main-table records keep sector fields", () => {
  for (const [label, exportData] of [
    ["research", RESEARCH_FUNDING],
    ["accreditation", ACCREDITATION]
  ]) {
    const primarySchools = Object.values(exportData.schools || {})
      .filter((school) => school.is_primary_tracker === true);
    const missingSector = primarySchools
      .filter((school) => !String(school.control_label || "").trim())
      .map((school) => school.institution_name || school.unitid);
    assert(primarySchools.length > 0, `${label} export has no primary tracker records`);
    assert(missingSector.length === 0, `${label} primary tracker records missing sector: ${missingSector.slice(0, 10).join(", ")}`);
  }
});

run("research export excludes Dartmouth award with zero live remaining funding", () => {
  const dartmouth = RESEARCH_FUNDING.schools?.["182670"];
  assert(dartmouth, "Dartmouth College research record is missing");
  const staleAward = (dartmouth.grants || []).find((grant) =>
    /F31DA060690|ASST_NON_F31DA060690_7529/.test(
      `${grant.grant_id || ""} ${grant.grant_id_core || ""} ${grant.source_url || ""}`
    )
  );
  assert(!staleAward, "Dartmouth award F31DA060690 should not appear after live USAspending zeroes out the remaining amount");
});

run("research export excludes de minimis disrupted grant balances", () => {
  const smallAwards = [];
  for (const school of Object.values(RESEARCH_FUNDING.schools || {})) {
    for (const grant of school.grants || []) {
      const amount = Number(grant.award_remaining);
      if (Number.isFinite(amount) && amount < MIN_RESEARCH_AWARD_REMAINING) {
        smallAwards.push(`${school.institution_name || school.unitid}: ${grant.grant_id || grant.source_url} (${amount})`);
      }
    }
  }
  assert(
    smallAwards.length === 0,
    `Research export should not include grants below $${MIN_RESEARCH_AWARD_REMAINING}: ${smallAwards.slice(0, 10).join(", ")}`
  );
});

run("research export totals match displayed grants after de minimis filtering", () => {
  for (const [unitid, school] of Object.entries(RESEARCH_FUNDING.schools || {})) {
    const grants = school.grants || [];
    const grantTotal = grants.reduce((sum, grant) => sum + Number(grant.award_remaining || 0), 0);
    const grantCount = grants.length;
    const exportedTotal = Number(school.total_disrupted_award_remaining || 0);
    const exportedCount = Number(school.total_disrupted_grants || 0);
    const indexTotal = Number(RESEARCH_FUNDING_INDEX[unitid]?.total_disrupted_award_remaining || 0);
    const indexCount = Number(RESEARCH_FUNDING_INDEX[unitid]?.total_disrupted_grants || 0);

    assert(exportedCount === grantCount, `${school.institution_name || unitid} grant count ${exportedCount} does not match displayed rows ${grantCount}`);
    assert(Math.abs(exportedTotal - grantTotal) < 0.01, `${school.institution_name || unitid} total ${exportedTotal} does not match displayed rows ${grantTotal}`);
    assert(indexCount === grantCount, `${school.institution_name || unitid} index grant count ${indexCount} does not match displayed rows ${grantCount}`);
    assert(Math.abs(indexTotal - grantTotal) < 0.01, `${school.institution_name || unitid} index total ${indexTotal} does not match displayed rows ${grantTotal}`);
  }
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
