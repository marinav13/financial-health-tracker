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

run("sample school chart series covers the 2014-2024 IPEDS range", () => {
  const sample = JSON.parse(fs.readFileSync(path.join(SCHOOLS_DIR, "101709.json"), "utf8"));
  const years = numericSeries(sample, "revenue_total_adjusted").map((point) => point.year);
  assert(years[0] === 2014, `Expected first revenue year to be 2014, got ${years[0]}`);
  assert(years[years.length - 1] === 2024, `Expected latest revenue year to be 2024, got ${years[years.length - 1]}`);
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
