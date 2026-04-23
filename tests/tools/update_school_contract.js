#!/usr/bin/env node
/**
 * Regenerates tests/fixtures/school_contract.json from the committed
 * data/schools/<unitid>.json files.
 *
 * Run this whenever the pipeline adds or removes fields on a school export:
 *   node tests/tools/update_school_contract.js
 *
 * The contract only captures keys that appear in EVERY school file, so
 * optional fields don't force churn on the snapshot. Review the resulting
 * diff carefully: removed keys likely break js/school.js or js/charts.js.
 */

const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..", "..");
const SCHOOLS_DIR = path.join(ROOT, "data", "schools");
const CONTRACT_PATH = path.join(ROOT, "tests", "fixtures", "school_contract.json");

function keysPresentInAll(records, selector) {
  if (records.length === 0) return [];
  let shared = null;
  for (const record of records) {
    const value = selector(record);
    const keys = value && typeof value === "object" && !Array.isArray(value) ? Object.keys(value) : [];
    if (shared === null) {
      shared = new Set(keys);
    } else {
      for (const key of Array.from(shared)) {
        if (!keys.includes(key)) shared.delete(key);
      }
    }
  }
  return Array.from(shared || []).sort();
}

function main() {
  const files = fs.readdirSync(SCHOOLS_DIR).filter((file) => file.endsWith(".json"));
  if (files.length === 0) {
    console.error(`No school JSON files found in ${SCHOOLS_DIR}`);
    process.exit(1);
  }

  const records = files.map((file) => JSON.parse(fs.readFileSync(path.join(SCHOOLS_DIR, file), "utf8")));

  const existing = fs.existsSync(CONTRACT_PATH)
    ? JSON.parse(fs.readFileSync(CONTRACT_PATH, "utf8"))
    : {};

  const snapshot = {
    _description: existing._description
      || "Frozen shape contract for data/schools/<unitid>.json. Update with tests/tools/update_school_contract.js when fields legitimately change.",
    _how_to_update: existing._how_to_update
      || "Run: node tests/tools/update_school_contract.js",
    top_level: keysPresentInAll(records, (record) => record),
    profile: keysPresentInAll(records, (record) => record.profile),
    summary: keysPresentInAll(records, (record) => record.summary),
    series: keysPresentInAll(records, (record) => record.series)
  };

  fs.writeFileSync(CONTRACT_PATH, JSON.stringify(snapshot, null, 2) + "\n");
  console.log(`Wrote ${CONTRACT_PATH} from ${files.length} school files.`);
}

main();
