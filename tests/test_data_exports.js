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
const SCHOOL_CONTRACT = JSON.parse(fs.readFileSync(path.join(ROOT, "tests", "fixtures", "school_contract.json"), "utf8"));
const METADATA = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "metadata.json"), "utf8"));
const ACCREDITATION = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "accreditation.json"), "utf8"));
const RESEARCH_FUNDING = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "research_funding.json"), "utf8"));
const RESEARCH_FUNDING_INDEX = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "research_funding_index.json"), "utf8"));
const CLOSURE_STATUS = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "closure_status_by_unitid.json"), "utf8"));
const HCM_STATUS = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "hcm2_by_unitid.json"), "utf8"));
const FEDERAL_COMPOSITE = JSON.parse(fs.readFileSync(path.join(ROOT, "data", "federal_composite_scores_by_unitid.json"), "utf8"));
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

run("school JSON files match the frozen shape contract", () => {
  // This is the pipeline-to-frontend contract: the R export pipeline writes
  // data/schools/<unitid>.json; js/school.js and js/charts.js read specific
  // keys off `profile`, `summary`, and `series`. If the pipeline silently
  // drops a field (rename, refactor, bad join), users get broken pages.
  //
  // The contract in tests/fixtures/school_contract.json is the set of keys
  // that must appear in EVERY school file. When you intentionally add or
  // remove a field, regenerate it with:
  //   node tests/tools/update_school_contract.js
  // and review the diff in the same PR that changes the pipeline.

  const files = fs.readdirSync(SCHOOLS_DIR).filter((file) => file.endsWith(".json"));
  assert(files.length > 0, "No school JSON files found to validate against the contract.");

  const required = {
    top_level: SCHOOL_CONTRACT.top_level || [],
    profile: SCHOOL_CONTRACT.profile || [],
    summary: SCHOOL_CONTRACT.summary || [],
    series: SCHOOL_CONTRACT.series || []
  };

  const missing = [];           // keys the contract requires but a file is missing
  const unexpectedCandidates = { profile: new Map(), summary: new Map(), series: new Map() };
  const observedTopLevel = new Map();
  let fileCount = 0;

  for (const file of files) {
    const school = JSON.parse(fs.readFileSync(path.join(SCHOOLS_DIR, file), "utf8"));
    fileCount++;

    for (const key of required.top_level) {
      if (!Object.prototype.hasOwnProperty.call(school, key)) {
        missing.push(`${file}: top-level.${key}`);
      }
    }
    for (const key of Object.keys(school)) {
      observedTopLevel.set(key, (observedTopLevel.get(key) || 0) + 1);
    }

    for (const nested of ["profile", "summary", "series"]) {
      const value = school[nested];
      if (value == null || typeof value !== "object" || Array.isArray(value)) continue;
      for (const key of required[nested]) {
        if (!Object.prototype.hasOwnProperty.call(value, key)) {
          missing.push(`${file}: ${nested}.${key}`);
        }
      }
      for (const key of Object.keys(value)) {
        const counter = unexpectedCandidates[nested];
        counter.set(key, (counter.get(key) || 0) + 1);
      }
    }
  }

  assert(
    missing.length === 0,
    `School JSON is missing contract keys in ${missing.length} place(s). First 10: ${missing.slice(0, 10).join("; ")}. ` +
    `If the pipeline intentionally removed a field, run: node tests/tools/update_school_contract.js and review the diff.`
  );

  // Additions: any key that appears in EVERY file but isn't in the contract
  // is new shared schema that the frontend should know about. Flag it so new
  // fields can't silently ship without updating the contract.
  const addedTopLevel = Array.from(observedTopLevel.entries())
    .filter(([key, count]) => count === fileCount && !required.top_level.includes(key))
    .map(([key]) => key);
  const addedNested = [];
  for (const nested of ["profile", "summary", "series"]) {
    for (const [key, count] of unexpectedCandidates[nested]) {
      if (count === fileCount && !required[nested].includes(key)) {
        addedNested.push(`${nested}.${key}`);
      }
    }
  }

  assert(
    addedTopLevel.length === 0 && addedNested.length === 0,
    `School JSON has new keys present in every file that aren't in the contract: ` +
    `${[...addedTopLevel.map((k) => `top-level.${k}`), ...addedNested].join(", ")}. ` +
    `If these are intentional, run: node tests/tools/update_school_contract.js and update the snapshot.`
  );
});

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

run("known research institution aliases stay in the primary tracker table", () => {
  const expectedResearchMatches = [
    ["199148", "University of North Carolina at Greensboro"],
    ["240444", "University of Wisconsin-Madison"],
    ["240453", "University of Wisconsin-Milwaukee"],
    ["141574", "University of Hawaii at Manoa"],
    ["102553", "University of Alaska Anchorage"],
    ["102614", "University of Alaska Fairbanks"],
    ["229115", "Texas Tech University"],
    ["145637", "University of Illinois Urbana-Champaign"],
    ["146719", "Loyola University Chicago"],
    ["126562", "University of Colorado Denver/Anschutz Medical Campus"],
    ["230764", "University of Utah"],
    ["161253", "University of Maine"],
    ["166629", "University of Massachusetts-Amherst"],
    ["166638", "University of Massachusetts-Boston"],
    ["178420", "University of Missouri-St Louis"],
    ["200332", "North Dakota State University-Main Campus"],
    ["120883", "University of the Pacific"],
    ["100751", "The University of Alabama"],
    ["129020", "University of Connecticut"],
    ["209551", "University of Oregon"],
    ["163338", "University of Maryland Eastern Shore"],
    ["135726", "University of Miami"],
    ["219976", "Lipscomb University"],
    ["243744", "Stanford University"],
    ["231174", "University of Vermont"],
    ["215293", "University of Pittsburgh-Pittsburgh Campus"],
    ["155317", "University of Kansas"],
    ["110422", "California Polytechnic State University-San Luis Obispo"],
    ["139931", "Georgia Southern University"],
    ["228644", "The University of Texas Health Science Center at San Antonio"],
    ["492689", "Texas Tech University Health Sciences Center-El Paso"],
    ["132903", "University of Central Florida"]
  ];

  for (const [unitid, name] of expectedResearchMatches) {
    const school = RESEARCH_FUNDING.schools?.[unitid];
    assert(school, `Research export is missing expected matched school ${name} (${unitid})`);
    assert(school.is_primary_tracker === true, `${name} should be in the primary tracker table`);
    assert(school.has_financial_profile === true, `${name} should link to a financial profile`);
    assert(school.financial_unitid === unitid, `${name} should keep financial_unitid ${unitid}`);
  }

  const unmatchedNames = Object.values(RESEARCH_FUNDING.schools || {})
    .filter((school) => school.is_primary_tracker !== true)
    .map((school) => school.institution_name);
  for (const staleName of [
    "University of North Carolina Greensboro",
    "University of Wisconsin System",
    "University of Hawaii",
    "University of Alaska Anchorage Campus",
    "Texas Tech University System",
    "University of Illinois",
    "Loyola University of Chicago",
    "University of Colorado at Denver-Downtown Campus",
    "Utah State Higher Education System--University of Utah",
    "University of Maine System",
    "University of Mass at Boston",
    "University of Missouri-Saint Louis",
    "University of Oregon Eugene",
    "University of Alabama in Tuscaloosa",
    "University of Connecticut Storrs",
    "University of Miami Coral Gables",
    "University of Maryland Es"
  ]) {
    assert(!unmatchedNames.includes(staleName), `${staleName} should not appear in the research "other institutions" table`);
  }
});

run("University of Hawai'i at Hilo accreditation actions match the primary tracker", () => {
  const hilo = ACCREDITATION.schools?.["141565"];
  assert(hilo, "Accreditation export is missing University of Hawaii at Hilo");
  assert(hilo.is_primary_tracker === true, "University of Hawaii at Hilo should be in the primary accreditation table");
  assert(hilo.has_financial_profile === true, "University of Hawaii at Hilo should link to a financial profile");
  assert(hilo.financial_unitid === "141565", "University of Hawaii at Hilo should keep financial_unitid 141565");

  const unmatchedHilo = Object.values(ACCREDITATION.schools || {})
    .filter((school) => school.is_primary_tracker !== true)
    .map((school) => school.institution_name)
    .filter((name) => /Hawai.i.*Hilo|Hawaii.*Hilo/i.test(name || ""));
  assert(unmatchedHilo.length === 0, `Hilo accreditation aliases should not appear in "other institutions": ${unmatchedHilo.join(", ")}`);
});

run("trailing-The accreditation aliases do not leak into accreditation other-institutions exports", () => {
  const expectedAccreditationMatches = [
    ["131283", "The Catholic University of America"],
    ["131469", "George Washington University"],
    ["187134", "The College of New Jersey"],
    ["195234", "The College of Saint Rose"],
    ["197285", "The College of Westchester"],
    ["202763", "The University of Findlay"],
    ["237312", "University of Charleston"],
    ["164988", "Boston University"]
  ];

  const otherInstitutions = Object.values(ACCREDITATION.schools || {})
    .filter((school) => school.is_primary_tracker !== true);

  for (const [unitid, name] of expectedAccreditationMatches) {
    const school = ACCREDITATION.schools?.[unitid];
    if (school) {
      assert(school.is_primary_tracker === true, `${name} should be in the primary accreditation table`);
      assert(school.has_financial_profile === true, `${name} should link to a financial profile`);
      assert(school.financial_unitid === unitid, `${name} should keep financial_unitid ${unitid}`);
    }

    const leakedAlias = otherInstitutions.find((candidate) => {
      const institutionName = String(candidate.institution_name || "");
      const financialUnitid = String(candidate.financial_unitid || "");
      return institutionName === name || financialUnitid === unitid;
    });
    assert(
      !leakedAlias,
      `${name} should not appear in the accreditation other-institutions export`
    );
  }
});

run("WSCUC exports prefer richer DAPIP letter detail only when it beats the scraper heading", () => {
  const findByName = (name) =>
    Object.values(ACCREDITATION.schools || {}).find((school) => school.institution_name === name);

  const providence = findByName("Providence Christian College");
  const academy = findByName("Academy of Art University");
  const sdcc = findByName("San Diego Christian College");
  const eastBay = findByName("California State University-East Bay");

  assert(providence, "Accreditation export is missing Providence Christian College");
  assert(academy, "Accreditation export is missing Academy of Art University");
  assert(sdcc, "Accreditation export is missing San Diego Christian College");
  assert(eastBay, "Accreditation export is missing California State University-East Bay");

  assert(
    (providence.actions || []).some((row) =>
      row.action_label_short ===
      "Placed on Probation because it is out of compliance with standards concerning financial sustainability and quality assurance"
    ),
    "Providence should use the richer WSCUC probation summary from DAPIP letter text"
  );
  assert(
    (academy.actions || []).some((row) =>
      row.action_label_short ===
      "Removed Notice of Concern and issued a Warning because it is out of compliance with standards concerning student completion and resource planning"
    ),
    "Academy of Art should use the richer WSCUC warning summary from DAPIP letter text"
  );
  assert(
    (sdcc.actions || []).some((row) =>
      row.action_label_short ===
      "Removed Show Cause and issued a Warning because it has not demonstrated compliance with Standard 3, CFR 3.4 on financial sustainability and resource planning"
    ),
    "San Diego Christian should use the richer WSCUC warning summary from DAPIP letter text"
  );
  assert(
    !(sdcc.actions || []).some((row) =>
      row.action_date === "2024-06-01" &&
      row.action_label_short === "Remove an order to show cause, issue a warning, issue a good cause extension"
    ),
    "San Diego Christian should not keep the duplicate WSCUC special-visit row once the DAPIP-backed sanction summary is present"
  );
  assert(
    (sdcc.actions || []).some((row) =>
      row.action_date === "2023-11-15" &&
      row.action_label_short ===
        "Continued Show Cause because it has not demonstrated compliance with Standard 3, CFR 3.4 on financial sustainability and resource planning"
    ),
    "San Diego Christian should summarize the November 2023 WSCUC show-cause letter from substantive letter text"
  );
  assert(
    (eastBay.actions || []).some((row) =>
      row.action_label_short ===
      "Issued a Notice of Concern over Standard 3, CFRs 3.4 and 3.5 on financial sustainability and resource planning"
    ),
    "CSU East Bay should use the richer WSCUC notice summary from DAPIP letter text"
  );
});

run("SACSCOC exports keep substantive sanction text and drop low-signal monitoring/report rows", () => {
  const findByName = (name) =>
    Object.values(ACCREDITATION.schools || {}).find((school) => school.institution_name === name);

  const guilford = findByName("Guilford College");
  const highPoint = findByName("High Point University");
  const saintAugustine = findByName("Saint Augustine's University");

  assert(guilford, "Accreditation export is missing Guilford College");
  assert(highPoint, "Accreditation export is missing High Point University");
  assert(saintAugustine, "Accreditation export is missing Saint Augustine's University");

  assert(
    (guilford.actions || []).some((row) =>
      String(row.action_date || "").startsWith("2024-12") &&
      row.action_label_short ===
        "Continued on probation for good cause for twelve months for failure to comply with Core Requirement 13.1 (Financial resources) and Standard 13.3 (Financial responsibility)"
    ),
    "Guilford should keep the detailed December 2024 probation summary from the DAPIP letter text"
  );
  assert(
    !(guilford.actions || []).some((row) =>
      /^Disclosure Statement Regarding /i.test(String(row.action_label_short || ""))
    ),
    "Guilford should not show generic disclosure-statement labels when a detailed sanction row exists"
  );
  assert(
    (highPoint.actions || []).some((row) =>
      row.action_date === "2023-06-15" &&
      row.action_label_short ===
        "Recommended warning for twelve months for failure to comply with Core Requirement 12.1, Standard 8.2.a, and Standard 14.1"
    ),
    "High Point should keep the full standards-backed June 2023 warning summary"
  );
  assert(
    !Object.values(ACCREDITATION.schools || {}).some((school) =>
      school.accreditors === "SACSCOC" &&
      (school.actions || []).some((row) =>
        /^(Requested (?:to submit a )?(?:Referral|Monitoring) Report|No additional report requested)/i.test(
          String(row.action_label_short || "")
        )
      )
    ),
    "SACSCOC exports should not surface low-signal requested-report or no-additional-report rows"
  );
  assert(
    (saintAugustine.actions || []).some((row) =>
      row.action_date === "2024-12-01" &&
      row.action_label_short ===
        "Removed from membership for failure to comply with standards concerning governance, financial resources, financial documents, financial responsibility, control of finances, and sponsored research/external funds"
    ),
    "Saint Augustine should use the richer SACSCOC removal summary instead of the truncated scraper text"
  );
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

run("research export keeps positive de minimis disrupted grant balances", () => {
  const nonPositiveAwards = [];
  const smallPositiveAwards = [];
  for (const school of Object.values(RESEARCH_FUNDING.schools || {})) {
    for (const grant of school.grants || []) {
      const amount = Number(grant.award_remaining);
      if (!Number.isFinite(amount) || amount <= 0) {
        nonPositiveAwards.push(`${school.institution_name || school.unitid}: ${grant.grant_id || grant.source_url} (${amount})`);
      } else if (amount < 100) {
        smallPositiveAwards.push(`${school.institution_name || school.unitid}: ${grant.grant_id || grant.source_url} (${amount})`);
      }
    }
  }
  assert(
    nonPositiveAwards.length === 0,
    `Research export should not include zero or negative remaining awards: ${nonPositiveAwards.slice(0, 10).join(", ")}`
  );
  assert(
    smallPositiveAwards.length > 0,
    "Research export should retain positive sub-$100 disrupted grants so UI-only suppression does not alter the exported dataset."
  );
});

run("research export totals match exported grant rows", () => {
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
