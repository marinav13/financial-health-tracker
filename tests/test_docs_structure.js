const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..");
const README = fs.readFileSync(path.join(ROOT, "README.md"), "utf8");
const OPERATIONS = fs.readFileSync(path.join(ROOT, "docs", "OPERATIONS_MANUAL.md"), "utf8");
const HANDOFF = fs.readFileSync(path.join(ROOT, "docs", "DEPLOY_HANDOFF.md"), "utf8");

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

console.log("\n=== Documentation Structure Tests ===\n");

run("README points readers to the operations manual and deploy handoff", () => {
  assert(README.includes("docs/OPERATIONS_MANUAL.md"), "Expected README to point to docs/OPERATIONS_MANUAL.md");
  assert(README.includes("docs/DEPLOY_HANDOFF.md"), "Expected README to point to docs/DEPLOY_HANDOFF.md");
  assert(README.includes("Private source-and-pipeline repo"), "Expected README to state the private pipeline-repo role");
  assert(README.includes("hechinger/FinancialHealth"), "Expected README to name the public site repo");
});

run("repo docs avoid Looker Studio framing", () => {
  assert(!/Looker Studio/i.test(README), "README should not mention Looker Studio");
  assert(!/Looker Studio/i.test(OPERATIONS), "Operations manual should not mention Looker Studio");
  assert(!/Looker Studio/i.test(HANDOFF), "Deploy handoff should not mention Looker Studio");
});

run("operations manual owns workflow and rebuild detail", () => {
  assert(/## Active Workflows/.test(OPERATIONS), "Expected Active Workflows section in operations manual");
  assert(/## Build Architecture/.test(OPERATIONS), "Expected Build Architecture section in operations manual");
  assert(/refresh-ipeds-site-data\.yml/.test(OPERATIONS), "Expected weekly refresh workflow coverage");
});

run("deploy handoff explains the two-repo publish model", () => {
  assert(HANDOFF.includes("## How the site is served"), "Expected serving section");
  assert(HANDOFF.includes("## How data reaches the site"), "Expected data-flow section");
  assert(HANDOFF.includes("hechinger/FinancialHealth"), "Expected the public site repo to be named");
  assert(HANDOFF.includes("PUBLIC_SITE_DEPLOY_TOKEN"), "Expected the publish step's token requirement to be documented");
  assert(HANDOFF.includes("Never edit `data/` in the public repo by hand"), "Expected the hand-edit warning for public data");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
