/**
 * Deployed GitHub Pages parity check.
 *
 * Compares key deployed assets against the committed artifact after Pages
 * publishes. This catches stale deploys, missing files, and accidental deploys
 * from something other than the current repository contents.
 */

const fs = require("fs");
const path = require("path");

const ROOT = path.resolve(__dirname, "..");
const BASE_URL = (process.env.PAGES_BASE_URL || "https://marinav13.github.io/financial-health-tracker").replace(/\/+$/, "");
const ATTEMPTS = Math.max(1, Number(process.env.PAGES_PARITY_ATTEMPTS || 1));
const RETRY_MS = Math.max(0, Number(process.env.PAGES_PARITY_RETRY_MS || 0));
const ASSET_PATHS = [
  "data/metadata.json",
  "data/schools_index.json",
  "data/college_cuts_index.json",
  "data/accreditation_index.json",
  "data/research_funding_index.json"
];
const PAGE_CHECKS = [
  { path: "index.html", marker: 'id="school-search"' },
  { path: "cuts.html", marker: 'id="cuts-list"' },
  { path: "research.html", marker: 'id="research-list"' },
  { path: "accreditation.html", marker: 'id="accreditation-status"' },
  { path: "school.html?unitid=222178", marker: 'id="school-name"' }
];

function committedText(relativePath) {
  return fs.readFileSync(path.join(ROOT, relativePath), "utf8").replace(/\r\n/g, "\n").trim();
}

async function deployedText(relativePath) {
  const url = `${BASE_URL}/${relativePath}`;
  const response = await fetch(url, { headers: { "cache-control": "no-cache" } });
  if (!response.ok) {
    throw new Error(`${url} returned ${response.status}`);
  }
  return (await response.text()).replace(/\r\n/g, "\n").trim();
}

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

let passed = 0;
let failed = 0;

async function run(name, fn) {
  let lastError = null;
  for (let attempt = 1; attempt <= ATTEMPTS; attempt += 1) {
    try {
      await fn();
      console.log(`  PASS: ${name}`);
      passed++;
      return;
    } catch (error) {
      lastError = error;
      if (attempt < ATTEMPTS && RETRY_MS > 0) {
        await sleep(RETRY_MS);
      }
    }
  }

  try {
    throw lastError;
  } catch (error) {
    console.log(`  FAIL: ${name}: ${error?.message || String(error)}`);
    failed++;
  }
}

console.log("\n=== Deployed GitHub Pages Parity Check ===\n");
console.log(`Base URL: ${BASE_URL}\n`);

(async () => {
  for (const { path: pagePath, marker } of PAGE_CHECKS) {
    await run(`${pagePath} is deployed and recognizable`, async () => {
      const html = await deployedText(pagePath);
      assert(html.includes(marker), `Expected deployed page to include ${marker}`);
    });
  }

  for (const assetPath of ASSET_PATHS) {
    await run(`${assetPath} matches committed artifact`, async () => {
      const deployed = await deployedText(assetPath);
      const committed = committedText(assetPath);
      assert(deployed === committed, `Deployed ${assetPath} does not match the committed file.`);
    });
  }

  console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
  if (failed > 0) process.exit(1);
})();
