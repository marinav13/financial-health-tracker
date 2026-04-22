/**
 * Lightweight JavaScript static analysis.
 *
 * This intentionally avoids a full lint dependency while still catching common
 * AI-edit damage: syntax errors, merge-conflict markers, and reintroduced
 * unsafe/raw helper escape hatches.
 */

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const ROOT = path.resolve(__dirname, "..");
const JS_DIRS = ["js", "tests"];
const DISALLOWED_PATTERNS = [
  {
    pattern: new RegExp(["<{7}", "={7}", ">{7}"].join("|")),
    message: "merge-conflict marker",
    appliesTo: () => true
  },
  {
    pattern: /\brenderHtmlCell\b|\b__trackerHtml\b/,
    message: "raw HTML table-cell escape hatch",
    appliesTo: (file) => file.startsWith("js/")
  },
  {
    pattern: /\beval\s*\(/,
    message: "eval()",
    appliesTo: (file) => file.startsWith("js/")
  },
  {
    pattern: /\bnew\s+Function\s*\(/,
    message: "new Function()",
    appliesTo: (file) => file.startsWith("js/")
  }
];

function walk(dir) {
  const fullDir = path.join(ROOT, dir);
  if (!fs.existsSync(fullDir)) return [];
  return fs.readdirSync(fullDir, { withFileTypes: true }).flatMap((entry) => {
    const relative = path.join(dir, entry.name);
    if (entry.isDirectory()) return walk(relative);
    return entry.isFile() && entry.name.endsWith(".js") ? [relative] : [];
  });
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

const files = JS_DIRS.flatMap(walk).sort();

console.log("\n=== Lightweight JS Static Analysis ===\n");

run("discovers JavaScript files", () => {
  assert(files.length > 0, "Expected at least one JavaScript file to analyze.");
});

for (const file of files) {
  run(`${file} parses with node --check`, () => {
    const result = spawnSync(process.execPath, ["--check", file], {
      cwd: ROOT,
      encoding: "utf8"
    });
    assert(result.status === 0, (result.stderr || result.stdout || "").trim());
  });

  run(`${file} avoids disallowed static patterns`, () => {
    const source = fs.readFileSync(path.join(ROOT, file), "utf8");
    const normalizedFile = file.replace(/\\/g, "/");
    const hit = DISALLOWED_PATTERNS.find(({ pattern, appliesTo }) => appliesTo(normalizedFile) && pattern.test(source));
    assert(!hit, `Found ${hit?.message}.`);
  });
}

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
