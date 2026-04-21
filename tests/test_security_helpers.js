/**
 * Regression tests for frontend HTML/URL helper contracts.
 *
 * Run with: node tests/test_security_helpers.js
 */

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const ROOT = path.resolve(__dirname, "..");
const APP_SRC = fs.readFileSync(path.join(ROOT, "js", "app.js"), "utf8");

function escapeHtml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

function createTestElement(tagName) {
  const attributes = {};
  return {
    _textContent: "",
    setAttribute(name, value) {
      attributes[name] = String(value);
    },
    get textContent() {
      return this._textContent;
    },
    set textContent(value) {
      this._textContent = String(value ?? "");
    },
    get outerHTML() {
      const attrText = Object.entries(attributes)
        .map(([name, value]) => ` ${name}="${escapeHtml(value)}"`)
        .join("");
      return `<${tagName}${attrText}>${escapeHtml(this._textContent)}</${tagName}>`;
    },
    click() {},
    remove() {}
  };
}

function loadTrackerApp() {
  const context = {
    console: { error() {} },
    URL,
    URLSearchParams,
    Blob: function Blob() {},
    setTimeout,
    fetch: async () => ({ ok: true, json: async () => [] }),
    document: {
      body: { dataset: {} },
      getElementById: () => null,
      createElement: createTestElement
    },
    window: {
      location: { origin: "https://tracker.test", search: "" },
      TrackerApp: {}
    }
  };
  context.global = context;
  vm.runInNewContext(APP_SRC, context, { filename: "js/app.js" });
  return context.window.TrackerApp;
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

console.log("\n=== Frontend Security Helper Tests ===\n");

const app = loadTrackerApp();

run("safeUrl accepts https URLs and normalizes them", () => {
  assert(app.safeUrl("https://example.edu/path?q=1") === "https://example.edu/path?q=1", "Expected https URL to pass");
});

run("safeUrl rejects executable and non-web URL schemes", () => {
  assert(app.safeUrl("javascript:alert(1)") === "", "Expected javascript: URL to be rejected");
  assert(app.safeUrl("data:text/html,<script>alert(1)</script>") === "", "Expected data: URL to be rejected");
  assert(app.safeUrl("ftp://example.edu/file") === "", "Expected ftp: URL to be rejected");
});

run("renderExternalLink escapes label text and href attribute context", () => {
  const html = app.renderExternalLink(
    'https://example.edu/" onmouseover="alert(1)',
    '<img src=x onerror="alert(1)">'
  );
  assert(html.includes('href="https://example.edu/%22%20onmouseover=%22alert(1)"'), "Expected URL serialization inside one href");
  assert(!html.includes('" onmouseover="'), "Expected no injected onmouseover attribute");
  assert(!html.includes("<img"), "Expected label HTML to be escaped");
  assert(html.includes("&lt;img"), "Expected escaped label text");
});

run("renderPaginationButtons marks only the current page", () => {
  const html = app.renderPaginationButtons({ currentPage: 2, totalPages: 3 });
  const currentMatches = html.match(/aria-current="page"/g) || [];
  assert(currentMatches.length === 1, "Expected exactly one aria-current marker");
  assert(html.includes('aria-label="Current page, page 2"'), "Expected current page label");
  assert(html.includes('aria-label="Go to page 3"'), "Expected target page label");
});

run("paginateItems clamps pages and returns the current slice", () => {
  const page = app.paginateItems(["a", "b", "c", "d", "e"], 99, 2);
  assert(page.totalPages === 3, "Expected three total pages");
  assert(page.currentPage === 3, "Expected current page to clamp to last page");
  assert(page.start === 4, "Expected final page start offset");
  assert(page.pageItems.length === 1 && page.pageItems[0] === "e", "Expected final page slice");
});

run("renderSortableHeader puts aria-sort on the active table header only", () => {
  const active = app.renderSortableHeader("funding", { key: "funding", direction: "desc" }, "Funding cut");
  const inactive = app.renderSortableHeader("state", { key: "funding", direction: "desc" }, "State");
  assert(active.includes("<th aria-sort=\"descending\">"), "Expected active header to expose descending sort");
  assert(!inactive.includes("aria-sort="), "Expected inactive header to omit aria-sort");
  assert(active.includes('data-sort-key="funding"'), "Expected sort key data attribute");
});

run("renderSchoolLink escapes labels at the helper boundary", () => {
  const html = app.renderSchoolLink("123", '<svg onload="alert(1)">Bad U</svg>', "school.html");
  assert(html.includes('href="school.html?unitid=123"'), "Expected school link");
  assert(!html.includes("<svg"), "Expected label markup to be escaped");
  assert(!html.includes('" onload="'), "Expected no injected onload attribute");
  assert(html.includes("&lt;svg"), "Expected escaped label in link text");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
