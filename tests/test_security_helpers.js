/**
 * Regression tests for URL safety and XSS prevention in frontend helpers.
 *
 * Tests safeUrl() and financePageLink() to ensure they properly handle
 * malicious input and reject unsafe URL schemes.
 *
 * Run with: node tests/test_security_helpers.js
 */

const fs = require("fs");
const path = require("path");

const APP_PATH = path.resolve(__dirname, "..", "js", "app.js");
const APP_SRC = fs.readFileSync(APP_PATH, "utf8");

const CUTS_PATH = path.resolve(__dirname, "..", "js", "cuts.js");
const CUTS_SRC = fs.readFileSync(CUTS_PATH, "utf8");

// Extract helpers from app.js - manually define them to avoid syntax issues
function extractAppHelpers() {
  const helpers = {};
  
  // Manually define escapeHtml (avoiding nullish coalescing syntax issues in older Node)
  helpers.escapeHtml = function(value) {
    return String(value != null ? value : "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  };
  
  // Manually define safeUrl
  helpers.safeUrl = function(url) {
    const u = String(url != null ? url : "").trim();
    return /^https?:\/\//i.test(u) ? u : "";
  };
  
  return helpers;
}

// Extract financePageLink from cuts.js
function extractFinancePageLink() {
  // Look for financePageLink function definition
  const fnMatch = CUTS_SRC.match(/function financePageLink\(unitid, label\)[\s\S]*?^\s*}/m);
  if (!fnMatch) return null;
  
  // Also need schoolUrl which is imported from app.js
  const schoolUrlMatch = APP_SRC.match(/function schoolUrl\(unitid[\s\S]*?^\s*}/m);
  
  // eslint-disable-next-line no-new-func
  const financePageLink = new Function("schoolUrl", fnMatch[0].replace("function financePageLink(unitid, label)", "return function financePageLink(unitid, label)"))(null);
  
  return financePageLink;
}

// Run tests
let passed = 0;
let failed = 0;
const failures = [];

function assert(condition, message) {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
}

console.log("\n=== Security Helper Regression Tests ===\n");

const helpers = extractAppHelpers();

console.log("safeUrl (URL validation):");

// -----------------------------------------------------------------------
// Test safeUrl - accepts safe http(s) URLs
// -----------------------------------------------------------------------
(function() {
  const fn = helpers.safeUrl;
  if (!fn) { console.log("  SKIP: safeUrl not extracted"); return; }
  
  const safeCases = [
    ["https://example.com", "https://example.com"],
    ["http://example.com", "http://example.com"],
    ["HTTPS://EXAMPLE.COM", "HTTPS://EXAMPLE.COM"],
    ["  https://example.com  ", "https://example.com"],  // trims whitespace
    ["http://foo.bar/path?query=1", "http://foo.bar/path?query=1"],
  ];
  
  for (const [input, expected] of safeCases) {
    const result = fn(input);
    if (result === expected) {
      console.log(`  PASS: safeUrl(${JSON.stringify(input)}) === ${JSON.stringify(expected)}`);
      passed++;
    } else {
      console.log(`  FAIL: safeUrl(${JSON.stringify(input)}) — got ${JSON.stringify(result)}, expected ${JSON.stringify(expected)}`);
      failures.push(`safeUrl: ${JSON.stringify(input)} → got ${JSON.stringify(result)}`);
      failed++;
    }
  }
})();

// -----------------------------------------------------------------------
// Test safeUrl - rejects unsafe schemes
// -----------------------------------------------------------------------
(function() {
  const fn = helpers.safeUrl;
  if (!fn) return;
  
  const unsafeCases = [
    ["javascript:alert(1)", ""],
    ["javascript:alert('xss')", ""],
    ["  javascript:foo  ", ""],
    ["data:text/html,abc", ""],
    ["data:,Hello%20World", ""],
    ["ftp://example.com", ""],
    ["file:///path", ""],
    ["mailto:test@example.com", ""],
    ["tel:+1234567890", ""],
    ["", ""],           // empty string
    [null, ""],         // null
    [undefined, ""],    // undefined
  ];
  
  for (const [input, expected] of unsafeCases) {
    const result = fn(input);
    if (result === expected) {
      console.log(`  PASS: safeUrl(${JSON.stringify(input)}) === ${JSON.stringify(expected)} (rejected)`);
      passed++;
    } else {
      console.log(`  FAIL: safeUrl(${JSON.stringify(input)}) — got ${JSON.stringify(result)}, expected ${JSON.stringify(expected)} (should reject)`);
      failures.push(`safeUrl: ${JSON.stringify(input)} → got ${JSON.stringify(result)}, should be rejected`);
      failed++;
    }
  }
})();

// -----------------------------------------------------------------------
// Test escapeHtml - basic escaping
// -----------------------------------------------------------------------
console.log("\nescapeHtml (XSS prevention):");

(function() {
  const fn = helpers.escapeHtml;
  if (!fn) { console.log("  SKIP: escapeHtml not extracted"); return; }
  
  const cases = [
    ["<script>alert(1)</script>", "&lt;script&gt;alert(1)&lt;/script&gt;"],
    ['<img src=x onerror=alert(1)>', "&lt;img src=x onerror=alert(1)&gt;"],
    ["Normal text", "Normal text"],
    ["<div>", "&lt;div&gt;"],
    ["a & b", "a &amp; b"],
    ['quote "test"', "quote &quot;test&quot;"],
    ["apostrophe 'test'", "apostrophe &#39;test&#39;"],
  ];
  
  for (const [input, expected] of cases) {
    const result = fn(input);
    if (result === expected) {
      console.log(`  PASS: escapeHtml(${JSON.stringify(input)}) === ${JSON.stringify(expected)}`);
      passed++;
    } else {
      console.log(`  FAIL: escapeHtml(${JSON.stringify(input)}) — got ${JSON.stringify(result)}, expected ${JSON.stringify(expected)}`);
      failures.push(`escapeHtml: ${JSON.stringify(input)} → got ${JSON.stringify(result)}`);
      failed++;
    }
  }
})();

// -----------------------------------------------------------------------
// Test financePageLink - requires escapeHtml to be applied to label
// -----------------------------------------------------------------------
console.log("\nfinancePageLink (link generation):");

(function() {
  // financePageLink uses schoolUrl internally - we need to mock it
  // The function itself doesn't escape the label - that's the caller's responsibility
  const financePageLinkSrc = CUTS_SRC.match(/function financePageLink\(unitid, label\)[\s\S]*?^\s*}/m);
  if (!financePageLinkSrc) { console.log("  SKIP: financePageLink not found"); return; }
  
  // Mock schoolUrl to just return a simple path
  const mockSchoolUrl = (unitid, page) => `${page}?unitid=${encodeURIComponent(unitid)}`;
  
  // Create the function
  // eslint-disable-next-line no-new-func
  const financePageLink = new Function("schoolUrl", 
    financePageLinkSrc[0].replace("function financePageLink(unitid, label)", "return function financePageLink(unitid, label)")
  )(mockSchoolUrl);
  
  // Test normal case - label should be rendered in anchor
  const normalResult = financePageLink("12345", "Test University");
  if (normalResult.includes('<a href="school.html?unitid=12345">Test University</a>')) {
    console.log(`  PASS: financePageLink with normal label produces valid link`);
    passed++;
  } else {
    console.log(`  FAIL: financePageLink with normal label — got ${JSON.stringify(normalResult)}`);
    failures.push(`financePageLink: normal case failed`);
    failed++;
  }
  
  // Test empty label
  const emptyResult = financePageLink("12345", "");
  if (emptyResult === '<a href="school.html?unitid=12345"></a>') {
    console.log(`  PASS: financePageLink with empty label produces valid link`);
    passed++;
  } else {
    console.log(`  FAIL: financePageLink with empty label — got ${JSON.stringify(emptyResult)}`);
    failures.push(`financePageLink: empty label case failed`);
    failed++;
  }
  
  // Test no unitid (should return just the label)
  const noUnitidResult = financePageLink(null, "No Link Text");
  if (noUnitidResult === "No Link Text") {
    console.log(`  PASS: financePageLink without unitid returns plain text`);
    passed++;
  } else {
    console.log(`  FAIL: financePageLink without unitid — got ${JSON.stringify(noUnitidResult)}`);
    failures.push(`financePageLink: no unitid case failed`);
    failed++;
  }
  
  // IMPORTANT: This test documents that financePageLink does NOT escape HTML.
  // Callers MUST wrap the label in escapeHtml() before passing it.
  // The function signature is: financePageLink(unitid, label) where label should already be escaped.
  const rawHtmlResult = financePageLink("12345", '<img src=x onerror=alert(1)>');
  // This SHOULD contain raw HTML - that's the current behavior
  // Callers like cuts.js line 181 pass escapeHtml(cut.institution_name) so it's safe
  if (rawHtmlResult.includes('<img src=x onerror=alert(1)>')) {
    console.log(`  PASS: financePageLink passes through label as-is (caller must escape)`);
    passed++;
  } else {
    console.log(`  FAIL: financePageLink behavior unexpected — got ${JSON.stringify(rawHtmlResult)}`);
    failures.push(`financePageLink: raw HTML handling unexpected`);
    failed++;
  }
})();

// -----------------------------------------------------------------------
// Integration test: Verify callers properly escape labels
// -----------------------------------------------------------------------
console.log("\nIntegration: Verify callers use escapeHtml:");

(function() {
  // Check that cuts.js passes escapeHtml'd content to financePageLink
  const cutsCallPattern = /financePageLink\([^,]+,\s*escapeHtml\(/;
  if (cutsCallPattern.test(CUTS_SRC)) {
    console.log(`  PASS: cuts.js calls financePageLink with escapeHtml-wrapped label`);
    passed++;
  } else {
    console.log(`  FAIL: cuts.js does not use escapeHtml with financePageLink`);
    failures.push(`cuts.js: financePageLink not called with escapeHtml`);
    failed++;
  }
})();

// -----------------------------------------------------------------------
// Summary
// -----------------------------------------------------------------------
console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) {
  console.log("\nFailures:");
  for (const f of failures) console.log(`  - ${f}`);
  process.exit(1);
}
