/**
 * syncTabs integration tests.
 *
 * These execute the real js/app.js `syncTabs` against a minimal DOM so we
 * assert the actual rendered hrefs, not just the presence of the symbol.
 *
 * Requirement (set: no-deep-link top nav): the top-nav tabs are pure
 * site-level navigation - each tab always points at its section's landing
 * page, regardless of whether a school is in view. Per-school navigation
 * lives in the in-body "Explore this institution" block, which only
 * surfaces links to sections that actually contain the school.
 */

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const ROOT = path.resolve(__dirname, "..");

function makeTab(id) {
  const attributes = new Map();
  return {
    id,
    href: "",
    classList: {
      _classes: new Set(),
      toggle(name, force) {
        if (force === true) this._classes.add(name);
        else if (force === false) this._classes.delete(name);
        else if (this._classes.has(name)) this._classes.delete(name);
        else this._classes.add(name);
      },
      contains(name) {
        return this._classes.has(name);
      }
    },
    setAttribute(name, value) {
      attributes.set(name, String(value));
    },
    getAttribute(name) {
      return attributes.has(name) ? attributes.get(name) : null;
    },
    removeAttribute(name) {
      attributes.delete(name);
    }
  };
}

function loadAppInContext(bodyDataset = {}) {
  const tabs = {
    "tab-home": makeTab("tab-home"),
    "tab-finances": makeTab("tab-finances"),
    "tab-cuts": makeTab("tab-cuts"),
    "tab-accreditation": makeTab("tab-accreditation"),
    "tab-research": makeTab("tab-research")
  };

  const documentStub = {
    body: { dataset: { ...bodyDataset } },
    getElementById(id) {
      return tabs[id] || null;
    },
    querySelector() {
      return null;
    },
    addEventListener() {}
  };

  const context = {
    console,
    Date,
    URL,
    URLSearchParams,
    document: documentStub,
    fetch: async () => ({ ok: true, async json() { return []; } }),
    window: {
      location: { search: "", pathname: "/index.html" },
      addEventListener() {},
      matchMedia: () => ({ matches: false, addEventListener() {}, removeEventListener() {} })
    }
  };
  context.window.document = documentStub;
  context.global = context;

  const source = fs.readFileSync(path.join(ROOT, "js", "app.js"), "utf8");
  vm.runInNewContext(source, context, { filename: "js/app.js" });

  return { tabs, trackerApp: context.window.TrackerApp };
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

console.log("\n=== syncTabs Integration Tests ===\n");

run("with explicit active tab, tabs point to landing pages", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs({ active: "finances" });
  assert(tabs["tab-home"].href === "index.html", `home href=${tabs["tab-home"].href}`);
  assert(tabs["tab-finances"].href === "school.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("top-nav tabs still point to landing pages (no deep-link)", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs({ active: "cuts" });
  assert(tabs["tab-home"].href === "index.html", `home href=${tabs["tab-home"].href}`);
  assert(tabs["tab-finances"].href === "school.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("body activeTab fallback wins when no explicit active option is passed", () => {
  const { tabs, trackerApp } = loadAppInContext({ activeTab: "cuts", searchSource: "research" });
  trackerApp.syncTabs();
  assert(tabs["tab-cuts"].getAttribute("aria-current") === "page", "cuts tab should have aria-current=page");
  assert(tabs["tab-research"].getAttribute("aria-current") === null, "research tab should not carry aria-current");
});

run("body searchSource fallback marks the matching tab active", () => {
  const { tabs, trackerApp } = loadAppInContext({ searchSource: "research" });
  trackerApp.syncTabs();
  assert(tabs["tab-research"].getAttribute("aria-current") === "page", "research tab should have aria-current=page");
  assert(tabs["tab-cuts"].getAttribute("aria-current") === null, "cuts tab should not carry aria-current");
});

run("aria-current is applied only to the active tab", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs({ active: "accreditation" });
  assert(tabs["tab-accreditation"].getAttribute("aria-current") === "page", "accreditation tab should have aria-current=page");
  assert(tabs["tab-finances"].getAttribute("aria-current") === null, "finances tab should not carry aria-current");
  assert(tabs["tab-cuts"].getAttribute("aria-current") === null, "cuts tab should not carry aria-current");
  assert(tabs["tab-research"].getAttribute("aria-current") === null, "research tab should not carry aria-current");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
