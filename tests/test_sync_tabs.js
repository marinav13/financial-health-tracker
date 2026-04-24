/**
 * syncTabs integration tests.
 *
 * These execute the real js/app.js `syncTabs` against a minimal DOM so we
 * assert the *actual* rendered hrefs, not just the presence of the symbol.
 *
 * Requirement (set: no-deep-link top nav): the top-nav tabs are pure
 * site-level navigation — each tab always points at its section's landing
 * page, regardless of whether a school is in view. Per-school navigation
 * lives in the in-body "Explore this institution" block, which only
 * surfaces links to sections that actually contain the school.
 *
 * An earlier iteration deep-linked the top nav to the current school. That
 * regressed UX for the majority of schools, which aren't tracked in
 * cuts/accreditation/research, because clicking a top tab from a school
 * detail page would land on an empty "No X found" state. Reverted; these
 * tests lock the landing-page-only contract.
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

function loadAppInContext() {
  const tabs = {
    "tab-finances": makeTab("tab-finances"),
    "tab-cuts": makeTab("tab-cuts"),
    "tab-accreditation": makeTab("tab-accreditation"),
    "tab-research": makeTab("tab-research")
  };

  const documentStub = {
    body: { dataset: {} },
    getElementById(id) {
      return tabs[id] || null;
    },
    querySelector() {
      return null;
    },
    addEventListener() {}
  };

  // js/app.js expects `fetch`, `window`, and various DOM globals. We only
  // exercise syncTabs so provide the minimum surface area.
  const context = {
    console,
    Date,
    URL,
    URLSearchParams,
    document: documentStub,
    // js/app.js calls initSearch() at load time, which fetches the schools
    // index. We don't exercise search here; return an empty response so
    // init completes cleanly and doesn't log noise.
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

run("with no unitid, tabs point to landing pages", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("", { active: "finances" });
  assert(tabs["tab-finances"].href === "index.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("with numeric unitid, tabs still point to landing pages (no deep-link)", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("100654", { active: "cuts" });
  assert(tabs["tab-finances"].href === "index.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("financialUnitid option does not change top-nav hrefs", () => {
  const { tabs, trackerApp } = loadAppInContext();
  // This is the exact call shape cuts.js/research.js/accreditation.js use on
  // school-context pages (a system member whose finances roll up to a parent
  // unitid). Top nav is site-level only — financialUnitid does not influence
  // hrefs. Per-school routing happens in the in-body related-pages block.
  trackerApp.syncTabs("100654", { active: "cuts", financialUnitid: "100663" });
  assert(tabs["tab-finances"].href === "index.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("with non-numeric namespaced unitid, tabs still point to landing pages", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("cut-abc123", { active: "cuts" });
  assert(tabs["tab-finances"].href === "index.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("aria-current is applied only to the active tab", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("100654", { active: "accreditation" });
  assert(tabs["tab-accreditation"].getAttribute("aria-current") === "page", "accreditation tab should have aria-current=page");
  assert(tabs["tab-finances"].getAttribute("aria-current") === null, "finances tab should not carry aria-current");
  assert(tabs["tab-cuts"].getAttribute("aria-current") === null, "cuts tab should not carry aria-current");
  assert(tabs["tab-research"].getAttribute("aria-current") === null, "research tab should not carry aria-current");
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
