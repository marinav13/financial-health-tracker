/**
 * syncTabs integration tests.
 *
 * These execute the real js/app.js `syncTabs` against a minimal DOM so we
 * assert the *actual* rendered hrefs, not just the presence of the symbol.
 * Previously all callers passed { financialUnitid: ... } and the function
 * silently ignored it, so cross-tab navigation on a school-context page
 * stripped the unitid from every tab link. A regression of that bug would
 * now fail one of the deep-link assertions below.
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

run("with numeric unitid, tabs deep-link to the same school", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("100654", { active: "cuts" });
  assert(tabs["tab-finances"].href === "school.html?unitid=100654", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html?unitid=100654", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html?unitid=100654", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html?unitid=100654", `research href=${tabs["tab-research"].href}`);
});

run("with financialUnitid option, finances tab uses the finance unitid while others use the page unitid", () => {
  const { tabs, trackerApp } = loadAppInContext();
  // This is the exact call shape cuts.js/research.js/accreditation.js use on
  // school-context pages: the page unitid differs from the finances unitid
  // (e.g. a system member whose finances roll up to a parent unitid).
  trackerApp.syncTabs("100654", { active: "cuts", financialUnitid: "100663" });
  assert(tabs["tab-finances"].href === "school.html?unitid=100663", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html?unitid=100654", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html?unitid=100654", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html?unitid=100654", `research href=${tabs["tab-research"].href}`);
});

run("with non-numeric namespaced unitid, tabs fall back to landing pages", () => {
  const { tabs, trackerApp } = loadAppInContext();
  // Namespaced unmatched ids (e.g. "cut-abc123") should never produce deep
  // links — renderRelatedInstitutionLinks also suppresses them via the
  // shared `relatedPageUnitid` helper, and syncTabs must stay consistent.
  trackerApp.syncTabs("cut-abc123", { active: "cuts" });
  assert(tabs["tab-finances"].href === "index.html", `finances href=${tabs["tab-finances"].href}`);
  assert(tabs["tab-cuts"].href === "cuts.html", `cuts href=${tabs["tab-cuts"].href}`);
  assert(tabs["tab-accreditation"].href === "accreditation.html", `accreditation href=${tabs["tab-accreditation"].href}`);
  assert(tabs["tab-research"].href === "research.html", `research href=${tabs["tab-research"].href}`);
});

run("with unitid but no financialUnitid, finances tab uses the page unitid", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("100654", { active: "finances" });
  assert(tabs["tab-finances"].href === "school.html?unitid=100654", `finances href=${tabs["tab-finances"].href}`);
});

run("aria-current is applied only to the active tab", () => {
  const { tabs, trackerApp } = loadAppInContext();
  trackerApp.syncTabs("100654", { active: "accreditation" });
  assert(tabs["tab-accreditation"].getAttribute("aria-current") === "page", "accreditation tab should have aria-current=page");
  assert(tabs["tab-finances"].getAttribute("aria-current") === null, "finances tab should not carry aria-current");
  assert(tabs["tab-cuts"].getAttribute("aria-current") === null, "cuts tab should not carry aria-current");
  assert(tabs["tab-research"].getAttribute("aria-current") === null, "research tab should not carry aria-current");
});

run("encodes URL-unsafe characters in unitid", () => {
  const { tabs, trackerApp } = loadAppInContext();
  // Numeric-only unitids are the only ones that deep-link (see
  // `isNumericUnitid` in app.js). Non-numeric should fall back to landing
  // pages rather than producing a potentially malformed URL.
  trackerApp.syncTabs("abc def", { active: "cuts" });
  assert(tabs["tab-cuts"].href === "cuts.html", `non-numeric unitid should not deep-link, got ${tabs["tab-cuts"].href}`);
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) process.exit(1);
