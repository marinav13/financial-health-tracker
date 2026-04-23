/**
 * Empty-data rendering — behavior locks for the case where a page's source
 * JSON has valid shape but zero rows. This scenario is not covered by the
 * JSON-export tests (which assert shape) or the live-data e2e specs (which
 * assert behavior against the real, populated exports).
 *
 * The contract these tests lock is user-visible, not implementation:
 *   1. No JS crash (no pageerror, no console.error beyond benign network 404s
 *      from assets the empty-data page doesn't need).
 *   2. Page's main container is visible — the controller reached its render
 *      path instead of bailing before the DOM was updated.
 *   3. No raw "undefined" / "[object Object]" / "NaN" leaks into body text.
 *   4. Filter/search controls do not throw when driven over empty data.
 *   5. aria-hidden attributes remain synchronized with actual visibility
 *      (hidden sections declare aria-hidden="true"; visible sections do not
 *      claim aria-hidden="true"). This is the claim made in CLAUDE.md:
 *      "Keep aria-* attributes synchronized with actual UI state."
 *
 * Each page's data fetch is intercepted via page.route BEFORE navigation so
 * the controller sees the empty fixture rather than the production export.
 * Glob pattern `**\/data/...` matches regardless of host/port.
 */

const { test, expect } = require('@playwright/test');

// Empty-but-valid fixtures. Shapes mirror the live exports at the structural
// level so page controllers hit the empty branch rather than throwing on
// missing top-level keys.
const EMPTY_ACCREDITATION = {
  generated_at: '2025-12-01T00:00:00Z',
  covered_accreditors: [],
  source_coverage: [],
  not_covered: [],
  schools: {}
};
const EMPTY_CUTS = {
  generated_at: '2025-12-01T00:00:00Z',
  recent: [],
  schools: {}
};
const EMPTY_RESEARCH = {
  generated_at: '2025-12-01T00:00:00Z',
  agencies: [],
  schools: {}
};
const EMPTY_SCHOOLS_INDEX = [];

/**
 * Collects pageerror and console.error events so assertions can run after
 * render has settled. Returns the live array; callers can check it at the
 * end of each test.
 */
function collectPageErrors(page) {
  const errors = [];
  page.on('pageerror', (error) => {
    errors.push(`pageerror: ${error.message}`);
  });
  page.on('console', (msg) => {
    if (msg.type() !== 'error') return;
    const text = msg.text();
    // Favicon 404s and similar asset noise are not empty-data bugs; skip.
    if (/favicon|\.png|\.jpg|\.svg/i.test(text)) return;
    errors.push(`console.error: ${text}`);
  });
  return errors;
}

/**
 * Asserts no raw placeholder strings leaked into the body. These are the
 * signals that a template interpolation hit undefined/null/object where it
 * should have hit an empty-state short-circuit.
 */
async function expectNoPlaceholderLeaks(page) {
  const bodyText = (await page.locator('body').innerText()).toLowerCase();
  expect(bodyText).not.toContain('undefined');
  expect(bodyText).not.toContain('[object object]');
  // Narrow NaN match — the word can legitimately appear inside other words
  // ("financial"), so require it as a token.
  expect(bodyText).not.toMatch(/\bnan\b/);
}

/**
 * Asserts that every element with aria-hidden="true" is actually hidden (or
 * has zero bounding box), and that no element with aria-hidden="false" is
 * invisible. Templating regressions that flip visibility without updating
 * aria-hidden are invisible to sighted users but break screen readers.
 */
async function expectAriaHiddenInSync(page) {
  const mismatches = await page.evaluate(() => {
    const violations = [];
    document.querySelectorAll('[aria-hidden]').forEach((el) => {
      const claim = el.getAttribute('aria-hidden');
      const style = window.getComputedStyle(el);
      const visuallyHidden =
        style.display === 'none' ||
        style.visibility === 'hidden' ||
        el.hasAttribute('hidden');
      if (claim === 'true' && !visuallyHidden) {
        // Allow aria-hidden="true" on purely decorative visible elements
        // (icons, dividers). Heuristic: if it has no text content and no
        // interactive descendants, don't flag.
        const txt = (el.textContent || '').trim();
        const interactive = el.querySelector('a, button, input, select, textarea');
        if (txt.length > 0 || interactive) {
          violations.push(`aria-hidden="true" but visible with text: ${el.id || el.tagName}`);
        }
      }
      if (claim === 'false' && visuallyHidden) {
        violations.push(`aria-hidden="false" but hidden: ${el.id || el.tagName}`);
      }
    });
    return violations;
  });
  expect(mismatches, mismatches.join('\n')).toEqual([]);
}

test.describe('Empty-data rendering', () => {
  test('accreditation.html handles {schools:{}} without crashing or leaking', async ({ page }) => {
    const errors = collectPageErrors(page);
    await page.route('**/data/accreditation.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(EMPTY_ACCREDITATION)
      })
    );
    await page.route('**/data/accreditation_index.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({})
      })
    );

    await page.goto('/accreditation.html');

    await expect(page.locator('#accreditation-status')).toBeVisible();
    await expectNoPlaceholderLeaks(page);

    // Filter drives the table controller. On empty data it must no-op, not throw.
    await page.locator('#accreditation-filter').fill('probation');

    // Primary table body has no rows.
    const primaryRows = page.locator('#accreditation-status table.history-table tbody tr');
    await expect.poll(() => primaryRows.count()).toBe(0);

    await expectAriaHiddenInSync(page);
    expect(errors).toEqual([]);
  });

  test('cuts.html handles {schools:{}} without crashing or leaking', async ({ page }) => {
    const errors = collectPageErrors(page);
    await page.route('**/data/college_cuts.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(EMPTY_CUTS)
      })
    );
    await page.route('**/data/college_cuts_index.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({})
      })
    );

    await page.goto('/cuts.html');

    await expect(page.locator('#cuts-list')).toBeVisible();
    await expectNoPlaceholderLeaks(page);

    await page.locator('#cuts-filter').fill('layoff');

    await expectAriaHiddenInSync(page);
    expect(errors).toEqual([]);
  });

  test('research.html handles {schools:{}} without crashing or leaking', async ({ page }) => {
    const errors = collectPageErrors(page);
    await page.route('**/data/research_funding.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(EMPTY_RESEARCH)
      })
    );
    await page.route('**/data/research_funding_index.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({})
      })
    );

    await page.goto('/research.html');

    await expect(page.locator('#research-list')).toBeVisible();
    await expectNoPlaceholderLeaks(page);

    await page.locator('#research-filter').fill('nih');

    await expectAriaHiddenInSync(page);
    expect(errors).toEqual([]);
  });

  test('index.html handles empty schools_index.json without crashing or leaking', async ({ page }) => {
    const errors = collectPageErrors(page);
    await page.route('**/data/schools_index.json', (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(EMPTY_SCHOOLS_INDEX)
      })
    );

    await page.goto('/index.html');

    await expect(page.locator('#school-search')).toBeVisible();
    await expectNoPlaceholderLeaks(page);

    // Typing a search term into an empty index must not throw.
    await page.locator('#school-search').fill('harvard');

    await expectAriaHiddenInSync(page);
    expect(errors).toEqual([]);
  });
});
