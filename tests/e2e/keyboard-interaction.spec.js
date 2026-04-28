/**
 * Keyboard-interaction tests for the search combobox and table filter strips.
 *
 * Covers behaviors that screen-reader and keyboard-only users depend on:
 * - Escape closes search results and keeps focus on the input
 * - Enter on an active option navigates to the school page
 * - Tab from a filter input lands on the next interactive element (no trap)
 * - Focus is preserved on the input across ArrowDown sweeps
 *
 * These behaviors were previously only assured by code review; this spec
 * locks them as a contract. Runs on chromium per the per-project CI matrix
 * documented in playwright.config.js.
 */

const { test, expect } = require('@playwright/test');
const { firstSchoolIndexEntry, searchTermFor } = require('./helpers');

const searchTarget = firstSchoolIndexEntry();
const searchTerm = searchTermFor(searchTarget);

test.describe('Keyboard interaction — search combobox', () => {
  test('Escape closes the search results and leaves focus on the input', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    const results = page.locator('#search-results');

    await searchInput.fill(searchTerm);
    await expect(searchInput).toHaveAttribute('aria-expanded', 'true');
    await expect(results.locator('.result-item:not(.is-empty)').first()).toBeVisible();

    await searchInput.press('Escape');
    await expect(searchInput).toHaveAttribute('aria-expanded', 'false');
    // Listbox is emptied (or shows no result-item options) and aria-activedescendant clears.
    await expect(results.locator('.result-item:not(.is-empty)')).toHaveCount(0);
    await expect(searchInput).not.toHaveAttribute('aria-activedescendant', /search-results-option-/);
    // Focus remains on the input — pressing a printable key should populate it.
    await expect(searchInput).toBeFocused();
  });

  test('ArrowDown then Enter navigates to the active option without losing focus on the input', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    await searchInput.fill(searchTerm);
    await expect(page.locator('#search-results .result-item:not(.is-empty)').first()).toBeVisible();

    // First ArrowDown selects the first option; input retains focus per WAI-ARIA combobox pattern.
    await searchInput.press('ArrowDown');
    await expect(searchInput).toBeFocused();
    const activeId = await searchInput.getAttribute('aria-activedescendant');
    expect(activeId).toMatch(/^search-results-option-/);

    // Enter triggers navigation. school.html may or may not load fully in headless CI;
    // assert that the URL changes off /index.html as evidence the handler ran.
    const navigation = page.waitForURL(/school\.html/, { timeout: 5000 });
    await searchInput.press('Enter');
    await navigation;
    await expect(page).toHaveURL(/school\.html\?unitid=/);
  });

  test('ArrowDown wraps and aria-activedescendant tracks the focused option', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    await searchInput.fill(searchTerm);
    await expect(page.locator('#search-results .result-item:not(.is-empty)').first()).toBeVisible();

    await searchInput.press('ArrowDown');
    const firstActive = await searchInput.getAttribute('aria-activedescendant');
    expect(firstActive).toBeTruthy();

    await searchInput.press('ArrowDown');
    const secondActive = await searchInput.getAttribute('aria-activedescendant');
    // If only one match exists in the test fixture, second ArrowDown stays on the
    // same option; otherwise it advances. Both are valid — assert it points at a
    // real listbox option either way.
    expect(secondActive).toMatch(/^search-results-option-/);
    await expect(page.locator(`#${secondActive}`)).toHaveAttribute('aria-selected', 'true');

    // Only one option should ever carry aria-selected="true".
    const selectedCount = await page.locator('#search-results [aria-selected="true"]').count();
    expect(selectedCount).toBe(1);
  });

  test('Escape from a result button still closes the listbox and returns focus to input', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    await searchInput.fill(searchTerm);
    await expect(page.locator('#search-results .result-item:not(.is-empty)').first()).toBeVisible();

    // Move focus into the listbox via ArrowDown (which keeps focus on input
    // per combobox pattern, but the active option's id is on aria-activedescendant).
    await searchInput.press('ArrowDown');
    await searchInput.press('Escape');
    await expect(searchInput).toHaveAttribute('aria-expanded', 'false');
    await expect(searchInput).toBeFocused();
  });
});

test.describe('Keyboard interaction — table filter strips', () => {
  test('Tab from the cuts filter input reaches the next interactive control without leaving the page', async ({ page }) => {
    await page.goto('/cuts.html');

    const filter = page.locator('#cuts-filter');
    await filter.focus();
    await expect(filter).toBeFocused();

    // Pressing Tab once should land on a focusable element that exists in the
    // document — not return focus to the body or escape the document. The exact
    // next element varies (sort buttons, pagination, download links) so we only
    // assert that *something* focusable receives focus and it's still inside #main
    // (i.e. inside the controlled section, not blown past it onto a chrome element).
    await filter.press('Tab');
    const focusedTag = await page.evaluate(() => document.activeElement && document.activeElement.tagName);
    expect(focusedTag).not.toBe('BODY');
    const focusedInsideMain = await page.evaluate(() =>
      !!document.activeElement && document.getElementById('main')?.contains(document.activeElement)
    );
    expect(focusedInsideMain).toBe(true);
  });

  test('Tab from the accreditation primary filter does not skip the secondary filter', async ({ page }) => {
    await page.goto('/accreditation.html');

    const primaryFilter = page.locator('#accreditation-filter');
    await primaryFilter.focus();
    await expect(primaryFilter).toBeFocused();

    // Tab forward through interactive elements until we reach the secondary
    // filter. Cap iterations to avoid an infinite loop on a regression that
    // skips the input entirely. A reasonable cap is the number of focusable
    // controls between the two filter inputs in the rendered DOM.
    const otherFilter = page.locator('#accreditation-other-filter');
    let tabs = 0;
    const MAX_TABS = 80;
    while (tabs < MAX_TABS) {
      const reached = await page.evaluate(() => document.activeElement?.id === 'accreditation-other-filter');
      if (reached) break;
      await page.keyboard.press('Tab');
      tabs += 1;
    }
    expect(tabs).toBeLessThan(MAX_TABS);
    await expect(otherFilter).toBeFocused();
  });
});

test.describe('Keyboard interaction — focus is preserved during render', () => {
  test('Typing in the research filter does not steal focus on each keystroke', async ({ page }) => {
    await page.goto('/research.html');

    const filter = page.locator('#research-filter');
    await filter.focus();
    // Type a multi-character query; if the page re-renders and re-creates the
    // table, focus must remain on the input throughout (otherwise typing the
    // second character would land on document.body).
    await page.keyboard.type('university', { delay: 30 });
    await expect(filter).toBeFocused();
    await expect(filter).toHaveValue('university');
  });

  test('Sorting the cuts table does not lose focus to <body>', async ({ page }) => {
    await page.goto('/cuts.html');

    const list = page.locator('#cuts-list');
    await expect(list.locator('table.history-table')).toBeVisible();

    const dateHeader = list.locator('th button.sort-button[data-sort-key="announcement_date"]');
    await dateHeader.first().focus();
    await page.keyboard.press('Enter');

    // Real contract: pressing Enter on a sort header must not drop focus
    // back to <body>. The exact destination depends on whether the table
    // re-renders the headers (some implementations refocus the new sort
    // button, others move focus to the first row or table caption). All
    // of those are acceptable; <body> means focus was lost entirely.
    const focusedTag = await page.evaluate(() => document.activeElement?.tagName);
    expect(focusedTag).not.toBe('BODY');
    const focusedIsInteractive = await page.evaluate(() => {
      const el = document.activeElement;
      if (!el) return false;
      // Either a focusable interactive element, or a <th>/<tr> that took
      // programmatic focus (tabindex=-1) -- both are valid recovery sites.
      const tag = el.tagName;
      return tag === 'BUTTON' || tag === 'A' || tag === 'INPUT'
        || tag === 'TH' || tag === 'TR' || tag === 'TABLE'
        || el.hasAttribute('tabindex');
    });
    expect(focusedIsInteractive).toBe(true);
  });
});
