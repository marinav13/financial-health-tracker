/**
 * End-to-end tests for search functionality.
 * 
 * Tests that:
 * - Search input accepts text
 * - Results appear as user types
 * - Results are filtered correctly
 * - "No results" message shows for unknown schools
 */

const { test, expect } = require('@playwright/test');
const { firstSchoolIndexEntry, schoolIndexEntryByUnitid, searchTermFor, stateWithMultipleSchools } = require('./helpers');

const searchTarget = firstSchoolIndexEntry();
const searchTerm = searchTermFor(searchTarget);
const stateSearch = stateWithMultipleSchools();
const caltech = schoolIndexEntryByUnitid('110404');

test.describe('Search functionality', () => {
  test('shows search input on index page', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await expect(searchInput).toBeVisible();
    await expect(searchInput).toHaveAttribute('placeholder', /Start typing/);
  });

  test('filters results as user types', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await searchInput.fill(searchTerm);
    
    const results = page.locator('#search-results');
    await expect(results).toBeVisible();
    
    const resultItems = results.locator('.result-item:not(.is-empty)');
    await expect(resultItems.first()).toBeVisible();
    
    const firstResult = await resultItems.first().textContent();
    expect(firstResult.toLowerCase()).toContain(searchTerm.toLowerCase());
  });

  test('search exposes combobox state and keyboard active option', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    const results = page.locator('#search-results');
    await expect(searchInput).toHaveAttribute('role', 'combobox');
    await expect(searchInput).toHaveAttribute('aria-controls', 'search-results');
    await expect(searchInput).toHaveAttribute('aria-expanded', 'false');

    await searchInput.fill(searchTerm);
    await expect(searchInput).toHaveAttribute('aria-expanded', 'true');
    await expect(results).toHaveAttribute('role', 'listbox');

    await searchInput.press('ArrowDown');
    const activeId = await searchInput.getAttribute('aria-activedescendant');
    expect(activeId).toMatch(/^search-results-option-/);
    await expect(searchInput).toBeFocused();
    await expect(page.locator(`#${activeId}`)).toHaveAttribute('aria-selected', 'true');
  });

  test('shows no results for unknown school', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await searchInput.fill('XYZNONEXISTENTSCHOOL12345');
    
    await expect(page.locator('#search-results .result-item.is-empty')).toContainText('No matching institutions found');
  });

  test('shows a tracked-school count for state results in a scrollable dropdown', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    await searchInput.fill(stateSearch.state);

    const results = page.locator('#search-results');
    await expect(results).toBeVisible();
    await expect(results.locator('.result-section-title')).toHaveCount(1);
    await expect(results.locator('.result-section-title').first()).toContainText(`Schools (${stateSearch.count} tracked institutions)`);

    const optionCount = await results.locator('.result-item[role="option"]').count();
    expect(optionCount).toBe(stateSearch.count);

    const scrollMetrics = await results.evaluate((node) => ({
      overflowY: window.getComputedStyle(node).overflowY,
      scrollHeight: node.scrollHeight,
      clientHeight: node.clientHeight
    }));
    expect(['auto', 'scroll']).toContain(scrollMetrics.overflowY);
    expect(scrollMetrics.scrollHeight).toBeGreaterThan(scrollMetrics.clientHeight);
  });

  test('alias search returns California Institute of Technology for Caltech', async ({ page }) => {
    await page.goto('/index.html');

    const searchInput = page.locator('#school-search');
    await searchInput.fill('Caltech');

    const firstLabel = page.locator('#search-results .result-item[role="option"] .result-item-label').first();
    await expect(firstLabel).toContainText(caltech.institution_name);
  });

  test('search works on school page', async ({ page }) => {
    await page.goto('/school.html');
    
    const searchInput = page.locator('#school-search');
    await expect(searchInput).toBeVisible();
    
    await searchInput.fill(searchTerm);
    
    const results = page.locator('#search-results');
    await expect(results).toBeVisible();
    
    const resultItems = results.locator('.result-item:not(.is-empty)');
    await expect(resultItems.first()).toBeVisible();
  });
});
