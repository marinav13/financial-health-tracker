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
const { firstSchoolIndexEntry, searchTermFor } = require('./helpers');

const searchTarget = firstSchoolIndexEntry();
const searchTerm = searchTermFor(searchTarget);

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
    await expect(page.locator(`#${activeId}`)).toBeFocused();
  });

  test('shows no results for unknown school', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await searchInput.fill('XYZNONEXISTENTSCHOOL12345');
    
    await expect(page.locator('#search-results .result-item.is-empty')).toContainText('No matching institutions found');
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
