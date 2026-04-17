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
    await searchInput.fill('Georgetown');
    await searchInput.press('Enter');
    
    // Wait for search to complete
    await page.waitForTimeout(500);
    
    const results = page.locator('#search-results');
    await expect(results).toBeVisible();
    
    // Verify results contain Georgetown
    const resultItems = results.locator('.result-item:not(.is-empty)');
    const count = await resultItems.count();
    expect(count).toBeGreaterThan(0);
    
    const firstResult = await resultItems.first().textContent();
    expect(firstResult).toContain('Georgetown');
  });

  test('shows "no results" for unknown school', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await searchInput.fill('XYZNONEXISTENTSCHOOL12345');
    await searchInput.press('Enter');
    
    await page.waitForTimeout(500);
    
    const emptyResult = page.locator('#search-results .result-item.is-empty');
    await expect(emptyResult).toBeVisible();
    await expect(emptyResult).toContainText(/No matching/i);
  });

  test('search works on school page', async ({ page }) => {
    await page.goto('/school.html');
    
    const searchInput = page.locator('#school-search');
    await expect(searchInput).toBeVisible();
    
    await searchInput.fill('Harvard');
    await searchInput.press('Enter');
    
    await page.waitForTimeout(500);
    
    const results = page.locator('#search-results');
    await expect(results).toBeVisible();
    
    const resultItems = results.locator('.result-item:not(.is-empty)');
    const count = await resultItems.count();
    expect(count).toBeGreaterThan(0);
  });
});