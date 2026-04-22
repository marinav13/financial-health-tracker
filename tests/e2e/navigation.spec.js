/**
 * End-to-end tests for navigation and school detail page.
 * 
 * Tests that:
 * - Clicking a search result navigates to school page with correct unitid
 * - School page loads with correct institution name
 * - Tabs work (fianances, cuts, research, accreditation)
 * - School page shows data for the selected institution
 */

const { test, expect } = require('@playwright/test');
const { firstSchoolIndexEntry, searchTermFor, schoolWithCharts } = require('./helpers');

const searchTarget = firstSchoolIndexEntry();
const searchTerm = searchTermFor(searchTarget);
const chartSchoolUnitid = schoolWithCharts();

test.describe('School navigation', () => {
  test('navigates from search to school page', async ({ page }) => {
    await page.goto('/index.html');
    
    const searchInput = page.locator('#school-search');
    await searchInput.fill(searchTerm);
    
    const firstResult = page.locator('#search-results .result-item:not(.is-empty)').first();
    await expect(firstResult).toBeVisible();
    await firstResult.click();
    
    // Should navigate to school page
    await expect(page).toHaveURL(/school\.html\?unitid=/);
    
    // URL should contain a unitid
    const url = page.url();
    const unitid = new URL(url).searchParams.get('unitid');
    expect(unitid).toBeDefined();
    expect(parseInt(unitid)).toBeGreaterThan(0);
  });

  test('school page loads with data', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    // Should show institution name
    const schoolName = page.locator('#school-name');
    await expect(schoolName).toBeVisible();
    
    const nameText = await schoolName.textContent();
    expect(nameText).toBeTruthy();
    expect(nameText.length).toBeGreaterThan(0);
  });

  test('school detail hides the landing intro callout', async ({ page }) => {
    await page.goto('/school.html');
    await expect(page.locator('#school-intro-callout')).not.toHaveClass(/is-hidden/);

    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    await expect(page.locator('#school-name')).toBeVisible();
    await expect(page.locator('#school-intro-callout')).toHaveClass(/is-hidden/);
  });

  test('top tabs navigate to section landing pages from school detail', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    // Default tab is finances - verify it's active
    const financesTab = page.locator('#tab-finances');
    await expect(financesTab).toHaveClass(/is-active/);
    
    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
    
    await page.locator('#tab-research').click();
    await expect(page).toHaveURL(/\/research\.html$/);
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(5);
  });

  test('research landing page does not auto-scroll to state summary on load', async ({ page }) => {
    await page.goto('/research.html');
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(5);
  });

  test('research landing page resets browser scroll restoration on reload', async ({ page }) => {
    await page.goto('/research.html');
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    await page.evaluate(() => window.scrollTo(0, 1200));
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBeGreaterThan(1000);

    await page.reload();
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(5);
  });

  test('top cuts tab opens the cuts landing page from school detail', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    await page.locator('#tab-cuts').click();
    await expect(page).toHaveURL(/\/cuts\.html$/);
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);
  });
});
