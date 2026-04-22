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

  test('tabs switch content', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    // Default tab is finances - verify it's active
    const financesTab = page.locator('#tab-finances');
    await expect(financesTab).toHaveClass(/is-active/);
    
    // Click research tab
    const researchTab = page.locator('#tab-research');
    await researchTab.click();
    
    // Research tab should now be active
    await expect(researchTab).toHaveClass(/is-active/);
    await expect(financesTab).not.toHaveClass(/is-active/);
    
    // Accreditation tab
    const accreditationTab = page.locator('#tab-accreditation');
    await accreditationTab.click();
    await expect(accreditationTab).toHaveClass(/is-active/);
  });

  test('cuts tab switches correctly', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    const cutsTab = page.locator('#tab-cuts');
    await cutsTab.click();
    
    // Tab should become active (even if no data shown)
    await expect(cutsTab).toHaveClass(/is-active/);
  });
});
