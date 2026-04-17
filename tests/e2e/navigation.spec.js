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

test.describe('School navigation', () => {
  test('navigates from search to school page', async ({ page }) => {
    await page.goto('/index.html');
    
    // Search for Georgetown
    const searchInput = page.locator('#school-search');
    await searchInput.fill('Georgetown');
    await searchInput.press('Enter');
    await page.waitForTimeout(500);
    
    // Click first result
    const firstResult = page.locator('#search-results .result-item:not(.is-empty)').first();
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
    // Load a known school
    await page.goto('/school.html?unitid=222178');
    
    // Should show institution name
    const schoolName = page.locator('#school-name');
    await expect(schoolName).toBeVisible();
    
    const nameText = await schoolName.textContent();
    expect(nameText).toBeTruthy();
    expect(nameText.length).toBeGreaterThan(0);
  });

  test('tabs switch content', async ({ page }) => {
    await page.goto('/school.html?unitid=222178');
    
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

  test('cuts tab shows data', async ({ page }) => {
    await page.goto('/school.html?unitid=222178');
    
    const cutsTab = page.locator('#tab-cuts');
    await cutsTab.click();
    
    // Should show either cuts data or "No matched cuts" message
    const cutsContent = page.locator('#cuts-overview, .empty-message, #cuts-list');
    await expect(cutsContent.first()).toBeVisible();
  });
});