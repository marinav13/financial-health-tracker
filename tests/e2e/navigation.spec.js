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
const {
  firstSchoolIndexEntry,
  searchTermFor,
  schoolWithCharts,
  latestEnrollmentText,
  schoolWithRelatedPages,
  schoolWithoutRelatedPages,
  relatedPagesForSchool
} = require('./helpers');

const searchTarget = firstSchoolIndexEntry();
const searchTerm = searchTermFor(searchTarget);
const chartSchoolUnitid = schoolWithCharts();
const chartSchoolEnrollmentText = latestEnrollmentText(chartSchoolUnitid);
const relatedSchoolUnitid = schoolWithRelatedPages();
const noRelatedSchoolUnitid = schoolWithoutRelatedPages();

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
    await expect(page.locator('#enrollment-total')).toHaveText(chartSchoolEnrollmentText);
  });

  test('school detail hides the landing intro callout', async ({ page }) => {
    await page.goto('/school.html');
    await expect(page.locator('#school-intro-callout')).not.toHaveClass(/is-hidden/);

    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    await expect(page.locator('#school-name')).toBeVisible();
    await expect(page.locator('#school-intro-callout')).toHaveClass(/is-hidden/);
  });

  test('top tabs deep-link to the same school from school detail', async ({ page }) => {
    // Requirement change (2026-04-23): syncTabs previously stripped the unitid
    // from top-nav hrefs on school-context pages because it silently ignored the
    // unitid arg (and the financialUnitid option callers were passing). Now that
    // syncTabs honors both, top tabs on school.html?unitid=<numeric> must keep
    // the user inside the same school rather than dumping them to the bare
    // landing pages. This test locks the corrected behavior.
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);

    const financesTab = page.locator('#tab-finances');
    await expect(financesTab).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', `school.html?unitid=${chartSchoolUnitid}`);
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', `cuts.html?unitid=${chartSchoolUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', `accreditation.html?unitid=${chartSchoolUnitid}`);
    await expect(page.locator('#tab-research')).toHaveAttribute('href', `research.html?unitid=${chartSchoolUnitid}`);

    await page.locator('#tab-research').click();
    await expect(page).toHaveURL(new RegExp(`/research\\.html\\?unitid=${chartSchoolUnitid}$`));
    await expect(page.locator('#research-list table.history-table, #research-content')).toBeVisible();
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

  test('top cuts tab carries the school unitid forward from school detail', async ({ page }) => {
    // Requirement change (2026-04-23): corrected syncTabs keeps the current
    // school in the URL when switching sections, so this click now lands on
    // cuts.html?unitid=<same> rather than the bare cuts landing page.
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);

    await page.locator('#tab-cuts').click();
    await expect(page).toHaveURL(new RegExp(`/cuts\\.html\\?unitid=${chartSchoolUnitid}$`));
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);
  });

  test('school related pages link only datasets with records', async ({ page }) => {
    const expectedLinks = relatedPagesForSchool(relatedSchoolUnitid);
    expect(expectedLinks.length).toBeGreaterThan(0);

    await page.goto(`/school.html?unitid=${relatedSchoolUnitid}`);
    const section = page.locator('#school-related-section');
    await expect(section).toBeVisible();
    await expect(section).not.toHaveAttribute('aria-hidden', 'true');

    const links = section.locator('a');
    await expect(links).toHaveCount(expectedLinks.length);
    for (let index = 0; index < expectedLinks.length; index += 1) {
      await expect(links.nth(index)).toHaveText(expectedLinks[index].label);
      await expect(links.nth(index)).toHaveAttribute('href', expectedLinks[index].href);
    }
  });

  test('school related pages section is hidden when no side records exist', async ({ page }) => {
    await page.goto(`/school.html?unitid=${noRelatedSchoolUnitid}`);
    const section = page.locator('#school-related-section');
    await expect(section).toBeHidden();
    await expect(section).toHaveAttribute('aria-hidden', 'true');
    await expect(section.locator('a')).toHaveCount(0);
  });
});
