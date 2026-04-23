/**
 * Cross-page syncTabs integration.
 *
 * The unit test at tests/test_sync_tabs.js exercises the syncTabs function
 * directly. This spec drives the full loop on real pages: cuts.html,
 * accreditation.html, and research.html each call syncTabs twice — once
 * pre-data-load with just the URL unitid, and once post-data-load with the
 * school's financial_unitid. Loading those pages with a numeric unitid and
 * reading the rendered top-nav hrefs is the only way to catch a regression
 * where a page-specific init flow stops forwarding financialUnitid.
 *
 * Also covers the namespaced (unmatched) unitid case to lock the fallback.
 */

const { test, expect } = require('@playwright/test');
const {
  schoolWithCuts,
  schoolWithAccreditation,
  schoolWithResearchSource,
  unmatchedCutSchool,
  unmatchedAccreditationSchool,
  unmatchedResearchSchool
} = require('./helpers');

const cutsUnitid = schoolWithCuts();
const accredUnitid = schoolWithAccreditation();
const researchUnitid = schoolWithResearchSource();
const unmatchedCuts = unmatchedCutSchool();
const unmatchedAccred = unmatchedAccreditationSchool();
const unmatchedResearch = unmatchedResearchSchool();

test.describe('Top-nav deep-linking across section pages', () => {
  test('cuts.html?unitid=<numeric> keeps the unitid on every top tab', async ({ page }) => {
    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);
    // Wait for page-specific init to finish its second syncTabs call with
    // the loaded school's financial_unitid. Active class on tab-cuts is set
    // during that call, so this is a reliable barrier.
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', `school.html?unitid=${cutsUnitid}`);
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', `cuts.html?unitid=${cutsUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', `accreditation.html?unitid=${cutsUnitid}`);
    await expect(page.locator('#tab-research')).toHaveAttribute('href', `research.html?unitid=${cutsUnitid}`);
  });

  test('accreditation.html?unitid=<numeric> keeps the unitid on every top tab', async ({ page }) => {
    await page.goto(`/accreditation.html?unitid=${accredUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', `school.html?unitid=${accredUnitid}`);
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', `cuts.html?unitid=${accredUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', `accreditation.html?unitid=${accredUnitid}`);
    await expect(page.locator('#tab-research')).toHaveAttribute('href', `research.html?unitid=${accredUnitid}`);
  });

  test('research.html?unitid=<numeric> keeps the unitid on every top tab', async ({ page }) => {
    await page.goto(`/research.html?unitid=${researchUnitid}`);
    await expect(page.locator('#tab-research')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', `school.html?unitid=${researchUnitid}`);
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', `cuts.html?unitid=${researchUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', `accreditation.html?unitid=${researchUnitid}`);
    await expect(page.locator('#tab-research')).toHaveAttribute('href', `research.html?unitid=${researchUnitid}`);
  });

  test('cuts.html with a namespaced unmatched unitid falls back to landing hrefs', async ({ page }) => {
    // Namespaced ids (e.g. "cut-foo") represent records that never matched an
    // IPEDS institution. renderRelatedInstitutionLinks suppresses those, and
    // syncTabs must stay consistent — deep-linking to school.html?unitid=cut-foo
    // would produce a page that can't resolve the unitid.
    await page.goto(`/cuts.html?unitid=${encodeURIComponent(unmatchedCuts)}`);
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('accreditation.html with a namespaced unmatched unitid falls back to landing hrefs', async ({ page }) => {
    await page.goto(`/accreditation.html?unitid=${encodeURIComponent(unmatchedAccred)}`);
    await expect(page.locator('#tab-accreditation')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('research.html with a namespaced unmatched unitid falls back to landing hrefs', async ({ page }) => {
    await page.goto(`/research.html?unitid=${encodeURIComponent(unmatchedResearch)}`);
    await expect(page.locator('#tab-research')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });
});
