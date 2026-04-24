/**
 * Cross-page syncTabs integration.
 *
 * The unit test at tests/test_sync_tabs.js exercises the syncTabs function
 * directly. This spec drives the full loop on real pages: cuts.html,
 * accreditation.html, and research.html each call syncTabs twice — once
 * pre-data-load with just the URL unitid, and once post-data-load with the
 * school's financial_unitid.
 *
 * Requirement: the top nav is site-level navigation only. Regardless of
 * the URL unitid (numeric or namespaced/unmatched) or whether the page's
 * second syncTabs call supplies a financialUnitid, every top tab must
 * point at the section's landing page. Per-school navigation lives in the
 * in-body "Explore this institution" block (renderRelatedInstitutionLinks),
 * not the top nav.
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

test.describe('Top-nav is site-level navigation across section pages', () => {
  test('cuts.html?unitid=<numeric> keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);
    // Wait for page-specific init to finish its second syncTabs call with
    // the loaded school's financial_unitid. Active class on tab-cuts is set
    // during that call, so this is a reliable barrier.
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('accreditation.html?unitid=<numeric> keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/accreditation.html?unitid=${accredUnitid}`);
    await expect(page.locator('#tab-accreditation')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('research.html?unitid=<numeric> keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/research.html?unitid=${researchUnitid}`);
    await expect(page.locator('#tab-research')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('cuts.html with a namespaced unmatched unitid keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/cuts.html?unitid=${encodeURIComponent(unmatchedCuts)}`);
    await expect(page.locator('#tab-cuts')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('accreditation.html with a namespaced unmatched unitid keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/accreditation.html?unitid=${encodeURIComponent(unmatchedAccred)}`);
    await expect(page.locator('#tab-accreditation')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });

  test('research.html with a namespaced unmatched unitid keeps top tabs on landing pages', async ({ page }) => {
    await page.goto(`/research.html?unitid=${encodeURIComponent(unmatchedResearch)}`);
    await expect(page.locator('#tab-research')).toHaveClass(/is-active/);

    await expect(page.locator('#tab-finances')).toHaveAttribute('href', 'index.html');
    await expect(page.locator('#tab-cuts')).toHaveAttribute('href', 'cuts.html');
    await expect(page.locator('#tab-accreditation')).toHaveAttribute('href', 'accreditation.html');
    await expect(page.locator('#tab-research')).toHaveAttribute('href', 'research.html');
  });
});
