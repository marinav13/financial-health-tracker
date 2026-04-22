/**
 * Interaction regressions for stateful frontend controls.
 *
 * These tests cover behavior that basic structure checks cannot see:
 * pagination state, sortable header ARIA state, hidden-section semantics, and
 * source-link rendering through the shared helpers.
 */

const { test, expect } = require('@playwright/test');
const {
  schoolWithCuts,
  schoolWithAccreditation,
  schoolWithResearchSource,
  schoolWithoutEndowment,
  schoolWithClosureStatus,
  unmatchedCutSchool,
  unmatchedResearchSchool,
  unmatchedAccreditationSchool
} = require('./helpers');

const cutsUnitid = schoolWithCuts();
const accreditationUnitid = schoolWithAccreditation();
const researchUnitid = schoolWithResearchSource();
const noEndowmentUnitid = schoolWithoutEndowment();
const closureStatusUnitid = schoolWithClosureStatus();
const unmatchedCutUnitid = unmatchedCutSchool();
const unmatchedResearchUnitid = unmatchedResearchSchool();
const unmatchedAccreditationUnitid = unmatchedAccreditationSchool();

test.describe('Frontend state synchronization', () => {
  test('research pagination exposes exactly one current page and changes table rows', async ({ page }) => {
    await page.goto('/research.html');

    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expect(list.locator('.pagination-button[data-page="2"]')).toBeVisible();

    const currentBefore = list.locator('.pagination-button[aria-current="page"]');
    await expect(currentBefore).toHaveCount(1);
    await expect(currentBefore).toHaveText('1');

    const firstInstitutionBefore = await list.locator('tbody tr:first-child td:first-child').textContent();
    await list.locator('.pagination-button[data-page="2"]').click();

    const currentAfter = list.locator('.pagination-button[aria-current="page"]');
    await expect(currentAfter).toHaveCount(1);
    await expect(currentAfter).toHaveText('2');

    const firstInstitutionAfter = await list.locator('tbody tr:first-child td:first-child').textContent();
    expect(firstInstitutionAfter).toBeTruthy();
    expect(firstInstitutionAfter).not.toBe(firstInstitutionBefore);
  });

  test('sortable headers move aria-sort with the active sort state', async ({ page }) => {
    await page.goto('/research.html');

    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();

    await expect(list.locator('th[aria-sort="descending"]')).toHaveCount(1);
    await expect(list.locator('th[aria-sort="descending"]')).toContainText('Funding cut or frozen');

    await list.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();

    await expect(list.locator('th[aria-sort]')).toHaveCount(1);
    await expect(list.locator('th[aria-sort="ascending"]')).toContainText('State');

    const firstState = (await list.locator('tbody tr:first-child td').nth(1).textContent()).trim();
    expect(firstState.length).toBeGreaterThan(0);
  });

  test('cuts sortable headers move aria-sort with the active sort state', async ({ page }) => {
    await page.goto('/cuts.html');

    const list = page.locator('#cuts-list');
    await expect(list.locator('table.history-table')).toBeVisible();

    await expect(list.locator('th[aria-sort="descending"]')).toHaveCount(1);
    await expect(list.locator('th[aria-sort="descending"]')).toContainText('Date');

    await list.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();

    await expect(list.locator('th[aria-sort]')).toHaveCount(1);
    await expect(list.locator('th[aria-sort="ascending"]')).toContainText('State');
  });

  test('hidden secondary sections keep aria-hidden synchronized with visibility', async ({ page }) => {
    await page.goto('/cuts.html');

    const otherSection = page.locator('#cuts-other-list').locator('xpath=ancestor::section[1]');

    await expect(otherSection).not.toHaveClass(/is-hidden/);
    await expect(otherSection).not.toHaveAttribute('aria-hidden', 'true');

    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);

    await expect(otherSection).toHaveClass(/is-hidden/);
    await expect(otherSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('accreditation secondary sections keep aria-hidden synchronized with visibility', async ({ page }) => {
    await page.goto('/accreditation.html');

    const otherSection = page.locator('#accreditation-other-status').locator('xpath=ancestor::section[1]');
    await expect(otherSection).not.toHaveClass(/is-hidden/);
    await expect(otherSection).not.toHaveAttribute('aria-hidden', 'true');

    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);

    await expect(otherSection).toHaveClass(/is-hidden/);
    await expect(otherSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('research secondary sections keep aria-hidden synchronized with visibility', async ({ page }) => {
    await page.goto('/research.html');

    const otherSection = page.locator('#research-other-list').locator('xpath=ancestor::section[1]');
    await expect(otherSection).not.toHaveClass(/is-hidden/);
    await expect(otherSection).not.toHaveAttribute('aria-hidden', 'true');

    await page.goto(`/research.html?unitid=${researchUnitid}`);

    await expect(otherSection).toHaveClass(/is-hidden/);
    await expect(otherSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('rendered source links use safe http URLs and hardened rel attributes', async ({ page }) => {
    await page.goto(`/research.html?unitid=${researchUnitid}`);

    const sourceLinks = page.locator('#research-list tbody a', { hasText: 'Source' });
    await expect(sourceLinks.first()).toBeVisible();

    const count = Math.min(await sourceLinks.count(), 10);
    expect(count).toBeGreaterThan(0);

    for (let i = 0; i < count; i += 1) {
      const link = sourceLinks.nth(i);
      const href = await link.getAttribute('href');
      const rel = await link.getAttribute('rel');
      expect(href).toMatch(/^https?:\/\//);
      expect(rel || '').toContain('noopener');
      expect(rel || '').toContain('noreferrer');
    }
  });

  test('unmatched section records do not guess cross-page institution links', async ({ page }) => {
    const cases = [
      { url: `/cuts.html?unitid=${unmatchedCutUnitid}`, prefix: 'cut-' },
      { url: `/research.html?unitid=${unmatchedResearchUnitid}`, prefix: 'research-' },
      { url: `/accreditation.html?unitid=${unmatchedAccreditationUnitid}`, prefix: 'accred-' }
    ];

    for (const item of cases) {
      await page.goto(item.url);
      const relatedLinks = page.locator('.related-links a');
      const count = await relatedLinks.count();
      for (let i = 0; i < count; i += 1) {
        const href = await relatedLinks.nth(i).getAttribute('href');
        expect(href || '').not.toContain(`unitid=${item.prefix}`);
      }
    }
  });

  test('school detail hidden sections synchronize aria-hidden', async ({ page }) => {
    await page.goto(`/school.html?unitid=${noEndowmentUnitid}`);

    const endowmentSection = page.locator('#endowment-section');
    await expect(endowmentSection).toHaveClass(/is-hidden/);
    await expect(endowmentSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('school closure status remains hidden while closure display is disabled', async ({ page }) => {
    await page.goto(`/school.html?unitid=${closureStatusUnitid}`);

    const closureFlag = page.locator('#school-closure-flag');
    await expect(closureFlag).toHaveClass(/is-hidden/);
    await expect(closureFlag).toBeEmpty();
  });
});
