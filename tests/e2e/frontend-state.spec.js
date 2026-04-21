/**
 * Interaction regressions for stateful frontend controls.
 *
 * These tests cover behavior that basic structure checks cannot see:
 * pagination state, sortable header ARIA state, hidden-section semantics, and
 * source-link rendering through the shared helpers.
 */

const { test, expect } = require('@playwright/test');

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
    const closuresSection = page.locator('#cuts-closures-list').locator('xpath=ancestor::section[1]');

    await expect(otherSection).not.toHaveClass(/is-hidden/);
    await expect(otherSection).not.toHaveAttribute('aria-hidden', 'true');
    await expect(closuresSection).toHaveClass(/is-hidden/);
    await expect(closuresSection).toHaveAttribute('aria-hidden', 'true');

    await page.goto('/cuts.html?unitid=101709');

    await expect(otherSection).toHaveClass(/is-hidden/);
    await expect(otherSection).toHaveAttribute('aria-hidden', 'true');
    await expect(closuresSection).toHaveClass(/is-hidden/);
    await expect(closuresSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('accreditation secondary sections keep aria-hidden synchronized with visibility', async ({ page }) => {
    await page.goto('/accreditation.html');

    const otherSection = page.locator('#accreditation-other-status').locator('xpath=ancestor::section[1]');
    await expect(otherSection).not.toHaveClass(/is-hidden/);
    await expect(otherSection).not.toHaveAttribute('aria-hidden', 'true');

    await page.goto('/accreditation.html?unitid=104717');

    await expect(otherSection).toHaveClass(/is-hidden/);
    await expect(otherSection).toHaveAttribute('aria-hidden', 'true');
  });

  test('rendered source links use safe http URLs and hardened rel attributes', async ({ page }) => {
    await page.goto('/research.html?unitid=100654');

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
});
