/**
 * Accessibility checks for stateful interaction paths.
 *
 * Pa11y covers static URLs. These Playwright + axe checks exercise filtered,
 * sorted, paginated, and detail states after client-side rendering.
 */

const fs = require('fs');
const { test, expect } = require('@playwright/test');
const {
  schoolWithCuts,
  schoolWithVisibleAccreditation,
  schoolWithResearchSource,
  expectAriaHiddenInSync
} = require('./helpers');

const axeSource = fs.readFileSync(require.resolve('axe-core'), 'utf8');
const cutsUnitid = schoolWithCuts();
const accreditationUnitid = schoolWithVisibleAccreditation();
const researchUnitid = schoolWithResearchSource();

async function runAxe(page, label) {
  await page.addScriptTag({ content: axeSource });
  const results = await page.evaluate(async () => window.axe.run(document, {
    runOnly: {
      type: 'tag',
      values: ['wcag2a', 'wcag2aa']
    }
  }));
  const violations = results.violations.map((violation) => {
    const targets = violation.nodes.map((node) => node.target.join(' ')).join('; ');
    return `${violation.id}: ${violation.help} (${targets})`;
  });
  expect(violations, `${label}\n${violations.join('\n')}`).toEqual([]);
}

async function firstUsefulWord(locator) {
  const text = String(await locator.first().textContent() || '');
  const word = text.split(/\s+/).find((part) => /^[A-Za-z]{4,}$/.test(part));
  return word || text.trim().slice(0, 6);
}

test.describe('Stateful accessibility checks', () => {
  test('research filtered, sorted, and paginated state has no axe violations', async ({ page }) => {
    await page.goto('/research.html');
    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'research initial');

    const filterTerm = await firstUsefulWord(list.locator('tbody tr td:first-child'));
    await page.locator('#research-filter').fill(filterTerm);
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'research filtered');

    await list.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();
    await expect(list.locator('th[aria-sort="ascending"]')).toContainText('State');
    await expectAriaHiddenInSync(page, expect, 'research sorted');

    const secondPage = list.locator('.pagination-button:not(.pagination-nav)[data-page="2"]');
    if (await secondPage.isVisible()) {
      await secondPage.click();
      await expect(list.locator('.pagination-button[aria-current="page"]')).toHaveText('2');
      await expectAriaHiddenInSync(page, expect, 'research paginated');
    }

    await runAxe(page, 'research filtered/sorted/paginated state');
  });

  test('cuts filtered, sorted, and detail states have no axe violations', async ({ page }) => {
    await page.goto('/cuts.html');
    const list = page.locator('#cuts-list');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'cuts initial');

    const filterTerm = await firstUsefulWord(list.locator('tbody tr td:first-child'));
    await page.locator('#cuts-filter').fill(filterTerm);
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'cuts filtered');

    await list.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();
    await expect(list.locator('th[aria-sort="ascending"]')).toContainText('State');
    await expectAriaHiddenInSync(page, expect, 'cuts sorted');
    await runAxe(page, 'cuts filtered/sorted state');

    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);
    await expect(page.locator('#cuts-list table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'cuts detail');
    await runAxe(page, 'cuts detail state');
  });

  test('accreditation filtered and detail states have no axe violations', async ({ page }) => {
    await page.goto('/accreditation.html');
    const list = page.locator('#accreditation-status');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'accreditation initial');

    const filterTerm = await firstUsefulWord(list.locator('tbody tr td:first-child'));
    await page.locator('#accreditation-filter').fill(filterTerm);
    await expect(list.locator('table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'accreditation filtered');

    const secondPage = list.locator('.pagination-button:not(.pagination-nav)[data-page="2"]');
    if (await secondPage.isVisible()) {
      await secondPage.click();
      await expect(list.locator('.pagination-button[aria-current="page"]')).toHaveText('2');
      await expectAriaHiddenInSync(page, expect, 'accreditation paginated');
    }

    await runAxe(page, 'accreditation filtered/paginated state');

    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);
    await expect(page.locator('#accreditation-status table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'accreditation detail');
    await runAxe(page, 'accreditation detail state');
  });

  test('research detail grant table state has no axe violations', async ({ page }) => {
    await page.goto(`/research.html?unitid=${researchUnitid}`);
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    await expectAriaHiddenInSync(page, expect, 'research detail initial');
    await page.locator('#research-list button[data-sort-key="termination_date"][data-sort-direction="asc"]').click();
    await expect(page.locator('#research-list th[aria-sort="ascending"]')).toContainText('Termination date');
    await expectAriaHiddenInSync(page, expect, 'research detail sorted');
    await runAxe(page, 'research detail grant table state');
  });
});
