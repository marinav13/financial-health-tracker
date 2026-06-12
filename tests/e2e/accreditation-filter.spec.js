/**
 * Accreditation primary-table filter input — behavior lock.
 *
 * The existing rendering tests stub makeTableController and never drive the
 * filter input for real, so "the filter input narrows the table" was not
 * under test. This spec loads accreditation.html, reads the first rendered
 * institution name from the live DOM, uses it as the filter term, and
 * asserts:
 *   - the row count is non-zero and ≤ the unfiltered count
 *   - every remaining row's institution cell contains the filter substring
 *   - clearing the input restores the original row count
 *
 * Deriving the term from the DOM avoids a fragile test-time mirror of the
 * app's isRecentDisplayAction / dedupeActions / is_primary_tracker pipeline.
 */

const { test, expect } = require('@playwright/test');

test.describe('Accreditation filter input', () => {
  test('typing narrows the primary table and clearing restores it', async ({ page }) => {
    await page.goto('/accreditation.html');

    const primaryTable = page.locator('#accreditation-status table.history-table');
    await expect(primaryTable).toBeVisible();

    const rows = primaryTable.locator('tbody tr');
    const initialCount = await rows.count();
    expect(initialCount).toBeGreaterThan(1); // need ≥ 2 rows to meaningfully narrow

    const firstInstitution = (await rows.first().locator('td').first().textContent() || '').trim();
    expect(firstInstitution.length).toBeGreaterThan(0);

    const filter = page.locator('#accreditation-filter');
    const results = page.locator('#accreditation-search-results');

    // Filtering commits only when the user picks an autocomplete option.
    await filter.fill(firstInstitution);
    await expect(results).toBeVisible();

    const firstOption = results.locator('.result-item[role="option"]').first();
    await expect(firstOption).toBeVisible();
    const selectedLabel = ((await firstOption.locator('.result-item-label').textContent()) || '').trim();
    await firstOption.click();

    const lower = selectedLabel.toLowerCase();
    await expect.poll(async () => {
      const institutionCells = await primaryTable.locator('tbody tr td:first-child').allTextContents();
      const count = institutionCells.length;
      if (count < 1 || count > initialCount) return false;
      return institutionCells.every((cell) => cell.toLowerCase().includes(lower));
    }, {
      message: `filter "${selectedLabel}" should leave only matching primary-table rows`
    }).toBe(true);

    // Clearing the input fires the same commit mechanism (the input handler
    // detects typedValue='' with a committed filterValue and dispatches
    // tracker:search-commit) and restores the full row count.
    await filter.fill('');
    await expect.poll(() => rows.count()).toBe(initialCount);
  });

  test('landing page omits the removed other-institutions table and filter', async ({ page }) => {
    await page.goto('/accreditation.html');

    await expect(page.locator('#accreditation-other-status')).toHaveCount(0);
    await expect(page.locator('#accreditation-other-filter')).toHaveCount(0);
  });
});
