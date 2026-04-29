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
    await filter.fill(firstInstitution);

    // Filter applies on 'input'; poll until the row count settles at something
    // that both (a) is at least one and (b) is less than or equal to the
    // initial count. Anything else (no change, empty result) indicates a
    // broken filter wiring.
    await expect.poll(async () => {
      const count = await rows.count();
      return { count, narrowed: count >= 1 && count <= initialCount };
    }, {
      message: `filter "${firstInstitution}" should narrow the primary table to ≥1 and ≤${initialCount} rows`
    }).toMatchObject({ narrowed: true });

    // Every remaining row must contain the filter term (case-insensitive,
    // mirroring normalizeSearchText). The table must have *only* matching
    // rows after filter — a stale row through would indicate a filter bug.
    const institutionCells = await primaryTable.locator('tbody tr td:first-child').allTextContents();
    const lower = firstInstitution.toLowerCase();
    expect(institutionCells.length).toBeGreaterThan(0);
    for (const cell of institutionCells) {
      expect(cell.toLowerCase()).toContain(lower);
    }

    // Clear the filter — the table should restore to the original row count.
    await filter.fill('');
    await expect.poll(() => rows.count()).toBe(initialCount);
  });

  test('other-institutions filter excludes Warner Pacific University policy-review rows', async ({ page }) => {
    await page.goto('/accreditation.html');

    const otherSection = page.locator('#accreditation-other-status');
    const otherTable = otherSection.locator('table.history-table');
    await expect(otherTable).toBeVisible();

    const filter = page.locator('#accreditation-other-filter');
    await filter.fill('Warner Pacific');

    const rows = otherTable.locator('tbody tr');
    await expect.poll(() => rows.count()).toBe(0);
    await expect(otherSection).not.toContainText('Warner Pacific University');
    await expect(otherSection).toContainText('No accreditation actions from 2019 to the present are available for other institutions.');
  });

  test('other-institutions table excludes Puerto Rico rows', async ({ page }) => {
    await page.goto('/accreditation.html');

    const otherTable = page.locator('#accreditation-other-status table.history-table');
    await expect(otherTable).toBeVisible();
    await expect(otherTable).not.toContainText('Puerto Rico');
  });
});
