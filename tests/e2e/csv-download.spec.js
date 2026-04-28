/**
 * CSV download content verification.
 *
 * The static export tests verify the JSON contracts on disk; the rendering
 * tests verify the table HTML. Nothing in the previous suite actually
 * clicked the "Download Displayed Table" button and inspected the CSV.
 *
 * This spec does. It loads accreditation.html, clicks the primary-table
 * download button, captures the blob download, reads it, and asserts:
 *   - The header row matches the contract defined in js/accreditation.js
 *   - The CSV contains exactly one data row per visible table row
 *     (downloadButton.onclick exports pageState.pageItems, not the full set)
 *   - The first data row's institution name matches the first rendered row
 *
 * This closes the gap where a silent change to downloadHeaders, downloadRow,
 * or downloadRowsCsv could ship broken CSVs.
 */

const fs = require('fs');
const { test, expect } = require('@playwright/test');

// js/app.js csvEscape quotes a field only if it contains ", comma, or newline,
// and doubles internal quotes. Mirror that here so we can parse the CSV
// without pulling in a dependency.
function parseCsv(text) {
  const rows = [];
  let row = [];
  let field = '';
  let inQuotes = false;
  for (let i = 0; i < text.length; i += 1) {
    const ch = text[i];
    if (inQuotes) {
      if (ch === '"') {
        if (text[i + 1] === '"') { field += '"'; i += 1; }
        else { inQuotes = false; }
      } else {
        field += ch;
      }
    } else if (ch === '"') {
      inQuotes = true;
    } else if (ch === ',') {
      row.push(field); field = '';
    } else if (ch === '\n') {
      row.push(field); field = '';
      rows.push(row); row = [];
    } else if (ch === '\r') {
      // ignore
    } else {
      field += ch;
    }
  }
  if (field.length > 0 || row.length > 0) {
    row.push(field);
    rows.push(row);
  }
  return rows;
}

test.describe('CSV download content', () => {
  test('accreditation primary CSV matches the visible table', async ({ page }) => {
    await page.goto('/accreditation.html');

    const primaryTable = page.locator('#accreditation-status table.history-table');
    await expect(primaryTable).toBeVisible();

    const visibleRows = primaryTable.locator('tbody tr');
    const visibleRowCount = await visibleRows.count();
    expect(visibleRowCount).toBeGreaterThan(0);

    const firstRowInstitution = (await visibleRows.first().locator('td').first().textContent() || '').trim();
    expect(firstRowInstitution.length).toBeGreaterThan(0);

    const downloadButton = page.locator('#accreditation-table-download');
    await expect(downloadButton).toBeVisible();

    const [download] = await Promise.all([
      page.waitForEvent('download'),
      downloadButton.click()
    ]);

    expect(download.suggestedFilename()).toBe('accreditation-primary.csv');

    const downloadPath = await download.path();
    expect(downloadPath).toBeTruthy();
    const csvText = fs.readFileSync(downloadPath, 'utf8');

    const parsed = parseCsv(csvText);
    expect(parsed.length).toBeGreaterThanOrEqual(2); // header + at least one data row

    const [header, ...dataRows] = parsed;
    expect(header).toEqual(['Institution', 'Accreditor', 'Action', 'Scope', 'State', 'Sector', 'Date', 'Source']);

    // Download exports the displayed page, so row counts must match.
    expect(dataRows.length).toBe(visibleRowCount);

    // First CSV data row's institution cell must match the first visible row.
    expect(dataRows[0][0]).toBe(firstRowInstitution);

    // No stray blank institution names in the export — that would indicate
    // broken downloadRow mapping.
    for (const dataRow of dataRows) {
      expect(dataRow[0].trim().length).toBeGreaterThan(0);
    }
  });
});
