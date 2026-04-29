/**
 * Accreditation rendering — display_action + date-parsing integration lock.
 *
 * These two behaviors were previously covered by tests/test_accreditation_rendering.js,
 * which ran the controller against a stubbed DOM (elements whose classList.add
 * was a no-op and whose innerHTML was a plain string). The assertions passed
 * against a simulator of the rendering contract rather than against the real
 * browser layout, so a regression in the live DOM path could have shipped
 * undetected.
 *
 * This Playwright spec routes /data/accreditation.json to a fixture and then
 * drives the real accreditation.html in a real browser. It locks two
 * behaviors the Node-level stub test used to cover:
 *
 *   1. `display_action === false` actions are filtered out of the rendered
 *      table. The export pipeline is authoritative for whether an action
 *      should display; the browser must not reclassify scraped phrasing.
 *
 *   2. Month-name dates like "January 2020" parse as real dates so the
 *      action renders and is treated as recent, rather than being silently
 *      dropped by a stricter ISO-only parser.
 */

const { test, expect } = require('@playwright/test');

const FIXTURE = {
  generated_at: '2025-12-01T00:00:00Z',
  covered_accreditors: ['MSCHE'],
  schools: {
    '100': {
      unitid: '100',
      financial_unitid: '100',
      has_financial_profile: true,
      is_primary_tracker: true,
      institution_name: 'Example University',
      city: 'Boston',
      state: 'Massachusetts',
      control_label: 'Public',
      category: 'Degree-granting, primarily baccalaureate or above',
      actions: [
        {
          accreditor: 'MSCHE',
          action_type: 'warning',
          action_label: 'Warning',
          action_status: 'active',
          action_date: 'January 2020',
          notes: 'Public warning issued',
          source_url: 'https://example.org/accreditation',
          display_action: true
        },
        {
          accreditor: 'MSCHE',
          action_type: 'warning',
          action_label: 'Hidden exported action',
          action_status: 'active',
          action_date: '2020-01-01',
          notes: 'This would pass the old browser regex but should stay hidden.',
          source_url: 'https://example.org/hidden',
          display_action: false
        }
      ]
    }
  }
};

const INDEX_FIXTURE = {
  '100': {
    unitid: '100',
    financial_unitid: '100',
    has_financial_profile: true,
    is_primary_tracker: true,
    institution_name: 'Example University',
    state: 'Massachusetts',
    control_label: 'Public',
    action_count: 1
  }
};

test.describe('Accreditation display_action + date parsing', () => {
  test.beforeEach(async ({ page }) => {
    // Intercept the JSON fetch before any navigation so the controller sees
    // the fixture rather than the production export. Using `**/data/...` so
    // the route matches regardless of host/port.
    await page.route('**/data/accreditation_index.json', (route) => {
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(INDEX_FIXTURE)
      });
    });
    await page.route('**/data/college_cuts_index.json', (route) => {
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({})
      });
    });
    await page.route('**/data/research_funding_index.json', (route) => {
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({})
      });
    });
    await page.route('**/data/metadata.json', (route) => {
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify({ generated_at: '2025-12-01' })
      });
    });
    await page.route('**/data/accreditation.json', (route) => {
      route.fulfill({
        status: 200,
        contentType: 'application/json; charset=utf-8',
        body: JSON.stringify(FIXTURE)
      });
    });
  });

  test('renders display_action=true and suppresses display_action=false', async ({ page }) => {
    await page.goto('/accreditation.html?unitid=100');

    const statusSection = page.locator('#accreditation-status');
    await expect(statusSection).toBeVisible();

    // The visible action must render as a real table row.
    const rows = statusSection.locator('table.history-table tbody tr');
    await expect.poll(() => rows.count(), {
      message: 'Expected the visible warning row to render after fixture load'
    }).toBeGreaterThan(0);

    const rowText = (await statusSection.innerText()).toLowerCase();
    expect(rowText).toContain('warning');
    // The display_action=false row carries a distinctive label; it must not
    // appear anywhere in the rendered section.
    expect(rowText).not.toContain('hidden exported action');
  });

  test('accepts month-name accreditation dates like "January 2020"', async ({ page }) => {
    await page.goto('/accreditation.html?unitid=100');

    const statusSection = page.locator('#accreditation-status');
    await expect(statusSection).toBeVisible();

    // The month-name date must render somewhere in the table cell. Strict
    // ISO-only parsing would drop this action; we need to see it in the DOM.
    await expect(statusSection).toContainText('January 2020');
  });
});
