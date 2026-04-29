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
    action_count: 1,
    landing_actions: [
      {
        accreditor: 'MSCHE',
        action_type: 'warning',
        action_label: 'Landing warning',
        action_date: '2020-01-01',
        notes: 'Visible landing row',
        source_url: 'https://example.org/accreditation',
        display_action: true
      },
      {
        accreditor: 'MSCHE',
        action_type: 'warning',
        action_label: 'Hidden landing action',
        action_date: '2020-01-02',
        notes: 'This landing row should stay hidden.',
        source_url: 'https://example.org/hidden-landing',
        display_action: false
      },
      {
        accreditor: 'MSCHE',
        action_type: 'adverse_action',
        action_label: 'Staff acted to acknowledge receipt of the supplemental information report requested by the Commission action of November 7, 2025.',
        action_date: '2026-02-27',
        notes: 'Procedural-only landing row',
        source_url: 'https://example.org/procedural-landing',
        display_action: true
      },
      {
        accreditor: 'MSCHE',
        action_type: 'adverse_action',
        action_label_short: "To approve the teach-out plan as required of candidate institutions in accordance with the Commission's Teach-Out Plans and Agreement(s) Policy and Procedures and federal regulation.",
        action_label: "To acknowledge receipt of the teach-out plan. To approve the teach-out plan as required of candidate institutions in accordance with the Commission's Teach-Out Plans and Agreement(s) Policy and Procedures and federal regulation.",
        action_date: '2023-03-09',
        notes: 'Candidate institution compliance row should stay hidden.',
        source_url: 'https://example.org/candidate-teachout',
        display_action: true
      },
      {
        accreditor: 'MSCHE',
        action_type: 'adverse_action',
        action_label_short: 'Change of Legal Status (effective January 3, 2024)',
        action_label: 'To include the change in legal status, form of control, or ownership within the institution\'s scope of accreditation effective January 3, 2024.',
        action_date: '2023-09-25',
        notes: 'Generic legal-status row should stay hidden.',
        source_url: 'https://example.org/legal-status',
        display_action: true
      },
      {
        accreditor: 'MSCHE',
        action_type: 'adverse_action',
        action_label_short: 'Merger of Example College with State University (effective June 30, 2025)',
        action_label: 'To include the change in legal status, form of control, or ownership within the institution\'s scope of accreditation effective June 30, 2025. To note the complex substantive change request includes the merger of Example College with State University, effective June 30, 2025, the anticipated date of the transaction. To note that State University is the surviving institution.',
        action_date: '2025-04-24',
        notes: 'Merger row should remain visible.',
        source_url: 'https://example.org/merger',
        display_action: true
      },
      {
        accreditor: 'NWCCU',
        action_type: 'warning',
        action_label_short: 'Accredited – Spring 2026 Policies, Regulations, and Financial Review',
        action_label: 'Accredited – Spring 2026 Policies, Regulations, and Financial Review',
        action_date: '2026-04-28',
        notes: 'Status: Accredited | Evaluation: Spring 2026 Policies, Regulations, and Financial Review | Reason: As of their most recent evaluation, this institution is substantially compliant with the Standards, Policies, and Eligibility Requirements of the Northwest Commission on Colleges and Universities.',
        source_url: 'https://example.org/nwccu-accredited',
        display_action: true
      }
    ]
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

  test('landing view suppresses display_action=false index rows', async ({ page }) => {
    await page.goto('/accreditation.html');

    const statusSection = page.locator('#accreditation-status');
    await expect(statusSection).toBeVisible();
    await expect(statusSection).toContainText('Landing warning');
    await expect(statusSection).toContainText('Merger of Example College with State University');
    await expect(statusSection).not.toContainText('Hidden landing action');
    await expect(statusSection).not.toContainText('supplemental information report requested by the Commission action of November 7, 2025');
    await expect(statusSection).not.toContainText('teach-out plan as required of candidate institutions');
    await expect(statusSection).not.toContainText('Change of Legal Status (effective January 3, 2024)');
    await expect(statusSection).not.toContainText('Policies, Regulations, and Financial Review');
  });
});
