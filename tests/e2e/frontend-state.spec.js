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
  schoolWithVisibleAccreditation,
  schoolWithResearchSource,
  schoolWithoutEndowment,
  schoolWithTuitionDependenceAtDisplayedMedian,
  schoolWithClosureStatus,
  relatedPagesForSchool,
  unmatchedCutSchool,
  unmatchedResearchSchool,
  unmatchedAccreditationSchool
} = require('./helpers');

const cutsUnitid = schoolWithCuts();
const accreditationUnitid = schoolWithVisibleAccreditation();
const researchUnitid = schoolWithResearchSource();
const noEndowmentUnitid = schoolWithoutEndowment();
const tuitionMedianUnitid = schoolWithTuitionDependenceAtDisplayedMedian();
const closureStatusUnitid = schoolWithClosureStatus();
const unmatchedCutUnitid = unmatchedCutSchool();
const unmatchedResearchUnitid = unmatchedResearchSchool();
const unmatchedAccreditationUnitid = unmatchedAccreditationSchool();
const patternOnlyWarningBadgeUnitid = '119173'; // Mount Saint Mary's University: pattern badge (enr -19.7%, ntr -30.7%, losses 3/5) with only 3 reds total
const patternAndWideWarningBadgeUnitid = '101116';
const wideOnlyWarningBadgeUnitid = '144281'; // Columbia College Chicago: 6 reds, no pattern badge (enr -8.2% not below -10%)
const noWarningBadgeUnitid = '104586';

function parseVisibleMonthYear(value) {
  const text = String(value || '').trim();
  if (!text) return Number.NaN;
  const parsed = new Date(text);
  return Number.isNaN(parsed.getTime()) ? Number.NaN : parsed.getTime();
}

test.describe('Frontend state synchronization', () => {
  test('research pagination exposes exactly one current page and changes table rows', async ({ page }) => {
    await page.goto('/research.html');

    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expect(list.locator('.pagination-button:not(.pagination-nav)[data-page="2"]')).toBeVisible();

    const currentBefore = list.locator('.pagination-button[aria-current="page"]');
    await expect(currentBefore).toHaveCount(1);
    await expect(currentBefore).toHaveText('1');

    const firstInstitutionBefore = await list.locator('tbody tr:first-child td:first-child').textContent();
    await list.locator('.pagination-button:not(.pagination-nav)[data-page="2"]').click();

    const currentAfter = list.locator('.pagination-button[aria-current="page"]');
    await expect(currentAfter).toHaveCount(1);
    await expect(currentAfter).toHaveText('2');

    const firstInstitutionAfter = await list.locator('tbody tr:first-child td:first-child').textContent();
    expect(firstInstitutionAfter).toBeTruthy();
    expect(firstInstitutionAfter).not.toBe(firstInstitutionBefore);
  });

  test('landing pages use index payloads without requesting full side-data exports', async ({ page }) => {
    const requested = [];
    page.on('request', (request) => requested.push(request.url()));

    await page.goto('/cuts.html');
    await expect(page.locator('#cuts-list table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/college_cuts_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/college_cuts.json'))).toBe(false);

    requested.length = 0;
    await page.goto('/accreditation.html');
    await expect(page.locator('#accreditation-status table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/accreditation_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/accreditation.json'))).toBe(false);

    requested.length = 0;
    await page.goto('/research.html');
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/research_funding_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/research_funding.json'))).toBe(false);
  });

  test('detail pages lazy-load full exports after index presence checks', async ({ page }) => {
    const requested = [];
    page.on('request', (request) => requested.push(request.url()));

    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);
    await expect(page.locator('#cuts-list table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/college_cuts_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/college_cuts.json'))).toBe(true);

    requested.length = 0;
    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);
    await expect(page.locator('#accreditation-status table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/accreditation_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/accreditation.json'))).toBe(true);

    requested.length = 0;
    await page.goto(`/research.html?unitid=${researchUnitid}`);
    await expect(page.locator('#research-list table.history-table')).toBeVisible();
    expect(requested.some((url) => url.endsWith('/data/research_funding_index.json'))).toBe(true);
    expect(requested.some((url) => url.endsWith('/data/research_funding.json'))).toBe(true);
  });

  test('research detail table shows description instead of grant id', async ({ page }) => {
    await page.goto(`/research.html?unitid=${researchUnitid}`);

    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();
    await expect(list.locator('thead th')).toContainText(['Agency', 'Grant', 'Description', 'Funding still disrupted', 'Termination date', 'Source']);
    await expect(list.locator('thead th')).not.toContainText(['Grant ID']);
  });

  test('school tuition dependence copy treats equal displayed percentages as at the median', async ({ page }) => {
    await page.goto(`/school.html?unitid=${tuitionMedianUnitid}`);

    const tuitionCopy = page.locator('#tuition-sentence-copy');
    await expect(tuitionCopy).toBeVisible();
    await expect(tuitionCopy).toContainText('about the same as the median');
    await expect(tuitionCopy).not.toContainText('above the median');
    await expect(tuitionCopy).not.toContainText('below the median');
  });

  test('sortable headers move aria-sort with the active sort state', async ({ page }) => {
    await page.goto('/research.html');

    const list = page.locator('#research-list');
    await expect(list.locator('table.history-table')).toBeVisible();

    await expect(list.locator('th[aria-sort="descending"]')).toHaveCount(1);
    await expect(list.locator('th[aria-sort="descending"]')).toContainText('Disrupted grants');

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

  test('accreditation sortable headers move aria-sort in landing and detail tables', async ({ page }) => {
    await page.goto('/accreditation.html');

    const landingList = page.locator('#accreditation-status');
    await expect(landingList.locator('table.history-table')).toBeVisible();
    await expect(landingList.locator('th[aria-sort="descending"]')).toContainText('Date');
    // Date column moved from col 6 -> col 5 when the visible Accreditor
    // column was hidden (it still ships in the CSV).
    const landingDates = await landingList.locator('tbody tr td:nth-child(5)').evaluateAll((cells) =>
      cells.slice(0, 5).map((cell) => (cell.textContent || '').trim())
    );
    expect(landingDates.length).toBeGreaterThan(1);
    for (let index = 1; index < landingDates.length; index += 1) {
      expect(parseVisibleMonthYear(landingDates[index - 1]) >= parseVisibleMonthYear(landingDates[index])).toBe(true);
    }

    await landingList.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();
    await expect(landingList.locator('th[aria-sort]')).toHaveCount(1);
    await expect(landingList.locator('th[aria-sort="ascending"]')).toContainText('State');

    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);

    const detailList = page.locator('#accreditation-status');
    await expect(detailList.locator('table.history-table')).toBeVisible();
    await expect(detailList.locator('th[aria-sort="descending"]')).toContainText('Date');
    // Date column moved from col 5 -> col 4 in the detail table when
    // the visible Accreditor column was hidden.
    const detailDates = await detailList.locator('tbody tr td:nth-child(4)').evaluateAll((cells) =>
      cells.slice(0, 5).map((cell) => (cell.textContent || '').trim())
    );
    expect(detailDates.length).toBeGreaterThan(0);
    if (detailDates.length > 1) {
      for (let index = 1; index < detailDates.length; index += 1) {
        expect(parseVisibleMonthYear(detailDates[index - 1]) >= parseVisibleMonthYear(detailDates[index])).toBe(true);
      }
    }

    // Detail-table sort coverage moved from "accreditor" (column hidden)
    // to "state" — same aria-sort wiring path, different sort key.
    await detailList.locator('button[data-sort-key="state"][data-sort-direction="asc"]').click();
    await expect(detailList.locator('th[aria-sort]')).toHaveCount(1);
    await expect(detailList.locator('th[aria-sort="ascending"]')).toContainText('State');
  });

  test('cuts landing omits the removed secondary section', async ({ page }) => {
    await page.goto('/cuts.html');

    await expect(page.locator('#cuts-other-list')).toHaveCount(0);
    await expect(page.locator('#cuts-other-filter')).toHaveCount(0);

    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);

    await expect(page.locator('#cuts-other-list')).toHaveCount(0);
    await expect(page.locator('#cuts-overview')).toHaveCount(0);
  });

  test('accreditation landing omits the removed secondary section', async ({ page }) => {
    await page.goto('/accreditation.html');

    await expect(page.locator('#accreditation-other-status')).toHaveCount(0);
    await expect(page.locator('#accreditation-other-filter')).toHaveCount(0);

    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);

    await expect(page.locator('#accreditation-other-status')).toHaveCount(0);
    await expect(page.locator('#accreditation-overview')).toHaveClass(/is-hidden/);
    await expect(page.locator('#accreditation-overview')).not.toContainText('This institution appears');
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

  test('research other higher-ed table excludes standalone foundation organizations', async ({ page }) => {
    await page.goto('/research.html');

    const otherSection = page.locator('#research-other-list');
    await expect(otherSection.locator('table.history-table')).toBeVisible();

    const filter = page.locator('#research-other-filter');
    await filter.fill('foundation');

    await expect(otherSection).not.toContainText('The Water Research Foundation');
    await expect(otherSection).not.toContainText('Oklahoma Medical Research Foundation');
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

  test('cuts detail only shows related institution pages with corresponding records', async ({ page }) => {
    const expectedLinks = relatedPagesForSchool(cutsUnitid, { current: 'cuts' }).filter((item) => item.label !== 'College Cuts');

    await page.goto(`/cuts.html?unitid=${cutsUnitid}`);
    const links = page.locator('.related-links a');
    await expect(links).toHaveCount(expectedLinks.length);
    for (let index = 0; index < expectedLinks.length; index += 1) {
      await expect(links.nth(index)).toHaveText(expectedLinks[index].label);
      await expect(links.nth(index)).toHaveAttribute('href', expectedLinks[index].href);
    }
  });

  test('accreditation detail only shows related institution pages with corresponding records', async ({ page }) => {
    const expectedLinks = relatedPagesForSchool(accreditationUnitid, { current: 'accreditation' }).filter((item) => item.label !== 'Accreditation');

    await page.goto(`/accreditation.html?unitid=${accreditationUnitid}`);
    const links = page.locator('.related-links a');
    await expect(links).toHaveCount(expectedLinks.length);
    for (let index = 0; index < expectedLinks.length; index += 1) {
      await expect(links.nth(index)).toHaveText(expectedLinks[index].label);
      await expect(links.nth(index)).toHaveAttribute('href', expectedLinks[index].href);
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

  test('school warning badges distinguish recurring patterns from widespread warnings', async ({ page }) => {
    await page.goto(`/school.html?unitid=${patternOnlyWarningBadgeUnitid}`);

    let badgeGroup = page.locator('#school-warning-summary');
    let badges = badgeGroup.locator('.school-warning-summary');
    await expect(badgeGroup).not.toHaveClass(/is-hidden/);
    await expect(badges).toHaveCount(1);
    await expect(badges.nth(0)).toContainText('Significant enrollment declines and losses');
    await expect(badges.nth(0)).not.toHaveClass(/is-broad/);
    await expect(badges.nth(0)).toHaveAttribute('aria-label', /operating losses in at least 3 of the last 5 years/i);

    await page.goto(`/school.html?unitid=${patternAndWideWarningBadgeUnitid}`);

    badgeGroup = page.locator('#school-warning-summary');
    badges = badgeGroup.locator('.school-warning-summary');
    await expect(badgeGroup).not.toHaveClass(/is-hidden/);
    await expect(badges).toHaveCount(2);
    await expect(badges.nth(0)).toContainText('Significant enrollment declines and losses');
    await expect(badges.nth(1)).toContainText('At least 6 warning signs');
    await expect(badges.nth(1)).toHaveClass(/is-broad/);
    await expect(badges.nth(1)).toHaveAttribute('aria-label', /at least 6/i);

    await page.goto(`/school.html?unitid=${wideOnlyWarningBadgeUnitid}`);

    badgeGroup = page.locator('#school-warning-summary');
    badges = badgeGroup.locator('.school-warning-summary');
    await expect(badgeGroup).not.toHaveClass(/is-hidden/);
    await expect(badges).toHaveCount(1);
    await expect(badges.nth(0)).toContainText('At least 6 warning signs');
    await expect(badges.nth(0)).toHaveClass(/is-broad/);
    await expect(badges.nth(0)).toHaveAttribute('aria-label', /at least 6/i);

    await page.goto(`/school.html?unitid=${noWarningBadgeUnitid}`);

    badgeGroup = page.locator('#school-warning-summary');
    badges = badgeGroup.locator('.school-warning-summary');
    await expect(badgeGroup).toHaveClass(/is-hidden/);
    await expect(badges).toHaveCount(0);
  });

  test('school page without unitid shows a clean empty state', async ({ page }) => {
    await page.goto('/school.html');

    await expect(page.locator('#school-name')).toHaveText('No school selected');
    await expect(page.locator('#school-meta-wrap')).toHaveClass(/is-hidden/);
    await expect(page.locator('#financial-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#enrollment-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#staffing-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#endowment-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#aid-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#download-school-data')).toHaveClass(/is-hidden/);
    await expect(page.locator('#share-school-profile')).toHaveClass(/is-hidden/);
  });

  test('school page with invalid unitid shows a clean load-error state', async ({ page }) => {
    await page.goto('/school.html?unitid=does-not-exist');

    await expect(page.locator('#school-name')).toHaveText('This school page could not be loaded.');
    await expect(page.locator('#school-meta-wrap')).toHaveClass(/is-hidden/);
    await expect(page.locator('#school-outcomes-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#financial-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#enrollment-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#staffing-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#endowment-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#aid-section')).toHaveClass(/is-hidden/);
    await expect(page.locator('#download-school-data')).toHaveClass(/is-hidden/);
    await expect(page.locator('#share-school-profile')).toHaveClass(/is-hidden/);
  });
});
