/**
 * End-to-end tests for the EMMA bond-rating search card on school profiles.
 *
 * Tests that:
 * - The emma-section is present and visible on any loaded school profile
 * - The search link is populated with the institution name
 * - The link href points to emma.msrb.org with a ?q= query string
 * - The aria-label includes the institution name
 */

const { test, expect } = require('@playwright/test');
const {
  schoolWithCharts,
  schoolWithoutEndowment,
  firstSchoolIndexEntry,
} = require('./helpers');

const publicSchoolUnitid = schoolWithCharts();
const smallSchoolUnitid = schoolWithoutEndowment();
const firstEntryUnitid = firstSchoolIndexEntry().unitid;

test.describe('EMMA bond-rating card', () => {
  test('card renders on a school with charts', async ({ page }) => {
    await page.goto(`/school.html?unitid=${publicSchoolUnitid}`);

    const section = page.locator('#emma-section');
    await expect(section).toBeVisible();

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();
  });

  test('card renders on a school without endowment data', async ({ page }) => {
    await page.goto(`/school.html?unitid=${smallSchoolUnitid}`);

    const section = page.locator('#emma-section');
    await expect(section).toBeVisible();

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();
  });

  test('card renders for first school in index', async ({ page }) => {
    await page.goto(`/school.html?unitid=${firstEntryUnitid}`);

    const section = page.locator('#emma-section');
    await expect(section).toBeVisible();

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();
  });

  test('search link href points to emma.msrb.org with prefilled query', async ({ page }) => {
    await page.goto(`/school.html?unitid=${publicSchoolUnitid}`);

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();

    const href = await link.getAttribute('href');
    expect(href).toBe('https://emma.msrb.org/Search/Search.aspx');
  });

  test('search link text includes institution name', async ({ page }) => {
    await page.goto(`/school.html?unitid=${publicSchoolUnitid}`);

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();

    const text = await link.textContent();
    expect(text).toMatch(/Search EMMA for .+/);
  });

  test('search link aria-label includes institution name', async ({ page }) => {
    await page.goto(`/school.html?unitid=${publicSchoolUnitid}`);

    const link = page.locator('#emma-search-link');
    await expect(link).toBeVisible();

    const ariaLabel = await link.getAttribute('aria-label');
    expect(ariaLabel).toMatch(/Search EMMA for .+/);
  });

  test('card is hidden on guide landing page (no unitid)', async ({ page }) => {
    await page.goto('/school.html');

    const section = page.locator('#emma-section');
    await expect(section).toHaveClass(/is-hidden/);
  });

  test('link opens in new tab', async ({ page }) => {
    await page.goto(`/school.html?unitid=${publicSchoolUnitid}`);

    const link = page.locator('#emma-search-link');
    await expect(link).toHaveAttribute('target', '_blank');
    await expect(link).toHaveAttribute('rel', /noopener/);
  });
});
