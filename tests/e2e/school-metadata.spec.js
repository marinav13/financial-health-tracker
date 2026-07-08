const { test, expect } = require('@playwright/test');
const { schoolWithCharts } = require('./helpers');

const unitid = schoolWithCharts();

test.describe('School page metadata', () => {
  test('school profile sets per-school title, canonical, and og tags', async ({ page }) => {
    await page.goto(`/school.html?unitid=${unitid}`);
    await expect(page.locator('#school-name')).not.toHaveText('Loading...');
    const name = (await page.locator('#school-name').textContent()).trim();
    expect(name).toBeTruthy();

    const expectedTitle = `${name} \u2014 College Financial Health Tracker`;
    await expect(page).toHaveTitle(expectedTitle);

    const canonical = await page.locator('link[rel="canonical"]').getAttribute('href');
    expect(canonical.endsWith(`/school.html?unitid=${unitid}`)).toBeTruthy();

    const ogUrl = await page.locator('meta[property="og:url"]').getAttribute('content');
    expect(ogUrl).toBe(canonical);

    const ogTitle = await page.locator('meta[property="og:title"]').getAttribute('content');
    expect(ogTitle).toBe(expectedTitle);

    const twitterTitle = await page.locator('meta[name="twitter:title"]').getAttribute('content');
    expect(twitterTitle).toBe(expectedTitle);
  });

  test('guide landing without a unitid keeps the static canonical', async ({ page }) => {
    await page.goto('/school.html');
    await expect(page.locator('#school-intro-callout')).toBeVisible();
    const canonical = await page.locator('link[rel="canonical"]').getAttribute('href');
    expect(canonical.includes('unitid=')).toBeFalsy();
    expect(await page.title()).toContain('College Financial Health Tracker');
  });
});
