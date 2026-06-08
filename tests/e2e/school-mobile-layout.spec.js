const { test, expect } = require('@playwright/test');
const { schoolWithCharts } = require('./helpers');

const mobileSchoolUnitid = schoolWithCharts();

test.describe('School mobile layout', () => {
  test('school detail page stays within the phone viewport', async ({ browser, baseURL }) => {
    const context = await browser.newContext({
      viewport: { width: 390, height: 844 },
      isMobile: true,
      hasTouch: true
    });
    const page = await context.newPage();

    await page.goto(`${baseURL}/school.html?unitid=${mobileSchoolUnitid}`);
    await expect(page.locator('#chart-enrollment .chart-svg')).toBeVisible();

    const dimensions = await page.evaluate(() => ({
      clientWidth: document.documentElement.clientWidth,
      scrollWidth: document.documentElement.scrollWidth
    }));

    expect(dimensions.scrollWidth).toBeLessThanOrEqual(dimensions.clientWidth + 1);

    await context.close();
  });
});
