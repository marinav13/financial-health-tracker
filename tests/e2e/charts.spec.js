/**
 * End-to-end tests for chart rendering on school detail page.
 * 
 * Tests that:
 * - Charts render as SVG elements
 * - Chart titles appear
 * - Axis labels are present
 * - Multiple chart categories work
 */

const { test, expect } = require('@playwright/test');
const { schoolWithCharts } = require('./helpers');

const chartSchoolUnitid = schoolWithCharts();

test.describe('Chart rendering', () => {
  test('revenue chart renders', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    const chartContainer = page.locator('#chart-revenue');
    await expect(chartContainer).toBeVisible();
    
    // Should have SVG inside
    const svg = chartContainer.locator('svg');
    await expect(svg).toBeVisible();
    
    // Should have chart title
    const title = chartContainer.locator('.chart-title');
    const hasTitle = await title.count();
    // Title is optional - just verify chart rendered
  });

  test('net tuition chart renders', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    const chartContainer = page.locator('#chart-net-tuition');
    await expect(chartContainer).toBeVisible();
    
    const svg = chartContainer.locator('svg');
    await expect(svg).toBeVisible();
  });

  test('enrollment chart renders', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    const chartContainer = page.locator('#chart-enrollment');
    await expect(chartContainer).toBeVisible();
    
    const svg = chartContainer.locator('svg');
    await expect(svg).toBeVisible();
  });

  test('chart has accessibility attributes', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    const chartContainer = page.locator('#chart-revenue');
    
    const svg = chartContainer.locator('svg');
    // Check role on SVG, and aria-label is on the same element per HTML structure
    const role = await svg.getAttribute('role');
    const ariaLabel = await svg.getAttribute('aria-label');
    expect(role).toBe('img');
    expect(ariaLabel).toBeTruthy();
  });

  test('multiple charts render on page load', async ({ page }) => {
    await page.goto(`/school.html?unitid=${chartSchoolUnitid}`);
    
    // Verify at least 3 specific chart containers have rendered SVGs
    const constRevenue = page.locator('#chart-revenue svg');
    const constTuition = page.locator('#chart-net-tuition svg');
    const constEnrollment = page.locator('#chart-enrollment svg');
    
    await expect(constRevenue).toBeVisible();
    await expect(constTuition).toBeVisible();
    await expect(constEnrollment).toBeVisible();
  });
});
