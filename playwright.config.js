/**
 * Playwright configuration for Financial Health Tracker e2e tests.
 * 
 * Run with:
 *   npm install
 *   npm run test:e2e
 * 
 * For headed mode (see browser):
 *   npm run test:e2e:headed
 */

const { defineConfig, devices } = require('@playwright/test');

module.exports = defineConfig({
  // Run tests sequentially to avoid port conflicts with local server
  fullyParallel: false,
  
  // Fail fast on first failure
  forbidOnly: !!process.env.CI,
  
  // Retry failures in CI
  retries: process.env.CI ? 2 : 0,
  
  // Workers match CI environment
  workers: process.env.CI ? 1 : undefined,
  
  // Reporter
  reporter: 'list',
  
  // Global timeout
  timeout: 30000,
  
  // Use baseURL for local server
  use: {
    baseURL: 'http://localhost:8080',
    trace: 'on-first-retry',
    // Bypass CSP for test instrumentation. The app ships a strict
    // Content-Security-Policy meta tag (script-src 'self') which blocks
    // Playwright's page.addScriptTag({ content: ... }) used by axe-core
    // injection in accessibility-states.spec.js. CSP still applies in
    // production; this only affects what test harnesses can inject.
    bypassCSP: true,
  },
  
  // Projects for different browsers and viewports.
  //
  // Chromium runs the full spec suite — the primary coverage tier.
  //
  // Firefox and mobile-chrome each run a single canary spec (navigation) to
  // catch regressions that would only surface under a non-Chromium engine or
  // under a phone-sized viewport. Expanding these two tiers to the full spec
  // suite triples CI time for limited additional signal, so we keep them
  // narrowly scoped. If a spec starts hitting cross-browser or responsive
  // issues, add it to the testMatch list below rather than broadening to `/`.
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
    {
      name: 'firefox',
      use: { ...devices['Desktop Firefox'] },
      testMatch: /navigation\.spec\.js$/,
    },
    {
      name: 'mobile-chrome',
      use: { ...devices['Pixel 7'] },
      testMatch: /navigation\.spec\.js$/,
    },
  ],
  
  // Web server configuration
  webServer: {
    command: 'npx http-server -p 8080',
    port: 8080,
    reuseExistingServer: !process.env.CI,
    timeout: 120 * 1000,
  },
});