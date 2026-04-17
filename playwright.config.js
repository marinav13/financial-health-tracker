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
  },
  
  // Projects for different browsers
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
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