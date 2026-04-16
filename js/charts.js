/**
 * FILE: charts.js
 * PURPOSE: Creates interactive line charts for the Financial Health Tracker
 * FEATURES:
 *   - Renders SVG line charts with multiple data series
 *   - Dynamic axis scaling and gridlines
 *   - Hover tooltip showing data for specific years
 *   - Responsive chart legend
 *   - Supports currency, percentage, and number formatting
 *
 * DATA FLOW: school.html calls renderLineChart() with series data from school JSON
 * Example data structure:
 *   {
 *     title: "Revenue vs Expenses",
 *     series: [
 *       { label: "Revenue", color: "#005ab5", values: [{year: 2020, value: 100000}, ...] },
 *       { label: "Expenses", color: "#dc3220", values: [{year: 2020, value: 95000}, ...] }
 *     ]
 *   }
 */

/**
 * Formats a numeric value for display in charts
 * @description Converts numbers to human-readable strings with locale-specific formatting.
 * Handles currency, percentages, or plain numbers. Shows "No data" for null/undefined/NaN.
 * @param {number|null|undefined} value - The numeric value to format
 * @param {string} format - Format type: "currency", "percent", or "number" (default: "number")
 * @returns {string} Formatted string (e.g., "$1,234,567", "25%", "12,345")
 */
function formatChartValue(value, format = "number") {
  if (value === null || value === undefined || Number.isNaN(value)) return "No data";
  if (format === "currency") {
    return new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    }).format(value);
  }
  if (format === "percent") {
    return `${new Intl.NumberFormat("en-US", {
      maximumFractionDigits: 0
    }).format(value)}%`;
  }
  return new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 0
  }).format(value);
}

/**
 * Calculates a "nice" ceiling value for chart Y-axis scaling
 * @description Finds a round number greater than the input value that's suitable as a chart max.
 * Uses logarithmic scaling to pick nice increments (1, 2, 5, or 10 × 10^n).
 * Example: niceCeiling(12345) might return 20000 (much cleaner than 12345)
 * @param {number} value - The raw maximum data value to round up
 * @returns {number} A "nice" rounded ceiling value for chart axis
 */
function niceCeiling(value) {
  if (!Number.isFinite(value) || value <= 0) return 1;
  const exponent = Math.floor(Math.log10(value));
  const base = 10 ** exponent;
  const normalized = value / base;
  let step;
  if (normalized <= 1) step = 1;
  else if (normalized <= 2) step = 2;
  else if (normalized <= 5) step = 5;
  else step = 10;
  return step * base;
}

/**
 * Renders an interactive SVG line chart in a DOM container
 * @description Creates a complete multi-series line chart with:
 *   - Dynamic scaling based on data ranges
 *   - Horizontal gridlines and Y-axis labels
 *   - Year labels on X-axis
 *   - Interactive tooltip on hover (if enabled)
 *   - Legend showing series colors and labels (if enabled)
 *   - Accessible SVG with ARIA labels and title elements
 *
 * @param {string} containerId - ID of HTML element to render chart into
 * @param {Object} config - Chart configuration object
 * @param {string} config.title - Chart title (displayed above chart)
 * @param {Array<Object>} config.series - Array of data series
 * @param {string} config.series[].label - Display name for series (in legend)
 * @param {string} config.series[].color - Hex color for this series line
 * @param {Array<Object>} config.series[].values - Array of {year, value} points
 * @param {string} config.format - Value format: "currency", "percent", or "number"
 * @param {boolean} config.showTooltip - Whether to show hover tooltip (default: true)
 * @param {boolean} config.showLegend - Whether to show legend (default: true)
 * @param {Function} config.tooltipRows - Optional custom function to generate tooltip rows
 * @returns {void} Modifies DOM directly
 */
function renderLineChart(containerId, config) {
  const container = document.getElementById(containerId);
  if (!container) return;

  // Chart configuration defaults
  const showTooltip = config.showTooltip !== false;
  const showLegend = config.showLegend !== false;

  // Filter out empty series (series with no values)
  const seriesList = (config.series || []).filter((s) => Array.isArray(s.values) && s.values.length > 0);
  if (!seriesList.length) {
    container.innerHTML = `<p class="metric-copy">No data available.</p>`;
    return;
  }

  // Extract all data points to determine axis ranges
  const allPoints = seriesList.flatMap((s) => s.values);
  const values = allPoints.map((p) => Number(p.value)).filter((v) => !Number.isNaN(v));
  const years = allPoints.map((p) => Number(p.year));
  const format = config.format || "number";

  // Calculate axis ranges
  const minYear = Math.min(...years);
  const maxYear = Math.max(...years);
  const rawMaxY = Math.max(...values);
  const minY = 0; // Always start Y-axis at 0
  const maxY = niceCeiling((rawMaxY <= 0 ? 1 : rawMaxY) * 1.05); // Add 5% padding

  // SVG dimensions and padding (padding is left/right/top/bottom space for axes)
  const width = 760;
  const height = 260;
  const pad = {
    top: 18,
    right: 20,
    bottom: 34,
    left: format === "currency" ? 132 : 60 // More space for currency labels
  };
  const innerW = width - pad.left - pad.right;
  const innerH = height - pad.top - pad.bottom;

  // Create scale functions: convert data coords to SVG pixel coords
  const ySpan = maxY === minY ? 1 : maxY - minY;
  const xSpan = maxYear === minYear ? 1 : maxYear - minYear;
  const xScale = (year) => pad.left + ((year - minYear) / xSpan) * innerW;
  const yScale = (value) => pad.top + (1 - ((value - minY) / ySpan)) * innerH;

  // Generate horizontal gridlines and Y-axis labels (5 lines from bottom to top)
  const gridLines = [];
  const yTicks = [];
  for (let i = 0; i < 5; i += 1) {
    const y = pad.top + (i / 4) * innerH;
    const tickValue = maxY - ((maxY - minY) * i / 4); // Decrement from max to 0
    gridLines.push(`<line x1="${pad.left}" y1="${y}" x2="${width - pad.right}" y2="${y}" stroke="#e5e7eb" stroke-width="1" />`);
    yTicks.push(`<text x="${pad.left - 10}" y="${y + 4}" text-anchor="end" font-size="12" fill="#6b7280">${formatChartValue(tickValue, format)}</text>`);
  }

  // Generate X-axis year labels (one for each year in range)
  const yearTicks = [];
  for (let year = minYear; year <= maxYear; year += 1) {
    const x = xScale(year);
    yearTicks.push(`<text x="${x}" y="${height - 8}" text-anchor="middle" font-size="12" fill="#6b7280">${year}</text>`);
  }

  // Generate line paths (SVG path commands for each series)
  const paths = seriesList.map((series) => {
    const d = series.values.map((point, index) => {
      const cmd = index === 0 ? "M" : "L"; // M = move to, L = line to
      return `${cmd} ${xScale(Number(point.year))} ${yScale(Number(point.value))}`;
    }).join(" ");
    return `<path d="${d}" fill="none" stroke="${series.color}" stroke-width="3" stroke-linejoin="round" stroke-linecap="round" />`;
  }).join("");

  // Generate data point circles and optional tooltips
  const pointGroups = seriesList.map((series) => series.values.map((point) => {
    const x = xScale(Number(point.year));
    const y = yScale(Number(point.value));
    const title = showTooltip
      ? `<title>${series.label}: ${formatChartValue(Number(point.value), format)} (${point.year})</title>`
      : "";
    return `<circle cx="${x}" cy="${y}" r="3.5" fill="${series.color}" opacity="0.9">${title}</circle>`;
  }).join("")).join("");

  // Build chart title, legend, and accessibility description
  const title = config.title ? `<p class="chart-title">${config.title}</p>` : "";
  const legend = seriesList.map((series) => (
    `<span><span class="legend-dot" style="background:${series.color}"></span>${series.label}</span>`
  )).join("");
  const descriptionId = `${containerId}-desc`;
  const descriptionText = config.title || "Chart";
  // Accessibility: provide full data description in hidden text for screen readers
  const description = `<p id="${descriptionId}" style="position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;" aria-hidden="true">${descriptionText}. Data: ${seriesList.map((s) => `${s.label}: ${s.values.map((p) => `${p.year}: ${formatChartValue(Number(p.value), format)}`).join(", ")}`).join(". ")}</p>`;

  // Render complete SVG chart HTML
  container.innerHTML = `
    ${title}
    ${description}
    ${showTooltip ? '<div class="chart-tooltip" aria-hidden="true"></div>' : ""}
    <svg class="chart-svg" viewBox="0 0 ${width} ${height}" aria-label="${config.title || "Chart"}" role="img">
      <rect x="0" y="0" width="${width}" height="${height}" fill="#fbfdff"></rect>
      ${gridLines.join("")}
      ${yTicks.join("")}
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      ${paths}
      ${pointGroups}
      ${yearTicks.join("")}
    </svg>
    ${showLegend ? `<div class="chart-legend">${legend}</div>` : ""}
  `;

  // Set up interactive tooltip on mouse move
  const svg = container.querySelector("svg");
  const tooltip = container.querySelector(".chart-tooltip");
  if (!svg || !showTooltip || !tooltip) return;

  // Get unique sorted years for nearest-neighbor tooltip matching
  const yearsSorted = Array.from(new Set(years)).sort((a, b) => a - b);

  /**
   * Renders tooltip showing data for nearest year to mouse position
   * @description Finds the year closest to the mouse X position and displays
   * data for all series at that year
   * @param {MouseEvent} evt - Mouse movement event from SVG
   */
  const renderTooltip = (evt) => {
    const rect = svg.getBoundingClientRect();
    const localX = ((evt.clientX - rect.left) / rect.width) * width;

    // Find the nearest year to the mouse X position
    let bestYear = yearsSorted[0];
    let bestDistance = Math.abs(localX - xScale(bestYear));

    yearsSorted.forEach((year) => {
      const distance = Math.abs(localX - xScale(year));
      if (distance < bestDistance) {
        bestDistance = distance;
        bestYear = year;
      }
    });

    // Build tooltip rows for all series at the nearest year
    const rows = seriesList.map((series) => {
      const point = series.values.find((value) => Number(value.year) === bestYear);
      if (!point) return null;
      return `<span class="chart-tooltip-row">${series.label}: ${formatChartValue(Number(point.value), format)}</span>`;
    }).filter(Boolean);

    // Allow custom tooltip row generation via config function
    const customRows = typeof config.tooltipRows === "function"
      ? config.tooltipRows(bestYear, seriesList, formatChartValue)
      : null;
    const tooltipRows = Array.isArray(customRows) && customRows.length ? customRows : rows;

    if (!tooltipRows.length) return;

    // Position and render tooltip
    tooltip.innerHTML = `<strong>${bestYear}</strong>${tooltipRows.join("")}`;
    tooltip.style.left = `${((xScale(bestYear) / width) * 100).toFixed(1)}%`;
    tooltip.style.display = "block";
  };

  // Attach tooltip listener
  svg.addEventListener("mousemove", renderTooltip);
  svg.addEventListener("mouseenter", renderTooltip);
  svg.addEventListener("mouseleave", () => {
    tooltip.style.display = "none";
  });
}
