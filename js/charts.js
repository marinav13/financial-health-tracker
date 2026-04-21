/**
 * FILE: charts.js
 * Renders interactive SVG line charts with tooltips and legends.
 * Called from school.html with series data from school JSON.
 * Supports currency, percentage, and number formatting.
 */

// ------ Value Formatting ------

// Formats numbers for display: currency ($1,234), percent (25%), or plain (12,345)
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

// ------ Axis Scaling ------

// Calculates "nice" ceiling for Y-axis (1, 2, 5, or 10 × 10^n)
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

// ------ Chart Rendering ------

function renderLineChart(containerId, config) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const showTooltip = config.showTooltip !== false;
  const showLegend = config.showLegend !== false;

  const seriesList = (config.series || []).filter((s) => Array.isArray(s.values) && s.values.length > 0);
  if (!seriesList.length) {
    container.innerHTML = `<p class="metric-copy">No data available.</p>`;
    return;
  }

  const allPoints = seriesList.flatMap((s) => s.values);
  const values = allPoints.map((p) => Number(p.value)).filter((v) => !Number.isNaN(v));
  const years = allPoints.map((p) => Number(p.year));
  const format = config.format || "number";

  const minYear = Math.min(...years);
  const maxYear = Math.max(...years);
  const rawMaxY = Math.max(...values);
  const minY = 0;
  const maxY = niceCeiling((rawMaxY <= 0 ? 1 : rawMaxY) * 1.05); // 5% padding above max

  const width = 760;
  const height = 260;
  const pad = {
    top: 18,
    right: 20,
    bottom: 34,
    left: format === "currency" ? 132 : 60
  };
  const innerW = width - pad.left - pad.right;
  const innerH = height - pad.top - pad.bottom;

  // Scale functions: data coords → SVG pixel coords
  const ySpan = maxY === minY ? 1 : maxY - minY;
  const xSpan = maxYear === minYear ? 1 : maxYear - minYear;
  const xScale = (year) => pad.left + ((year - minYear) / xSpan) * innerW;
  const yScale = (value) => pad.top + (1 - ((value - minY) / ySpan)) * innerH;

  // 5 horizontal gridlines from top to bottom
  const gridLines = [];
  const yTicks = [];
  for (let i = 0; i < 5; i += 1) {
    const y = pad.top + (i / 4) * innerH;
    const tickValue = maxY - ((maxY - minY) * i / 4);
    gridLines.push(`<line x1="${pad.left}" y1="${y}" x2="${width - pad.right}" y2="${y}" stroke="#e5e7eb" stroke-width="1" />`);
    yTicks.push(`<text x="${pad.left - 10}" y="${y + 4}" text-anchor="end" font-size="12" fill="#6b7280">${formatChartValue(tickValue, format)}</text>`);
  }

  // X-axis year labels
  const yearTicks = [];
  for (let year = minYear; year <= maxYear; year += 1) {
    const x = xScale(year);
    yearTicks.push(`<text x="${x}" y="${height - 8}" text-anchor="middle" font-size="12" fill="#6b7280">${year}</text>`);
  }

  // Line paths (M = move to, L = line to)
  const paths = seriesList.map((series) => {
    const d = series.values.map((point, index) => {
      const cmd = index === 0 ? "M" : "L";
      return `${cmd} ${xScale(Number(point.year))} ${yScale(Number(point.value))}`;
    }).join(" ");
    return `<path d="${d}" fill="none" stroke="${series.color}" stroke-width="3" stroke-linejoin="round" stroke-linecap="round" />`;
  }).join("");

  // Data points with optional tooltips
  const pointGroups = seriesList.map((series) => series.values.map((point) => {
    const x = xScale(Number(point.year));
    const y = yScale(Number(point.value));
    const title = showTooltip
      ? `<title>${series.label}: ${formatChartValue(Number(point.value), format)} (${point.year})</title>`
      : "";
    return `<circle cx="${x}" cy="${y}" r="3.5" fill="${series.color}" opacity="0.9">${title}</circle>`;
  }).join("")).join("");

  const title = config.title ? `<p class="chart-title">${config.title}</p>` : "";
  const legend = seriesList.map((series) => (
    `<span><span class="legend-dot" style="background:${series.color}"></span>${series.label}</span>`
  )).join("");
  const descriptionId = `${containerId}-desc`;
  const descriptionText = config.title || "Chart";
  const description = `<p id="${descriptionId}" style="position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;" aria-hidden="true">${descriptionText}. Data: ${seriesList.map((s) => `${s.label}: ${s.values.map((p) => `${p.year}: ${formatChartValue(Number(p.value), format)}`).join(", ")}`).join(". ")}</p>`;

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

  // Interactive tooltip: show data for nearest year to mouse position
  const svg = container.querySelector("svg");
  const tooltip = container.querySelector(".chart-tooltip");
  if (!svg || !showTooltip || !tooltip) return;

  const yearsSorted = Array.from(new Set(years)).sort((a, b) => a - b);

  const renderTooltip = (evt) => {
    const rect = svg.getBoundingClientRect();
    const localX = ((evt.clientX - rect.left) / rect.width) * width;

    // Find year closest to mouse X position
    let bestYear = yearsSorted[0];
    let bestDistance = Math.abs(localX - xScale(bestYear));

    yearsSorted.forEach((year) => {
      const distance = Math.abs(localX - xScale(year));
      if (distance < bestDistance) {
        bestDistance = distance;
        bestYear = year;
      }
    });

    const rows = seriesList.map((series) => {
      const point = series.values.find((value) => Number(value.year) === bestYear);
      if (!point) return null;
      return `<span class="chart-tooltip-row">${series.label}: ${formatChartValue(Number(point.value), format)}</span>`;
    }).filter(Boolean);

    const customRows = typeof config.tooltipRows === "function"
      ? config.tooltipRows(bestYear, seriesList, formatChartValue)
      : null;
    const tooltipRows = Array.isArray(customRows) && customRows.length ? customRows : rows;

    if (!tooltipRows.length) return;

    tooltip.innerHTML = `<strong>${bestYear}</strong>${tooltipRows.join("")}`;
    tooltip.style.left = `${((xScale(bestYear) / width) * 100).toFixed(1)}%`;
    tooltip.style.display = "block";
    tooltip.setAttribute("aria-hidden", "false");
  };

  svg.addEventListener("mousemove", renderTooltip);
  svg.addEventListener("mouseenter", renderTooltip);
  svg.addEventListener("mouseleave", () => {
    tooltip.style.display = "none";
    tooltip.setAttribute("aria-hidden", "true");
  });
}