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

// Abbreviated form of formatChartValue for Y-axis tick labels on
// narrow viewports. Trades precision (which the tooltip still has)
// for width: "$50M" instead of "$50,000,000", "32K" instead of
// "32,000". Keeps full formatChartValue behavior for percent and for
// any value below 1,000 where abbreviation gains nothing. Tooltips
// and aria descriptions continue to use the unabbreviated formatter.
function formatChartAxisTick(value, format = "number", abbreviate = false) {
  if (!abbreviate) return formatChartValue(value, format);
  if (value === null || value === undefined || Number.isNaN(value)) return "No data";
  if (format === "percent") return formatChartValue(value, format);
  const abs = Math.abs(value);
  const sign = value < 0 ? "-" : "";
  const prefix = format === "currency" ? "$" : "";
  const trim = (n) => n.toFixed(1).replace(/\.0$/, "");
  if (abs >= 1e9) return `${sign}${prefix}${trim(abs / 1e9)}B`;
  if (abs >= 1e6) return `${sign}${prefix}${trim(abs / 1e6)}M`;
  if (abs >= 1e3) return `${sign}${prefix}${Math.round(abs / 1e3)}K`;
  return formatChartValue(value, format);
}

function escapeChartHtml(value) {
  return String(value ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

const DEFAULT_CHART_COLOR_PRIMARY = "#005ab5";
const DEFAULT_CHART_COLOR_SECONDARY = "#e69f00";

function safeChartColor(value, fallback = DEFAULT_CHART_COLOR_PRIMARY) {
  const color = String(value || "").trim();
  // Valid CSS hex colors are exactly 3, 4, 6, or 8 hex digits. The prior
  // {3,8} quantifier accepted 5 and 7, which no browser renders.
  return /^#(?:[0-9a-f]{3,4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.test(color) ? color : fallback;
}

function renderTooltipRow(value) {
  const text = String(value ?? "").replace(/<[^>]*>/g, "");
  return `<span class="chart-tooltip-row">${escapeChartHtml(text)}</span>`;
}

function setElementClassState(element, className, shouldHaveClass) {
  if (!element) return;
  if (element.classList && typeof element.classList.toggle === "function") {
    element.classList.toggle(className, shouldHaveClass);
    return;
  }

  const current = String(element.getAttribute?.("class") || "")
    .split(/\s+/)
    .filter(Boolean);
  const next = shouldHaveClass
    ? Array.from(new Set([...current, className]))
    : current.filter((token) => token !== className);

  if (typeof element.setAttribute === "function") {
    element.setAttribute("class", next.join(" "));
  }
}

// ------ Axis Scaling ------

// Calculates "nice" ceiling for Y-axis (1, 2, 5, or 10 x 10^n)
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
  const enableSeriesToggle = config.enableSeriesToggle === true;

  const fullSeriesList = (config.series || [])
    .filter((s) => Array.isArray(s.values) && s.values.length > 0)
    .map((series, index) => ({
      ...series,
      color: safeChartColor(
        series.color,
        index === 1 ? DEFAULT_CHART_COLOR_SECONDARY : DEFAULT_CHART_COLOR_PRIMARY
      )
    }));
  if (!fullSeriesList.length) {
    container.innerHTML = `<p class="metric-copy">No data available.</p>`;
    return;
  }

  const format = config.format || "number";
  const width = 760;
  const height = 260;
  // Detect a narrow viewport so we can shrink the chart's outer padding
  // and let the (CSS-bumped) axis labels reclaim space. Falls back to
  // desktop sizing in non-DOM environments (the JS unit tests).
  const isMobile =
    typeof window !== "undefined" &&
    typeof window.matchMedia === "function" &&
    window.matchMedia("(max-width: 700px)").matches;
  // Narrower pad.left on mobile — the abbreviated Y-axis labels
  // ("$50M", "32K") emitted by formatChartAxisTick fit comfortably in
  // ~70u currency / ~56u non-currency at the bumped 24u tick font.
  const pad = isMobile
    ? {
        top: 14,
        right: format === "currency" ? 22 : 18,
        bottom: 36,
        left: format === "currency" ? 76 : 56
      }
    : {
        top: 18,
        right: format === "currency" ? 40 : 32,
        bottom: 34,
        left: format === "currency" ? 138 : 80
      };
  const innerW = width - pad.left - pad.right;
  const innerH = height - pad.top - pad.bottom;
  const safeTitle = escapeChartHtml(config.title || "Chart");

  let title = "";
  if (config.title) {
    const match = String(config.title).match(/^(.+?)\s*\(([^()]+)\)\s*$/);
    if (match) {
      title = `<p class="chart-title">${escapeChartHtml(match[1])}<span class="sub">${escapeChartHtml(match[2])}</span></p>`;
    } else {
      title = `<p class="chart-title">${safeTitle}</p>`;
    }
  }

  const activeSeries = new Set(fullSeriesList.map((_series, index) => index));

  function renderCurrentState() {
    const seriesList = fullSeriesList.filter((_series, index) => activeSeries.has(index));
    if (!seriesList.length) {
      activeSeries.add(0);
      return renderCurrentState();
    }

    const allPoints = seriesList.flatMap((s) => s.values);
    const values = allPoints.map((p) => Number(p.value)).filter((v) => !Number.isNaN(v));
    const years = allPoints.map((p) => Number(p.year));
    const minYear = Math.min(...years);
    const maxYear = Math.max(...years);
    const rawMinY = Math.min(...values);
    const rawMaxY = Math.max(...values);
    const minY = rawMinY < 0 ? -niceCeiling(Math.abs(rawMinY) * 1.05) : 0;
    const maxY = rawMaxY > 0 ? niceCeiling(rawMaxY * 1.05) : 0;
    const ySpan = maxY === minY ? 1 : maxY - minY;
    const xSpan = maxYear === minYear ? 1 : maxYear - minYear;
    const xScale = (year) => pad.left + ((year - minYear) / xSpan) * innerW;
    const yScale = (value) => pad.top + (1 - ((value - minY) / ySpan)) * innerH;

    const gridLines = [];
    const yTicks = [];
    for (let i = 0; i < 5; i += 1) {
      const y = pad.top + (i / 4) * innerH;
      const tickValue = maxY - ((maxY - minY) * i / 4);
      gridLines.push(`<line x1="${pad.left}" y1="${y}" x2="${width - pad.right}" y2="${y}" stroke="#e5e7eb" stroke-width="1" />`);
      yTicks.push(`<text class="chart-axis-tick chart-axis-tick--y" x="${pad.left - 10}" y="${y + 4}" text-anchor="end" font-size="14" fill="#6b7280">${formatChartAxisTick(tickValue, format, isMobile)}</text>`);
    }

    const yearTicks = [];
    for (let year = minYear; year <= maxYear; year += 1) {
      const x = xScale(year);
      yearTicks.push(`<text class="chart-axis-tick chart-axis-tick--x" x="${x}" y="${height - 8}" text-anchor="middle" font-size="14" fill="#6b7280">${year}</text>`);
    }

    const paths = seriesList.map((series) => {
      const d = series.values.map((point, index) => {
        const cmd = index === 0 ? "M" : "L";
        return `${cmd} ${xScale(Number(point.year))} ${yScale(Number(point.value))}`;
      }).join(" ");
      return `<path d="${d}" fill="none" stroke="${series.color}" stroke-width="3" stroke-linejoin="round" stroke-linecap="round" />`;
    }).join("");

    const pointGroups = seriesList.map((series) => series.values.map((point) => {
      const x = xScale(Number(point.year));
      const y = yScale(Number(point.value));
      const pointTitle = showTooltip
        ? `<title>${escapeChartHtml(series.label)}: ${escapeChartHtml(formatChartValue(Number(point.value), format))} (${escapeChartHtml(point.year)})</title>`
        : "";
      return `<circle cx="${x}" cy="${y}" r="3.5" fill="${series.color}" opacity="0.9">${pointTitle}</circle>`;
    }).join("")).join("");

    const legend = fullSeriesList.map((series, index) => {
      const isActive = activeSeries.has(index);
      const marker = `<svg class="legend-dot" viewBox="0 0 10 10" aria-hidden="true" focusable="false"><circle cx="5" cy="5" r="5" fill="${series.color}"></circle></svg>`;
      if (enableSeriesToggle && fullSeriesList.length > 1) {
        return `<button type="button" class="chart-legend-button${isActive ? " is-active" : ""}" data-series-index="${index}" aria-pressed="${isActive ? "true" : "false"}">${marker}${escapeChartHtml(series.label)}</button>`;
      }
      return `<span>${marker}${escapeChartHtml(series.label)}</span>`;
    }).join("");

    const descriptionId = `${String(containerId).replace(/[^\w-]/g, "-")}-desc`;
    const description = `<p id="${descriptionId}" class="sr-only">${safeTitle}. Data: ${seriesList.map((s) => `${escapeChartHtml(s.label)}: ${s.values.map((p) => `${escapeChartHtml(p.year)}: ${escapeChartHtml(formatChartValue(Number(p.value), format))}`).join(", ")}`).join(". ")}</p>`;

    container.innerHTML = `
      ${title}
      ${description}
      ${showTooltip ? '<div class="chart-tooltip" aria-hidden="true"></div>' : ""}
      <svg class="chart-svg" viewBox="0 0 ${width} ${height}" aria-label="${safeTitle}" aria-describedby="${descriptionId}" role="img">
        <rect x="0" y="0" width="${width}" height="${height}" fill="#fbfdff"></rect>
        ${gridLines.join("")}
        ${yTicks.join("")}
        <line x1="${pad.left}" y1="${yScale(0)}" x2="${width - pad.right}" y2="${yScale(0)}" stroke="#94a3b8" />
        <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" stroke="#94a3b8" />
        ${paths}
        ${pointGroups}
        ${yearTicks.join("")}
      </svg>
      ${showLegend ? `<div class="chart-legend">${legend}</div>` : ""}
    `;

    const legendButtons = container.querySelectorAll?.(".chart-legend-button") || [];
    legendButtons.forEach((button) => {
      button.addEventListener("click", () => {
        const index = Number(button.getAttribute("data-series-index"));
        if (!Number.isFinite(index)) return;
        if (activeSeries.has(index) && activeSeries.size === 1) return;
        if (activeSeries.has(index)) activeSeries.delete(index);
        else activeSeries.add(index);
        renderCurrentState();
      });
    });

    const svg = container.querySelector("svg");
    const tooltip = container.querySelector(".chart-tooltip");
    if (!svg || !showTooltip || !tooltip) return;

    const yearsSorted = Array.from(new Set(years)).sort((a, b) => a - b);
    const renderTooltip = (evt) => {
      const rect = svg.getBoundingClientRect();
      const localX = ((evt.clientX - rect.left) / rect.width) * width;

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
        return `${series.label}: ${formatChartValue(Number(point.value), format)}`;
      }).filter(Boolean);

      const customRows = typeof config.tooltipRows === "function"
        ? config.tooltipRows(bestYear, seriesList, formatChartValue)
        : null;
      const tooltipRows = Array.isArray(customRows) && customRows.length ? customRows : rows;
      if (!tooltipRows.length) return;

      tooltip.innerHTML = `<strong>${escapeChartHtml(bestYear)}</strong>${tooltipRows.map(renderTooltipRow).join("")}`;
      tooltip.style.left = `${((xScale(bestYear) / width) * 100).toFixed(1)}%`;
      setElementClassState(tooltip, "visible", true);
    };

    svg.addEventListener("mousemove", renderTooltip);
    svg.addEventListener("mouseenter", renderTooltip);
    svg.addEventListener("mouseleave", () => {
      setElementClassState(tooltip, "visible", false);
    });
  }

  renderCurrentState();
}
