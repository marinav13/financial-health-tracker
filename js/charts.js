function formatCompactNumber(value) {
  if (value === null || value === undefined || Number.isNaN(value)) return "No data";
  return new Intl.NumberFormat("en-US", {
    notation: "compact",
    maximumFractionDigits: 1
  }).format(value);
}

function renderLineChart(containerId, config) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const seriesList = (config.series || []).filter((s) => Array.isArray(s.values) && s.values.length > 0);
  if (!seriesList.length) {
    container.innerHTML = `<p class="metric-copy">No data available.</p>`;
    return;
  }

  const allPoints = seriesList.flatMap((s) => s.values);
  const values = allPoints.map((p) => Number(p.value)).filter((v) => !Number.isNaN(v));
  const years = allPoints.map((p) => Number(p.year));
  const minY = Math.min(...values);
  const maxY = Math.max(...values);
  const minYear = Math.min(...years);
  const maxYear = Math.max(...years);

  const width = 760;
  const height = 260;
  const pad = { top: 18, right: 20, bottom: 34, left: 48 };
  const innerW = width - pad.left - pad.right;
  const innerH = height - pad.top - pad.bottom;
  const ySpan = maxY === minY ? 1 : maxY - minY;
  const xSpan = maxYear === minYear ? 1 : maxYear - minYear;

  const xScale = (year) => pad.left + ((year - minYear) / xSpan) * innerW;
  const yScale = (value) => pad.top + (1 - ((value - minY) / ySpan)) * innerH;

  const gridLines = [];
  for (let i = 0; i < 5; i += 1) {
    const y = pad.top + (i / 4) * innerH;
    gridLines.push(`<line x1="${pad.left}" y1="${y}" x2="${width - pad.right}" y2="${y}" stroke="#e5e7eb" stroke-width="1" />`);
  }

  const yearTicks = [];
  for (let year = minYear; year <= maxYear; year += 1) {
    const x = xScale(year);
    yearTicks.push(`<text x="${x}" y="${height - 8}" text-anchor="middle" font-size="12" fill="#6b7280">${year}</text>`);
  }

  const paths = seriesList.map((series) => {
    const d = series.values.map((point, index) => {
      const cmd = index === 0 ? "M" : "L";
      return `${cmd} ${xScale(Number(point.year))} ${yScale(Number(point.value))}`;
    }).join(" ");
    return `<path d="${d}" fill="none" stroke="${series.color}" stroke-width="3" stroke-linejoin="round" stroke-linecap="round" />`;
  }).join("");

  const title = config.title ? `<p class="chart-title">${config.title}</p>` : "";
  const legend = seriesList.map((series) => (
    `<span><span class="legend-dot" style="background:${series.color}"></span>${series.label}</span>`
  )).join("");

  container.innerHTML = `
    ${title}
    <svg class="chart-svg" viewBox="0 0 ${width} ${height}" aria-label="${config.title || "Chart"}" role="img">
      <rect x="0" y="0" width="${width}" height="${height}" fill="#fbfdff"></rect>
      ${gridLines.join("")}
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      ${paths}
      ${yearTicks.join("")}
      <text x="8" y="${pad.top + 6}" font-size="12" fill="#6b7280">${formatCompactNumber(maxY)}</text>
      <text x="8" y="${height - pad.bottom}" font-size="12" fill="#6b7280">${formatCompactNumber(minY)}</text>
    </svg>
    <div class="chart-legend">${legend}</div>
  `;
}
