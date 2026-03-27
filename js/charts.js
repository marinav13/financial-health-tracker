function formatCompactNumber(value) {
  if (value === null || value === undefined || Number.isNaN(value)) return "No data";
  return new Intl.NumberFormat("en-US", {
    notation: Math.abs(value) >= 1000 ? "compact" : "standard",
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
  const minYear = Math.min(...years);
  const maxYear = Math.max(...years);
  const rawMaxY = Math.max(...values);
  const minY = 0;
  const maxY = rawMaxY <= 0 ? 1 : rawMaxY;

  const width = 760;
  const height = 260;
  const pad = { top: 18, right: 20, bottom: 34, left: 52 };
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

  const pointGroups = seriesList.map((series) => series.values.map((point) => {
    const x = xScale(Number(point.year));
    const y = yScale(Number(point.value));
    return `<circle cx="${x}" cy="${y}" r="3.5" fill="${series.color}" opacity="0.9"><title>${series.label}: ${formatCompactNumber(Number(point.value))} (${point.year})</title></circle>`;
  }).join("")).join("");

  const title = config.title ? `<p class="chart-title">${config.title}</p>` : "";
  const legend = seriesList.map((series) => (
    `<span><span class="legend-dot" style="background:${series.color}"></span>${series.label}</span>`
  )).join("");

  container.innerHTML = `
    ${title}
    <div class="chart-tooltip" aria-hidden="true"></div>
    <svg class="chart-svg" viewBox="0 0 ${width} ${height}" aria-label="${config.title || "Chart"}" role="img">
      <rect x="0" y="0" width="${width}" height="${height}" fill="#fbfdff"></rect>
      ${gridLines.join("")}
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" stroke="#94a3b8" />
      ${paths}
      ${pointGroups}
      ${yearTicks.join("")}
      <text x="12" y="${pad.top + 6}" font-size="12" fill="#6b7280">${formatCompactNumber(maxY)}</text>
      <text x="18" y="${height - pad.bottom}" font-size="12" fill="#6b7280">0</text>
    </svg>
    <div class="chart-legend">${legend}</div>
  `;

  const svg = container.querySelector("svg");
  const tooltip = container.querySelector(".chart-tooltip");
  if (!svg || !tooltip) return;

  const yearsSorted = Array.from(new Set(years)).sort((a, b) => a - b);

  const showTooltip = (evt) => {
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
      return `<span class="chart-tooltip-row">${series.label}: ${formatCompactNumber(Number(point.value))}</span>`;
    }).filter(Boolean);

    if (!rows.length) return;

    tooltip.innerHTML = `<strong>${bestYear}</strong>${rows.join("")}`;
    tooltip.style.left = `${((xScale(bestYear) / width) * 100).toFixed(2)}%`;
    tooltip.style.top = `${((pad.top + 12) / height) * 100}%`;
    tooltip.classList.add("visible");
  };

  svg.addEventListener("mousemove", showTooltip);
  svg.addEventListener("touchmove", (evt) => {
    if (evt.touches && evt.touches[0]) showTooltip(evt.touches[0]);
  }, { passive: true });
  svg.addEventListener("mouseleave", () => tooltip.classList.remove("visible"));
  svg.addEventListener("touchend", () => tooltip.classList.remove("visible"));
}
