/**
 * Smoke tests for js/charts.js.
 *
 * Tests that renderLineChart produces valid SVG markup with the expected
 * structure and accessibility attributes.
 *
 * Run with: node tests/test_js_charts.js
 */

const fs = require("fs");
const path = require("path");

const CHARTS_PATH = path.resolve(__dirname, "..", "js", "charts.js");
const CHARTS_SRC = fs.readFileSync(CHARTS_PATH, "utf8");

// Extract and evaluate the pure helper functions without a DOM.
// formatChartValue and niceCeiling are fully isolated.
const pureFns = {};
const pureFnPattern = /^(function (\w+)[^{]+{[\s\S]*?\n})/gm;
let match;
while ((match = pureFnPattern.exec(CHARTS_SRC)) !== null) {
  try {
    // eslint-disable-next-line no-new-func
    const fn = new Function(`return ${match[1]}`)();
    pureFns[match[2]] = fn;
  } catch (_) {
    // Skip if the extraction produces a syntax error
  }
}

// Extract renderLineChart for DOM-mocked testing.
// We inline a minimal DOM mock rather than adding a JSDOM dependency.
function createMockDOM() {
  const elements = {};
  return {
    getElementById: (id) => {
      if (!elements[id]) {
        elements[id] = new MockElement(id);
      }
      return elements[id];
    },
    _elements: elements,
  };
}

function runChartTest(name, config, assertions) {
  try {
    const dom = createMockDOM();
    global.document = dom;

    // eslint-disable-next-line no-new-func
    const chartFn = new Function(`
      const document = global.document;
      ${CHARTS_SRC}
      return renderLineChart;
    `)();

    chartFn(config.containerId, config);

    assertions(dom, dom._elements);

    console.log(`  PASS: ${name}`);
    return true;
  } catch (err) {
    console.log(`  FAIL: ${name}: ${err.message}`);
    return false;
  }
}

// Simple element class for DOM mock.
// innerHTML setter parses the string into a proper child-node tree so that
// renderLineChart's post-render querySelector calls can traverse it.
class MockElement {
  constructor(id) {
    this.id = id;
    this.tagName = "DIV";
    this.children = [];
    this._attrs = {};
    this._html = "";
    this._style = {};
    this._listeners = {};
  }

  get innerHTML() { return this._html; }
  set innerHTML(val) {
    this._html = String(val ?? "");
    this.children = [];
    this._parseHTML(this._html);
  }
  get style() { return this._style; }

  // Minimal HTML tokenizer — extracts opening tags, attributes, and closing tags.
  _parseHTML(html) {
    const tagPattern = /<(\w+)([^>]*)>/g;
    let match;
    while ((match = tagPattern.exec(html)) !== null) {
      const tagName = match[1].toUpperCase();
      const attrStr = match[2];
      const attrs = {};
      // Skip self-closing / void tags
      if (attrStr.trim().endsWith("/") || ["br", "hr", "img", "input", "meta", "link", "area", "base", "col", "embed", "param", "source", "track", "wbr"].includes(tagName.toLowerCase())) {
        continue;
      }
      // Parse attributes
      const attrPattern = /([\w-]+)="([^"]*)"/g;
      let attrMatch;
      while ((attrMatch = attrPattern.exec(attrStr)) !== null) {
        attrs[attrMatch[1]] = attrMatch[2];
      }
      const child = new MockElement("");
      child.tagName = tagName;
      child._attrs = attrs;
      this.children.push(child);
    }
  }

  querySelector(selector) {
    const trimmed = selector.trim();
    // Tag selector: "svg", "path", "circle"
    const tagMatch = trimmed.match(/^[a-zA-Z][a-zA-Z0-9]*$/);
    if (tagMatch) {
      const tag = tagMatch[0].toUpperCase();
      for (const child of this.children) {
        if (child.tagName === tag) return child;
      }
      return null;
    }
    // Class selector: ".chart-tooltip"
    const classMatch = trimmed.match(/^\.([\w-]+)$/);
    if (classMatch) {
      const cls = classMatch[1];
      return this.children.find(c => (c._attrs.class || "").includes(cls)) || null;
    }
    return null;
  }

  querySelectorAll(selector) {
    const results = [];
    const trimmed = selector.trim();
    const tagMatch = trimmed.match(/^[a-zA-Z][a-zA-Z0-9]*$/);
    if (tagMatch) {
      const tag = tagMatch[0].toUpperCase();
      const walk = (el) => {
        if (el.tagName === tag) results.push(el);
        el.children.forEach(walk);
      };
      this.children.forEach(walk);
    }
    return results;
  }

  addEventListener(type, handler) {
    this._listeners[type] = this._listeners[type] || [];
    this._listeners[type].push(handler);
  }
  classList_contains(c) { return (this._attrs.class || "").includes(c); }
  getAttribute(name) { return this._attrs[name] ?? null; }
  setAttribute(name, val) { this._attrs[name] = val; }
  getBoundingClientRect() { return { left: 0, top: 0, width: 760, height: 260 }; }
}

let passed = 0;
let failed = 0;
const failures = [];

function assert(condition, message) {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
}

console.log("\n=== Charts.js Smoke Tests ===\n");

// -----------------------------------------------------------------------
// Pure function tests: formatChartValue
// -----------------------------------------------------------------------
console.log("formatChartValue:");

(function() {
  const fn = pureFns["formatChartValue"];
  if (!fn) { console.log("  SKIP: formatChartValue not extracted"); return; }

  const cases = [
    [["42.5", "number"], "43" ],
    [["0", "number"], "0" ],
    [["1000000", "number"], "1,000,000" ],
    [["1234.56", "currency"], "$1,235" ],
    [[null, "number"], "No data" ],
    [[undefined, "percent"], "No data" ],
    [["0", "percent"], "0%" ],
    [["50", "percent"], "50%" ],
  ];

  for (const [[value, fmt], expected] of cases) {
    const result = fn(value, fmt);
    if (result === expected) {
      console.log(`  PASS: formatChartValue(${JSON.stringify(value)}, ${JSON.stringify(fmt)}) === ${JSON.stringify(expected)}`);
      passed++;
    } else {
      console.log(`  FAIL: formatChartValue(${JSON.stringify(value)}, ${JSON.stringify(fmt)}) — got ${JSON.stringify(result)}, expected ${JSON.stringify(expected)}`);
      failures.push(`formatChartValue: ${JSON.stringify(value)} ${fmt} → got ${JSON.stringify(result)}`);
      failed++;
    }
  }
})();

// -----------------------------------------------------------------------
// Pure function tests: niceCeiling
// -----------------------------------------------------------------------
console.log("\nniceCeiling:");

(function() {
  const fn = pureFns["niceCeiling"];
  if (!fn) { console.log("  SKIP: niceCeiling not extracted"); return; }

  const cases = [
    [1, 1],
    [1.5, 2],
    [2, 2],
    [2.1, 5],
    [5, 5],
    [5.1, 10],
    [10, 10],
    [10.1, 20],
    [100, 100],
    [101, 200],
    [0, 1],
    [-5, 1],
  ];

  for (const [input, expected] of cases) {
    const result = fn(input);
    if (result === expected) {
      console.log(`  PASS: niceCeiling(${input}) === ${expected}`);
      passed++;
    } else {
      console.log(`  FAIL: niceCeiling(${input}) — got ${result}, expected ${expected}`);
      failures.push(`niceCeiling: ${input} → got ${result}`);
      failed++;
    }
  }
})();

// -----------------------------------------------------------------------
// renderLineChart: empty state
// -----------------------------------------------------------------------
console.log("\nrenderLineChart (empty state):");

(function() {
  const ok = runChartTest("renders empty-state message when series is missing", {
    containerId: "chart-empty",
    config: { containerId: "chart-empty", series: [] }
  }, (dom) => {
    const el = dom.getElementById("chart-empty");
    assert(el.innerHTML.includes("No data available"), "should contain 'No data available'");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders empty-state message when series is null", {
    containerId: "chart-null-series",
    config: { containerId: "chart-null-series", series: null }
  }, (dom) => {
    const el = dom.getElementById("chart-null-series");
    assert(el.innerHTML.includes("No data available"), "should contain 'No data available'");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// renderLineChart: basic render with data
// -----------------------------------------------------------------------
const basicConfig = {
  containerId: "chart-basic",
  title: "Revenue Over Time",
  format: "currency",
  showTooltip: false,
  showLegend: false,
  series: [
    { label: "Revenue", color: "#005b8e", values: [
      { year: "2020", value: "1000000" },
      { year: "2021", value: "1200000" },
      { year: "2022", value: "950000" },
      { year: "2023", value: "1100000" },
      { year: "2024", value: "1300000" },
    ]}
  ]
};

(function() {
  const ok = runChartTest("renders an SVG element", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes("<svg"), "should contain <svg>");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("SVG has role='img' and aria-label", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes('role="img"'), "should have role='img'");
    assert(el.innerHTML.includes('aria-label="Revenue Over Time"'), "should have aria-label from title");
    assert(el.innerHTML.includes('aria-describedby="chart-basic-desc"'), "should reference hidden description");
    assert(!/id="chart-basic-desc"[^>]*aria-hidden/.test(el.innerHTML), "description should not be aria-hidden");
    assert(/id="chart-basic-desc"[^>]*class="sr-only"/.test(el.innerHTML), "description should use the shared sr-only class");
    assert(!/id="chart-basic-desc"[^>]*style=/.test(el.innerHTML), "description should not rely on inline styles");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("SVG has viewBox", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes('viewBox="0 0 760 260"'), "should have correct viewBox");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders a chart title", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes("Revenue Over Time"), "should contain title text");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders a path element", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes("<path"), "should contain <path>");
    assert(el.innerHTML.includes('stroke="#005b8e"'), "path should have correct stroke color");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders circle point elements", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes("<circle"), "should contain <circle> elements");
    assert(el.innerHTML.includes('fill="#005b8e"'), "circles should have correct fill");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders year labels on x-axis", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes("2020") && el.innerHTML.includes("2024"), "should contain year labels 2020–2024");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders grid lines", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes('stroke="#e5e7eb"'), "should contain grid lines with correct color");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("renders axis baseline", basicConfig, (dom) => {
    const el = dom.getElementById("chart-basic");
    assert(el.innerHTML.includes('stroke="#94a3b8"'), "should contain axis lines with correct color");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("supports negative values without clipping points", {
    containerId: "chart-negative",
    title: "State Funding",
    format: "currency",
    showTooltip: false,
    series: [
      { label: "State funding", color: "#005b8e", values: [
        { year: "2022", value: "-500000" },
        { year: "2023", value: "250000" },
        { year: "2024", value: "-100000" }
      ]}
    ]
  }, (dom) => {
    const el = dom.getElementById("chart-negative");
    const pointMatches = el.innerHTML.match(/<circle cx="[^"]+" cy="[^"]+" r="3\.5" fill="/g) || [];
    assert(pointMatches.length === 3, `should render 3 data points, got ${pointMatches.length}`);
    const circles = [...el.querySelectorAll("circle")].filter((circle) => circle.getAttribute("r") === "3.5");
    circles.forEach((circle) => {
      const cy = Number(circle.getAttribute("cy"));
      assert(cy >= 18 && cy <= 226, `point y coordinate should stay inside plot area, got ${cy}`);
    });
    assert(el.innerHTML.includes("-$500,000"), "hidden chart description should include negative values");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// renderLineChart: legend
// -----------------------------------------------------------------------
(function() {
  const ok = runChartTest("renders legend when showLegend=true", {
    containerId: "chart-legend",
    showLegend: true,
    series: [
      { label: "Revenue", color: "#005b8e", values: [{ year: "2020", value: "100" }] },
      { label: "Expenses", color: "#d94a4a", values: [{ year: "2020", value: "90" }] }
    ]
  }, (dom) => {
    const el = dom.getElementById("chart-legend");
    assert(el.innerHTML.includes("Revenue"), "legend should contain 'Revenue'");
    assert(el.innerHTML.includes("Expenses"), "legend should contain 'Expenses'");
    assert(el.innerHTML.includes('class="chart-legend"'), "should have chart-legend class");
    assert(el.innerHTML.includes('<svg class="legend-dot"'), "legend should render SVG color markers");
    assert(!el.innerHTML.includes('style="background:'), "legend should not rely on inline background styles");
  });
  if (ok) passed++; else failed++;
})();

(function() {
  const ok = runChartTest("skips legend when showLegend=false", {
    containerId: "chart-no-legend",
    showLegend: false,
    series: [{ label: "Revenue", color: "#005b8e", values: [{ year: "2020", value: "100" }] }]
  }, (dom) => {
    const el = dom.getElementById("chart-no-legend");
    assert(!el.innerHTML.includes('class="chart-legend"'), "should not contain legend");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// renderLineChart: multiple series
// -----------------------------------------------------------------------
(function() {
  const ok = runChartTest("renders multiple series as separate paths", {
    containerId: "chart-multi",
    showLegend: false,
    series: [
      { label: "Revenue", color: "#005b8e", values: [{ year: "2020", value: "100" }, { year: "2021", value: "120" }] },
      { label: "Expenses", color: "#d94a4a", values: [{ year: "2020", value: "90" }, { year: "2021", value: "95" }] }
    ]
  }, (dom) => {
    const el = dom.getElementById("chart-multi");
    const pathMatches = el.innerHTML.match(/<path/g);
    assert(pathMatches && pathMatches.length === 2, `should contain 2 paths, got ${pathMatches ? pathMatches.length : 0}`);
    assert(el.innerHTML.includes("#005b8e") && el.innerHTML.includes("#d94a4a"), "should contain both series colors");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// renderLineChart: escaping and tooltip safety
// -----------------------------------------------------------------------
(function() {
  const ok = runChartTest("escapes chart title, series labels, and tooltip rows", {
    containerId: "chart-escape",
    title: '<img src=x onerror="alert(1)">Revenue',
    showLegend: true,
    showTooltip: true,
    series: [
      {
        label: '<svg onload="alert(1)">Revenue</svg>',
        color: 'red" onload="alert(1)',
        values: [{ year: "2024", value: "100" }]
      }
    ],
    tooltipRows: () => ['<img src=x onerror="alert(1)">Unsafe & row']
  }, (dom) => {
    const el = dom.getElementById("chart-escape");
    assert(!el.innerHTML.includes("<img"), "chart markup should not contain raw img tags");
    assert(!el.innerHTML.includes("<svg onload"), "chart markup should not contain raw event-handler SVG");
    assert(!el.innerHTML.includes('onerror="'), "chart markup should not retain raw event handler attributes from malicious title");
    assert(!el.innerHTML.includes('onload="'), "chart markup should not retain raw event handler attributes from labels/colors");
    assert(el.innerHTML.includes("&lt;img"), "title should be escaped as text");
    assert(el.innerHTML.includes("&lt;svg"), "series label should be escaped as text");
    assert(el.innerHTML.includes('stroke="#005ab5"'), "unsafe color should fall back to the default palette");

    const svg = el.querySelector("svg");
    const tooltip = el.querySelector(".chart-tooltip");
    assert(svg && tooltip, "tooltip-enabled chart should render SVG and tooltip elements");
    svg._listeners.mousemove[0]({ clientX: 0 });
    assert(!tooltip.innerHTML.includes("<img"), "tooltip should not contain raw custom-row tags");
    assert(!tooltip.innerHTML.includes("onerror"), "tooltip should strip malicious tag attributes from custom rows");
    assert(tooltip.innerHTML.includes("Unsafe &amp; row"), "tooltip row text should be escaped");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// renderLineChart: container not found
// -----------------------------------------------------------------------
(function() {
  const ok = runChartTest("returns early when container not found", {
    containerId: "nonexistent-chart",
    series: [{ label: "Test", color: "#000", values: [{ year: "2020", value: "1" }] }]
  }, (dom) => {
    // No assertion needed — should simply not throw
    assert(true, "should return without error");
  });
  if (ok) passed++; else failed++;
})();

// -----------------------------------------------------------------------
// Summary
// -----------------------------------------------------------------------
console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
if (failed > 0) {
  console.log("\nFailures:");
  for (const f of failures) console.log(`  - ${f}`);
  process.exit(1);
}
