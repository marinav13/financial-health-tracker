const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..', '..');

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(ROOT, relativePath), 'utf8'));
}

function firstSchoolIndexEntry() {
  const schools = readJson('data/schools_index.json');
  const entry = schools.find((school) => school.unitid && (school.institution_unique_name || school.institution_name));
  if (!entry) throw new Error('No school index entry available for e2e tests');
  return entry;
}

function schoolIndexEntryByUnitid(unitid) {
  const schools = readJson('data/schools_index.json');
  const entry = schools.find((school) => String(school.unitid || '') === String(unitid || ''));
  if (!entry) throw new Error(`No school index entry found for unitid ${unitid}`);
  return entry;
}

function searchTermFor(entry) {
  return String(entry.institution_name || entry.institution_unique_name || '')
    .split(/\s+/)
    .filter((word) => /^[A-Za-z]{4,}$/.test(word))[0] || String(entry.institution_name || entry.institution_unique_name);
}

function stateWithMultipleSchools(minCount = 12) {
  const schools = readJson('data/schools_index.json');
  const counts = schools.reduce((map, school) => {
    const state = String(school.state || '').trim();
    if (!state) return map;
    map.set(state, (map.get(state) || 0) + 1);
    return map;
  }, new Map());
  const candidates = Array.from(counts.entries())
    .filter(([, count]) => count >= minCount)
    .sort((a, b) => a[0].localeCompare(b[0]));
  if (!candidates.length) {
    throw new Error(`No state with at least ${minCount} schools available for e2e tests`);
  }
  const [state, count] = candidates[0];
  return { state, count };
}

function schoolWithCharts() {
  const schoolsDir = path.join(ROOT, 'data', 'schools');
  const files = fs.readdirSync(schoolsDir).filter((file) => file.endsWith('.json')).sort();
  for (const file of files) {
    const school = JSON.parse(fs.readFileSync(path.join(schoolsDir, file), 'utf8'));
    const series = school.series || {};
    if (
      (series.revenue_total_adjusted || []).length > 1 &&
      (series.net_tuition_per_fte_adjusted || []).length > 1 &&
      (series.enrollment_headcount_total || []).length > 1
    ) {
      return school.unitid || path.basename(file, '.json');
    }
  }
  throw new Error('No school with chart series available for e2e tests');
}

function schoolWithoutEndowment() {
  const schoolsDir = path.join(ROOT, 'data', 'schools');
  const files = fs.readdirSync(schoolsDir).filter((file) => file.endsWith('.json')).sort();
  for (const file of files) {
    const school = JSON.parse(fs.readFileSync(path.join(schoolsDir, file), 'utf8'));
    if (!hasMeaningfulSeries(school.series || {}, 'endowment_value_adjusted')) {
      return school.unitid || path.basename(file, '.json');
    }
  }
  throw new Error('No school without endowment series available for e2e tests');
}

function roundDisplayPercent(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) return null;
  return Math.abs(numeric) < 1 ? Math.round(numeric * 10) / 10 : Math.round(numeric);
}

function schoolWithTuitionDependenceAtDisplayedMedian() {
  const schoolsDir = path.join(ROOT, 'data', 'schools');
  const files = fs.readdirSync(schoolsDir).filter((file) => file.endsWith('.json')).sort();
  for (const file of files) {
    const school = JSON.parse(fs.readFileSync(path.join(schoolsDir, file), 'utf8'));
    const summary = school.summary || {};
    const tuition = roundDisplayPercent(summary.tuition_dependence_pct);
    const median = roundDisplayPercent(summary.sector_median_tuition_dependence_pct);
    const controlLabel = String(school.profile?.control_label || '').trim();
    if (tuition === null || median === null || !controlLabel) continue;
    if (tuition === median) {
      return school.unitid || path.basename(file, '.json');
    }
  }
  throw new Error('No school with displayed tuition dependence equal to displayed sector median available for e2e tests');
}

function latestEnrollmentText(unitid) {
  const school = readJson(path.join('data', 'schools', `${unitid}.json`));
  const points = (school.series?.enrollment_headcount_total || [])
    .filter((point) => point && point.year != null && point.value != null)
    .map((point) => ({ year: Number(point.year), value: Number(point.value) }))
    .filter((point) => Number.isFinite(point.year) && Number.isFinite(point.value));
  if (!points.length) return '';
  const latest = points[points.length - 1];
  return `As of ${latest.year}, this institution reported a headcount of ${new Intl.NumberFormat('en-US', {
    minimumFractionDigits: 0,
    maximumFractionDigits: 0
  }).format(latest.value)} students.`;
}

function schoolWithClosureStatus() {
  const closure = readJson('data/closure_status_by_unitid.json');
  const schoolsDir = path.join(ROOT, 'data', 'schools');
  const found = Object.keys(closure.schools || {}).find((unitid) =>
    fs.existsSync(path.join(schoolsDir, `${unitid}.json`))
  );
  if (!found) throw new Error('No closure-status school with school detail JSON available for e2e tests');
  return found;
}

function indexedRelatedRecord(index, unitid, countField) {
  const direct = index[String(unitid)];
  const hasRecord = (record) => {
    if (!record) return false;
    const count = Number(record[countField]);
    return Number.isFinite(count) ? count > 0 : true;
  };
  if (hasRecord(direct)) return direct;
  return Object.values(index).find((record) =>
    String(record.financial_unitid || '') === String(unitid) && hasRecord(record)
  );
}

function relatedPagesForSchool(unitid, options = {}) {
  const current = String(options.current || 'finances');
  const specs = [
    {
      label: 'College Cuts',
      page: 'cuts.html',
      index: readJson('data/college_cuts_index.json'),
      countField: 'cut_count'
    },
    {
      label: 'Accreditation',
      page: 'accreditation.html',
      index: readJson('data/accreditation_index.json'),
      countField: 'action_count'
    },
    {
      label: 'Research Funding Cuts',
      page: 'research.html',
      index: readJson('data/research_funding_index.json'),
      countField: 'total_disrupted_grants'
    }
  ];
  const links = specs
    .map((spec) => {
      const record = indexedRelatedRecord(spec.index, unitid, spec.countField);
      if (!record) return null;
      return {
        label: spec.label,
        href: `${spec.page}?unitid=${encodeURIComponent(record.unitid || unitid)}`,
        // Carry through the financial_unitid so the Finances spec
        // below can be derived from whichever cuts/accred/research
        // record matched the lookup, mirroring the JS path:
        // app.js renderRelatedInstitutionLinks pushes a Finances
        // link iff school.financial_unitid is numeric.
        financialUnitid: String(record.financial_unitid || '')
      };
    })
    .filter(Boolean);

  // Finances link parity with app.js renderRelatedInstitutionLinks:
  // the JS pushes a 'Finances' entry whenever the calling page has a
  // numeric financial_unitid AND the current page isn't itself
  // school.html. The test helper synthesizes the same expectation by
  // walking the cuts/accred/research records we just collected, taking
  // the first numeric financial_unitid we see, and emitting the link
  // pointed at school.html?unitid=<financial_unitid>.
  //
  // We additionally require the schools_index.json catalog to
  // recognize that financial_unitid before we add the link — the JS
  // doesn't gate on this, but our test fixtures only include schools
  // listed in schools_index.json, and emitting a phantom Finances link
  // for an unknown school would break every assertion downstream.
  const schoolsIndex = readJson('data/schools_index.json');
  const knownSchoolUnitids = new Set(
    (Array.isArray(schoolsIndex) ? schoolsIndex : [])
      .map((entry) => String(entry?.unitid || ''))
      .filter(Boolean)
  );
  const financeUnitid = links
    .map((link) => link.financialUnitid)
    .find((value) => /^[0-9]+$/.test(value));

  // Match the order app.js renderRelatedInstitutionLinks emits:
  // Finances first, then Cuts, Accreditation, Research. The test
  // asserts order via links.nth(index).toHaveText(...), so the
  // helper has to mirror that sequence exactly.
  if (current !== 'finances' && financeUnitid && knownSchoolUnitids.has(financeUnitid)) {
    links.unshift({
      label: 'Finances',
      href: `school.html?unitid=${encodeURIComponent(financeUnitid)}`
    });
  }

  // Don't leak the helper's internal bookkeeping field to callers —
  // assertions test {label, href} only.
  return links.map(({ label, href }) => ({ label, href }));
}

function schoolWithRelatedPages() {
  const schools = readJson('data/schools_index.json');
  const found = schools.find((school) =>
    school.unitid &&
    fs.existsSync(path.join(ROOT, 'data', 'schools', `${school.unitid}.json`)) &&
    relatedPagesForSchool(school.unitid).length > 0
  );
  if (!found) throw new Error('No school with related side pages available for e2e tests');
  return found.unitid;
}

function schoolWithoutRelatedPages() {
  const schools = readJson('data/schools_index.json');
  const found = schools.find((school) =>
    school.unitid &&
    fs.existsSync(path.join(ROOT, 'data', 'schools', `${school.unitid}.json`)) &&
    relatedPagesForSchool(school.unitid).length === 0
  );
  if (!found) throw new Error('No school without related side pages available for e2e tests');
  return found.unitid;
}

function firstDataSchool(relativePath, predicate) {
  const data = readJson(relativePath);
  const entries = Object.entries(data.schools || {});
  const found = entries.find(([, school]) => predicate(school));
  if (!found) throw new Error(`No matching school in ${relativePath}`);
  return found[0];
}

function schoolWithCuts() {
  return firstDataSchool('data/college_cuts.json', (school) => Array.isArray(school.cuts) && school.cuts.length > 0);
}

function parseAccreditationActionDate(action) {
  const raw = String(action.action_date || '').trim();
  if (!raw) return null;
  if (/^\d{4}-\d{2}-\d{2}$/.test(raw)) {
    const parsed = new Date(`${raw}T00:00:00`);
    return Number.isNaN(parsed.getTime()) ? null : parsed;
  }
  if (/^\d{4}-\d{2}$/.test(raw)) {
    const parsed = new Date(`${raw}-01T00:00:00`);
    return Number.isNaN(parsed.getTime()) ? null : parsed;
  }
  const parsed = new Date(raw);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
}

function accreditationActionYear(action) {
  const explicitYear = Number(action.action_year || '');
  if (!Number.isNaN(explicitYear) && explicitYear > 0) return explicitYear;
  const dateText = String(action.action_date || '');
  const match = dateText.match(/\b(19|20)\d{2}\b/);
  return match ? Number(match[0]) : NaN;
}

function normalizeAccreditationActionText(text) {
  return String(text || '').toLowerCase().replace(/\s+/g, ' ').trim();
}

const MSCHE_PROCEDURAL_DROP_PATTERNS = [
  /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?supplemental information report/i,
  /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?monitoring report/i,
  /^\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?candidate assessment/i,
  /^\s*(?:staff acted on behalf of the commission )?to request an? updated teach-?out plan/i,
  /^\s*to require [^.]{0,200}?teach-?out plan/i,
  /^\s*to request [^.]{0,200}?teach-?out plan/i,
  /^\s*to note the follow-up team visit/i,
  /^\s*to note that the complex substantive change visit occurred/i,
  /^\s*to note that an? updated teach-?out plan [^.]{0,80}? will not be required/i,
  /^\s*(?:staff acted on behalf of the commission )?to temporarily waive substantive change policy/i,
  /^\s*to approve the teach-?out plan as required of candidate/i,
  /^\s*to reject the teach-?out plan/i,
  /^\s*to note that the supplemental information report was not conducive/i
];

const TRUSTED_ACCREDITATION_ACTION_TYPES = new Set([
  'adverse_action', 'warning', 'probation', 'show_cause', 'removed', 'notice'
]);

function hasAccreditationActionOccurred(action) {
  const actionDate = parseAccreditationActionDate(action);
  if (actionDate) return actionDate.getTime() <= Date.now();
  const year = accreditationActionYear(action);
  return !Number.isNaN(year) && year >= 2019 && year <= new Date().getFullYear();
}

function isVisibleAccreditationAction(action) {
  const type = normalizeAccreditationActionText(action.action_type);
  const accreditor = String(action.accreditor || '').toUpperCase();
  const label = normalizeAccreditationActionText(action.action_label || action.action_label_raw);
  const shortLabel = normalizeAccreditationActionText(action.action_label_short);
  const notes = normalizeAccreditationActionText(action.notes);
  const haystack = `${type} ${label} ${notes}`;
  const contentOnly = `${label} ${notes}`;

  if (
    shortLabel === 'voluntarily surrendered accreditation' ||
    /\bvoluntar(?:ily|y)\s+surrender(?:ed)?\s+accreditation\b/.test(label)
  ) {
    return false;
  }

  if (accreditor === 'MSCHE' && type === 'monitoring') return false;

  if (accreditor === 'MSCHE') {
    const candidateLabels = [
      action.action_label_short,
      action.action_label,
      action.action_label_raw
    ].filter((value) => typeof value === 'string' && value.length > 0);
    for (const pattern of MSCHE_PROCEDURAL_DROP_PATTERNS) {
      if (candidateLabels.some((candidate) => pattern.test(candidate))) {
        return false;
      }
    }
  }

  if (/substantive change|program addition/.test(haystack) && !TRUSTED_ACCREDITATION_ACTION_TYPES.has(type)) {
    return false;
  }

  const statusActionPattern = /warning|probation|formal notice of concern|notice of concern|\bmonitoring\b|removed from (warning|probation|formal notice of concern|notice of concern|notice|monitoring)|removed from membership|placed on probation|issue a notice of concern|continue a warning|continued on warning|continued on probation|denied reaffirmation/;
  const closureActionPattern = /accepted notification of institutional closure|accept(?:ed)? teach-?out plan|teach out plan|teach-?out plan|removed from membership/;
  const requiredReportPattern = /require (?:the institution to provide )?(?:an )?(?:interim|progress|follow-?up|monitoring) report/;
  const standaloneLowSignalPattern = /^(special visit|interim report|progress report|accepted progress report|accepted interim report|follow-?up report|monitoring report|second monitoring report|third monitoring report)$/;
  const hasSpecialVisit = /special visit/.test(haystack);
  const hasSanctionDecision =
    statusActionPattern.test(contentOnly) ||
    closureActionPattern.test(contentOnly) ||
    requiredReportPattern.test(contentOnly);

  if (hasSpecialVisit && !hasSanctionDecision) return false;
  if (statusActionPattern.test(contentOnly) || closureActionPattern.test(contentOnly) || requiredReportPattern.test(contentOnly)) {
    return true;
  }
  if (standaloneLowSignalPattern.test(label)) return false;

  return ['warning', 'probation', 'monitoring', 'notice'].includes(type) ||
    /removed from membership|teach-?out|institutional closure/.test(haystack);
}

function schoolWithVisibleAccreditation() {
  return firstDataSchool('data/accreditation.json', (school) =>
    Array.isArray(school.actions) &&
    school.actions.some((action) =>
      action &&
      action.display_action !== false &&
      !Number.isNaN(accreditationActionYear(action)) &&
      accreditationActionYear(action) >= 2019 &&
      hasAccreditationActionOccurred(action) &&
      isVisibleAccreditationAction(action)
    )
  );
}

function schoolWithResearchSource() {
  return firstDataSchool('data/research_funding.json', (school) =>
    Array.isArray(school.grants) && school.grants.some((grant) => /^https?:\/\//.test(String(grant.source_url || '')))
  );
}

function namespacedDataSchool(relativePath, prefix, predicate = () => true, fallbackUnitid = '') {
  const data = readJson(relativePath);
  const found = Object.entries(data.schools || {}).find(([unitid, school]) =>
    String(unitid).startsWith(prefix) && predicate(school)
  );
  if (!found) {
    if (fallbackUnitid) return fallbackUnitid;
    throw new Error(`No ${prefix} unmatched school in ${relativePath}`);
  }
  return found[0];
}

function unmatchedCutSchool() {
  // Use a synthetic namespaced ID when the live export has no unmatched row.
  return namespacedDataSchool('data/college_cuts.json', 'cut-', (school) => Array.isArray(school.cuts) && school.cuts.length > 0, 'cut-synthetic-unmatched-e2e');
}

function unmatchedResearchSchool() {
  return namespacedDataSchool('data/research_funding.json', 'research-', (school) => Array.isArray(school.grants) && school.grants.length > 0, 'research-synthetic-unmatched-e2e');
}

function unmatchedAccreditationSchool() {
  return namespacedDataSchool('data/accreditation.json', 'accred-', (school) => Array.isArray(school.actions) && school.actions.length > 0, 'accred-synthetic-unmatched-e2e');
}

/**
 * Asserts that every element with aria-hidden="true" is actually hidden (or
 * has zero bounding box), and that no element with aria-hidden="false" is
 * invisible. Templating regressions that flip visibility without updating
 * aria-hidden are invisible to sighted users but break screen readers.
 *
 * Callers pass their Playwright `page` and `expect`. We take `expect` as an
 * argument instead of requiring '@playwright/test' here so this module stays
 * importable from non-test contexts.
 */
async function expectAriaHiddenInSync(page, expect, label = '') {
  const mismatches = await page.evaluate(() => {
    const violations = [];
    document.querySelectorAll('[aria-hidden]').forEach((el) => {
      const claim = el.getAttribute('aria-hidden');
      const style = window.getComputedStyle(el);
      const visuallyHidden =
        style.display === 'none' ||
        style.visibility === 'hidden' ||
        el.hasAttribute('hidden');
      if (claim === 'true' && !visuallyHidden) {
        // Allow aria-hidden="true" on purely decorative visible elements
        // (icons, dividers). Heuristic: if it has no text content and no
        // interactive descendants, don't flag.
        const txt = (el.textContent || '').trim();
        const interactive = el.querySelector('a, button, input, select, textarea');
        if (txt.length > 0 || interactive) {
          violations.push(`aria-hidden="true" but visible with text: ${el.id || el.tagName}`);
        }
      }
      if (claim === 'false' && visuallyHidden) {
        violations.push(`aria-hidden="false" but hidden: ${el.id || el.tagName}`);
      }
    });
    return violations;
  });
  const header = label ? `aria-hidden sync [${label}]:\n` : '';
  expect(mismatches, `${header}${mismatches.join('\n')}`).toEqual([]);
}

module.exports = {
  firstSchoolIndexEntry,
  schoolIndexEntryByUnitid,
  searchTermFor,
  stateWithMultipleSchools,
  schoolWithCharts,
  schoolWithoutEndowment,
  schoolWithTuitionDependenceAtDisplayedMedian,
  latestEnrollmentText,
  schoolWithClosureStatus,
  schoolWithRelatedPages,
  schoolWithoutRelatedPages,
  relatedPagesForSchool,
  schoolWithCuts,
  schoolWithVisibleAccreditation,
  schoolWithResearchSource,
  unmatchedCutSchool,
  unmatchedResearchSchool,
  unmatchedAccreditationSchool,
  expectAriaHiddenInSync
};
