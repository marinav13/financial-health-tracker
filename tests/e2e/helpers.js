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

function searchTermFor(entry) {
  return String(entry.institution_name || entry.institution_unique_name || '')
    .split(/\s+/)
    .filter((word) => /^[A-Za-z]{4,}$/.test(word))[0] || String(entry.institution_name || entry.institution_unique_name);
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

function hasMeaningfulSeries(series, field) {
  return (series[field] || []).some((point) => {
    const value = Number(point && point.value);
    return Number.isFinite(value) && value !== 0;
  });
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

function latestEnrollmentText(unitid) {
  const school = readJson(path.join('data', 'schools', `${unitid}.json`));
  const points = (school.series?.enrollment_headcount_total || [])
    .filter((point) => point && point.year != null && point.value != null)
    .map((point) => ({ year: Number(point.year), value: Number(point.value) }))
    .filter((point) => Number.isFinite(point.year) && Number.isFinite(point.value));
  if (!points.length) return '';
  const latest = points[points.length - 1];
  return `Total enrollment for ${latest.year}: ${new Intl.NumberFormat('en-US', {
    minimumFractionDigits: 0,
    maximumFractionDigits: 0
  }).format(latest.value)}`;
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

function relatedPagesForSchool(unitid) {
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
  return specs
    .map((spec) => {
      const record = indexedRelatedRecord(spec.index, unitid, spec.countField);
      if (!record) return null;
      return {
        label: spec.label,
        href: `${spec.page}?unitid=${encodeURIComponent(record.unitid || unitid)}`
      };
    })
    .filter(Boolean);
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
  const notes = normalizeAccreditationActionText(action.notes);
  const haystack = `${type} ${label} ${notes}`;
  const contentOnly = `${label} ${notes}`;

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

function namespacedDataSchool(relativePath, prefix, predicate = () => true) {
  const data = readJson(relativePath);
  const found = Object.entries(data.schools || {}).find(([unitid, school]) =>
    String(unitid).startsWith(prefix) && predicate(school)
  );
  if (!found) throw new Error(`No ${prefix} unmatched school in ${relativePath}`);
  return found[0];
}

function unmatchedCutSchool() {
  return namespacedDataSchool('data/college_cuts.json', 'cut-', (school) => Array.isArray(school.cuts) && school.cuts.length > 0);
}

function unmatchedResearchSchool() {
  return namespacedDataSchool('data/research_funding.json', 'research-', (school) => Array.isArray(school.grants) && school.grants.length > 0);
}

function unmatchedAccreditationSchool() {
  return namespacedDataSchool('data/accreditation.json', 'accred-', (school) => Array.isArray(school.actions) && school.actions.length > 0);
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
  searchTermFor,
  schoolWithCharts,
  schoolWithoutEndowment,
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
