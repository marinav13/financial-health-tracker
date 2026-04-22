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

function schoolWithAccreditation() {
  return firstDataSchool('data/accreditation.json', (school) => Array.isArray(school.actions) && school.actions.length > 0);
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
  schoolWithAccreditation,
  schoolWithResearchSource,
  unmatchedCutSchool,
  unmatchedResearchSchool,
  unmatchedAccreditationSchool
};
