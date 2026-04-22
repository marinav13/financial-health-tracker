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
  schoolWithCuts,
  schoolWithAccreditation,
  schoolWithResearchSource,
  unmatchedCutSchool,
  unmatchedResearchSchool,
  unmatchedAccreditationSchool
};
