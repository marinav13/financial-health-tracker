# CLAUDE.md

## Purpose
This is a static site for exploring higher-ed finance and enrollment trends plus related accreditation, cuts, and research datasets.

Pages include:
- `index.html`
- `school.html`
- `accreditation.html`
- `cuts.html`
- `research.html`

When you click on an individual institution, when you navigate to cuts, it should bring you to the main cuts page. Not a cuts page for the institution. Instead there are links at the bottom of the page that appear if they do exist. We don't need blank cut URLS for institutions without cuts. 

JavaScript is framework-free and shared logic should be centralized where practical.

---

## Working style
Write professional, production-oriented code.

Prefer:
- clear, direct, non-repetitive implementations
- small focused functions
- single-source-of-truth helpers for shared behavior
- minimal, localized edits
- explicit naming over clever naming
- behavior-preserving refactors

Avoid:
- patching symptoms when the root cause is identifiable
- duplicating similar logic across page-specific files
- inconsistent patterns between `app.js`, `school.js`, `accreditation.js`, `cuts.js`, and `research.js`
- broad rewrites unless explicitly requested
- cosmetic churn

---

## Editing rules
Before changing code:
1. Find the existing pattern used elsewhere in the repo.
2. Check whether the logic already exists in a shared helper.
3. Reuse or extend shared helpers instead of copying logic.
4. Keep changes scoped to the files directly required.

If a change affects multiple pages, look for the correct common abstraction first.
Do not create near-duplicate helpers with slightly different behavior unless there is a documented reason.

---

## Correctness rules
- Fix code, not tests.
- Do not weaken tests to accommodate wrong behavior.
- Do not silently change product behavior unless requested.
- Keep accessibility state synchronized with visual state.
- Handle missing or malformed external data gracefully.
- Fail clearly rather than silently when core assumptions break.

---

## Security rules
- Do not use `innerHTML` for JSON-backed or external data unless content is safely sanitized and there is no safer alternative.
- Escape dynamic text at the helper boundary.
- Validate URLs before rendering links.
- Do not assume callers will remember to sanitize input.
- Treat scraper output, CSV fields, and remote data as untrusted.

---

## Accessibility rules
- Keep `aria-*` attributes synchronized with actual UI state.
- Preserve keyboard interaction when modifying controls.
- Preserve visible labels or equivalent accessible naming.
- Do not add accessibility attributes cosmetically without wiring state correctly.

---

## Data and scraper rules
Some external sources are inherently fragile. Do not treat fragility alone as a bug.

But do:
- guard against shape changes
- surface load failures to users when appropriate
- avoid silently publishing obviously partial or broken outputs
- make failure handling explicit

---

## Accreditation action pipeline (3 layers)
MSCHE board-action rows go through three independent stages between scrape
and render. A row's final fate depends on all three. When debugging a
missing or mislabeled action, always inspect each layer in order.

1. **classify** — `classify_action()` in `scripts/shared/accreditation_helpers.R`
   assigns `action_type` (e.g. `warning`, `probation`, `show_cause`,
   `adverse_action`, `monitoring`, `other`) by keyword match against the
   raw board-action text.

2. **summarize** — `derive_action_label_short()` in
   `scripts/shared/export_helpers.R` produces the short table label.
   For MSCHE rows, nine ordered named patterns (Pattern 0a, 0a-2, 0b,
   1, 2, 3, 4, 5, 6) try to extract a structured summary; on miss,
   the fallback strips boilerplate preambles ("Staff acted on behalf
   of the Commission ", "To acknowledge receipt of ...") and returns
   the first remaining sentence with a length cap.

3. **drop** — `MSCHE_PROCEDURAL_DROP_PATTERNS` in `js/accreditation.js`
   filters routine-procedural rows (supplemental info report requests,
   follow-up visit notes, candidate-assessment paperwork, COVID-era
   waivers) from the global table. Drops are anchored at start of
   string and matched against `action_label_short` first, then
   `action_label` / `action_label_raw` as a safety net.

When a known action does not surface on the page, check in this order:
- Was it scraped at all? (check `data_pipelines/accreditation/*.csv`)
- What `action_type` did `classify_action` assign?
- What did `derive_action_label_short` emit?
- Did a procedural drop pattern match the emitted label?

If the drop layer matches but the underlying action is substantive
(e.g. `show_cause` types preamble-strip into something that looks
procedural), the right fix is a new pattern in stage 2 that produces
a summary which does not match any drop pattern -- not loosening the
drop array. `TRUSTED_ACTION_TYPES` in `js/accreditation.js` already
exempts adverse types from the keyword-substring filter at the bottom
of `isTrackedAction`, but does NOT exempt them from
`MSCHE_PROCEDURAL_DROP_PATTERNS`. Hold the override-CSV line for
unmatched outliers; do not grow the regex layer indefinitely.

---

## Refactoring rules
Refactor only when it reduces duplication, drift, or risk.

Good refactors:
- extracting repeated logic into a shared helper
- removing stale code paths
- consolidating duplicate rendering logic
- standardizing inconsistent data handling

Bad refactors:
- renaming or moving code without a real payoff
- changing architecture during a bug fix
- touching unrelated files

---

## Testing expectations
For non-trivial changes:
- run the narrowest relevant test(s) first
- then run broader validation if the change affects shared behavior

Favor tests that verify behavior, state synchronization, rendering safety, and failure handling.
Do not write brittle tests that only mirror implementation details.

R lintr is upstream of R smoke; investigate both when either fails.
A red lintr step blocks the smoke step from ever running on CI, which
can mask latent test failures locally and on `main` for weeks.

---

## Output expectations
When making changes:
- explain the root cause briefly
- explain why the chosen fix is minimal and correct
- mention any duplicated logic or drift discovered
- call out uncertainty clearly instead of guessing

---

## Repo-specific priorities
Prioritize:
1. correctness
2. maintainability
3. shared-pattern consistency
4. accessibility
5. safe handling of external data

Do not recommend adding methodology text or enabling closure display unless explicitly asked.
The public Supabase anonymous key is intentional and should not be flagged by itself as a secret exposure.