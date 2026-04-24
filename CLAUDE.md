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