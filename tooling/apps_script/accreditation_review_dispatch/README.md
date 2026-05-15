# Accreditation Review Dispatch Apps Script

This bound Google Apps Script automates same-day accreditation publishes.

It does two things:

1. An installable edit trigger watches the `accreditation_review` tab.
2. A 15-minute clock trigger sends a GitHub `repository_dispatch` event if an approved row was edited since the last successful dispatch.

The script source is mirrored here so the automation logic is reviewable and restorable even though the live trigger runs inside Google.

## What editors experience

When this automation is turned on:

1. An editor changes an approved row in `accreditation_review`.
2. The sheet quietly marks itself "dirty".
3. About every 15 minutes, Apps Script checks whether anything publish-worthy changed.
4. If yes, it tells GitHub to run `Publish Editorial Overrides`.
5. The workflow rebuilds the accreditation exports and pushes the update.

Editors do not need to open GitHub for routine copy edits once this is working.

## Script Properties

Set these in the Apps Script project under `Project Settings` -> `Script properties`:

- `GITHUB_OWNER`
  - For this repo: `marinav13`
- `GITHUB_REPO`
  - For this repo: `financial-health-tracker`
- `GITHUB_TOKEN`
  - Fine-grained personal access token with `Contents: write` on this repo
- `REVIEW_SHEET_TAB`
  - Optional
  - Default: `accreditation_review`
- `DISPATCH_EVENT_TYPE`
  - Optional
  - Default: `accreditation_review_publish`
- `DISPATCH_INTERVAL_MINUTES`
  - Optional
  - Allowed values: `1`, `5`, `10`, `15`, `30`
  - Recommended: `15`

## Trigger Setup

1. Open the bound Apps Script project from the Google Sheet.
2. Replace the default script code with `Code.gs` from this folder.
3. Update the manifest to match `appsscript.json`.
4. Set the Script Properties listed above.
5. Run `installTriggers()` once from the Apps Script editor and approve permissions.

`installTriggers()` removes any prior copies of the two automation triggers and recreates them:

- `onAccreditationReviewEdit`
- `dispatchAccreditationReviewIfDirty`

## Dirty-Flag Logic

The sheet is marked dirty only when:

- the edit happens on the `accreditation_review` tab
- and the edit touches one of these publish-relevant columns:
  - `review_status`
  - `editor_action_label_short`
  - `editor_action_date`
  - `editor_action_type`
  - `editor_source_url`
  - `editor_source_title`

For the non-status fields above, the row must currently be `approved`.
For `review_status` itself, any change marks the sheet dirty so rows can both
appear on the site and be removed from it.

Edits to `editor_notes`, `reviewer`, and `reviewed_at` do not trigger a publish on their own.

## Debugging

Run `getAccreditationReviewDispatchStatus()` in the Apps Script editor to inspect:

- whether the sheet is currently marked dirty
- when it was last marked dirty
- the last dispatch timestamp
- the last dispatch HTTP status
- the last dispatch response body snippet

If dispatches stop unexpectedly, verify:

- the `Publish Editorial Overrides` workflow exists on the repo default branch
- the GitHub token is still valid
- the sheet tab is still named `accreditation_review`
- the installable triggers still exist in the Apps Script project
