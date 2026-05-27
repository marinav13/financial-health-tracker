# Archived Accreditation Review Dispatch Apps Script

This folder is a historical reference only.

The sheet-driven Apps Script automation that used to fire GitHub
`repository_dispatch` events has been retired. The supported production path is
now:

1. Editors review rows in the Google Sheet.
2. A maintainer manually runs `Publish Editorial Overrides` in GitHub Actions.

Important current-state notes:

- [`C:\Users\mv3031\Desktop\Financial_Health_Project\Financial Health Tracker - For Github\.github\workflows\publish-editorial-overrides.yml`](C:\Users\mv3031\Desktop\Financial_Health_Project\Financial Health Tracker - For Github\.github\workflows\publish-editorial-overrides.yml)
  no longer listens for `repository_dispatch`.
- Nothing in this folder is expected to be deployed or configured in Google.
- Any references to script properties, installable triggers, or GitHub dispatch
  tokens below are obsolete for the active workflow.

The source remains in-repo only so the retired automation can be audited or
reconstructed later if policy changes.
