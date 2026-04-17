# Website Manager Note

This repo already contains everything needed to publish the interactive — HTML, CSS, JavaScript, and data files.

## Files to deploy

Copy these into your website's interactive folder (e.g., `/interactives/fitness/`):

- `index.html`
- `school.html`
- `styles.css`
- `js/`
- `data/`

The school page expects URLs like `/interactives/fitness/school.html?unitid=172264`.

## Recommended launch steps

1. Pull or download the repo.
2. Copy the static files into the website folder.
3. Confirm the final public URL structure.
4. Test on staging before publishing.

## Pre-launch checklist

- [ ] Landing page loads
- [ ] Search works
- [ ] Several school pages open correctly
- [ ] Charts load and show hover values
- [ ] CSV download works
- [ ] Layout looks good on mobile
- [ ] Methodology panel opens

**Test schools:** one private nonprofit, one public flagship, one with federal grants data, one with no state appropriations data.

## Deployment options

### Option 1 (recommended): New folder

Create a fresh folder:

```
/interactives/collegefinancialhealth/
```

This keeps the interactive separate, makes staging easier, and gives a stable permanent URL.

### Option 2: Redirect from old path

Build in the new folder first, then redirect the old fitness tracker path to the new location later. Safer than immediate replacement.

### Option 3: Direct replacement

Replace the old fitness tracker files directly. Riskier — only do this after staging approval.

## Data refresh after launch

Yes — the data refreshes three times a year automatically.

GitHub Actions runs `.github/workflows/refresh-ipeds-site-data.yml` every Monday at noon. It rebuilds the data files and commits them back to the repo.

After each refresh, copy the updated `data/`, `index.html`, `school.html`, `styles.css`, and `js/` into your website folder.

## How the automated refresh works

The workflow:

1. Rebuilds accreditor scrapes
2. Rebuilds college cuts from Supabase
3. Rebuilds grant witness joins
4. Imports closure data from the Google Sheet
5. Rebuilds the site JSON files
6. Commits updated files back to the repo

Your deploy system then picks up the new files from the repo.

## What the refresh does NOT do

- It does not republish the site — your deploy system handles that
- It does not need Google Cloud setup
- It does not scrape federal closure data (that comes from the Google Sheet instead)

The site deploy only ever needs these files from the repo:

```
index.html, school.html, styles.css, js/, data/
```
