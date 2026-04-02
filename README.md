# Financial_Health_Project

This project has the data and scripts for the college financial health tracker.

If you are new to the project, start here:

- read [WALKTHROUGH_GUIDE.txt](C:\Users\mv3031\Desktop\Financial_Health_Project\WALKTHROUGH_GUIDE.txt)
- if you want to build a custom website version, use the new section called `Custom Interactive Build Guide`

Quick summary:

- `scripts/build_ipeds_tracker_dataset.R` builds the raw IPEDS file
- `scripts/build_ipeds_canonical_dataset.R` builds the canonical IPEDS dataset used by the site and workbook
- `scripts/build_article_workbook.R` builds the workbook XML
- the next web step is to create `scripts/build_web_exports.R`

Plain-English next build order for a custom interactive:

1. Create `build_web_exports.R`
2. Generate `schools_index.json`
3. Generate one test `schools/{unitid}.json`
4. Build a barebones `index.html`
5. Build a barebones `school.html`
6. Test it locally with a simple server

## Website Manager Setup Note

This section is for the person who will help move the tracker onto the Hechinger website.

### What is already in the GitHub repo

Yes. The GitHub repo already contains the core files needed for the static interactive:

- `index.html`
- `school.html`
- `styles.css`
- `js/`
- `data/`
- `.github/workflows/refresh-ipeds-site-data.yml`

That means the repo already has:

- the landing page
- the school detail page
- the CSS styling
- the JavaScript logic
- the exported JSON and CSV data files
- the GitHub Actions workflow that refreshes the data

What it does not include is any organization-specific CMS wrapper, WordPress template, or site navigation code. If your website manager needs the interactive to live inside an existing template, they may need to wrap these files inside the site structure or place them in the correct interactive folder.

### Suggested website folder structure

Recommended structure:

- `/interactives/collegefinancialhealth/index.html`
- `/interactives/collegefinancialhealth/school.html`
- `/interactives/collegefinancialhealth/styles.css`
- `/interactives/collegefinancialhealth/js/`
- `/interactives/collegefinancialhealth/data/`

The `school.html` page expects a URL pattern like:

- `/interactives/collegefinancialhealth/school.html?unitid=172264`

### Website deployment options

Option 1. Preferred

Create a new folder at:

- `/interactives/collegefinancialhealth/`

This is the cleanest option because it:

- keeps the new interactive separate from the old fitness tracker
- makes staging and QA easier
- gives the project a stable permanent URL

Option 2. Redirect the old fitness tracker later

If desired, the current fitness tracker path can later redirect to:

- `/interactives/collegefinancialhealth/`

Option 3. Direct replacement of the old fitness folder

This is possible, but it is the riskiest option and should only happen after staging is approved.

### What the website manager needs to do

1. Pull or download the GitHub repo.
2. Copy the static interactive files into the website interactive folder.
3. Confirm the final public URL structure.
4. Test the landing page and a few school pages on staging.
5. Test desktop and mobile.
6. Test the `Download This College's Data` button.
7. Publish the interactive.

### What should be tested before launch

Please test:

- the landing page loads
- the search box works
- a few school pages open correctly
- the charts load and show hover values
- the CSV download works
- the layout looks good on mobile
- the methodology panel opens

Recommended school tests:

- a private nonprofit school
- a public flagship
- a school with federal grants data
- a school with no state appropriations data

### How data updates work

Yes. The data can still be refreshed three times a year after the interactive is moved to the website.

The GitHub repo includes a workflow at:

- `.github/workflows/refresh-ipeds-site-data.yml`

That workflow:

1. rebuilds the raw IPEDS data in `ipeds/`
2. rebuilds the canonical IPEDS dataset in `ipeds/`
3. rebuilds the scorecard and graduation joins in `scorecard/`
4. rebuilds the static site exports in `data/`
5. commits the updated files back to the repo

After launch, there are three practical ways to use those updates:

- Manual publish from this repo
- Website pulls directly from this repo
- Separate website repo with periodic file copy from this repo

If the website deploy process pulls directly from this repo, it should pull only the finished static interactive files:

- `index.html`
- `school.html`
- `styles.css`
- `js/`
- `data/`

Those files can then be published to:

- `/interactives/collegefinancialhealth/`

This is often the cleanest long-term setup because this repo stays the source of truth for the interactive while the website simply publishes the finished static files.

### Simple handoff summary

If you are the website manager, the short version is:

- the repo already has the HTML, CSS, JavaScript, and exported data files
- place those files in `/interactives/collegefinancialhealth/`
- test the pages on staging
- publish
- use the GitHub Actions workflow for future data refreshes
