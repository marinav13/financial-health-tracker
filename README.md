# Financial_Health_Project

## GitHub Actions refresh

This repo includes a GitHub Actions workflow at:

- `.github/workflows/refresh-ipeds-site-data.yml`

It does four things:

1. rebuilds the raw IPEDS data
2. rebuilds the reporting and Looker-ready datasets
3. rebuilds the static website exports in `data/`
4. commits and pushes changed data files back to `main`

It can run:

- manually from the GitHub `Actions` tab
- automatically three times a year on March 1, July 1, and November 1

The workflow updates:

- `raw_build/`
- `looker_studio/`
- `reporting/`
- `data/`
