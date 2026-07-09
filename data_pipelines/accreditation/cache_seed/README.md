# Accreditation cache seeds

`nwccu_directory.html` is a browser-rendered snapshot of
https://nwccu.org/institutional-directory/ (rendered 2026-07-01). The live
page is JavaScript-rendered, so a plain HTTP fetch returns an empty shell
that fails `validate_nwccu_directory_html()`; the scraper then falls back
to the cached copy in `data_pipelines/accreditation/cache/` (gitignored,
persisted between CI runs via actions/cache). Both refresh workflows copy
this seed into the cache directory when the cached copy is missing, so a
cold cache (new repo, cache eviction) cannot hard-fail the refresh.

To refresh the snapshot: open the directory page in a real browser (or
chromote), let it render, save the full rendered HTML over this file, and
make sure it still ends with a newline.
