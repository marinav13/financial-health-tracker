#!/usr/bin/env Rscript
# Runs lintr over scripts/ and tests/ and fails the build if any lint
# findings surface. Configuration lives in `.lintr` at the repo root so
# the rules are the same locally and in CI.
#
# Invoked from CI via: Rscript tests/run_lintr.R
#
# Intent: catch real bugs (undefined symbols, unreachable code, malformed
# `seq_len` / `seq_along` usage, etc.) without forcing the repo through a
# style-only cleanup. Style linters can be enabled later, one at a time,
# once the codebase is brought into compliance.
#
# Enabled linters (see .lintr):
#   seq_linter                 - `seq(1, length(x))` bug that breaks on empty vectors
#   unreachable_code_linter    - code after `stop()` / `return()` / `next`
#   T_and_F_symbol_linter      - `T` / `F` used as synonyms for TRUE / FALSE
#   equals_na_linter           - literal NA compared with `==` (should use is.na())
#   duplicate_argument_linter  - duplicate argument names in function calls
#
# Intentionally NOT enabled:
#   object_usage_linter        - flags thousands of false positives in this repo.
#                                Most helpers are loaded via `source()` (not
#                                packaged), and dplyr/tidyr use non-standard
#                                evaluation for column names. Static analysis
#                                cannot tell either apart from undefined
#                                symbols, so enabling this linter drowns real
#                                bugs in noise. Revisit if/when the codebase
#                                is converted into a proper R package with
#                                NAMESPACE imports.
#
# NOTE: `.lintr` uses DCF (Debian Control File) format and does not reliably
# strip `#` comments, especially inside multi-line values. Keep the config
# file itself comment-free; document changes here.
#
# See https://lintr.r-lib.org/reference/default_linters.html for the full
# linter catalogue.

suppressPackageStartupMessages({
  if (!requireNamespace("lintr", quietly = TRUE)) {
    stop("lintr is not installed. Install with install.packages('lintr').")
  }
})

dirs <- c("scripts", "tests")
findings <- list()

for (dir in dirs) {
  if (!dir.exists(dir)) next
  dir_findings <- lintr::lint_dir(dir)
  findings[[dir]] <- dir_findings
}

all_findings <- do.call(c, findings)

if (length(all_findings) == 0) {
  cat(sprintf(
    "run_lintr: OK (0 findings across %s)\n",
    paste(dirs, collapse = ", ")
  ))
  quit(save = "no", status = 0)
}

# Print each finding with file:line:col and the message for easy CI triage.
cat(sprintf(
  "run_lintr: %d finding(s) across %s:\n",
  length(all_findings),
  paste(dirs, collapse = ", ")
))
for (finding in all_findings) {
  cat(sprintf(
    "  %s:%s:%s [%s] %s\n",
    finding$filename,
    finding$line_number,
    finding$column_number %||% 1,
    finding$linter,
    finding$message
  ))
}
quit(save = "no", status = 1)
