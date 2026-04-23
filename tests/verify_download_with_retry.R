# Lightweight verification for the download_with_retry refactor.
#
# Covers the four checks that don't need the full test-support harness:
#   1. All modified R files parse cleanly (catches syntax errors).
#   2. download_with_retry is defined with the expected signature.
#   3. download_with_retry actually retries on a bad URL and uses a
#      backoff of at least ~2s between the first two attempts.
#   4. download_if_missing short-circuits when the destination already
#      exists (no network call, just returns the path).
#
# Deliberately avoids sourcing test_support.R so this file runs even on
# machines that don't have dplyr / readxl / httr2 / etc. installed.
#
# Run from the project root:
#   Rscript tests/verify_download_with_retry.R

cat("verify_download_with_retry.R\n")
cat("----------------------------\n")

# ---- 1. Parse check --------------------------------------------------------
parse_targets <- c(
  "scripts/shared/utils.R",
  "scripts/shared/ipeds_row_builders.R",
  "scripts/shared/grant_witness_helpers.R",
  "scripts/build_outcomes_join.R"
)
cat("[1/4] Parsing modified R files...\n")
for (f in parse_targets) {
  if (!file.exists(f)) {
    stop(sprintf("Expected file not found (are you in the project root?): %s", f), call. = FALSE)
  }
  parse(file = f)
  cat(sprintf("       OK  %s\n", f))
}

# ---- 2. Signature check ----------------------------------------------------
cat("[2/4] Loading utils.R and checking download_with_retry signature...\n")
# Source utils.R in a fresh env so we don't clobber the global namespace.
utils_env <- new.env(parent = baseenv())
sys.source("scripts/shared/utils.R", envir = utils_env)

if (!exists("download_with_retry", envir = utils_env, inherits = FALSE)) {
  stop("download_with_retry is not defined in scripts/shared/utils.R", call. = FALSE)
}
dwr <- get("download_with_retry", envir = utils_env)
actual_args <- names(formals(dwr))
expected_args <- c("url", "destfile", "mode", "quiet", "timeout", "retries")
if (!identical(actual_args, expected_args)) {
  stop(sprintf(
    "download_with_retry signature drifted.\n  Expected: (%s)\n  Actual:   (%s)",
    paste(expected_args, collapse = ", "),
    paste(actual_args, collapse = ", ")
  ), call. = FALSE)
}
cat(sprintf("       OK  signature: (%s)\n", paste(actual_args, collapse = ", ")))

# ---- 3. Retry + backoff behavior -------------------------------------------
cat("[3/4] Exercising retry + backoff on a guaranteed-bad URL...\n")
# 127.0.0.1:1 is the canonical 'nothing listens here' target. download.file
# will fail fast without needing internet. We ask for 3 attempts and verify
# that the elapsed time is at least ~2s (Sys.sleep(2^1) between attempts 1
# and 2). With timeout=1 and 3 attempts, the minimum plausible wall time is
# ~2s (first backoff) + ~4s (second backoff) = ~6s, but we only assert >=2s
# to tolerate slow hosts.
bad_url <- "http://127.0.0.1:1/does-not-exist.bin"
dest_bad <- tempfile(fileext = ".bin")
t_start <- Sys.time()
outcome <- tryCatch(
  dwr(bad_url, destfile = dest_bad, timeout = 1L, retries = 3L),
  error = function(e) e,
  warning = function(w) w
)
elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

if (!inherits(outcome, "error")) {
  stop("download_with_retry should have errored on an unreachable URL", call. = FALSE)
}
msg <- conditionMessage(outcome)
if (!grepl("failed after 3 attempts", msg, fixed = TRUE)) {
  stop(sprintf("Retry error message should mention '3 attempts'. Got: %s", msg), call. = FALSE)
}
if (elapsed < 2) {
  stop(sprintf(
    "Expected >=2s elapsed from backoff sleeps between attempts; got %.2fs. ",
    elapsed
  ), "download_with_retry may not be sleeping between retries.", call. = FALSE)
}
cat(sprintf("       OK  failed as expected after 3 attempts (%.1fs elapsed)\n", elapsed))
cat(sprintf("       msg: %s\n", msg))

# ---- 4. download_if_missing short-circuit ----------------------------------
cat("[4/4] Checking download_if_missing short-circuits when file exists...\n")
row_env <- new.env(parent = utils_env)
sys.source("scripts/shared/ipeds_row_builders.R", envir = row_env)
if (!exists("download_if_missing", envir = row_env, inherits = FALSE)) {
  stop("download_if_missing is not defined in scripts/shared/ipeds_row_builders.R",
       call. = FALSE)
}
dim_fn <- get("download_if_missing", envir = row_env)

tmp_dir <- tempfile("dim_")
dir.create(tmp_dir)
pre_existing <- file.path(tmp_dir, "preloaded.txt")
writeLines("sentinel", pre_existing)
# Point the url at an unreachable host — if short-circuit fails, dwr will
# error after backoff. A successful short-circuit returns immediately.
t0 <- Sys.time()
returned <- tryCatch(
  dim_fn("http://127.0.0.1:1/never-fetched.txt", pre_existing),
  error = function(e) e
)
dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
if (inherits(returned, "error")) {
  stop(sprintf("Short-circuit failed: download_if_missing tried to fetch (%s)",
               conditionMessage(returned)), call. = FALSE)
}
if (dt > 1.5) {
  stop(sprintf(
    "download_if_missing returned but took %.1fs — suspicious, it should short-circuit instantly.",
    dt
  ), call. = FALSE)
}
cat(sprintf("       OK  short-circuited in %.3fs (no network call made)\n", dt))

cat("----------------------------\n")
cat("All 4 checks passed.\n")
