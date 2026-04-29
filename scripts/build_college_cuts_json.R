# scripts/build_college_cuts_json.R
#
# DEPRECATED: Do not use this script to rebuild data/college_cuts.json.
# The canonical site export is owned by scripts/build_web_exports.R, which
# writes the school-grouped schema consumed by js/cuts.js. This legacy helper
# produced a different { generated_at, recent } shape and could silently
# overwrite the production file with incompatible JSON.

main <- function() {
  stop(
    paste(
      "scripts/build_college_cuts_json.R is deprecated and intentionally blocked.",
      "Use scripts/build_web_exports.R to rebuild data/college_cuts.json with the production school-grouped schema.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

if (sys.nframe() == 0) {
  main()
}
