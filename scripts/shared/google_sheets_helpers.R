extract_google_sheet_id <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) return(NA_character_)

  if (!grepl("^https?://", text, ignore.case = TRUE, perl = TRUE)) {
    return(text)
  }

  match <- regexec("/spreadsheets/d/([a-zA-Z0-9-_]+)", text, perl = TRUE)
  captures <- regmatches(text, match)[[1]]
  if (length(captures) >= 2L && nzchar(captures[[2L]])) {
    return(captures[[2L]])
  }

  stop(
    sprintf("Could not extract a Google Sheet ID from: %s", text),
    call. = FALSE
  )
}

resolve_google_auth_json <- function(auth_json = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_)) {
  if (is.null(auth_json) || (length(auth_json) == 1L && is.na(auth_json))) {
    return(NA_character_)
  }

  auth_path <- trimws(as.character(auth_json %||% ""))
  if (!nzchar(auth_path)) {
    return(NA_character_)
  }
  if (!file.exists(auth_path)) {
    stop(
      sprintf("Google service account JSON not found: %s", auth_path),
      call. = FALSE
    )
  }
  auth_path
}

authenticate_google_sheets <- function(auth_json = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_),
                                       email = NA_character_,
                                       cache_dir = file.path(getwd(), ".secrets", "googlesheets4"),
                                       scopes = "spreadsheets",
                                       verbose = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  auth_path <- resolve_google_auth_json(auth_json)
  user_email <- trimws(as.character(email %||% ""))
  if (!nzchar(user_email)) user_email <- NA_character_

  if (!is.na(auth_path)) {
    if (verbose) {
      message("Authenticating with service account JSON: ", auth_path)
    }
    googlesheets4::gs4_auth(
      path = auth_path,
      scopes = scopes,
      cache = FALSE
    )
  } else {
    if (verbose) {
      message("Authenticating with interactive OAuth cache at: ", cache_dir)
    }
    googlesheets4::gs4_auth(
      email = user_email,
      scopes = scopes,
      cache = cache_dir
    )
  }
}

google_sheet_tab_exists <- function(ss, sheet_name) {
  info <- googlesheets4::gs4_get(ss)
  props <- googlesheets4::sheet_properties(info)
  if (!nrow(props)) return(FALSE)
  sheet_name %in% props$name
}

ensure_google_sheet_tab <- function(ss, sheet_name) {
  if (!google_sheet_tab_exists(ss, sheet_name)) {
    googlesheets4::sheet_add(ss, sheet = sheet_name)
  }
  invisible(ss)
}

read_google_sheet_table <- function(ss,
                                    sheet_name,
                                    col_types = NULL,
                                    na = "",
                                    trim_ws = TRUE,
                                    verbose = FALSE) {
  if (!google_sheet_tab_exists(ss, sheet_name)) {
    if (verbose) {
      message("Google Sheet tab does not exist yet: ", sheet_name)
    }
    return(data.frame(stringsAsFactors = FALSE))
  }

  googlesheets4::read_sheet(
    ss = ss,
    sheet = sheet_name,
    col_types = col_types,
    na = na,
    trim_ws = trim_ws,
    .name_repair = "unique"
  )
}
