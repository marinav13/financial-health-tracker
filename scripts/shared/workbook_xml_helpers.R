# scripts/shared/workbook_xml_helpers.R
#
# Workbook SpreadsheetML/XML helpers extracted from workbook_helpers.R.
# Source this after workbook_table_helpers.R.

worksheet_signature <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("EMPTY")
  paste(capture.output(dput(df)), collapse = "\n")
}

# Renders a single table cell as SpreadsheetML XML.
xml_cell <- function(value, style_id = NULL) {
  style_attr <- if (is.null(style_id)) "" else paste0(' ss:StyleID="', style_id, '"')
  if (length(value) == 0 || is.na(value) || identical(as.character(value), "")) {
    return(paste0('<Cell', style_attr, '><Data ss:Type="String"></Data></Cell>'))
  }
  num <- suppressWarnings(as.numeric(value))
  if (!is.na(num) && !grepl("[A-Za-z]", as.character(value))) {
    return(paste0('<Cell', style_attr, '><Data ss:Type="Number">', as.character(num), '</Data></Cell>'))
  }
  paste0('<Cell', style_attr, '><Data ss:Type="String">', escape_xml(value), '</Data></Cell>')
}

# Renders a full worksheet (name + data frame) as a SpreadsheetML XML block.
worksheet_xml <- function(name, df) {
  if (is.null(df) || nrow(df) == 0) {
    return(paste0(
      '<Worksheet ss:Name="', escape_xml(name), '"><Table>',
      '<Row><Cell ss:StyleID="Header"><Data ss:Type="String">No rows</Data></Cell></Row>',
      '</Table></Worksheet>'
    ))
  }
  headers <- names(df)
  out <- c(paste0('<Worksheet ss:Name="', escape_xml(name), '">'), '  <Table>')
  out <- c(out, paste0('    <Row>', paste(vapply(headers, xml_cell, character(1), style_id = "Header"), collapse = ""), '</Row>'))
  for (i in seq_len(nrow(df))) {
    vals <- unname(as.list(df[i, , drop = FALSE]))
    out <- c(out, paste0('    <Row>', paste(vapply(vals, xml_cell, character(1)), collapse = ""), '</Row>'))
  }
  out <- c(out, '  </Table>', '</Worksheet>')
  paste(out, collapse = "\n")
}
