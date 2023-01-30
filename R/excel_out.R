#' Export data frames to Excel
#'
#' One or more data frames can be exported to Excel in one of two ways: one per worksheet or all stacked in a single worksheet
#' @param .data A data frame or list of data frames wrapped in a list. Use list(df1, df2, etc.).
#' @param path A file path to export to.
#' @param output The desired output.  Use 'tab' to write each data frame to its own worksheet.  Use 'stack' to stack all data frames in one worksheet.  Defaults to 'tab'.
#' @param tab.names Logical.  Activated when output is set to 'tab'.  Tab names must be passed in the list of data frames for x (i.e. list(Name1 = df1, Name2 = df2, etc.)).  Defaults to FALSE.
#' @param title A single title or vector of titles.  If one title is listed, then there must be one title for each data frame.  Defaults to NULL.
#' @param subtitle A single subtitle or vector of subtitles.  If one subtitle is listed, then there must be one subtitle for each data frame.  Defaults to NULL.
#' @param footnote A single footnote or vector of footnotes.  If one footnote is listed, then there must be one footnote for each data frame.  Defaults to NULL.
#'
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr spread
#' @importFrom writexl write_xlsx
#'
#' @export

excel_out <- function(.data, path, output = 'tab', tab.names = FALSE, title = NULL, subtitle = NULL, footnote = NULL) {

  # Error messages
  options(warn = -1)
  if(any(grepl('list\\(', .data) == F)) {stop(paste0('Your data frame(s) needs to be wrapped in list()'))}
  options(warn = 0)
  if(output == 'tab' & tab.names == T & is.null(names(.data))) {stop(paste0("You forgot to supply names to your data frame objects in .data = list()"))}
  if(grepl('.xlsx', path) == F) {stop(paste0("Oops! Looks like you forgot the .xlsx extension in your path"))}

  # Determine number of data frames
  n.df <- length(.data)

  # Function for generically naming columns
  rename_columns <- function(i) {paste0('col', i)}


  # Table generating function
  generate.tables <- function(i) {

    # Data frame
    df <- data.frame(lapply(.data[[i]], as.character), stringsAsFactors = F)

    # Create titles
    if(length(title) == 0) {df.title <- data.frame(col1 = NA, stringsAsFactors = F)
    } else {
      df.title <- data.frame(col1 = title[i], stringsAsFactors = F)
    }

    # Create subtitles
    if(length(subtitle) == 0) {df.subtitle <- data.frame(col1 = NA, stringsAsFactors = F)
    } else {
      df.subtitle <- data.frame(col1 = subtitle[i], stringsAsFactors = F)
    }

    # Create footnote
    if(length(footnote) == 0) {df.footnote <- data.frame(col1 = NA, stringsAsFactors = F)
    } else {
      df.footnote <- data.frame(col1 = footnote[i], stringsAsFactors = F)
    }

    # Preserve column names of data frame
    df.header <- data.frame(title = colnames(df))
    df.header <- mutate(df.header,
                        title = factor(title, levels = title, ordered = T))
    df.header <- spread(df.header, key = title, value = 1)
    df.header <- data.frame(lapply(df.header, as.character), stringsAsFactors = F)
    colnames(df.header)[1] <- 'col1'

    # Rename first columns of data frame for binding
    colnames(df)[1] <- 'col1'

    # Create a blank row
    blank <- data.frame(col1 = NA, stringsAsFactors = F)

    # Assemble final table
    t <- bind_rows(df.title, df.subtitle, df.header, df, df.footnote)

    # Collapse for missing title, subtitle, and/or footnote
    if(is.null(footnote)) {
      t <- data.frame(t[1:(nrow(t) - 1),], stringsAsFactors = F)
    } else if (is.na(footnote[i]) | trimws(footnote[i]) == '') {
      t <- data.frame(t[1:(nrow(t) - 1),], stringsAsFactors = F)
    } else {
      t <- t
    }

    if(is.null(subtitle)) {
      t <- data.frame(t[c(1,3:nrow(t)),], stringsAsFactors = F)
    } else if (is.na(subtitle[i]) | trimws(subtitle[i]) == '') {
      t <- data.frame(t[c(1,3:nrow(t)),], stringsAsFactors = F)
    } else {
      t <- t
    }

    if(is.null(title)) {
      t <- data.frame(t[2:nrow(t),], stringsAsFactors = F)
    } else if(is.na(title[i]) | trimws(title[i]) == '') {
      t <- data.frame(t[2:nrow(t),], stringsAsFactors = F)
    } else {
      t <- t
    }

    # Rename columns
    colnames(t) <- lapply(1:length(t), rename_columns)

    # Add empty row at the bottom if 'stack' output selected
    if(output == 'stack' & i != n.df) {
      empty <- data.frame(col1 = NA, stringsAsFactors = F)
      t <- bind_rows(t, empty, empty)
    }

    t

  } # close generate.tables

  # Generate list of tables
  tables <- lapply(1:n.df, generate.tables)


  # Output - 'stack'
  if(output == 'stack') {
    out <- bind_rows(tables)
    writexl::write_xlsx(out, path = path, col_names = F)
  }

  # Output - 'tab'
  if(output == 'tab') {
    out <- lapply(1:n.df, function(i) {tables[[i]]})
    if(tab.names == FALSE) {names(out) <- NULL} else {names(out) <- names(.data)}
    writexl::write_xlsx(out, path = path, col_names = F)
  }


}
