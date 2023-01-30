#' Suppress repeated row labels in summary tables
#'
#' @description suppress_repeats() is a function designed to improve the readibility of summary tables. It works best with grouped data frames, just prior to creating your output.
#' @param .data a grouped data frame
#' @param columns an optional character vector of column names on which to operate
#' @return A tbl, no longer grouped
#'
#' @importFrom dplyr ungroup mutate_at lag row_number
#'
#'
#'
#' @export suppress_repeats

suppress_repeats <- function(.data, columns = NULL) {
  . <- funs <- NULL
  out <- .data
  if(is.null(columns)){
  if(!('grouped_df' %in% class(out))){stop('You must either supply a grouped data frame (class = "grouped_df") to suppress_repeats or supply the names of the grouped columns as a character vector')}
    else{
    columns <- as.character(unlist(attr(out, 'vars')))
    }
  }
  out <- ungroup(out)
  out[columns] <- lapply(out[columns], FUN = function(x) ifelse(is.na(x), '', x))
  out <- mutate_at(out,
                          .vars = columns,
                          .funs = funs(ifelse(. == lag(.) & row_number() != 1,
                                              '',
                                              as.character(.))))
  out
}











