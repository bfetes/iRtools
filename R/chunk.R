#' Divide a dataframe column into vectors, each the length of a specified increment

#' @description chunk() takes a dataframe column and divides it into vectors the length of a specified increment, plus a smaller vector if there are any remaining values. The vectors are saved as a list, which can be used to filter in a looped database query. The function was created as a means to filter by local values when the number of such values exceeds the filter limit.

#' @param .data The dataframe containing the column to be divided
#' @param col The column to be divided, quoted
#' @param increment The increment by which to divide the column
#' @return Returns a list of vectors that together contain a non-duplicated set of all unique values in the column. Each vector is the length of the specified increment, except for a possible smaller vector that contains remaining values.
#'
#' @importFrom dplyr pull
#' @importFrom dplyr distinct
#'
#' @export chunk

chunk <- function(.data, col, increment) {
  x <- distinct(.data[col])
  n <- nrow(x)
  r <- rep(1:ceiling(n/increment), each = increment)[1:n]
  d <- split(x, r)
  v <- lapply(d, function(col){pull(col)})
  return(v)
}
