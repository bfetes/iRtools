#' Add empty rows to a data frame
#'
#' Insert empty rows into a data frame by a specified group.
#' @param .data  A data frame.
#' @param group The grouping after which blank rows will be added.  Entering a number will add blank rows after each numeric grouping of rows (i.e. enter 3 to insert blank rows after every third row).  Specify a quoted column name or vector of quoted column names to group the data frame and add blank rows after each grouping.  Defaults to 1.
#' @param n The number of empty rows to insert after each group.  Defaults to 1.
#' @keywords blank rows
#'
#' @importFrom dplyr bind_rows select distinct mutate row_number left_join mutate_all group_by filter arrange ungroup
#'
#' @export add_empty_rows

add_empty_rows <- function(.data, group = 1, n = 1) {
  Order <- NULL

  # Error checks
  if(length(group) == 1){
    if(is.numeric(group) & (group < 1 | grepl('\\.', group))) {
      stop(paste0("group must be an integer greater than or equal to 1 or a valid column name"))
    }
  }

  if(length(group) > 1 & all(is.numeric(group))) {
    stop(paste0('group must be a single integer value greater than or equal to 1 or a valid column name'))
  }

  # suppressWarnings(if(is.numeric(group) & (group < 1 | grepl('\\.', group))) {
  #   stop(paste0("group must be an integer greater than or equal to 1 or a valid column name"))
  # })

  if(is.numeric(n) == F | n < 1 | grepl('\\.', n)) {
    stop(paste0("n must be an integer greater than or equal to 1"))
  }

  # Case when group is numeric (ie. add blank row(s) after every group of lines)
  if(is.numeric(group)) {

    # If nrow(.data) is not a multiple of group, need to add (then remove) rows to .data to attain even divisibility
    if(nrow(.data) %% group != 0) {

      # Number of rows to add to .data (then remove)
      m <- ceiling(nrow(.data)/group) * group - nrow(.data)

      # Row from .data to repeat
      .data.last <- .data[nrow(.data), ]

      # Duplicate this row the appropriate amount (m) to bind to .data
      .data.add <- .data.last[rep(seq_len(nrow(.data.last)), each = m),]

      .data <- bind_rows(.data, .data.add)

      # Add ordering column
      .data$Order <- rep(1:(nrow(.data)/group), each = group)

      # Remove duplicated rows that were added
      .data <- .data[1:(nrow(.data) - m), ]

    } else {

      # Add ordering column
      .data$Order <- rep(1:(nrow(.data)/group), each = group)

    }

    # Case when incorrect column name is provided (Error message)
  } else if(is.character(class(group)) & all(group %in% colnames(.data)) == FALSE) {

    stop(paste0("All items in the specified group are not column names of your object"))

    # Case when group contains all proper column names
  } else if(all(group %in% colnames(.data)) == TRUE) {

    # Preserve original column names
    cn <- colnames(.data)

    # Remove spaces from group and colnames for dplyr transformation
    group <- gsub(' ', '', group)
    colnames(.data) <- gsub(' ', '', colnames(.data))

    # Create a sorting order with the Order field
    id <- select(.data, {{group}})
    id <- distinct(id)
    id <- mutate(id, Order = row_number())
    max.digits <- nchar(max(id$Order))
    id <- mutate(id, Order = paste0(paste(rep('0', max.digits), collapse = ''), Order))
    id <- mutate(id, Order = substr(Order, nchar(Order) - max.digits + 1, nchar(Order)))

    .data <- left_join(.data, id, by = group)

    # Restore original column names + the newly added 'Order' column
    colnames(.data) <- c(cn, 'Order')

  }

  # Convert data frame to all character
  .data <- mutate_all(.data, as.character)

  # Create blanks
  blanks <- group_by(.data, Order)
  blanks <- filter(blanks, row_number() == 1)
  blanks <- ungroup(blanks)

  blanks[,1:(ncol(blanks) - 1)] <- ''

  blanks <- blanks[rep(row.names(blanks), n),]

  # Create output
  out <- bind_rows(.data, blanks)
  out <- arrange(out, Order)
  out <- select(out, -Order)

  out[1:(nrow(out) - n), ]

}
