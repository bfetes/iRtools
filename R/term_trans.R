#' Translate or transform a column with term-related data.

#' @description term_trans() takes a dataframe column of term-related data and translates it into different values that represent the same term, or transforms the data into values that represent past or future terms.

#' @param col The column to be translated or transformed. Acceptable term-related data includes term code (of either character or numerical type), term name, and academic year.
#' @param trans The desired translation or transformation. Translations include 'term code', 'term name', 'acad year', 'term begin date', and 'term end date'. Translations to term dates correspond to the undergraduate academic calendar. Academic year data can be translated into 'term code' and 'term name' only. Transformations include 'next', 'previous', 'next fall spring', 'previous fall spring', 'next fall', 'previous fall', 'next spring', and 'previous spring'. Academic year data can be transformed into 'next' and 'previous' only.
#' @param trans_season Only required when translating academic year data. Specify 'fall', 'spring', or 'summer'.
#' @return Returns a vector of translated or transformed term-related data.
#'
#' @export term_trans

term_trans <- function(col, trans, trans_season = NULL) {

  transform_type <- season <- NULL

  # identify whether input is ACAD_YEAR, TERM_NAME, or TERM_CODE
  type <- ifelse(
    col %in% unique(trans_data$`acad year`) &
      is.character(col), "acad year", ifelse(
        col %in% unique(trans_data$`acad year num`) &
          is.numeric(col), "acad year num", ifelse(
            col %in% unique(trans_data$`term name`), "term name", ifelse(
              col %in% unique(trans_data$`term code`) &
                is.character(col), "term code" , ifelse(
                  col %in% unique(trans_data$`term code num`) &
                    is.numeric(col), "term code num", "unknown"
                )
            )
         )
      )
  )

  # identify single type of data
  type <- sort(unique(type))

  # produce error if multiple types of data or all values unknown
  if(
    length(type) > 1 |
    (length(type) == 1 &
     "unknown" %in% type)
  ) {
    stop("All values must be of one type - acad year, term name, or term code. All values must be contained in available academic calendar data, which spans Fall 1925-Fall 2023.")
  }

  # produce error if requested trans is not acceptable
  if(
    !(trans %in% c('term code', 'term name', 'acad year', 'term begin date', 'term end date', 'next', 'previous', 'next fall spring', 'previous fall spring', 'next fall', 'previous fall', 'next spring', 'previous spring'))
  ) {
    stop("trans is not of acceptable type. Acceptable types include 'term code', 'term name', 'acad year', 'term begin date', 'term end date', 'next', 'previous', 'next fall spring', 'previous fall spring', 'next fall', 'previous fall', 'next spring', 'previous spring'")
  }

  # produce error if requested trans is same type as input
  if(
    type == trans |
    (type == "term code num" & trans == "term code") |
    (type == "acad year num" & trans == "acad year")
  ) {
    stop(paste("Translation cannot be same type (", trans,  ") as column.", sep = ""))
  }

  # produce errors related to requesting academic year translation
  if(type == "acad year" | type == "acad year num") {

    # requested translation for academic year is not term name, term code, next, or previous
    if(
      !(trans %in% c("term name", "term code", "next", "previous"))
    ) {
      stop("Value of trans when data is academic year must be either 'term name', 'term code', 'next', or 'previous'.")
    }

    # season is not specified when translating academic year into term name or term code
    if(
      trans %in% c("term name", "term code") &
      is.null(trans_season)
    ) {
      stop("trans_season must be specified when translating academic year into term name or term code. trans_season must be either 'fall', 'spring', or 'summer'.")
    }

    # season is specified but is not fall, spring, or summer when translating acad year to term name or term code
    if(
      trans %in% c("term name", "term code") &
      !is.null(trans_season) &
      !(trans_season %in% c("fall", "spring", "summer"))
    ) {
      stop("trans_season must be 'fall', 'spring', or 'summer'.")
    }
  }

  # specify season as NULL if it is specified when col type is not acad year
  # produce warning
  if(
    (!is.null(trans_season) &
     type != "acad year" &
     type != "acad year num") |
    (!is.null(trans_season) &
     (type == "acad year" |
      type == "acad year num") &
     !(trans %in% c("term name", "term code")))
  ) {
    trans_season <- NULL
    warning("Season should only be specified when translating academic year into term name or term code. Ignoring argument trans_season.")
  }

  # TRANSLATION
  if(trans %in% c("acad year", "term name", "term code", "term begin date", "term end date")) {

    if(type != "acad year" & type != "acad year num") {

      translation_data <- trans_data[, type] |>
        unique() |>
        merge(trans_data[, c(type, trans)] |> unique())

      translation <- data.frame(col)
      translation$order = 1:nrow(translation)
      translation <- translation |>
        merge(
          translation_data,
          by.x = 1,
          by.y = 1,
          all.x = T,
          sort = F
        )

      translation <- translation[order(translation$order), ]

      translation <- translation[, 3]
      return(translation)

    }

    if(type == "acad year" | type == "acad year num") {

      translation_data <- trans_data[, c(type, "season")] |>
        unique() |>
        subset(season == trans_season) |>
        merge(trans_data[, c(type, trans, "season")] |> unique())

      translation <- data.frame(col)
      translation$order = 1:nrow(translation)
      translation <- translation |>
        merge(
          translation_data,
          by.x = 1,
          by.y = 1,
          all.x = T,
          all.y = F,
          sort = F
        )

      translation <- translation[order(translation$order), ]

      translation <- translation[, 4]

      return(translation)

    }
  }

  # TRANSFORMATION
  if(trans %in% c("next", "previous", "next fall spring", "previous fall spring", "next fall", "next spring", "previous fall", "previous spring")) {

    trans_type <- paste("trans", type)

    transformation_data <- trans_data[, c(type, "transform_type", trans_type)] |>
      unique() |>
      subset(transform_type == trans)

    transformation <- data.frame(col)
    transformation$order <- 1:nrow(transformation)
    transformation <- transformation |>
      merge(
        transformation_data,
        by.x = 1,
        by.y = 1,
        all.x = T,
        sort = F
      )

    transformation <- transformation[order(transformation$order), ]

    transformation <- transformation[, 4]

    return(transformation)

  }
}
