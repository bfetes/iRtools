#' Scrape ARWU table
#'
#' Scrapes an ARWU rankings table for a specified year from the following
#' page: \url{https://www.shanghairanking.com/rankings/arwu/2021} (2021 shown, but
#' other years can be specified)
#'
#' Also calculates a "computed score". As ARWU only publishes the exact total
#' score and rank for the top 100 institutions, we use the ARWU formula to
#' compute the total score from component scores, and use the computed score
#' to calculate the exact rank. Note that the computed score is different than the #' the total score due to scaling that occurs to make the score of the top
#' institution 100.
#'
#' @param year An integer. Currently, only years from 2004 onward # 'are compatible due to a change in the table layout at this point.
#'
#' @return A dataframe containing ARWU rankings and scores for the given
#' year.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom readr type_convert
#' @importFrom dplyr bind_cols
#'
#'
#' @export scrape_arwu

scrape_arwu <- function(year) {
  url = "http://shanghairanking.com"
  path = paste0("/api/pub/v1/arwu/rank?version=",year)
  # list of various dataframes
  rank_data <- arwu_get_data(url, path)

  ranking_data <- bind_cols(
    rank_data$rankings %>%
      select(
        "Rank" = "ranking" ,
        "University" = "univNameEn",
        "Country" = "region",
        "National Rank" = "regionRanking",
        "Total Score" = "score",
      ),
    # dataframe of individual scores, with numbers for column names
    rank_data$rankings$indData %>%
      # order columns by order of numbers in rank_data$indicators$code
      # there is a difference in the 2004 and 2021 data structure in which the columns are not in the usual order; in 2021, for example, the PCP score and the PUB score columns are in an unusual order in rank_data_rankings$indData (PCP is fourth (column name = 144) and PUB is fifth (column name = 145), while it is usually the other way around - PUB precedes PCP); however, the vector rank_data$indicators$code shows the order in which the columns should be: 140, 141, 142, 143, 145, 144; this select statement reorders the columns by the vector, putting them in the usual order
      select(c(rank_data$indicators$code)) %>%
      # rename columns (replace numbers with names of individual scores)
      `names<-`(rank_data$indicators$nameEn)
  ) %>%
    dplyr::as_tibble() %>%
    compute_score_and_rank() %>%
    dplyr::mutate(Year = as.integer(year))

  suppressMessages(readr::type_convert(ranking_data, na = "null"))

  return(ranking_data)
}

# Helper functions -------------------------------------------------------------

arwu_get_data <- function(url, path) {
  content <- data <- NULL
  httr::GET(url=url, path=path) %>%
    magrittr::use_series(content) %>%
    rawToChar() %>%
    jsonlite::fromJSON() %>%
    magrittr::use_series(data)

}

# This function computes scores differently where N&S is missing, due to a
# different calculation methodology for these institutions.

compute_score_and_rank <- function(arwu_table) {
  .data <- NULL
  arwu_table %>%
    dplyr::mutate(
      `Computed Score` = dplyr::case_when(
        is.na(.data$`N&S`) ~
          (0.1 * .data$Alumni +
             0.2 * .data$Award +
             0.2 * .data$HiCi +
             0.2 * .data$PUB +
             0.1 * .data$PCP) * 1.25,
        TRUE ~
          (0.1 * .data$Alumni +
             0.2 * .data$Award +
             0.2 * .data$HiCi +
             0.2 * .data$`N&S` +
             0.2 * .data$PUB +
             0.1 * .data$PCP))) %>%
    dplyr::arrange(dplyr::desc(.data$`Computed Score`)) %>%
    dplyr::mutate(
      `Computed Rank` = dplyr::min_rank(
        dplyr::desc(.data$`Computed Score`)))

}

