#' Create a table from a data source

#' @description ir_tbl() is a wrapper function for dplyr::tbl, merging with dbplyr::in_schema and iRtools::ir_connect
#'
#' @param .dsn Data Source Name (DSN) of a valid locally-installed ODBC driver (64-bit Oracle in OraClient 12Home1)
#' @param .schema Database schema name
#' @param .table Database table name
#'
#' @importFrom odbc dbIsValid
#' @importFrom rlang sym
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema
#'
#' @export ir_tbl

ir_tbl <- function(.dsn, .schema, .table) {

  if(!exists(.dsn)) {ir_connect(.dsn = .dsn)}
  if(dbIsValid(eval(sym(.dsn)))) {ir_connect(.dsn = .dsn)}

  tbl(eval(sym(.dsn)), in_schema(schema = .schema, table = .table))

}

