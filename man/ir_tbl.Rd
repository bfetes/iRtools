% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_tbl.R
\name{ir_tbl}
\alias{ir_tbl}
\title{Create a table from a data source}
\usage{
ir_tbl(.dsn, .schema, .table)
}
\arguments{
\item{.dsn}{Data Source Name (DSN) of a valid locally-installed ODBC driver (64-bit Oracle in OraClient 12Home1)}

\item{.schema}{Database schema name}

\item{.table}{Database table name}
}
\description{
ir_tbl() is a wrapper function for dplyr::tbl, merging with dbplyr::in_schema and iRtools::ir_connect
}
