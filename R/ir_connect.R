#' Connect to an Oracle database

#' @description ir_connect() is a wrapper function for DBI::dbConnect that attempts to streamline connections to a University's Oracle databases. Using this function, it is possible for a user to specify only the Data Source Name (DSN).
#'
#' If a the user id or password for the DSN are not specified, ir_connect() will prompt the user to enter their credentials via the ir_credentials() function.
#'
#' @param .dsn Data Source Name (DSN) of a valid locally-installed ODBC driver (64-bit Oracle in OraClient 12Home1)
#' @param access_db Is .dsn a microsoft access database
#' @param out Type of output desired. The defalt, "assign", will return the connection as an object in the global environment with the same name as the DSN. Alternatively, "return" will simply return the connection. The "return" option should be used only when using oira_connect directly inside another function call such as DBI::dbGetQuery
#' @return ir_connect() returns an S4 object into the global environment that inherits from DBIConnection. This object is used to communicate with the database engine. The global object will have the same name as the DSN name.
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#'
#' @export ir_connect

ir_connect <- function(.dsn, access_db = FALSE, out = 'assign') {
  assert_username()
  assert_password()

  ## Oracle database

  if(access_db == FALSE){
    connect = function() {dbConnect(odbc(),
                                 DSN = .dsn,
                                 UID = Sys.getenv("YOUR_USERNAME"),
                                 PWD = Sys.getenv("YOUR_PASSWORD"),
                                 Port = 1521)}

    connection = tryCatch(connect(),
                          error = function() {
                            rm(.dsn, envir = .GlobalEnv)
                            connect()
                          })

    if(out == 'assign'){
      assign(.dsn, connection, envir = .GlobalEnv)
    }

    if(out == 'return'){
      return(connection)
    }
  }

  ## Access database
  if(access_db == TRUE){
    # make sure that the file exists before attempting to connect
    if (!file.exists(.dsn)) {
      stop("DB file does not exist at ", .dsn)
    }

    # Assemble connection strings
    dbq_string <- paste0("DBQ=", .dsn)
    driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
    db_connect_string <- paste0(driver_string, dbq_string)

    myconn <- dbConnect(odbc(),
                        .connection_string = db_connect_string)
    return(myconn)
  }
}

