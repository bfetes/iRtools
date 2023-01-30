#' Install database credentials in your `.Renviron` file for repeated use
#'
#' @description This function adds your reseptive system username and password to your
#' `.Renviron` file so it can be called securely without being stored in
#' your code. After you have installed these two credentials, they can be
#' called any time with `Sys.getenv("YOUR_USERNAME")` or
#' `Sys.getenv("YOUR_PASSWORD")`. If you do not have an
#' `.Renviron` file, the function will create one for you. If you already
#' have an `.Renviron` file, the function will append the key to your
#' existing file, while making a backup of your original file for disaster
#' recovery purposes.
#' @param username Your usernamed used for accessing databases formatted in quotes.
#' @param password The password used for accessing databases formatted in quotes.
#' @param install If TRUE, will install the key in your `.Renviron` file
#' for use in future sessions.  Defaults to FALSE (single session use).
#' @param overwrite If TRUE, will overwrite existing credentials that you already have in your `.Renviron` file.
#'
#' @export ir_credentials


ir_credentials <- function(username, password,
                                               install = FALSE, overwrite = FALSE) {
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    }
    else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- readLines(renv)
        newenv <- oldenv[-grep("YOUR_USERNAME|YOUR_PASSWORD", oldenv)]
        writeLines(newenv, renv)
      }
      else {
        tv <- readLines(renv)
        if (any(grepl("YOUR_USERNAME|YOUR_PASSWORD", tv))) {
          stop("ir_credentials already exist. You can overwrite them with the argument overwrite=TRUE", call. = FALSE)
        }
      }
    }

    keyconcat <- paste0("YOUR_USERNAME = '", username, "'")
    urlconcat <- paste0("YOUR_PASSWORD = '", password, "'")
    # Append credentials to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    write(urlconcat, renv, sep = "\n", append = TRUE)
    message('Your ir_credentials key and base URL have been stored in your .Renviron.  \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
  } else {
    message("To install your credentials for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(
      YOUR_USERNAME = username,
      YOUR_PASSWORD = password
    )
  }
}


