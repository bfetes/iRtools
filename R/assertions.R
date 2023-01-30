#' Check to see if username and password are stored
#' @importFrom assertthat assert_that


assert_username <- function() { # nolint start

  # Check if API key is set in environment
  assertthat::assert_that(Sys.getenv("YOUR_USERNAME") != "",
                          msg = "You need to register your username using the\n'ir_credentials()' function."
  )
}

assert_password <- function() { # nolint start

  # Check if API key is set in environment
  assertthat::assert_that(Sys.getenv("YOUR_PASSWORD") != "",
                          msg = "You need to register your password using the\n'ir_credentials()' function."
  )
}
