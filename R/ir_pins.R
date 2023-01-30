#' A wrapper function for pins()

#' @description ir_pins() is a wrapper function for pins::pin_list, pins::pin_read, and pins::pin_get() that attempts to streamline connections to the pins board.
#'
#' @param use Type of function desired. Options include 'list' (pin_list), 'read' (pin_read), or 'write' (pin_write).
#' @param board Borad to connect to. Default is pins::board_resconnect()
#' @param name Pin name to either write or retrieve
#' @param x An object (typically a data frame) to pin.
#'
#' @importFrom pins board_rsconnect pin_list pin_read pin_write
#'
#' @export ir_pins

ir_pins <- function(use, board = board_rsconnect(), name = NULL, x = NULL ){
  rsc = board
  name = name

  if(use == "list"){sort(pin_list(board = rsc))}

  else if(use == "read"){pin_read(board = rsc, name = name)}

  else if(use == "write"){pin_write(board = rsc, x = x, name = name, title = '')}
}
