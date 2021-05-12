#' Returns a calling function's name `levels_up` levels up the stack trace
#'
#' @param levels_up How many levels back in the stack trace to look for the
#' function name
#'
#' @return A calling function's name `levels_up` levels up the stack trace
#' @export
#'
#' @examples
#' f <- function(){
#'   message("You are in ", func_name())
#' }
func_name <- function(levels_up = 1){
  stopifnot(!is.null(levels_up))
  stopifnot(class(levels_up) == "numeric")
  stopifnot(length(levels_up) == 1)
  stopifnot(levels_up >= 0)

  fn_name <- fn_finder(skip_frames = levels_up)
  fn_name <- gsub("\t+", "", fn_name)
  fn_name <- gsub("\ +", "", fn_name)
  fn_name
}
