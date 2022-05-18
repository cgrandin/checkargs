#' Get a named list of the argument values in any function call
#'
#' @return A named list of the argument values used in a function call
#' @export
get_args <- function(){
browser()
  def_call <- sys.call(-1)
  out <- get(as.character(def_call[[1]]),
             mode = "function",
             sys.frame(-2))
  act_call <- match.call(definition = out,
                         call = def_call)
  out <- as.list(out)
  # Remove function source code (always at the end)
  out <- out[-length(out)]
  act <- as.list(act_call)[-1]
  inds <- names(out) %in% names(act)
  out[inds] <- act
  out
}
