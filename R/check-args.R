#' Verify that the argument `arg` is of the correct class, length, and
#' Verify that the argument `arg` is of the correct class, length, and
#' within in a set of values
#'
#' @param arg The object to check
#' @param chk_class A character string of the name of the class to ensure `arg` is
#' @param chk_len A numeric value to ensure `arg` has the length of
#' @param chk_is_in A vector of values to ensure `arg` is in
#' @param chk_dim A vector of the dimensions to ensure `arg` is the same dimensions as
#' @param allow_null If `TRUE` the argument is allowed to be `NULL`
#'
#' @return TRUE, invisibly or the function will throw an error if `arg` does not follow
#' the constraints given; FALSE is not returned
#' @importFrom crayon red green white yellow blue
#' @export
#'
#' @examples
#' \dontrun{
#' check_arg(23, "numeric", 1) # Succeeds
#' check_arg(23, "numeric", 1, 1:20) # Fails
#' }
check_arg <- function(arg = NULL,
                      chk_class = NULL,
                      chk_dim = NULL,
                      chk_len = NULL,
                      chk_is_in = NULL,
                      allow_null = FALSE){

  calling_func_name <- func_name(levels_up = 2)
  calling_args <- get_args()
  calling_arg_str <- paste0(calling_args$arg, ": ")

  if(!allow_null && is.null(arg)){
    message(white("Error from calling function "),
            green(calling_func_name))
    stop(white("is.null("),
         green(calling_args$arg),
         white(") is TRUE"),
         call. = FALSE)
  }

  if(!is.null(chk_dim) && !identical(dim(arg), as.integer(chk_dim))){
    calling_arg_dim_str <- paste(dim(arg), collapse = " ")
    chk_dim_str <- paste0("chk_dim: ", paste(eval(calling_args$chk_dim), collapse = " "))
    message(white("Error from calling function "), green(calling_func_name))
    stop(white("Dimension of "),
         green(calling_arg_str),
         green(calling_arg_dim_str),
         white(" is not equal to value of "),
         green(chk_dim_str),
         call. = FALSE)
  }

  if(!is.null(chk_len) && length(chk_len) > 1){
    stop("length of chk_len is greater than zero",
         call. = FALSE)
  }

  if(!is.null(chk_len) && chk_len != length(arg)){
    calling_arg_len_str <- paste(length(arg), collapse = " ")
    chk_len_str <- paste0("chk_len: ", paste(eval(calling_args$chk_len), collapse = " "))
    message(white("Error from calling function "),
            green(calling_func_name))
    stop(white("Length of "),
         green(calling_arg_str),
         green(calling_arg_len_str),
         white(" is not equal to value of "),
         green(chk_len_str),
         call. = FALSE)
  }

  if(!is.null(chk_class) && !any(class(arg) %in% chk_class)){
    calling_arg_class_str <- paste(class(arg), collapse = " ")
    chk_class_str <- paste0("chk_class: ", paste(eval(calling_args$chk_class), collapse = " "))
    message(white("Error from calling function "),
            green(calling_func_name))
    stop(white("Class requirement of "),
         green(calling_arg_str),
         green(calling_arg_class_str),
         white(" is not present in classes "),
         green(chk_class_str),
         call. = FALSE)
  }

  if(!is.null(chk_is_in)){
    if(sum(!is.na(match(chk_is_in, arg))) != length(arg)){
      calling_arg_is_in_str <- paste(arg, collapse = " ")
      chk_is_in_str <- paste0("chk_is_in: ", paste(eval(calling_args$chk_is_in), collapse = " "))
      message(white("Error from calling function "),
              green(calling_func_name, ":"))
      stop(white("Not all values in "),
           green(calling_arg_str),
           green(calling_arg_is_in_str),
           white(" are in "),
           green(chk_is_in_str),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
