#' Verify that an object adheres to constraints
#'
#' @description
#' Verify that the argument `arg` is not `NULL`, has the correct classes,
#' length, dimensions, and is within in a set of values
#'
#' @details
#' Any of `chk_class`, `chk_len`, `chk_dim`, and `chk_is_in` can be omitted.
#' If they are all omitted, only the check to make sure `arg` is not `NULL`
#' will be performed, unless `allow_null` is `TRUE`

#' @param arg The object to check
#' @param chk_class A vector of character strings of the names of the classes
#' to ensure `arg` has
#' @param chk_len A numeric value to ensure `arg` has the length of
#' @param chk_is_in A vector of values to ensure `arg` is in
#' @param chk_dim A vector of the dimensions that `arg` must have as its
#' dimensions
#' @param allow_null If `TRUE` the argument is allowed to be `NULL`
#'
#' @return TRUE, invisibly or the function will throw an error if `arg` does
#' not follow the constraints given; `FALSE` is not returned
#' @importFrom crayon red green white yellow blue
#' @importFrom purrr map map_chr
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

  # _nm = name
  # _bt = backticked,
  # _nl = ends in colon and newline
  func_nm <- func_name(levels_up = 2)
  func_nm_bt <- paste0("`", func_nm, "()`")
  args <- get_args()
  arg_bt <- paste0("`", args$arg, "`")
  arg_nl <- paste0(args$arg, ":\n")

  if(!allow_null && is.null(arg)){
    message(white("Constraint problem with argument "), green(arg_bt),
            white(" in function "), green(func_nm_bt))
    stop(white("is.null("),
         green(args$arg),
         white(") is `TRUE`"),
         call. = FALSE)
  }

  if(!is.null(chk_dim) && !identical(dim(arg), as.integer(chk_dim))){
    arg_dim_str <- paste(dim(arg), collapse = "\n")
    chk_dim_str <- paste0("chk_dim: ", paste(chk_dim, collapse = "\n"))
    message(white("Dimensions incorrect for argument "), green(arg_bt),
            white(" in function "), green(func_nm_bt))
    stop(white("Dimensions of "), green(arg_bt), white(" are "),
         red(paste(dim(arg), collapse = ", ")), white(". They should be "),
         green(paste(chk_dim, collapse = ", ")),
         call. = FALSE)
  }

  if(!is.null(chk_len) && length(chk_len) > 1){
    stop("length of chk_len is greater than zero",
         call. = FALSE)
  }

  if(!is.null(chk_len) && chk_len != length(arg)){
    message(white("Length incorrect for argument "), green(arg_bt),
            white(" in function "), green(func_nm_bt))
    stop(white("Length of "), green(arg_bt), white(" is "),
         red(length(arg)), white(". It should be "), green(chk_len),
         call. = FALSE)
  }

  if(!is.null(chk_class)){
    # Cycle through the classes and record any which are not found in the variable
    not_in <- map(chk_class, ~{
      `if`(.x %in% class(arg), return(NULL), return(.x))
    })
    not_in <- not_in[lengths(not_in) >0]
    if(length(not_in)){
      not_in <- not_in %>% map_chr(~{.x})
      not_in <- paste(not_in, collapse = "\n")
      message(white("Class(es) missing from argument "), green(arg_bt),
              white(" in function "), green(func_nm_bt), white(":"))
      stop(white(paste0("Class(es) in ", arg_bt, ":\n")),
           green(paste(class(arg), collapse = "\n")),
           white(paste0("\nmissing class(es) in ", arg_bt, ":\n")),
           red(not_in),
           call. = FALSE)
    }
  }

  if(!is.null(chk_is_in)){
    if(any(c("data.frame", "list") %in% class(arg))){
      message(white("Range checking is not implemented for class "),
              white("`data.frame` or `list` for argument "),
              green(arg_bt),
              white(" in function "), green(func_nm_bt))
      stop("Range checking is not implemented for the `data.frame` or ",
           "`list` type",
           call. = FALSE)
    }else{
      # It's a vector
      if(!any(c("numeric", "integer") %in% class(arg))){
        message(white("Range checking is not possible for the non-numeric "),
                white("argument "), green(arg_bt),
                white(" in function "), green(func_nm_bt))
        stop("Range checking is not possible for classes other than ",
             "`numeric` or `integer`",
             call. = FALSE)
      }
      # It's a numeric or integer vector
      if(!all(arg >= min(chk_is_in) & arg <= max(chk_is_in))){
        message(white("Constraint problem with argument "), green(arg_bt),
                white(" in function "), green(func_nm_bt))
        stop(white("Some of the values in "), green(arg_bt), white(":\n"),
             red(paste(arg, collapse = ", ")),
             white("\nare not in the range of allowed values:\n"),
             green(paste(min(chk_is_in), " .. ", max(chk_is_in))),
             "\n",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}
