#' Get a calling function's name from within the function
#'
#' @details Not for external use. Use [func_name()] instead
#'
#' @param skip_frames The level in the calling stack to look. 1 is in the current
#' function, 2 is one before, etc.
#' @param skip_names Names returned to skip, these are not real function names but
#' generalized values used internally
#' @param ret_stack If TRUE, return the stack trace
#' @param extra_perf_per_level This is prepended by R and will be removed from the output
#'
#' @return The name of the calling function at level `skip_frames` in the stack trace
fn_finder <- function(skip_frames = 1,
                      skip_names = "(FUN)|(.+apply)|(replicate)",
                      ret_stack = FALSE,
                      extra_perf_per_level = "\t"){

  prefix <- sapply(3 + skip_frames + 1:sys.nframe(), function(i){
    sys.call(sys.parent(n = i))[[1]]
  })
  prefix[grep(skip_names, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(!length(prefix)){
    stop("Could not find any calling function at stack level ", skip_frames,
         call. = FALSE)
  }else if(ret_stack){
    paste(rev(prefix), collapse = "|")
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra_perf_per_level,
                                 length(prefix) - 1),
                             collapse = ""),
                       retval)
    }
    retval
  }
}
