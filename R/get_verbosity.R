#' Get Verbosity Level
#'
get_verbosity <- function() {
  # `GEMINI_VERBOSE` should be one of `numeric` or `FALSE`/`TRUE`. But we'll return it as numeric.
  suppressWarnings(max(
    as.logical(Sys.getenv("GEMINI_VERBOSE", TRUE)),
    as.numeric(Sys.getenv("GEMINI_VERBOSE", TRUE)),
    na.rm = TRUE
  ))
}
