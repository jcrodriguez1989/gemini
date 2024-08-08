#' Get Gemini Safety Settings From Environment Vars
#'
#' Set environment variables GEMINI_{HARM_CATEGORY_HARASSMENT, HARM_CATEGORY_HATE_SPEECH,
#' HARM_CATEGORY_SEXUALLY_EXPLICIT, HARM_CATEGORY_DANGEROUS_CONTENT} to affect the result of this
#' function.
#' More information about these variables in https://ai.google.dev/gemini-api/docs/safety-settings .
#'
get_safety_settings <- function() {
  safety_vars <- c(
    "HARM_CATEGORY_HARASSMENT", "HARM_CATEGORY_HATE_SPEECH", "HARM_CATEGORY_SEXUALLY_EXPLICIT",
    "HARM_CATEGORY_DANGEROUS_CONTENT"
  )
  safety_settings <- lapply(safety_vars, function(safety_var) {
    res <- NULL
    safety_value <- Sys.getenv(paste0("GEMINI_", safety_var))
    if (nchar(safety_value) > 0) {
      res <- list(category = safety_var, threshold = safety_value)
    }
    return(res)
  })
  return(safety_settings[!sapply(safety_settings, is.null)])
}
