#' Get Gemini's API URL
#'
#' @param gemini_api_key Gemini's API key.
#' @param endpoint Gemini's endpoint to target.
#' @param model Gemini's model to use.
#'
api_url <- function(gemini_api_key, endpoint = "generateContent",
                    model = Sys.getenv("GEMINI_MODEL", "gemini-pro")) {
  paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ifelse(nchar(endpoint) == 0, "", paste0(":", endpoint)),
    "?key=",
    gemini_api_key
  )
}
