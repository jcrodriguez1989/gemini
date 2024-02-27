#' List Gemini Models
#'
#' @param gemini_api_key Gemini's API key.
#'
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
list_models <- function(gemini_api_key = Sys.getenv("GEMINI_API_KEY")) {
  if (nchar(gemini_api_key) == 0) {
    stop("`GEMINI_API_KEY` not provided.")
  }
  fromJSON(GET(api_url(gemini_api_key, "", "")), content(as = "text", encoding = "UTF-8"))
}
