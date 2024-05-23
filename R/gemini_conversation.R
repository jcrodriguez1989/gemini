#' Get Gemini Conversation Endpoint
#'
#' @param prompt The prompt to start the conversation.
#' @param gemini_api_key Gemini's API key.
#' @param session Gemini conversation session.
#'
#' @importFrom httr content POST
#'
gemini_conversation <- function(prompt, gemini_api_key, session = NULL) {
  if (nchar(gemini_api_key) == 0) {
    stop("`GEMINI_API_KEY` not provided.")
  }
  if (get_verbosity()) {
    message(paste0("\n*** Gemini input:\n\n", prompt, "\n"))
  }
  if (is.null(session)) {
    session <- list(role = "user", parts = list(text = prompt))
  }
  # Run the API query.
  final_res <- list()
  keep_querying <- TRUE
  while (keep_querying) {
    post_res <- POST(
      api_url(gemini_api_key),
      encode = "json",
      body = list(contents = session)
    )
    if (!post_res$status_code %in% 200:299) {
      stop(content(post_res))
    }
    post_res <- content(post_res)
    final_res <- append(final_res, list(post_res))
    # In the case the finish_reason is the length of the message, then we need to keep querying.
    keep_querying <- !any(sapply(post_res$candidates, function(x) x$finishReason) == "STOP")
    # And update the session sent to Gemini, in order to continue the current session.
    parsed_response <- parse_response(list(post_res), verbosity = 0)
    if (nchar(parsed_response) == 0) {
      stop("Gemini didn't send a reply.")
    }
    session <- append(
      append(session, list(list(role = "model", parts = list(text = parsed_response)))),
      list(list(role = "user", parts = list(text = "continue")))
    )
  }
  final_res
}
