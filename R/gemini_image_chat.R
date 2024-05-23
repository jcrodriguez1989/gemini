#' Get Gemini Image Endpoint
#'
#' @param prompt The prompt to start the conversation.
#' @param image The path or URL of the image.
#' @param gemini_api_key Gemini's API key.
#'
#' @importFrom httr content POST
#' @importFrom jsonlite toJSON
#' @importFrom utils download.file
#' @importFrom xfun base64_encode
#'
gemini_image_chat <- function(prompt, image, gemini_api_key) {
  if (nchar(gemini_api_key) == 0) {
    stop("`GEMINI_API_KEY` not provided.")
  }
  if (get_verbosity()) {
    message(paste0("\n*** Gemini Image input:\n\n", prompt, "\n"))
  }
  if (!file.exists(image)) {
    image_url <- image
    image <- tempfile()
    download.file(image_url, image)
  }
  messages <- list(contents = list(list(parts = list(
    list(text = prompt),
    list(inline_data = list(mime_type = "image/jpeg", data = base64_encode(image)))
  ))))
  # Run the API query.
  final_res <- list()
  keep_querying <- TRUE
  while (keep_querying) {
    post_res <- POST(
      paste0(
        "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-vision:",
        "generateContent?key=", gemini_api_key
      ),
      encode = "json",
      body = toJSON(messages, auto_unbox = TRUE)
    )
    if (!post_res$status_code %in% 200:299) {
      stop(content(post_res))
    }
    post_res <- content(post_res)
    final_res <- append(final_res, list(post_res))
    # In the case the finish_reason is the length of the message, then we need to keep querying.
    keep_querying <- !any(sapply(post_res$candidates, function(x) x$finishReason) == "STOP")
    # And update the messages sent to Gemini, in order to continue the current session.
    parsed_response <- parse_response(list(post_res), verbosity = 0)
    if (nchar(parsed_response) == 0) {
      stop("Gemini didn't send a reply.")
    }
    messages <- append(
      append(messages, list(list(role = "model", parts = list(text = parsed_response)))),
      list(list(role = "user", parts = list(text = "continue")))
    )
  }
  final_res
}
