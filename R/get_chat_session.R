#' Get Chat Session
#'
#' @export
#'
get_chat_session <- function() {
  get("chat_session_messages", envir = .state)
}
