#' Get Chat Session
#'
#' @param session_id The ID of the session to be used.
#'
#' @export
#'
get_chat_session <- function(session_id = "1") {
  get("chat_session_messages", envir = .state)[[as.character(session_id)]]
}
