#' Get Chat Session
#'
#' @param session_id The ID of the session to be used. If `NULL`, it will return an empty session.
#'
#' @export
#'
get_chat_session <- function(session_id = "1") {
  if (is.null(session_id)) {
    return(NULL)
  }
  get("chat_session_messages", envir = .state)[[as.character(session_id)]]
}
