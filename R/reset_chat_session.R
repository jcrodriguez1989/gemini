#' Reset Chat Session
#'
#' This function is intended to be used with `ask_gemini`. If we are using `ask_gemini` to chat
#' with Gemini, and we want to start a new conversation, we must call `reset_chat_session`.
#'
#' @param session An optional session to set.
#' @param session_id The ID of the session to be used. If `NULL`, this function will have no effect.
#'
#' @export
#'
reset_chat_session <- function(session = list(), session_id = "1") {
  if (is.null(session_id)) {
    return()
  }
  all_sessions <- get("chat_session_messages", envir = .state)
  all_sessions[[as.character(session_id)]] <- session
  assign("chat_session_messages", all_sessions, envir = .state)
}
