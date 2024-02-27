#' Reset Chat Session
#'
#' This function is intended to be used with `ask_gemini`. If we are using `ask_gemini` to chat
#' with Gemini, and we want to start a new conversation, we must call `reset_chat_session`.
#'
#' @param session An optional session to set.
#'
#' @export
#'
reset_chat_session <- function(session = list()) {
  assign("chat_session_messages", session, envir = .state)
}
