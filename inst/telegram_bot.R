#!/usr/bin/env Rscript
library("telegram.bot") # https://ebeneditos.github.io/telegram.bot/

box::use(
  dplyr[filter, tribble],
  gemini[ask_gemini, ask_image_gemini, reset_chat_session],
  glue[glue],
  jsonlite[toJSON],
  logger[log_info]
)

# First argument is the bot token.
args <- commandArgs(trailingOnly = TRUE)
bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
if (length(args) > 0) {
  bot_token <- args[[1]]
}
# Second argument is the telegram bot's admin ID.
admin_chat_id <- Sys.getenv("TELEGRAM_ADMIN_ID")
if (length(args) > 1) {
  admin_chat_id <- args[[2]]
}

Sys.setenv(GEMINI_VERBOSE = FALSE)

log_info("Running bot ", bot_token, " ; With admin ", admin_chat_id)

bot <- Bot(token = bot_token)
updater <- Updater(token = bot_token)

# Get the msg in the desired language.
get_message <- function(msg_code, language, params = NULL) {
  msgs <- tribble(
    ~msg_code, ~language, ~msg,
    "hello", "es", "¡Hola {params[[1]]}! En que puedo ayudarte?",
    "hello", "en", "Hello {params[[1]]}! how can I help you?",
    "reset_session", "es", "Sesion reiniciada.",
    "reset_session", "en", "Session restarted.",
    "buy_premium", "es", paste0(
      "Gracias por tu interés, nos contactaremos contigo cuando las funcionalidades premium estén ",
      "disponibles."
    ),
    "buy_premium", "en", paste0(
      "Thank you for your interest, we will contact you when premium features are available."
    ),
    "error", "es", "Ocurrió un error, intenta de nuevo.",
    "error", "en", "An error occurred, please try again.",
    "offer_premium", "es", paste0(
      "Este tipo de mensaje solo está disponible para usuarios premium, responde /premium para ",
      "comprar este servicio."
    ),
    "offer_premium", "en", paste0(
      "This type of message is only available for premium users, reply /premium to purchase this ",
      "service."
    )
  )
  if (language %in% msgs$language) {
    res <- glue(filter(msgs, msg_code == !!msg_code, language == !!language)$msg)
  } else {
    res <- glue(filter(msgs, msg_code == !!msg_code, language == "en")$msg)
    reset_chat_session(session_id = "-1")
    res <- ask_gemini(glue("Translate the following msg into {language}: {res}"), "-1")
  }
  res
}

### Start
updater <- updater + CommandHandler("start", function(bot, update) {
  log_info("[start] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  first_name <- update$message$from$first_name
  bot$send_message(
    chat_id = update$message$chat_id,
    text = get_message("hello", update$message$from$language_code, first_name)
  )
})

### Reset bot session
updater <- updater + CommandHandler("reset_bot_session", function(bot, update) {
  log_info("[reset_bot_session] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  reset_chat_session(session_id = update$message$from$id)
  bot$send_message(
    chat_id = update$message$chat_id,
    text = get_message("reset_session", update$message$from$language_code)
  )
})

### Buy premium
updater <- updater + CommandHandler("premium", function(bot, update) {
  log_info("[premium] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  if (nchar(admin_chat_id) > 0) {
    bot$send_message(
      chat_id = admin_chat_id,
      text = paste(
        "PREMIUM REQUEST", toJSON(update$message$from, auto_unbox = TRUE), update$message$chat_id,
        sep = "\n"
      )
    )
  }
  bot$send_message(
    chat_id = update$message$chat_id,
    text = get_message("buy_premium", update$message$from$language_code)
  )
})

### Chat text

chat_text <- function(bot, update) {
  log_info("[chat_text] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  my_reply <- try(ask_gemini(update$message$text, session_id = update$message$from$id))
  if (inherits(my_reply, "try-error")) {
    my_reply <- get_message("error", update$message$from$language_code)
  }
  log_info("[chat_text - reply] - {my_reply}")
  bot$send_message(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_text, MessageFilters$text)

### Chat image
chat_image <- function(bot, update) {
  log_info("[chat_image] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  photo_id <- update$message$photo[[
    which.max(sapply(update$message$photo, function(x) x$file_size))
  ]]$file_id
  photo_path <- tempfile()
  bot$get_file(photo_id, photo_path)
  question <- ""
  if (!is.null(update$message$caption)) {
    question <- update$message$caption
  }
  my_reply <- try(ask_image_gemini(question, photo_path, session_id = update$message$from$id))
  if (inherits(my_reply, "try-error")) {
    my_reply <- get_message("error", update$message$from$language_code)
  }
  unlink(photo_path)
  log_info("[chat_image - reply] - {my_reply}")
  bot$send_message(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_image, MessageFilters$photo)

### Unavailable handlers
chat_unavailable <- function(bot, update) {
  log_info("[chat_unavailable] - {as.character(toJSON(update$message, auto_unbox = TRUE))}")
  bot$send_message(
    chat_id = update$message$chat_id,
    text = get_message("offer_premium", update$message$from$language_code)
  )
}
updater <- updater + MessageHandler(chat_unavailable)

updater$start_polling()
