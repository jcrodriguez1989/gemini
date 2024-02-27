library("telegram.bot") # https://ebeneditos.github.io/telegram.bot/

box::use(
  gemini[ask_gemini, ask_image_gemini, reset_chat_session],
  jsonlite[toJSON]
)

# cancu_testing_bot
bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")

bot <- Bot(token = bot_token)
updater <- Updater(token = bot_token)

### Reset bot session
updater <- updater + CommandHandler("reset_bot_session", function(bot, update) {
  reset_chat_session(session_id = update$message$from$id)
  bot$send_message(chat_id = update$message$chat_id, text = "Sesion reiniciada")
})

### Buy premium
updater <- updater + CommandHandler("premium", function(bot, update) {
  admin_chat_id <- Sys.getenv("TELEGRAM_ADMIN_ID")
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
    text = paste0(
      "Gracias por tu interés, nos contactaremos contigo cuando las funcionalidades premium estén ",
      "disponibles."
    )
  )
})

### Chat text

chat_text <- function(bot, update) {
  my_reply <- try(ask_gemini(update$message$text, session_id = update$message$from$id))
  if (inherits(my_reply, "try-error")) {
    my_reply <- "Ocurrió un error"
  }
  bot$send_message(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_text, MessageFilters$text)

### Chat image
chat_image <- function(bot, update) {
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
    my_reply <- "Ocurrió un error"
  }
  unlink(photo_path)
  bot$send_message(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_image, MessageFilters$photo)

### Unavailable handlers
chat_unavailable <- function(bot, update) {
  bot$send_message(
    chat_id = update$message$chat_id,
    text = paste0(
      "Este tipo de mensaje solo esta disponible para usuarios premium, ",
      "responde /premium para comprar este servicio."
    )
  )
}
updater <- updater + MessageHandler(chat_unavailable)

updater$start_polling()
