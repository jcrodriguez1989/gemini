library("gemini")
library("telegram.bot") # https://ebeneditos.github.io/telegram.bot/

# cancu_testing_bot
bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")

bot <- Bot(token = bot_token)
updater <- Updater(token = bot_token)
updater <- updater + CommandHandler("reset_bot_session", function(bot, update) {
  reset_chat_session()
  bot$sendMessage(chat_id = update$message$chat_id, text = "Bot session reset")
})

### Chat text

chat_text <- function(bot, update) {
  my_reply <- try(ask_gemini(update$message$text))
  if (inherits(my_reply, "try-error")) {
    my_reply <- "An error occurred"
  }
  bot$sendMessage(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_text, MessageFilters$text)

### Chat image
chat_image <- function(bot, update) {
  photo_id <- update$message$photo[[
    which.max(sapply(update$message$photo, function(x) x$file_size))
  ]]$file_id
  photo_path <- tempfile()
  bot$get_file(photo_id, photo_path)
  my_reply <- try(ask_image_gemini(update$message$caption, photo_path))
  if (inherits(my_reply, "try-error")) {
    my_reply <- "An error occurred"
  }
  unlink(photo_path)
  bot$sendMessage(chat_id = update$message$chat_id, text = my_reply)
}
updater <- updater + MessageHandler(chat_image, MessageFilters$photo)

updater$start_polling()
