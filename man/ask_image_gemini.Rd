% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ask_image_gemini.R
\name{ask_image_gemini}
\alias{ask_image_gemini}
\title{Ask Image Gemini}
\usage{
ask_image_gemini(
  question,
  image,
  session_id = "1",
  gemini_api_key = Sys.getenv("GEMINI_API_KEY")
)
}
\arguments{
\item{question}{The question to ask Gemini.}

\item{image}{The path or URL of the image.}

\item{session_id}{The ID of the session to be used.}

\item{gemini_api_key}{Gemini's API key.}
}
\value{
A character value with the response generated by Gemini.
}
\description{
Ask Image Gemini
}
\examples{
\dontrun{
cat(ask_image_gemini(
  "What is this picture?",
  "https://storage.googleapis.com/generativeai-downloads/images/scones.jpg"
))
}

}
