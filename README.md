
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Gemini coding assistant for RStudio

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/gemini)](https://CRAN.R-project.org/package=gemini) -->
<!-- [![R-CMD-check](https://github.com/jcrodriguez1989/gemini/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcrodriguez1989/gemini/actions/workflows/R-CMD-check.yaml) -->
<!-- [![CRAN downloads](https://cranlogs.r-pkg.org/badges/gemini)](https://cran.rstudio.com/web/packages/gemini/index.html) -->
<!-- badges: end -->
<center>
<img width="300" height="400" src="man/figures/gemini_meme.jpeg">
<p>
Meme by Programming Jokes I IT Humor & Memes
</p>
</center>

## Installation

Install the current released version of `{gemini}` from
[CRAN](https://cran.r-project.org/package=gemini):

``` r
install.packages("gemini")
```

Or install the development version from
[GitHub](https://github.com/jcrodriguez1989/gemini) with:

``` r
# install.packages("remotes")
remotes::install_github("jcrodriguez1989/gemini")
```

## Requirements

You need to setup your Gemini API key in R.

First you will need to obtain your Gemini API key. You can create an API
key by accessing [Gemini API
page](https://aistudio.google.com/app/apikey).

Then you have to assign your API key for usage in R, this can be done
just for the actual session, by doing:

``` r
Sys.setenv(GEMINI_API_KEY = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
```

Or you can do it persistent (session-wide), by assigning it in your
`.Renviron` file. For it, execute `usethis::edit_r_environ()`, and in
that file write a line at the end your API key as

``` r
GEMINI_API_KEY=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```
