#!/usr/bin/r

library(jsonlite, quietly = TRUE)

# Read JSON data from stdin
readJsonLines <- function() {
  f <- file("stdin")
  open(f)
  strData <- readLines(con = f, n = 1)
  if (length(strData) > 0) {
    close(f)
    jsonData <- try(jsonlite::fromJSON(strData), silent = TRUE)
    return(jsonData)
  } else {
    return(try(stop("No input received"), silent = TRUE))
  }
}

# For littler, use argv
# For Rscript, pick up any args after --args
args <- if (exists("argv")) argv else commandArgs(trailingOnly = TRUE)
cat(paste("args:", paste(args, collapse = " "), "\n"))

jsonData <- readJsonLines()
if (is(jsonData, "try-error")) {
  cat("Missing or invalid data")
} else {
  cat("Parsed!")
}
