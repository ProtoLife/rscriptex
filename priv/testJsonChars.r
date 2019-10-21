#!/usr/bin/r

library(jsonlite, quietly = TRUE)
library(futile.logger, quietly = TRUE)

parentFunc <- function(.where) {
  theFunction <- tryCatch(deparse(sys.call(.where - 1)[[1]]),
        error = function(e) "(shell)")
  theFunction <- ifelse(
    length(grep("flog\\.", theFunction)) == 0, theFunction, "(shell)")
  theFunction
}


# Set up message, warning, stop, stopifnot
message <- function(..., domain = NULL, appendLF = TRUE) {
  args <- list(...)
  flog.info(args[[1]], message_source = "stop")
  base::message(unlist(args), domain = domain, appendLF = appendLF)
}

warning <- function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL) {
  args <- list(...)
  flog.warn(args[[1]], message_source = "warning")
  base::warning(unlist(args), call. = call., immediate. = immediate.,
    noBreaks. = noBreaks., domain = domain)
}

stop <- function(..., call. = FALSE, domain = NULL, which_func = 1, message_source = "stop") {
  args <- list(...)
  which <- sys.nframe() - which_func
  caller <- if (which <= 0) "(shell)" else deparse(sys.calls()[[which]])
  flog.error(args[[1]], stop_context = jsonlite::unbox(caller),
    message_source = message_source,
    halted = jsonlite::unbox(TRUE))
  base::stop(unlist(args), call. = call., domain = domain)
}

stopifnot <- function (...) {
  n <- length(args <- list(...))
  if (n == 0L)
    return(invisible())
  for (i in 1L:n) if (!(is.logical(r <- .subset2(args, i)) && !anyNA(r) && all(r))) {
    mc <- match.call()
    ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
    if (length(ch) > 1L)
      ch <- paste(ch[1L], "....")
    stop(sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"),
      ch), call. = FALSE, domain = NA, which_func = 2, message_source = "stopifnot")
  }
  invisible()
}

makeRandomString <- function(n = 1, length = 12) {
  randomString <- c(1:n)
  for (i in 1:n) {
    randomString[i] <- paste(
      sample(c(0:9, letters, LETTERS), length, replace = TRUE),
      collapse = "")
  }
  randomString
}


# Generates a list object, then converts it to JSON and outputs it
jsonLayout <- function(level, msg, id = "", ...) {
  # Get name of the function 3 deep in the call stack
  theFunction <- parentFunc(-3)
  output_list <- list(
    level = jsonlite::unbox(names(level)),
    timestamp = jsonlite::unbox(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    session_id = jsonlite::unbox(sessionId),
    job_id = jsonlite::unbox(jobId),
    pid = jsonlite::unbox(Sys.getpid()),
    message = jsonlite::unbox(msg),
    func = jsonlite::unbox(theFunction),
    additional = ...
  )
  paste0(jsonlite::toJSON(output_list, simplifyVector = TRUE, auto_unbox = TRUE), "\n")
}


# MAIN SCRIPT SETUP
# For littler, use argv
# For Rscript, pick up any args after --args
# args[[1]] is the session id
# args[[2]] is the job id
# args[[3]] is the log file path
args <- if (exists("argv")) argv else commandArgs(trailingOnly = TRUE)
sessionId <<- if (length(args) >= 1) args[[1]] else paste0("S.", makeRandomString())
jobId <<- if (length(args) >= 2) args[[2]] else paste0("J.", makeRandomString())
logPath <<- if (length(args) >= 3) args[[3]] else "./log"
# Set up logging
threshold <- flog.threshold("TRACE")
layout <- flog.layout(jsonLayout)
appender <- flog.appender(appender.tee(logPath))

# MAIN SCRIPT CUSTOM LOGIC
# Read JSON data from stdin
readJsonChars <- function() {
  f <- file("stdin")
  open(f)
  strDataLength <- readLines(con = f, n = 1)
  if (length(strDataLength) > 0) {
    # Include terminating NUL
    nchars <- as.integer(strDataLength) + 1
    flog.info(paste("reading", nchars, "bytes..."))
    strData <- readChar(con = f, nchars, useBytes = TRUE)
    close(f)
    jsonData <- try(jsonlite::fromJSON(strData), silent = TRUE)
    return(jsonData)
  } else {
    return(try(stop("No input received"), silent = TRUE))
  }
}

flog.info("starting", args = args)
jsonData <- readJsonChars()
if (is(jsonData, "try-error")) {
  flog.error("Missing or invalid data")
} else {
  result <- list(a = 1, b = "hello")
  flog.info("parsed", result = result)
}
