#!/usr/bin/r

library(jsonlite, quietly = TRUE)
library(futile.logger, quietly = TRUE)
library(ulid, quietly = TRUE)


# Get the name of the nearest function call on the stack
parentFunc <- function(where) {
  theFunction <- tryCatch(
    deparse(sys.call(where - 1)[[1]]),
    error = function(e) "(shell)")
  ifelse(
    theFunction != "NULL" && length(grep("flog\\.", theFunction)) == 0,
    theFunction,
    "(shell)")
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

# We don't need the first 3 zeros (won't rollover until 26th century?)
# Also throw away last 5 entropy bytes.
# Return lowercase with prefix
makeUlid <- function(prefix, ts = NULL) {
  ulid <- if (is.null(ts)) ulid::ULIDgenerate() else ulid::ts_generate(as.POSIXct(ts))
  paste0(prefix, tolower(substr(ulid, 4, 22)))
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
  flog.error(args[[1]], stop_context = caller,
    message_source = message_source,
    halted = TRUE)
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

# Like simpleError
makeTryError <- function(message, call = NULL, ...) {
  structure(
    list(
      message = as.character(message),
      call = call,
      ...
    ),
    class = c("try-error", "error", "condition")
  )
}

# Generates a list object, then converts it to JSON and outputs it
# msg is usually a character vector, but can also be a condition passed to warning, stop, etc.
jsonLayout <- function(level, msg, id = "", ...) {
  if (is(msg, "condition")) {
    msg <- msg$message
  }
  # Get name of the function 3 deep in the call stack
  theFunction <- parentFunc(-3)
  output_list <- list(
    level = jsonlite::unbox(names(level)),
    timestamp = jsonlite::unbox(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    session_id = jsonlite::unbox(getOption("session_id")),
    command_id = jsonlite::unbox(getOption("command_id")),
    main_pid = jsonlite::unbox(getOption("main_pid")),
    phase = jsonlite::unbox(getOption("phase")),
    pid = jsonlite::unbox(Sys.getpid()),
    message = jsonlite::unbox(msg),
    func = jsonlite::unbox(theFunction),
    additional = ...
  )
  paste0(jsonlite::toJSON(output_list, simplifyVector = TRUE, auto_unbox = TRUE), "\n")
}

# Save logging context in R options and set up logging to JSON file
initLogging <- function(context, logPath) {
  options(context)
  threshold <- flog.threshold("TRACE")
  layout <- flog.layout(jsonLayout)
  appender <- flog.appender(appender.tee(logPath))
  invisible(TRUE)
}

# Write args to JSON file
writeArgs <- function(context, args, argPath) {
  writeLines(jsonlite::toJSON(list(context = list(
    script_name = jsonlite::unbox(context$script_name),
    session_id = jsonlite::unbox(context$session_id),
    command_id = jsonlite::unbox(context$command_id),
    main_pid = jsonlite::unbox(context$main_pid),
    phase = jsonlite::unbox(context$phase)),
    args = args), pretty = TRUE), con = argPath)
  invisible(TRUE)
}

# Parse command line args portably, whether from littler or Rscript
parseArgs <- function() {
  if (exists("argv")) {
    # For littler, use argv
    if (length(argv) > 0) {
      return(as.character(argv))
    }
    return(list())
  }
  # For Rscript, pick up any args after --args
  commandArgs(trailingOnly = TRUE)
}

# Create a session environment
makeSessionEnv <- function(sessionId, demo = FALSE, restart = FALSE, clean = FALSE) {
  sessionEnv <- new.env(parent = emptyenv())
  sessionEnv$createdAt <- as.integer(Sys.time())
  sessionEnv$sessionId <- sessionId
  sessionEnv$sessName <- paste0("Session ", as.character(sessionEnv$createdAt))
  sessionEnv$userId <- "anonymous"
  sessionEnv$userEmail <- "anonymous@daptics.ai"
  sessionEnv$firstName <- "Anonymous"
  sessionEnv$lastName <- "User"
  sessionEnv$hostname <- Sys.info()[[4]]
  sessionEnv$workingDir <- getwd()
  sessionEnv$pathname <- dirname(sessionEnv$workingDir)
  sessionEnv$publicDir <- file.path(sessionEnv$pathname, "www")
  sessionEnv$logDir <- file.path(sessionEnv$workingDir, "output")
  sessionEnv$sessTag <- basename(sessionEnv$pathname)
  versionFile <- file.path(sessionEnv$publicDir, "git-version")
  sessionEnv$version <- readVersion(versionFile)
  sessionEnv$startupFlags <- list(demo = demo, restart = restart, clean = clean)
  sessionEnv$loginUri <- NA
  sessionEnv$apiBaseUri <- NA
  sessionEnv$privBaseUri <- NA
  sessionEnv$apiKey <- NA
  save(sessionEnv, file = "SessionEnv.RData")
  sessionEnv
}

# Script common startup
# args[[1]] is the session id
# args[[2]] is the command id
startup <- function(scriptName, args) {
  sessionId <- if (length(args) >= 1) args[[1]] else makeUlid("S")
  commandId <- if (length(args) >= 2) args[[2]] else makeUlid("C")
  sessionEnv <<- makeSessionEnv(sessionId)
  dir.create(sessionEnv$logDir, showWarnings = FALSE, recursive = TRUE)
  logPath <- file.path(sessionEnv$logDir, paste0(commandId, "_", scriptName, "_log.json"))
  argPath <- file.path(sessionEnv$logDir, paste0(commandId, "_", scriptName, "_arg.json"))
  # Set up logging and arg file
  context <- list(
    script_name = scriptName,
    session_id = sessionId,
    command_id = commandId,
    main_pid = Sys.getpid(),
    phase = "allocating")
  initLogging(context, logPath)
  writeArgs(context, args, argPath)
  invisible(TRUE)
}

#' Read a single line of text from the specified location, or return the default if the
#' file cannot be read.
#' @param path The file location.
#' @param default The string to return if the file can not be read.
#' @return A string: either the first line of text in the specified file, or the default.
readVersion <- function(path, default = "<unknown>") {
  if (file.exists(path)) { return(readLines(path, n = 1)) }
  default
}

# Read JSON data from stdin
readJsonChars <- function() {
  rtn <- NULL
  con <- file("stdin")
  open(con)
  strDataLength <- readLines(con = con, n = 1)
  if (length(strDataLength) > 0 && !is.na(strDataLength)) {
    # Include terminating NUL
    nchars <- as.integer(strDataLength) + 1
    flog.info(paste("reading", nchars, "bytes..."))
    strData <- readChar(con = con, nchars, useBytes = TRUE)
    rtn <- try(jsonlite::fromJSON(strData), silent = TRUE)
  } else {
    call <- sys.call()
    rtn <- makeTryError("No input received", call = call)
  }
  close(con)
  if (is(rtn, "try-error")) {
    flog.error(rtn$message)
  }
  rtn
}

# MAIN SCRIPT
args <- parseArgs()
startup("testJsonChars", args)
flog.info("starting", args = args)
jsonData <- readJsonChars()
if (!is(jsonData, "try-error")) {
  result <- list(a = 1, b = "hello", data = jsonData)
  flog.info("parsed", result = result)
}
