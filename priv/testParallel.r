#/usr/bin/r

library(parallel, quietly = TRUE)
library(doParallel, quietly = TRUE)

singleResult <- function(i) {
  cat(paste("singleResult(", i, "), os_pid:", Sys.getpid(), "\n"))
  Sys.sleep(20)
  i
}

parallelTest <- function(ncores) {
  foreach(i = 1:ncores) %dopar% singleResult(i)
}

cat(paste("test.r, os_pid:", Sys.getpid(), "\n"))

# For littler, use argv
# For Rscript, pick up any args after --args
args <- if (exists("argv")) argv else commandArgs(trailingOnly = TRUE)
cat(paste("args:", paste(args, collapse = " "), "\n"))

ncores <- detectCores()
cat(paste("cores:", ncores, "\n"))

registerDoParallel(ncores)
res <- parallelTest(ncores)
cat(paste("results:", paste(res, collapse = " "), "\n"))
