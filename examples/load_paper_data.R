# Read the installed paper data, preserving the supplied split and categories.
# Usage: paper <- source("examples/load_paper_data.R")$value
local({
  folder <- system.file("extdata", "paper", package = "SPADES.IND")
  if (!nzchar(folder)) stop("Install the package containing the paper data first.")
  files <- c("training_data.csv", "test_real_1000.csv")
  paths <- file.path(folder, files)
  expected <- c("07131c4af34dd46d4e45b91139e8e18d", "02d72a44c49529b065f82d824e9223b7")
  if (!all(file.exists(paths))) stop("Paper input files not found.")
  if (!identical(unname(tools::md5sum(paths)), expected)) stop("Paper input checksum mismatch.")
  read_one <- function(path) {
    d <- utils::read.csv(path, colClasses="character", na.strings=NULL,
                         check.names=FALSE, stringsAsFactors=FALSE)
    d$age <- as.integer(d$age)
    if (anyNA(d$age)) stop("Invalid age values.")
    d
  }
  training <- read_one(paths[1L])
  holdout <- read_one(paths[2L])
  stopifnot(nrow(training)==9000L, nrow(holdout)==1000L,
            ncol(training)==8L, identical(names(training),names(holdout)))
  cat("PAPER DATA | training: 9000 | holdout: 1000 | columns: 8 | checksums: OK\n")
  list(training=training, holdout=holdout, target="diagnosis",
       mode="classification", source_directory=folder)
})
