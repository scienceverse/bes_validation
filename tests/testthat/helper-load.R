# Loaded automatically by testthat before the test files.
#
# This project is not an R package, so there is no package namespace to
# load: this sources every pipeline function in R/ (the same files
# _targets.R loads with tar_source("code/R")) and gives the tests helpers to find
# project files and pipeline results.
#
# Three kinds of tests live in this folder:
#   - unit tests: small hand-made inputs with hand-worked answers; always run.
#   - pipeline tests: read results from the targets store (_targets/) and the
#     project's CSV files; skipped when the store is missing. Run
#     targets::tar_make() first so they test current results.
#   - corpus tests: read data/metacheck_results.rds; skipped when it is missing.

suppressPackageStartupMessages({
  library(dplyr)
  library(metacheck)
})

proj <- function(...) {
  normalizePath(file.path(testthat::test_path(), "..", "..", ...), winslash = "/", mustWork = FALSE)
}

for (f in list.files(proj("code/R"), pattern = "[.]R$", full.names = TRUE)) source(f, local = FALSE)

N_PAPERS <- 1861L

skip_if_no_store <- function() {
  testthat::skip_if_not(dir.exists(proj("_targets")), "no targets store; run targets::tar_make() first")
}

tar <- function(name) {
  targets::tar_read_raw(name, store = proj("_targets"))
}

cooper_csv <- function() proj("data/cooper_coded_data.csv")
sample_csv <- function() proj("data/papers_data.csv")

# One module's results from the result file.
load_corpus <- function(part) {
  path <- proj("data/metacheck_results.rds")
  testthat::skip_if_not(file.exists(path), "no data/metacheck_results.rds")
  module_results(readRDS(path), paste0(part, "_check"))
}

# The manuscript's own R code, evaluated chunk by chunk, so the tests check
# exactly what the manuscript computes rather than a copy of it. Only the
# chunks named are evaluated, in order, in one environment (chunks that write
# files, such as the bibliography and session info, are never run).
manuscript_env <- local({
  cache <- NULL
  function(labels = c("data", "license-value-check", "data-code-archiving-counts",
                      "full-317-audit-load", "a9-recheck-230-setup",
                      "a7-reproducibility-load", "a10-metacheck-limitations-load",
                      "a10-not-classified-load")) {
    if (!is.null(cache)) return(cache)
    lines <- unlist(lapply(c("manuscript", "appendix"), function(doc) {
      rfile <- tempfile(fileext = ".R")
      suppressMessages(knitr::purl(proj("documentation", paste0(doc, ".qmd")), output = rfile, documentation = 1, quiet = TRUE))
      readLines(rfile, warn = FALSE)
    }))
    starts <- grep("^## ----", lines)
    ends <- c(starts[-1] - 1, length(lines))
    # Quarto chunks carry their label as a "#| label: ..." line inside the
    # chunk, which purl() keeps; its own header lines are unlabelled.
    chunk_label <- mapply(function(s, e) {
      l <- grep("^#\\| *label:", lines[s:e], value = TRUE)
      if (length(l)) trimws(sub("^#\\| *label:", "", l[1])) else NA_character_
    }, starts, ends)
    env <- new.env(parent = globalenv())
    old <- setwd(proj()); on.exit(setwd(old))
    for (lab in labels) {
      i <- match(lab, chunk_label)
      if (is.na(i)) stop("manuscript chunk not found: ", lab)
      eval(parse(text = lines[starts[i]:ends[i]]), envir = env)
    }
    cache <<- env
    env
  }
})

# The abstract and body text of the manuscript and the appendix as one string.
manuscript_text <- function() paste(unlist(lapply(c("manuscript", "appendix"), function(doc) {
  readLines(proj("documentation", paste0(doc, ".qmd")), warn = FALSE)
})), collapse = "\n")
