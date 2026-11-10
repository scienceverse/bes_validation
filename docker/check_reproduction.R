# Rebuild the manuscript from the data and compare it with the published version.
#
# From the project folder:
#   Rscript docker/check_reproduction.R [published.pdf]
#
# 1. Deletes every stored result of the pipeline (targets::tar_destroy()), so nothing
#    is reused from an earlier run.
# 2. Runs the whole pipeline (targets::tar_make()), which computes all results from
#    data/metacheck_results.rds and the other files in data/, and renders the
#    manuscript and the appendix.
# 3. Renders the verification document, which recomputes every number independently.
# 4. Compares every number in the text of the newly rendered manuscript with the
#    published PDF, in order. The running head on each page is left out, because it
#    holds the page number, and page breaks can differ when another font is used.
#
# The published PDF defaults to /home/rstudio/published/manuscript.pdf, the copy the
# Docker image keeps of documentation/manuscript.pdf as it was in the repository.
# Exits with status 1 when a number differs, so a Docker build that runs this fails.

args <- commandArgs(TRUE)
published <- if (length(args)) args[1] else "/home/rstudio/published/manuscript.pdf"
if (!file.exists(published)) stop("Published PDF not found: ", published)
reference <- tempfile(fileext = ".pdf")
file.copy(published, reference)

numbers_in <- function(pdf) {
  pages <- pdftools::pdf_text(pdf)
  body <- vapply(pages, function(p) {
    lines <- strsplit(p, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    paste(lines[-1], collapse = " ")
  }, "")
  text <- gsub("[[:space:]]+", " ", paste(body, collapse = " "))
  regmatches(text, gregexpr("[0-9]+(?:[.,][0-9]+)*%?", text, perl = TRUE))[[1]]
}

started <- Sys.time()
targets::tar_destroy(ask = FALSE)
# Remove the rendered documents first, so that a render that fails without an error
# cannot leave the old PDF in place to be compared with itself.
outputs <- c("documentation/manuscript.pdf", "documentation/appendix.pdf", "documentation/manuscript_verification.pdf")
unlink(outputs)
targets::tar_make()
quarto::quarto_render("documentation/manuscript_verification.qmd")
missing <- outputs[!file.exists(outputs)]
if (length(missing)) stop("Not rendered: ", paste(missing, collapse = ", "))
minutes <- as.numeric(difftime(Sys.time(), started, units = "mins"))

published_numbers <- numbers_in(reference)
rebuilt_numbers <- numbers_in("documentation/manuscript.pdf")
n <- min(length(published_numbers), length(rebuilt_numbers))
differ <- which(published_numbers[seq_len(n)] != rebuilt_numbers[seq_len(n)])

cat(sprintf("\nRebuilt the pipeline, manuscript, appendix and verification in %.1f minutes.\n", minutes))
if (length(published_numbers) == length(rebuilt_numbers) && length(differ) == 0) {
  cat(sprintf("REPRODUCED: all %d numbers in the rebuilt manuscript are identical to the published version.\n",
              length(published_numbers)))
} else {
  cat(sprintf("NOT REPRODUCED: the published manuscript has %d numbers, the rebuilt one %d; %d of the first %d differ.\n",
              length(published_numbers), length(rebuilt_numbers), length(differ), n))
  for (i in head(differ, 10)) {
    cat(sprintf("  number %d: published %s, rebuilt %s\n", i, published_numbers[i], rebuilt_numbers[i]))
  }
  quit(status = 1)
}
