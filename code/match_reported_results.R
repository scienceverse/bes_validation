# Compare the statistics in each paper with the statistics the paper's code printed,
# and save the comparison in one file, data/match_reported_results.rds.
#
# reproducibility_check does this itself, but in metacheck commit e62da2e the comparison
# stopped on a list of one paper and the module hid the error, so the run saved the
# statistics without the comparison. This script runs the same function on the saved
# statistics. No paper's code is run again.
#
# NEEDS: metacheck with the fix in match_reported_output(), the private corpus
# data/bes.rds, and the run's folder with one file per paper.
#
# Usage, from the repository root:
#   Rscript code/match_reported_results.R
# Optional: --out DIR, the run's folder (default D:/bes_validation_run/final), and
# --rerun DIR, the folder of a rerun of some papers, whose files then take precedence.

suppressMessages(library(metacheck))
args <- commandArgs(TRUE)
i <- match("--out", args)
run_dir <- if (is.na(i)) "D:/bes_validation_run/final" else args[i + 1]
i <- match("--rerun", args)
# --rerun may name several folders (every argument after it up to the next --flag).
# They are listed after run_dir, so a later folder's file wins the de-duplication
# below; order the reruns oldest first so the most recent one takes precedence.
rerun_dir <- if (is.na(i)) NULL else {
  rest <- args[-(1:i)]
  stop_at <- grep("^--", rest)
  if (length(stop_at)) rest[seq_len(stop_at[1] - 1)] else rest
}

paper_files <- list.files(file.path(c(run_dir, rerun_dir), "papers"), pattern = "[.]rds$", full.names = TRUE)
paper_files <- paper_files[!duplicated(basename(paper_files), fromLast = TRUE)]
bes <- readRDS("data/bes.rds")

matches <- list()
n_with_output <- 0L
for (f in paper_files) {
  id <- sub("[.]rds$", "", basename(f))
  stat_output <- readRDS(f)$modules$reproducibility_check$stat_output
  if (length(stat_output) == 0) next
  n_with_output <- n_with_output + 1L
  matched <- metacheck:::match_reported_output(bes[id], stat_output, include_tables = TRUE)
  if (!is.null(matched) && nrow(matched) > 0) {
    matched$paper_id <- id
    matches[[id]] <- matched
  }
}

match_table <- dplyr::bind_rows(matches)
info <- utils::packageDescription("metacheck")
saveRDS(list(
  match_table = match_table,
  n_papers_with_output = n_with_output,
  metacheck_version = info$Version,
  metacheck_sha = info$RemoteSha
), "data/match_reported_results.rds")
cat(sprintf("%d papers with output, %d with reported results to compare, %d results, %d found.\n",
            n_with_output, length(matches), nrow(match_table), sum(match_table$found %in% TRUE)))
