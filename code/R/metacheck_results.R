# Reads data/metacheck_results.rds, the single result file of code/run_metacheck.R.
#
# The file holds run_info, one summary_table (one row per paper, the columns of
# all modules together), one list of tables per module, and errors. Each paper
# ran one chain of modules, so ODDPub's columns that share a name with a column
# of open_practices carry the suffix ".oddpub" in that one summary_table.
#
# module_results() returns one module's tables with the summary_table added, so
# the functions in this folder can read res_repo_check$table,
# res_code_check$summary_table and so on.

module_results <- function(metacheck_results, module) {
  stopifnot(module %in% names(metacheck_results))
  st <- metacheck_results$summary_table
  if (identical(module, "oddpub")) {
    st <- dplyr::select(st, paper_id, data_open = data_open.oddpub, code_open = code_open.oddpub)
  }
  c(metacheck_results[[module]], list(summary_table = st))
}

# Papers and scripts of reproducibility_check, under the names Appendix A.7 reads, with
# the counts of the comparison in data/match_reported_results.rds (see
# code/match_reported_results.R), which the run itself could not save.
repro_results <- function(res_reproducibility_check, match_table) {
  counts <- dplyr::summarise(match_table, n = dplyr::n(), k = sum(found %in% TRUE), .by = paper_id)
  st <- res_reproducibility_check$summary_table
  at <- match(st$paper_id, counts$paper_id)
  st$repro_tests_reported <- counts$n[at]
  st$repro_tests_matched <- counts$k[at]
  list(table = res_reproducibility_check$table, summary_table = st)
}

# The failed packages named in one paper's report. The report is a Quarto-markdown
# character vector, with this sentence as one element of it:
#   "**Failed:** pkg [category] (reason); pkg [category] (reason)"
# Matching within one element keeps a match from running on into unrelated text.
parse_failed_deps <- function(report_lines) {
  hit <- grep("\\*\\*Failed:\\*\\*", report_lines, value = TRUE)
  if (length(hit) == 0) return(NULL)
  failed_txt <- unlist(lapply(hit, function(line) {
    m <- regmatches(line, regexpr("\\*\\*Failed:\\*\\*\\s*(.*)$", line))
    if (length(m) == 0 || !nzchar(m)) return(character(0))
    sub("^\\*\\*Failed:\\*\\*\\s*", "", m)
  }))
  if (length(failed_txt) == 0) return(NULL)
  entries <- trimws(strsplit(paste(failed_txt, collapse = "; "), ";\\s*")[[1]])
  entries <- entries[nzchar(entries)]
  if (length(entries) == 0) return(NULL)
  pkg <- trimws(sub("^([^\\[(]+).*$", "\\1", entries))
  has_cat <- grepl("\\[[^]]+\\]", entries)
  category <- rep(NA_character_, length(entries))
  category[has_cat] <- gsub("^\\[|\\]$", "", regmatches(entries, regexpr("\\[[^]]+\\]", entries)))
  has_reason <- grepl("\\([^)]+\\)\\s*$", entries)
  reason <- rep(NA_character_, length(entries))
  reason[has_reason] <- gsub("^\\(|\\)$", "", trimws(regmatches(entries, regexpr("\\([^)]+\\)\\s*$", entries))))
  data.frame(package = pkg, category = category, reason = reason)
}

# The two sub-analyses of Appendix A.7.4 that the tables cannot give directly:
# which packages failed to install, from each paper's report, and how reported
# results matched the output of the code, from match_table.
extract_reproducibility_detail <- function(res_reproducibility_check, match_table) {
  st <- res_reproducibility_check$summary_table
  code_bearing <- st$paper_id[!is.na(st$repro_code_n) & st$repro_code_n > 0]

  reports <- res_reproducibility_check$report_by_paper[
    names(res_reproducibility_check$report_by_paper) %in% code_bearing]
  dep_rows <- lapply(names(reports), function(id) {
    fd <- parse_failed_deps(reports[[id]])
    if (!is.null(fd)) fd$paper_id <- id
    fd
  })
  per_paper_failed <- dplyr::bind_rows(dep_rows)
  if (nrow(per_paper_failed) == 0) {
    per_paper_failed <- data.frame(package = character(0), category = character(0),
                                   reason = character(0), paper_id = character(0))
  }
  pkg_counts <- per_paper_failed |>
    dplyr::distinct(paper_id, package) |>
    dplyr::count(package, name = "n_papers_failed") |>
    dplyr::arrange(dplyr::desc(n_papers_failed))

  zero_ok <- res_reproducibility_check$table |>
    dplyr::filter(paper_id %in% code_bearing) |>
    dplyr::summarise(any_ok = any(outcome == "ran_ok"), .by = paper_id) |>
    dplyr::filter(!any_ok) |>
    dplyr::pull(paper_id)
  failed_and_zero_ok <- intersect(unique(per_paper_failed$paper_id), zero_ok)
  pkg_counts_failed_and_zero_ok <- per_paper_failed |>
    dplyr::filter(paper_id %in% failed_and_zero_ok) |>
    dplyr::distinct(paper_id, package) |>
    dplyr::count(package, name = "n_papers_failed") |>
    dplyr::arrange(dplyr::desc(n_papers_failed))

  all_matches <- match_table
  by_paper_match <- all_matches |>
    dplyr::summarise(n = dplyr::n(), n_found = sum(found, na.rm = TRUE), .by = paper_id) |>
    dplyr::mutate(prop_matched = n_found / n)

  list(
    per_paper_failed_deps = per_paper_failed,
    pkg_counts_all = pkg_counts,
    failed_and_zero_ok_papers = failed_and_zero_ok,
    pkg_counts_failed_and_zero_ok = pkg_counts_failed_and_zero_ok,
    all_matches = all_matches,
    by_paper_match = by_paper_match
  )
}
