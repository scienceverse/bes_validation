# "Results you get for free alongside Metacheck": a corpus-wide summary
# of every diagnostic repo_check/data_check/code_check produces that
# Cooper et al.'s protocol never asked about at all. These are not part
# of the Cooper-vs-Metacheck comparison (there is no human-coded ground
# truth to compare against) -- they are additional structured output a
# researcher gets automatically, at no extra cost, from the exact same
# module run already performed for the comparison.
#

summarize_free_extras <- function(res_repo_check, res_data_check, res_code_check) {

  cf <- res_code_check$table
  n_papers_total <- length(unique(c(res_repo_check$table$paper_id,
                                    res_data_check$structure$paper_id,
                                    cf$paper_id)))

  # CODE_CHECK diagnostics (per code file; aggregated to per-paper "at
  # least one file with this issue")
  n_code_files <- nrow(cf)
  n_code_papers <- length(unique(cf$paper_id))

  # 1. Missing referenced files.
  has_missing <- !is.na(cf$loaded_files_missing) & cf$loaded_files_missing > 0
  papers_missing <- unique(cf$paper_id[has_missing])

  # 2. Absolute file paths hardcoded in code.
  has_abs <- cf$code_abs_path %in% TRUE
  papers_abs <- unique(cf$paper_id[has_abs])

  # 3. setwd() calls.
  has_setwd <- cf$code_setwd %in% TRUE
  papers_setwd <- unique(cf$paper_id[has_setwd])

  # Either portability issue (1/2/3 combined at the paper level).
  papers_portability <- unique(c(papers_missing, papers_abs, papers_setwd))

  # 4. Comment density.
  has_stats <- !is.na(cf$comment_lines) & !is.na(cf$code_lines) & (cf$comment_lines + cf$code_lines) > 0
  pct_comment_corpus <- sum(cf$comment_lines[has_stats]) / sum(cf$comment_lines[has_stats] + cf$code_lines[has_stats])
  n_zero_comment <- sum(has_stats & cf$percentage_comment == 0)

  # Of the zero-comment files, how many are Python files actually
  # documented via a docstring rather than genuinely undocumented?
  is_py_zero_comment <- has_stats & cf$percentage_comment == 0 & cf$language %in% "Python"
  n_py_zero_comment <- sum(is_py_zero_comment)
  n_py_zero_comment_has_docstring <- sum(cf$has_docstring[is_py_zero_comment] %in% TRUE)

  # 5. Scattered library/import loading (>3 lines apart).
  has_scattered <- !is.na(cf$library_max_between) & cf$library_max_between > 3

  # 6. Parse errors. Only R-type files that were both downloaded and
  # actually reached parse() are the right denominator (see Appendix
  # A.2 for the full derivation of why each restriction is necessary).
  is_r_lang <- cf$language %in% "R"
  was_downloaded <- !is.na(cf$file_location)
  reached_parse <- !is.na(cf$parse_error)
  parse_eligible <- is_r_lang & was_downloaded & reached_parse
  has_parse_error <- parse_eligible & cf$parse_error %in% TRUE

  # 7. Package version pinning.
  st <- res_code_check$summary_table
  code_bearing_papers <- st[st$code_n > 0 & !is.na(st$code_n), ]
  n_pin_papers <- nrow(code_bearing_papers)
  n_pin_papers_pinned <- sum(code_bearing_papers$code_version_pinned %in% TRUE)

  # DATA_CHECK spreadsheet-formatting findings
  df <- res_data_check$findings
  tab <- if (nrow(df) > 0) sort(table(df$check), decreasing = TRUE) else table(character(0))
  n_files_flagged <- if (nrow(df) > 0) length(unique(df$source_file)) else 0L

  # REPO_CHECK file-naming findings
  ni <- res_repo_check$naming_issues
  n_papers_with_repo <- sum(res_repo_check$summary_table$files_n > 0, na.rm = TRUE)
  ni_bad <- if (nrow(ni) > 0) ni[ni$severity == "bad", ] else ni
  ni_suggest <- if (nrow(ni) > 0) ni[ni$severity == "suggestion", ] else ni
  tab_bad <- if (nrow(ni) > 0) sort(table(ni_bad$rule), decreasing = TRUE) else table(character(0))
  tab_sugg <- if (nrow(ni) > 0) sort(table(ni_suggest$rule), decreasing = TRUE) else table(character(0))
  n_papers_naming <- if (nrow(ni) > 0) length(unique(ni$paper_id)) else 0L

  list(
    n_papers_total = n_papers_total,
    n_code_files = n_code_files,
    n_code_papers = n_code_papers,
    n_missing_files = sum(has_missing),
    pct_missing_files = 100*sum(has_missing)/n_code_files,
    n_papers_missing = length(papers_missing),
    pct_papers_missing = 100*length(papers_missing)/n_code_papers,
    n_abs_path = sum(has_abs),
    pct_abs_path = 100*sum(has_abs)/n_code_files,
    n_papers_abs = length(papers_abs),
    pct_papers_abs = 100*length(papers_abs)/n_code_papers,
    n_setwd = sum(has_setwd),
    pct_setwd = 100*sum(has_setwd)/n_code_files,
    n_papers_setwd = length(papers_setwd),
    pct_papers_setwd = 100*length(papers_setwd)/n_code_papers,
    n_papers_portability = length(papers_portability),
    pct_papers_portability = 100*length(papers_portability)/n_code_papers,
    pct_comment_corpus = 100*pct_comment_corpus,
    n_zero_comment = n_zero_comment,
    pct_zero_comment = 100*n_zero_comment/sum(has_stats),
    n_py_zero_comment = n_py_zero_comment,
    n_py_zero_comment_has_docstring = n_py_zero_comment_has_docstring,
    pct_py_zero_comment_has_docstring = 100*n_py_zero_comment_has_docstring/n_py_zero_comment,
    n_scattered = sum(has_scattered),
    n_scattered_denom = sum(!is.na(cf$library_max_between)),
    pct_scattered = 100*sum(has_scattered)/sum(!is.na(cf$library_max_between)),
    n_parse_denom = sum(parse_eligible),
    n_parse_error = sum(has_parse_error),
    pct_parse_error = 100*sum(has_parse_error)/sum(parse_eligible),
    n_pin_papers = n_pin_papers,
    n_pin_papers_pinned = n_pin_papers_pinned,
    pct_pin_papers_pinned = 100*n_pin_papers_pinned/n_pin_papers,
    spreadsheet_findings_table = tab,
    n_spreadsheet_files_flagged = n_files_flagged,
    n_naming_issues = nrow(ni),
    n_naming_bad = if (nrow(ni) > 0) nrow(ni_bad) else 0L,
    n_naming_suggestion = if (nrow(ni) > 0) nrow(ni_suggest) else 0L,
    naming_bad_table = tab_bad,
    naming_suggestion_table = tab_sugg,
    n_papers_naming = n_papers_naming,
    n_papers_with_repo = n_papers_with_repo,
    pct_papers_naming = if (nrow(ni) > 0) 100*n_papers_naming/n_papers_with_repo else 0
  )
}
