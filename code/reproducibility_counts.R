# What reproducibility_check did with the code of each paper. We count only scripts that
# the module tried to run as analyses of their own, and read the one-word reason the
# module saved for each script that did not run through (the "reason" column), so these
# numbers match the module's own grouping rather than re-deriving reasons from messages.
targets::tar_load(res_reproducibility_check, store = "_targets")
targets::tar_load(reproducibility_detail, store = "_targets")
rc_st <- res_reproducibility_check$summary_table
rc_all <- res_reproducibility_check$run_results
rc_install <- res_reproducibility_check$install_results
rc_changes <- res_reproducibility_check$modifications

# Outcomes that are not a standalone analysis script, so they are not counted among the
# scripts that were run (the module excludes the same set). Each is counted and named
# separately below. A script run by another through source() or import, a file that only
# defines functions, a JAGS/BUGS model file, a file that is not code in a language the
# module runs, a file the module did not download, and a failure of Docker on our side.
rc_excluded <- c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                 "defines_functions", "not_analysis", "docker_failed", "not_downloaded")
n_rc_excl <- function(outcome) sum(rc_all$outcome %in% outcome)
n_rc_source <- n_rc_excl(c("run_by_source", "run_by_import"))
n_rc_defines <- n_rc_excl("defines_functions")
n_rc_jags <- n_rc_excl("jags_model")
n_rc_not_analysis <- n_rc_excl("not_analysis")
n_rc_not_r_content <- n_rc_excl("not_r_content")
n_rc_not_downloaded <- n_rc_excl("not_downloaded")

rc_runs <- rc_all[!rc_all$outcome %in% rc_excluded & !is.na(rc_all$outcome), ]

n_rc_papers <- sum(rc_st$repro_code_n > 0, na.rm = TRUE)
n_rc_not_run <- n_mc_code - n_rc_papers
n_rc_scripts <- nrow(rc_runs)
n_rc_ok <- sum(rc_runs$outcome == "ran_ok")
rc_ok_by_paper <- split(rc_runs$outcome == "ran_ok", rc_runs$paper_id)
n_rc_all_ok <- sum(vapply(rc_ok_by_paper, all, logical(1)))
n_rc_none_ok <- sum(!vapply(rc_ok_by_paper, any, logical(1)))
n_rc_some_ok <- length(rc_ok_by_paper) - n_rc_all_ok - n_rc_none_ok

rc_ext <- tolower(tools::file_ext(rc_runs$file_name))
rc_is_python <- rc_ext %in% c("py", "ipynb")
n_rc_python <- sum(rc_is_python)
n_rc_python_ok <- sum(rc_is_python & rc_runs$outcome == "ran_ok")
n_rc_r <- n_rc_scripts - n_rc_python
n_rc_r_ok <- n_rc_ok - n_rc_python_ok

# The module's own one-word reason for each counted script that did not run through.
rc_reason <- rc_runs$reason
rc_reason[rc_runs$outcome == "ran_ok"] <- NA_character_
rc_message <- paste(rc_runs$error, rc_runs$stderr)
stopifnot(identical(is.na(rc_reason), rc_runs$outcome == "ran_ok"))
n_rc_failed <- sum(!is.na(rc_reason))
n_rc_failed_papers <- length(unique(rc_runs$paper_id[!is.na(rc_reason)]))
n_reason <- function(reason) sum(rc_reason %in% reason)
n_reason_papers <- function(reason) length(unique(rc_runs$paper_id[rc_reason %in% reason]))
n_rc_outcome <- function(outcome) sum(rc_runs$outcome == outcome)
n_rc_others_software <- sum(rc_reason %in% "package" & grepl("No module named '(cvat|motioneye)", rc_message))

n_rc_declared <- nrow(rc_install)
n_rc_not_installed <- sum(!rc_install$installed)
n_rc_not_installed_papers <- length(unique(rc_install$paper_id[!rc_install$installed]))
n_rc_not_on_cran <- sum(rc_install$category[!rc_install$installed] %in% "cran_unavailable")
rc_failed_packages <- sort(table(rc_install$package[!rc_install$installed]), decreasing = TRUE)
n_rc_change <- function(type) sum(rc_changes$change_type == type)
n_rc_changed_papers <- length(unique(rc_changes$paper_id))

rc_match <- reproducibility_detail$all_matches
rc_match_papers <- reproducibility_detail$by_paper_match
n_rc_stats <- nrow(rc_match)
n_rc_stats_found <- sum(rc_match$found %in% TRUE)
n_rc_stat_papers <- nrow(rc_match_papers)
n_rc_stat_all <- sum(rc_match_papers$prop_matched == 1)
n_rc_stat_none <- sum(rc_match_papers$prop_matched == 0)
n_rc_stat_some <- n_rc_stat_papers - n_rc_stat_all - n_rc_stat_none

# Cooper et al.'s rating of completeness for the papers whose code was run.
rc_cooper <- read.csv("data/cooper_coded_data.csv")
rc_papers <- read.csv("data/papers_data.csv", colClasses = "character")
rc_cooper$article_id <- rc_papers$article_id[match(as.character(rc_cooper$paper_number), rc_papers$paper_number)]
rc_complete <- rc_cooper$article_id[rc_cooper$data_completeness %in% "Complete"]
rc_missing <- rc_st$paper_id[rc_st$repro_missing_inputs > 0 & !is.na(rc_st$repro_missing_inputs)]
rc_complete_run <- intersect(rc_complete, names(rc_ok_by_paper))
n_rc_complete <- length(rc_complete_run)
n_rc_complete_missing <- length(intersect(rc_complete_run, rc_missing))
n_rc_complete_any_ok <- sum(vapply(rc_ok_by_paper[rc_complete_run], any, logical(1)))
n_rc_complete_all_ok <- sum(vapply(rc_ok_by_paper[rc_complete_run], all, logical(1)))
n_rc_missing_all <- length(intersect(names(rc_ok_by_paper), rc_missing))

rc_reason_rows <- c("file", "package", "other", "undefined", "time", "interactive",
                    "syntax", "environment", "download", "internet")
rc_reason_table <- data.frame(
  Reason = c("A file the script needs was not found", "A package could not be installed or loaded",
             "Another error while the code ran", "An object or function is not defined",
             "The time limit was reached", "The script asks a person to choose a file",
             "The file is not valid code in its language", "The software environment differs",
             "A download the script needs was too large", "The script needs the internet"),
  Scripts = vapply(rc_reason_rows, n_reason, 0L),
  `% of failed scripts` = sprintf("%.1f", 100 * vapply(rc_reason_rows, n_reason, 0L) / n_rc_failed),
  Papers = vapply(rc_reason_rows, n_reason_papers, 0L),
  check.names = FALSE
)
stopifnot(sum(rc_reason_table$Scripts) == n_rc_failed - n_reason("no_message"))