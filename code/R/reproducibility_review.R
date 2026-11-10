# The reproducibility review: for every paper whose code metacheck ran, each
# script's result, why it did not reproduce, and the notes of the review.
#
# Two inputs:
#   * the results of metacheck's reproducibility_check, one file per paper
#     (papers/<id>.rds of a run of code/run_metacheck.R), with the metacheck
#     commit of that run (run_info.rds);
#   * data/review/reproducibility_notes_data.csv, the notes written by hand,
#     each for one paper and the scripts its file_pattern matches, with the
#     metacheck commit the note is based on (see its codebook).
# The cause of a script without a note is classified from its outcome and error
# (repro_auto_cause()); a note gives the cause the reviewer found. A note whose
# commit differs from the run's commit is marked outdated, so it is checked again
# after each rerun.

# Scripts that are not analyses of their own, and are not counted
REPRO_NOT_COUNTED <- c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                       "defines_functions", "not_analysis", "docker_failed")

# The last error line of a script's output
repro_error_line <- function(error, stderr) {
  e <- gsub("[[:space:]]+", " ", paste(error, stderr))
  p <- regexpr("(Error in [^:]*:|Error:|[A-Za-z]+Error:|[A-Za-z]+Exception:)(?!.*(Error in [^:]*:|Error:|[A-Za-z]+Error:))",
               e, perl = TRUE)
  out <- ifelse(p > 0, substring(e, p), substring(e, pmax(1, nchar(e) - 200)))
  trimws(substr(sub(" Calls: .*| Backtrace:.*| Execution halted.*| In addition:.*", "", out), 1, 300))
}

# Every error line of a script's output, joined by " | ": the last one is often a
# follow-on error (rstan's sink() after a failed dyn.load()), so a search for a
# cause looks at all of them
repro_error_lines <- function(error, stderr) {
  vapply(paste(error, stderr), function(e) {
    l <- strsplit(e, "\n", fixed = TRUE)[[1]]
    l <- unique(trimws(l[grepl("^[[:space:]]*(Error|[A-Za-z]+Error:|[A-Za-z]+Exception:)", l) |
                           grepl("unable to load shared object|failed to map segment|Unable to call|error waiting for container", l)]))
    substr(paste(l, collapse = " | "), 1, 1000)
  }, character(1), USE.NAMES = FALSE)
}

# A cause code from the outcome, metacheck's reason and the error line, for a
# script no note covers. See the codebook of reproducibility_notes for the codes.
repro_auto_cause <- function(outcome, reason, error_line, pkg_category = NA_character_) {
  e <- ifelse(is.na(error_line), "", error_line)
  has <- function(p) grepl(p, e, perl = TRUE)
  out <- rep("OTHER", length(outcome))
  out[has("defunct|has been removed|is deprecated and|unused argument|no longer|Invalid option for argument")] <- "PACKAGE_CHANGED"
  out[reason %in% "file"] <- "FILE_NOT_FOUND"
  out[reason %in% "undefined"] <- "CODE_ERROR"
  out[reason %in% "syntax"] <- "CODE_ERROR"
  out[reason %in% "environment"] <- "ENVIRONMENT"
  out[reason %in% "package"] <- "PACKAGE_UNAVAILABLE"
  out[outcome %in% "dependency_unavailable" & pkg_category %in% "source_unknown"] <- "PACKAGE_SOURCE_UNKNOWN"
  out[outcome %in% "python2"] <- "PYTHON2"
  out[outcome %in% "needs_a_person"] <- "NEEDS_PERSON"
  out[outcome %in% c("needs_internet", "download_too_large")] <- "NEEDS_INTERNET"
  out[outcome %in% c("timed_out", "skipped_time_limit")] <- "TIME_LIMIT"
  out[outcome %in% "not_downloaded"] <- "NOT_DOWNLOADED"
  out[outcome %in% "ran_ok"] <- "RAN_OK"
  out[outcome %in% REPRO_NOT_COUNTED] <- "NOT_COUNTED"
  out
}

# The parts of metacheck's results the review reads, per paper, from the run
# folders of code/run_metacheck.R (papers/<id>.rds and run_info.rds), in the shape
# of data/metacheck_results.rds: reproducibility_check$run_results,
# $install_results and $table, data_check$structure, and run_info. When a paper
# is in several runs, give the newest run last: its results replace the earlier
# ones, and each paper keeps the commit of its own run (run_info$paper_runs).
repro_review_from_runs <- function(run_dirs) {
  per_paper <- list()
  for (run in run_dirs) {
    sha <- readRDS(file.path(run, "run_info.rds"))$metacheck_sha
    for (f in list.files(file.path(run, "papers"), pattern = "[.]rds$", full.names = TRUE)) {
      x <- readRDS(f)
      per_paper[[x$paper_id]] <- list(rc = x$modules$reproducibility_check,
                                      structure = x$modules$data_check$structure,
                                      run = basename(run), sha = sha)
    }
  }
  tag <- function(df, id) if (is.data.frame(df) && nrow(df)) { df$paper_id <- id; df } else NULL
  bind <- function(part, field) dplyr::bind_rows(lapply(names(per_paper), function(id)
    tag(if (is.null(field)) per_paper[[id]][[part]] else per_paper[[id]]$rc[[field]], id)))
  list(reproducibility_check = list(run_results = bind("rc", "run_results"),
                                    install_results = bind("rc", "install_results"),
                                    table = bind("rc", "table")),
       data_check = list(structure = bind("structure", NULL)),
       run_info = list(paper_runs = data.frame(
         paper_id = names(per_paper),
         run = vapply(per_paper, `[[`, "", "run"),
         metacheck_sha = vapply(per_paper, function(p) p$sha %||% NA_character_, ""))))
}

# One row per script of every paper metacheck ran code for, from metacheck's
# results (data/metacheck_results.rds, or repro_review_from_runs()). The commit
# is the run's (run_info$metacheck_sha), or each paper's (run_info$paper_runs).
repro_review_scripts <- function(metacheck_results) {
  rc <- metacheck_results$reproducibility_check
  runs <- metacheck_results$run_info$paper_runs
  if (is.null(runs)) runs <- data.frame(paper_id = unique(rc$run_results$paper_id), run = NA_character_,
                                        metacheck_sha = metacheck_results$run_info$metacheck_sha %||% NA_character_)
  st_all <- metacheck_results$data_check$structure
  rows <- lapply(split(rc$run_results, rc$run_results$paper_id), function(rr) {
    id <- rr$paper_id[1]
    ins <- rc$install_results[rc$install_results$paper_id %in% id, , drop = FALSE]
    tb <- rc$table[rc$table$paper_id %in% id, , drop = FALSE]
    st <- st_all[st_all$paper_id %in% id, , drop = FALSE]
    bad <- ins[!ins$installed, , drop = FALSE]
    pkg_cat <- vapply(seq_len(nrow(rr)), function(i) {
      hit <- bad$category[vapply(bad$package, function(p) grepl(p, paste(rr$error[i], rr$stderr[i]), fixed = TRUE), logical(1))]
      if (length(hit)) hit[1] else NA_character_
    }, character(1))
    line <- repro_error_line(rr$error, rr$stderr)
    line[rr$outcome %in% c("ran_ok", REPRO_NOT_COUNTED)] <- NA_character_
    reason <- rr$reason %||% rep(NA_character_, nrow(rr))
    # The file a script could not be tried without, and its size: the deposit
    # holds it, but it was not downloaded, mostly as it is over the size limit.
    # (.repro_not_downloaded() looks at scripts that stopped, as before they got
    # the outcome not_downloaded.)
    rr_stopped <- rr
    rr_stopped$outcome[rr_stopped$outcome %in% "not_downloaded"] <- "errored"
    nd <- tryCatch(metacheck:::.repro_not_downloaded(rr_stopped, st), error = function(e) NULL)
    nd_file <- rep(NA_character_, nrow(rr)); nd_mb <- rep(NA_real_, nrow(rr))
    if (!is.null(nd) && nrow(nd)) for (i in which(rr$outcome %in% "not_downloaded")) {
      m <- nd[nd$script == rr$file_name[i], , drop = FALSE]
      if (!nrow(m)) next
      nd_file[i] <- paste(m$file, collapse = "; ")
      nd_mb[i] <- round(sum(m$size, na.rm = TRUE) / 1024^2, 1)
    }
    data.frame(
      article_id = id,
      file_name = rr$file_name,
      # (runs before metacheck 0.4.3 have no file_path)
      file_path = if (is.null(tb$file_path)) NA_character_ else tb$file_path[match(rr$file_name, tb$file_name)],
      outcome = rr$outcome,
      reason = reason,
      auto_cause = repro_auto_cause(rr$outcome, reason, line, pkg_cat),
      error_line = line,
      error_lines = ifelse(is.na(line), NA_character_, repro_error_lines(rr$error, rr$stderr)),
      not_downloaded_file = nd_file,
      not_downloaded_mb = nd_mb,
      run = runs$run[match(id, runs$paper_id)],
      run_metacheck_version = substr(runs$metacheck_sha[match(id, runs$paper_id)], 1, 8),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  out
}

# Who could make a script run that did not run through, from its cause (the codes in
# the codebook of reproducibility_notes):
#   authors           the deposit or the code: a file not shared, an error in the code,
#                     a package whose source the code does not name, a step that needs
#                     a person
#   software_changed  R, Python or a package has changed since the paper was published
#   docker_image      what metacheck's Docker image provides or installs: a package that
#                     could not be installed, a system library the image lacks, or
#                     something else the computer that ran the code lacked
#   run_settings      the limits of the run: time, file size, internet
#   metacheck         metacheck's code: a path it did not map to the deposit, a fault
#   unclear           not known without a review: stopped on a file, or another error
# A script that ran through or is not counted has none. A cause not listed here is
# unclear, so a new code cannot go missing from the counts.
REPRO_CAUSE_ACTOR <- c(
  FILE_NOT_SHARED = "authors", CODE_ERROR = "authors", PACKAGE_SOURCE_UNKNOWN = "authors",
  NEEDS_PERSON = "authors",
  PACKAGE_CHANGED = "software_changed", R_DEFAULT_CHANGED = "software_changed",
  PYTHON2 = "software_changed",
  PACKAGE_UNAVAILABLE = "docker_image", IMAGE_LACKS_LIBRARY = "docker_image",
  ENVIRONMENT = "docker_image",
  TIME_LIMIT = "run_settings", NOT_DOWNLOADED = "run_settings", NEEDS_INTERNET = "run_settings",
  PATH_NOT_MAPPED = "metacheck", METACHECK_BUG = "metacheck",
  FILE_NOT_FOUND = "unclear", OTHER = "unclear")
REPRO_ACTORS <- c("authors", "software_changed", "docker_image", "run_settings", "metacheck", "unclear")

repro_cause_actor <- function(cause, outcome) {
  out <- unname(REPRO_CAUSE_ACTOR[cause])
  out[is.na(out)] <- "unclear"
  out[outcome %in% c("ran_ok", REPRO_NOT_COUNTED) | cause %in% c("RAN_OK", "NOT_COUNTED")] <- NA_character_
  out
}

# The notes joined to the scripts: the first note whose file_pattern matches a
# script (a regular expression on file_name, "*" for every script of the paper)
# gives its cause, status and comment. `outdated` is TRUE when the note was
# written for another commit than the run's, so it is to be checked again.
repro_review_join <- function(scripts, notes) {
  scripts$cause <- scripts$auto_cause
  scripts$status <- ifelse(scripts$outcome %in% c("ran_ok", REPRO_NOT_COUNTED), "none", "not_reviewed")
  scripts$comment <- NA_character_
  scripts$note_metacheck_version <- NA_character_
  scripts$note_date <- NA_character_
  if (is.null(notes) || !nrow(notes)) {
    scripts$outdated <- FALSE
    scripts$actor <- repro_cause_actor(scripts$cause, scripts$outcome)
    return(scripts)
  }
  for (k in seq_len(nrow(notes))) {
    pat <- if (identical(notes$file_pattern[k], "*")) "." else notes$file_pattern[k]
    hit <- scripts$article_id == notes$article_id[k] & is.na(scripts$comment) &
      grepl(pat, scripts$file_name, perl = TRUE)
    scripts$cause[hit] <- notes$cause[k]
    scripts$status[hit] <- notes$status[k]
    scripts$comment[hit] <- notes$comment[k]
    scripts$note_metacheck_version[hit] <- notes$metacheck_version[k]
    scripts$note_date[hit] <- notes$date[k]
  }
  scripts$outdated <- !is.na(scripts$note_metacheck_version) &
    substr(scripts$note_metacheck_version, 1, 8) != scripts$run_metacheck_version
  scripts$actor <- repro_cause_actor(scripts$cause, scripts$outcome)
  scripts
}

# One row per paper: how many scripts counted, ran through, could not be tried,
# and the causes of the others, with a verdict, and how many of those others each
# group of REPRO_CAUSE_ACTOR could make run.
repro_review_papers <- function(scripts) {
  by <- split(scripts, scripts$article_id)
  out <- lapply(by, function(s) {
    counted <- !s$outcome %in% c(REPRO_NOT_COUNTED, "not_downloaded")
    ok <- sum(s$outcome[counted] == "ran_ok")
    n <- sum(counted)
    failed <- counted & s$outcome != "ran_ok"
    # the most frequent first; equally frequent ones in alphabetical order
    causes <- sort(table(s$cause[failed]), decreasing = TRUE)
    by_actor <- vapply(REPRO_ACTORS, function(a) sum(failed & s$actor %in% a), integer(1))
    data.frame(
      article_id = s$article_id[1],
      scripts_n = n,
      ran_ok_n = ok,
      not_tried_n = sum(s$outcome == "not_downloaded"),
      # the download limit (MB per file) under which every script could be tried
      largest_not_downloaded_mb = if (any(!is.na(s$not_downloaded_mb))) max(s$not_downloaded_mb, na.rm = TRUE) else NA_real_,
      verdict = if (n == 0) "NOTHING_TO_RUN" else if (ok == n) "ALL_RAN" else if (ok > 0) "PARTLY_RAN" else "NONE_RAN",
      main_cause = if (length(causes)) names(causes)[1] else NA_character_,
      causes = paste(sprintf("%s (%d)", names(causes), as.integer(causes)), collapse = "; "),
      authors_n = by_actor[["authors"]],
      software_changed_n = by_actor[["software_changed"]],
      docker_image_n = by_actor[["docker_image"]],
      run_settings_n = by_actor[["run_settings"]],
      metacheck_n = by_actor[["metacheck"]],
      unclear_n = by_actor[["unclear"]],
      open_n = sum(failed & s$status %in% "open"),
      not_reviewed_n = sum(counted & s$status == "not_reviewed"),
      outdated_notes_n = sum(s$outdated),
      run = s$run[1],
      metacheck_version = s$run_metacheck_version[1],
      stringsAsFactors = FALSE)
  })
  do.call(rbind, unname(out))
}

# Saves one table of the review in data/, the way code/build_documentation.R writes
# its files, and returns the path, for a target of format "file".
write_repro_review_csv <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}

# The papers to run again after metacheck changed: only those with a script that
# did not run through for a cause a change addresses, so a rerun does not repeat
# the papers nothing changed for. `fixes` is a named list of regular expressions
# on the script's error line, one per change (name: what changed); a script not
# tried for a file of at most `max_file_mb` counts too, when that limit was raised.
# Returns one row per paper and reason.
repro_rerun_selection <- function(scripts, fixes = list(), max_file_mb = NA_real_) {
  failed <- !scripts$outcome %in% c("ran_ok", REPRO_NOT_COUNTED)
  rows <- lapply(names(fixes), function(fx) {
    hit <- failed & grepl(fixes[[fx]], paste(scripts$error_line, scripts$error_lines, scripts$outcome), perl = TRUE)
    if (!any(hit)) return(NULL)
    stats::aggregate(list(scripts_n = rep(1L, sum(hit))),
                     list(article_id = scripts$article_id[hit], reason = rep(fx, sum(hit))), sum)
  })
  if (!is.na(max_file_mb)) {
    hit <- scripts$outcome %in% "not_downloaded" & !is.na(scripts$not_downloaded_mb) &
      scripts$not_downloaded_mb <= max_file_mb
    if (any(hit)) rows <- c(rows, list(stats::aggregate(
      list(scripts_n = rep(1L, sum(hit))),
      list(article_id = scripts$article_id[hit], reason = rep(sprintf("files up to %s MB downloaded", max_file_mb), sum(hit))), sum)))
  }
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(article_id = character(0), reason = character(0), scripts_n = integer(0)))
  out <- do.call(rbind, rows)
  out[order(out$article_id), , drop = FALSE]
}
