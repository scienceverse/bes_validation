# Put the reproducibility_check results of a later run of some papers into the result
# file, and keep the results of every other module as they are.
#
# The comparison with Cooper et al. and the reviews written by hand describe the run
# recorded in run_info$metacheck_sha. The code of the papers whose code ran was run
# again with a later metacheck (code/run_metacheck.R with --ids and without --base,
# which combines only those papers and does not touch data/). From that run this script
# takes only what reproducibility_check produced: its tables, its reports, the repro_
# columns of the summary table and its rows in the error table. Every paper went
# through all modules again in that run, but the results of the other modules are not
# taken over.
#
# Some of those papers may have been run once more, in a run of their own (--rerun,
# with its list of papers in --rerun-ids): with the same metacheck, after a fault
# outside metacheck's code was put right, such as a Docker image that lacked the
# header files to compile some packages. For those papers the results of that run
# replace the ones of --new, in the same way.
#
# In the file it saves:
#   run_info$metacheck_sha          unchanged: the commit of the comparison and the reviews
#   run_info$paper_runs             the run and commit of each paper's reproducibility_check,
#                                   which code/R/reproducibility_review.R compares with the notes
#   run_info$reproducibility_check  the later run's own run_info (version, settings, Docker
#                                   images, times), with the papers taken from it
#   run_info$reproducibility_rerun  the same for the run of --rerun, when it is given
#
# Usage, from the repository root:
#   Rscript code/splice_reproducibility.R --new E:/bes_validation_run/repro_bd27c1db \
#     --ids E:/bes_validation_run/ids_repro_435.txt
# Optional: --old FILE, the result file to start from, and --save FILE, where to save
# the result (both data/metacheck_results.rds by default); --rerun FOLDER and
# --rerun-ids FILE, a run of some of these papers whose results replace those of --new.

args <- commandArgs(TRUE)
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) default else args[i + 1]
}
new_dir <- arg_value("--new", NA)
ids_file <- arg_value("--ids", NA)
rerun_dir <- arg_value("--rerun", NA)
rerun_ids_file <- arg_value("--rerun-ids", NA)
rerun2_dir <- arg_value("--rerun2", NA)
rerun2_ids_file <- arg_value("--rerun2-ids", NA)
old_file <- arg_value("--old", "data/metacheck_results.rds")
save_file <- arg_value("--save", "data/metacheck_results.rds")
if (is.na(new_dir) || is.na(ids_file)) stop("Give --new (the later run's folder) and --ids (its list of papers).")
if (is.na(rerun_dir) != is.na(rerun_ids_file)) stop("Give --rerun and --rerun-ids together.")
if (is.na(rerun2_dir) != is.na(rerun2_ids_file)) stop("Give --rerun2 and --rerun2-ids together.")
if (!is.na(rerun2_dir) && is.na(rerun_dir)) stop("Give --rerun before --rerun2.")

read_ids <- function(file) {
  x <- unique(trimws(readLines(file, warn = FALSE)))
  x[nzchar(x)]
}
old <- readRDS(old_file)
new <- readRDS(file.path(new_dir, "metacheck_results.rds"))
ids <- read_ids(ids_file)
rerun <- if (!is.na(rerun_dir)) readRDS(file.path(rerun_dir, "metacheck_results.rds"))
rerun_ids <- if (!is.na(rerun_dir)) read_ids(rerun_ids_file) else character(0)
# A second rerun, in its own folder, for papers reran with a different metacheck than
# --rerun (here 10.1111/2041-210X.13928, reran on dev with the #597 fix). Its papers
# keep their own run name and commit, like the first rerun's.
rerun2 <- if (!is.na(rerun2_dir)) readRDS(file.path(rerun2_dir, "metacheck_results.rds"))
rerun2_ids <- if (!is.na(rerun2_dir)) read_ids(rerun2_ids_file) else character(0)

# Every paper of a list must have a reproducibility_check result in its run: a paper
# without a saved result, or whose chain stopped at an earlier module, would otherwise
# keep no result at all.
check_run <- function(res, ids, what) {
  missing <- setdiff(ids, res$summary_table$paper_id)
  if (length(missing)) stop("No result in ", what, " for: ", paste(missing, collapse = ", "))
  earlier_error <- res$errors[res$errors$paper_id %in% ids & res$errors$module != "reproducibility_check", , drop = FALSE]
  if (nrow(earlier_error)) {
    stop("An earlier module failed in ", what, ", so reproducibility_check did not run, for: ",
         paste(unique(earlier_error$paper_id), collapse = ", "))
  }
}
check_run(new, ids, "the later run")
if (!is.null(rerun)) {
  check_run(rerun, rerun_ids, "the run of --rerun")
  # Each paper's own commit is recorded, so a rerun with another metacheck would not
  # be lost; but such a rerun is a later run of its own, not a correction of one.
  if (!identical(rerun$run_info$metacheck_sha, new$run_info$metacheck_sha)) {
    stop("The run of --rerun used metacheck ", rerun$run_info$metacheck_sha, ", the later run ",
         new$run_info$metacheck_sha, ".")
  }
}
# --rerun2 is deliberately a run of a different metacheck (the #597 fix), so its version
# is not required to match; each of its papers keeps its own commit in paper_runs.
if (!is.null(rerun2)) {
  check_run(rerun2, rerun2_ids, "the run of --rerun2")
  if (length(intersect(rerun2_ids, rerun_ids))) stop("--rerun2 and --rerun share papers.")
}
unknown <- setdiff(Reduce(union, list(ids, rerun_ids, rerun2_ids)), old$summary_table$paper_id)
if (length(unknown)) stop("Not in ", old_file, ": ", paste(unknown, collapse = ", "))

# The same as bind_tables() in code/run_metacheck.R: a column that is text in one table
# and TRUE/FALSE in another becomes text.
bind_tables <- function(dfs) {
  dfs <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, dfs)
  if (length(dfs) == 0) return(data.frame())
  kind <- function(x) {
    if (is.numeric(x)) "number" else if (is.logical(x) && all(is.na(x))) NA_character_ else class(x)[1]
  }
  cols <- unique(unlist(lapply(dfs, names)))
  mixed <- cols[vapply(cols, function(cl) {
    kinds <- unlist(lapply(dfs, function(d) if (cl %in% names(d)) kind(d[[cl]])))
    length(unique(kinds[!is.na(kinds)])) > 1
  }, logical(1))]
  dfs <- lapply(dfs, function(d) {
    for (cl in intersect(mixed, names(d))) {
      if (!is.list(d[[cl]])) d[[cl]] <- as.character(d[[cl]])
    }
    d
  })
  dplyr::bind_rows(dfs)
}

# The reproducibility_check results of papers `ids` from `res`, in place of those in `out`.
splice <- function(out, res, ids) {
  drop_ids <- function(d) if (is.data.frame(d) && nrow(d)) d[!d$paper_id %in% ids, , drop = FALSE] else d
  keep_ids <- function(d) if (is.data.frame(d) && nrow(d)) d[d$paper_id %in% ids, , drop = FALSE] else d
  rc_old <- out$reproducibility_check
  rc_new <- res$reproducibility_check

  # The tables: the rows of these papers from the later run, the rows of all others as before.
  for (f in setdiff(union(names(rc_old), names(rc_new)), "report_by_paper")) {
    out$reproducibility_check[[f]] <- bind_tables(list(drop_ids(rc_old[[f]]), keep_ids(rc_new[[f]])))
  }
  # The reports, one per paper.
  out$reproducibility_check$report_by_paper[ids] <- rc_new$report_by_paper[ids]

  # The summary table: all repro_ columns of these papers come from the later run. A
  # column only the earlier run has becomes empty for them; a column only the later run
  # has is empty for the other papers.
  cols <- union(grep("^repro_", names(out$summary_table), value = TRUE),
                grep("^repro_", names(res$summary_table), value = TRUE))
  at_old <- match(ids, out$summary_table$paper_id)
  at_new <- match(ids, res$summary_table$paper_id)
  for (cl in cols) {
    if (!cl %in% names(out$summary_table)) out$summary_table[[cl]] <- NA
    out$summary_table[[cl]][at_old] <- if (cl %in% names(res$summary_table)) res$summary_table[[cl]][at_new] else NA
  }

  # The error table: reproducibility_check's rows of these papers from the later run.
  old_err <- out$errors
  if (is.data.frame(old_err) && nrow(old_err)) {
    old_err <- old_err[!(old_err$paper_id %in% ids & old_err$module == "reproducibility_check"), , drop = FALSE]
  }
  new_err <- res$errors
  if (is.data.frame(new_err) && nrow(new_err)) {
    new_err <- new_err[new_err$paper_id %in% ids & new_err$module == "reproducibility_check", , drop = FALSE]
  }
  out$errors <- bind_tables(list(old_err, new_err))
  out
}

out <- splice(old, new, ids)
if (!is.null(rerun)) out <- splice(out, rerun, rerun_ids)
if (!is.null(rerun2)) out <- splice(out, rerun2, rerun2_ids)

# The run and commit of each paper's reproducibility_check: each rerun's for its papers,
# the later run's for the other papers of --ids, the file's own for every other paper
# that has scripts.
run_name <- function(dir) basename(normalizePath(dir, winslash = "/"))
# A run installed from a detached git worktree can have no metacheck_sha; fall back to
# its version string so the commit column still names the metacheck each paper used.
run_sha <- function(info) if (!is.null(info$metacheck_sha)) info$metacheck_sha else info$metacheck_version
run_papers <- unique(out$reproducibility_check$run_results$paper_id)
in_rerun2 <- run_papers %in% rerun2_ids
in_rerun <- run_papers %in% rerun_ids & !in_rerun2
in_new <- run_papers %in% ids & !in_rerun & !in_rerun2
# Fill the run name and commit per paper, highest-priority source last so it wins.
run_col <- rep(NA_character_, length(run_papers))
sha_col <- rep(old$run_info$metacheck_sha, length(run_papers))
run_col[in_new] <- run_name(new_dir);        sha_col[in_new] <- run_sha(new$run_info)
if (!is.null(rerun))  { run_col[in_rerun]  <- run_name(rerun_dir);  sha_col[in_rerun]  <- run_sha(rerun$run_info) }
if (!is.null(rerun2)) { run_col[in_rerun2] <- run_name(rerun2_dir); sha_col[in_rerun2] <- run_sha(rerun2$run_info) }
out$run_info$paper_runs <- data.frame(paper_id = run_papers, run = run_col, metacheck_sha = sha_col)
out$run_info$reproducibility_check <- c(new$run_info, list(paper_ids = setdiff(ids, union(rerun_ids, rerun2_ids))))
out$run_info$reproducibility_rerun <- if (!is.null(rerun)) c(rerun$run_info, list(paper_ids = rerun_ids))
out$run_info$reproducibility_rerun2 <- if (!is.null(rerun2)) c(rerun2$run_info, list(paper_ids = rerun2_ids))

saveRDS(out, save_file)
cat(sprintf("Saved %s: reproducibility_check of %d papers from metacheck %s (%s); all other results from %s.\n",
            save_file, length(union(ids, rerun_ids)), new$run_info$metacheck_version, substr(new$run_info$metacheck_sha, 1, 8),
            substr(old$run_info$metacheck_sha, 1, 8)))
if (!is.null(rerun)) {
  cat(sprintf("Of these, %d papers from the run of --rerun (%s).\n", length(rerun_ids), run_name(rerun_dir)))
}
if (!is.null(rerun2)) {
  cat(sprintf("Of these, %d paper(s) from the run of --rerun2 (%s), metacheck %s.\n",
              length(rerun2_ids), run_name(rerun2_dir), substr(run_sha(rerun2$run_info), 1, 8)))
}
