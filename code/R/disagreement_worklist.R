# Build the disagreement worklist from cooper_vs_metacheck's side-by-side
# comparison. Two outputs, kept separate so a diff shows whether the DATA
# changed (a metacheck rerun found different disagreements) or the human
# REVIEW changed (someone filled in a verdict/comment):
#   - the .rds return value: every disagreement row and its data columns,
#     entirely code-generated, overwritten fresh every run.
#   - the .xlsx file: row_id/article_id/disagreements plus, for each of the
#     8 verdicted columns, its verdict/comment/version-stamp cells (manually
#     edited) and the Cooper and Metacheck values the verdict is about. Any
#     other columns already present in this file (free-form notes) are left
#     untouched.
# 8 verdicted columns: data_availability, data_archive, data_license,
# data_download, code_archived, code_archive, code_download, any_readme.
# data_archive and code_archive compare platform SETS (a paper can cite
# more than one repository) and disagree whenever the sets are not
# identical; every other column is a straight boolean comparison.
#
#
# Carrying a verdict forward needs TWO prior sources, same as the
# original script: the previous run's `review` data frame (which has the
# raw cooper/mc VALUE columns a (paper, column, cooper value, mc value)
# key is built from) and the CURRENT xlsx file on disk (which has the
# human-written verdict/comment cells, possibly hand-edited since the
# prior run -- the xlsx only ever stores
# row_id/article_id/disagreements/reviewed plus the 7 verdict/comment
# pairs, never the value columns, so it cannot be used alone for the
# value-key join). The previous run's review is read directly from this
# same target's own prior cached value in the targets store (tar_read()
# on worklist_target_name), the way the original script read its own
# prior .rds output before overwriting it -- returns NULL if this target
# has never successfully run before (first run) or its store is missing.
build_disagreement_worklist <- function(side_by_side, xlsx_path, worklist_target_name) {
  prev_review <- tryCatch(
    targets::tar_read_raw(worklist_target_name)$review,
    error = function(e) NULL
  )
  cooper_bool <- function(x, positive, negative) {
    out <- rep(NA, length(x))
    out[x %in% positive] <- TRUE
    out[x %in% negative] <- FALSE
    out
  }

  bool_pairs <- list(
    data_availability = list(cooper = "data_availability", mc = "mc_data_availability",
                             positive = "Yes", negative = c("No", "No, but they are available on request")),
    data_license      = list(cooper = "data_license", mc = "mc_data_license",
                             positive = "Yes", negative = "No"),
    data_download     = list(cooper = "data_download", mc = "mc_data_download",
                             positive = c("Yes", "Yes, but not all data"), negative = "No"),
    code_archived     = list(cooper = "code_archived", mc = "mc_code_archived",
                             positive = "Yes", negative = "No"),
    code_download     = list(cooper = "code_download", mc = "mc_code_download",
                             positive = "Yes", negative = "No")
  )

  n <- nrow(side_by_side)
  disagree_cols <- rep("", n)
  flags <- list()
  for (nm in names(bool_pairs)) {
    p <- bool_pairs[[nm]]
    cb <- cooper_bool(side_by_side[[p$cooper]], p$positive, p$negative)
    mb <- side_by_side[[p$mc]]
    both <- !is.na(cb) & !is.na(mb)
    disagree <- both & (cb != mb)
    flags[[nm]] <- disagree
    direction <- ifelse(disagree, ifelse(cb, "cooper=Yes,mc=No", "cooper=No,mc=Yes"), NA)
    for (i in which(disagree)) {
      tag <- sprintf("%s[%s]", nm, direction[i])
      disagree_cols[i] <- if (nzchar(disagree_cols[i])) paste(disagree_cols[i], tag, sep = ";") else tag
    }
  }

  # data_archive / code_archive: a disagreement is any comparable paper whose
  # two platform sets are not identical -- the same rule, through the same
  # function, as the agreement percentages in compute_comparison_statistics()
  # (platform_set_category(), R/platforms.R), so the worklist lists exactly
  # the disagreements the manuscript counts.
  for (nm in c("data_archive", "code_archive")) {
    cv <- side_by_side[[nm]]; mv <- side_by_side[[paste0("mc_", nm)]]
    # Without the institutional-Figshare column (older comparison tables),
    # no deposit counts as institutional, as in platform_set_category()'s
    # own default; `NULL %in% TRUE` would give a zero-length vector instead.
    inst <- side_by_side[[paste0("mc_", nm, "_institutional_figshare")]]
    inst <- if (is.null(inst)) rep(FALSE, nrow(side_by_side)) else inst %in% TRUE
    category <- platform_set_category(cv, mv, inst)
    disagree <- !is.na(category) & category != "exact"
    flags[[nm]] <- disagree
    for (i in which(disagree)) {
      tag <- sprintf("%s[cooper=%s,mc=%s]", nm, cv[i], mv[i])
      disagree_cols[i] <- if (nzchar(disagree_cols[i])) paste(disagree_cols[i], tag, sep = ";") else tag
    }
  }

  ar_disagree <- !is.na(side_by_side$cooper_any_readme) & !is.na(side_by_side$mc_any_readme) &
    (side_by_side$cooper_any_readme != side_by_side$mc_any_readme)
  flags[["any_readme"]] <- ar_disagree
  ar_dir <- ifelse(ar_disagree, ifelse(side_by_side$cooper_any_readme, "cooper=Yes,mc=No", "cooper=No,mc=Yes"), NA)
  for (i in which(ar_disagree)) {
    tag <- sprintf("any_readme[%s]", ar_dir[i])
    disagree_cols[i] <- if (nzchar(disagree_cols[i])) paste(disagree_cols[i], tag, sep = ";") else tag
  }

  has_disagreement <- nzchar(disagree_cols)
  review <- side_by_side[has_disagreement, ]
  review$disagreements <- disagree_cols[has_disagreement]

  dup <- duplicated(review$article_id) | duplicated(review$article_id, fromLast = TRUE)
  review$row_id <- review$article_id
  if (any(dup)) {
    ave_idx <- ave(seq_along(review$article_id), review$article_id, FUN = seq_along)
    review$row_id[dup] <- paste0(review$article_id[dup], "__dup", ave_idx[dup])
  }
  review <- review[, c("row_id", setdiff(names(review), "row_id"))]

  verdict_cols <- c("data_availability", "data_archive", "data_license",
                    "data_download", "code_archived", "code_archive",
                    "code_download", "any_readme")

  # A separator that cannot occur in an id or a coded value, so two
  # different (paper, Cooper value, Metacheck value) combinations can never
  # produce the same key.
  build_key <- function(article_id, cooper_val, mc_val)
    paste(article_id, as.character(cooper_val), as.character(mc_val), sep = "\r")
  cooper_col_for <- function(col) if (col %in% c("data_archive", "code_archive")) col else
    if (col == "any_readme") "cooper_any_readme" else bool_pairs[[col]]$cooper
  mc_col_for <- function(col) if (col %in% c("data_archive", "code_archive")) paste0("mc_", col) else
    if (col == "any_readme") "mc_any_readme" else bool_pairs[[col]]$mc

  # Each verdict carries the same version stamp as a row of the manual
  # audit CSVs (see R/audit_staleness.R): the installed metacheck RemoteSha
  # when the verdict was written, the date it was written, and whether
  # that SHA was actually captured (TRUE) or the date only inferred from
  # git history (FALSE). One stamp per verdict column, not per row,
  # because a row's verdicts can be written at different times.
  stamp_suffixes <- c("_metacheck_version", "_metacheck_version_date", "_version_is_exact")
  for (col in verdict_cols) {
    review[[paste0(col, "_verdict")]] <- NA_character_
    review[[paste0(col, "_comment")]] <- NA_character_
    for (s in stamp_suffixes) review[[paste0(col, s)]] <- NA_character_
  }

  # Carry forward already-recorded verdicts/comments for a (paper, column,
  # cooper value, mc value) combination that is unchanged from the
  # existing xlsx at xlsx_path, keyed on the underlying facts rather than
  # row identity alone -- so a paper whose mc value changed but still
  # disagrees does not keep a verdict written about the old value.
  `%||%` <- function(a, b) if (is.null(a)) b else a
  carry_forward_from <- function(old, review) {
    has_row_id <- "row_id" %in% names(old)
    for (col in verdict_cols) {
      vcol <- paste0(col, "_verdict"); ccol <- paste0(col, "_comment")
      if (!all(c(vcol, ccol) %in% names(old))) next
      cooper_col <- cooper_col_for(col); mc_col <- mc_col_for(col)
      use_value_key <- all(c(cooper_col, mc_col) %in% names(old))
      if (!use_value_key && !has_row_id) next
      old_verdict <- old[[vcol]]
      has_old <- !is.na(old_verdict) & nzchar(trimws(old_verdict))
      if (!any(has_old)) next
      if (use_value_key) {
        lut_key <- build_key(old$article_id, old[[cooper_col]], old[[mc_col]])[has_old]
        new_key <- build_key(review$article_id, review[[cooper_col]], review[[mc_col]])
      } else {
        lut_key <- old$row_id[has_old]
        new_key <- review$row_id
      }
      lut_verdict <- old_verdict[has_old]
      lut_comment <- old[[ccol]][has_old]
      stamp_cols <- paste0(col, stamp_suffixes)
      lut_stamp <- lapply(stamp_cols, function(sc)
        if (sc %in% names(old)) as.character(old[[sc]])[has_old] else rep(NA_character_, sum(has_old)))
      first <- !duplicated(lut_key)
      lut_key <- lut_key[first]; lut_verdict <- lut_verdict[first]; lut_comment <- lut_comment[first]
      lut_stamp <- lapply(lut_stamp, `[`, first)
      m <- match(new_key, lut_key)
      # Only fill cells still blank: a later source wins over an earlier
      # one for any cell both provide.
      still_blank <- is.na(review[[vcol]]) | !nzchar(trimws(review[[vcol]] %||% ""))
      hit <- !is.na(m) & still_blank
      review[[vcol]][hit] <- lut_verdict[m[hit]]
      review[[ccol]][hit] <- lut_comment[m[hit]]
      for (j in seq_along(stamp_cols)) review[[stamp_cols[j]]][hit] <- lut_stamp[[j]][m[hit]]
    }
    review
  }

  # Join the xlsx's verdict/comment cells (possibly hand-edited since
  # prev_review was computed) onto prev_review's data columns, by
  # row_id, so carry_forward_from() has both the VALUES to key on and
  # the latest human-written VERDICTS to carry forward -- replicating
  # the original script's old_rds + old_xlsx join exactly.
  old_xlsx <- NULL
  if (file.exists(xlsx_path)) {
    old_xlsx <- as.data.frame(openxlsx::read.xlsx(xlsx_path), stringsAsFactors = FALSE)
    old_keyed <- old_xlsx
    if (!is.null(prev_review)) {
      value_cols <- setdiff(names(prev_review), names(old_xlsx))
      old_keyed <- cbind(old_xlsx,
                         prev_review[match(old_xlsx$row_id, prev_review$row_id), value_cols, drop = FALSE])
    }
    review <- carry_forward_from(old_keyed, review)
  }

  # Which columns disagree for each row, straight from the flags computed
  # above (not re-parsed from the disagreements text, whose platform values
  # can themselves contain ";").
  flag_mat <- do.call(cbind, lapply(verdict_cols, function(col) flags[[col]][has_disagreement]))
  colnames(flag_mat) <- verdict_cols
  flagged_cols_for_row <- function(i) verdict_cols[flag_mat[i, ]]

  # A verdict judges one specific disagreement. A column that no longer
  # disagrees for this paper keeps no verdict, comment or version stamp: the
  # carry-forward key matches on values, and a verdict could otherwise sit on
  # a column where Cooper and Metacheck now agree and still be counted.
  for (col in verdict_cols) {
    off <- !flag_mat[, col]
    for (cc in paste0(col, c("_verdict", "_comment", stamp_suffixes))) review[[cc]][off] <- NA_character_
  }
  review$reviewed <- vapply(seq_len(nrow(review)), function(i) {
    flagged <- flagged_cols_for_row(i)
    if (length(flagged) == 0) return(NA)
    vcols <- paste0(flagged, "_verdict")
    all(!is.na(unlist(review[i, vcols])) & nzchar(trimws(unlist(review[i, vcols]))))
  }, logical(1))

  # The Cooper and Metacheck value each verdict was written about. Written
  # into the xlsx next to the verdicts (write_disagreement_worklist_xlsx()),
  # so the next run's carry-forward keys on the values stored WITH each
  # verdict rather than on this target's previous cached value: those two
  # drift apart whenever a build stops between this target and the xlsx
  # write (2026-10-12: a run that ran out of memory left 16 verdicts on
  # columns that no longer disagreed).
  value_cols <- unique(unlist(lapply(verdict_cols, function(col) c(cooper_col_for(col), mc_col_for(col)))))

  list(review = review, verdict_cols = verdict_cols, stamp_suffixes = stamp_suffixes,
       value_cols = value_cols, flagged = flag_mat)
}

# The worklist counterpart of .audit_staleness(): one row per written
# verdict, flagged stale when its stamp is not an exact match for the
# currently installed metacheck -- a list of verdicts to re-check, not an
# error to fix automatically.
check_worklist_staleness <- function(disagreement_worklist, current_sha) {
  review <- disagreement_worklist$review
  out <- do.call(rbind, lapply(disagreement_worklist$verdict_cols, function(col) {
    v <- review[[paste0(col, "_verdict")]]
    has <- !is.na(v) & nzchar(trimws(v))
    if (!any(has)) return(NULL)
    exact <- as.character(review[[paste0(col, "_version_is_exact")]][has])
    ver <- review[[paste0(col, "_metacheck_version")]][has]
    data.frame(
      row_id = review$row_id[has], column = col, verdict = v[has],
      metacheck_version = ver,
      metacheck_version_date = review[[paste0(col, "_metacheck_version_date")]][has],
      version_is_exact = exact,
      stale = is.na(current_sha) | !(exact %in% "TRUE") | (exact %in% "TRUE" & ver != current_sha),
      stringsAsFactors = FALSE)
  }))
  list(verdicts = out, current_metacheck_sha = current_sha)
}

# Writes the xlsx file a caller should persist at xlsx_path: only
# row_id/article_id/disagreements/reviewed plus the 7 verdict/comment
# pairs, carrying forward any OTHER columns already present in the old
# xlsx that are not one of review's own data columns (free-form notes),
# matched by row_id, so manual annotations outside the 7 verdict columns
# are never dropped on a rewrite.
write_disagreement_worklist_xlsx <- function(worklist, xlsx_path) {
  review <- worklist$review
  verdict_cols <- worklist$verdict_cols

  stamp_suffixes <- worklist$stamp_suffixes
  xlsx_core_cols <- c("row_id", "article_id", "disagreements", "reviewed",
                      unlist(lapply(verdict_cols, function(col)
                        paste0(col, c("_verdict", "_comment", stamp_suffixes)))),
                      worklist$value_cols)
  xlsx_out <- review[, xlsx_core_cols]
  if (file.exists(xlsx_path)) {
    old_xlsx <- as.data.frame(openxlsx::read.xlsx(xlsx_path), stringsAsFactors = FALSE)
    extra_cols <- setdiff(names(old_xlsx), c(xlsx_core_cols, names(review)))
    if (length(extra_cols) > 0) {
      extra <- old_xlsx[match(xlsx_out$row_id, old_xlsx$row_id), extra_cols, drop = FALSE]
      xlsx_out <- cbind(xlsx_out, extra)
    }
  }

  # Only actually write if the LOGICAL content changed. openxlsx::write.xlsx()
  # is not byte-stable (the .xlsx zip container embeds its own creation
  # metadata), so writing unconditionally would change the file's hash on
  # every single run even when nothing meaningful changed -- which, since
  # this file is itself a tracked upstream input (disagreement_worklist_xlsx_input
  # in _targets.R, read to carry forward hand-written verdicts), would
  # make this and every downstream target look perpetually outdated.
  # Confirmed directly: without this check, two consecutive tar_make()
  # runs with zero real edits each still rewrote the file and re-triggered
  # the same three targets every time.
  needs_write <- TRUE
  if (file.exists(xlsx_path)) {
    on_disk <- tryCatch(
      as.data.frame(openxlsx::read.xlsx(xlsx_path), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    # Compared as text: read.xlsx() returns an all-NA or "TRUE"/"FALSE"
    # column (e.g. the version stamps) as logical, so a type-sensitive
    # comparison would report a change on every run.
    as_text <- function(df) {
      df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
      df
    }
    if (!is.null(on_disk) && identical(names(on_disk), names(xlsx_out)) &&
        isTRUE(all.equal(as_text(on_disk), as_text(xlsx_out), check.attributes = FALSE))) {
      needs_write <- FALSE
    }
  }

  if (needs_write) openxlsx::write.xlsx(xlsx_out, xlsx_path, overwrite = TRUE)
  xlsx_path
}
