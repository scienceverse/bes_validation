# Computes the full empirical comparison statistics referenced by the
# manuscript's "Empirical Comparison" section: descriptives, 2x2
# confusion tables, sensitivity/specificity, McNemar's test, and the two
# asymmetric miss-rate percentages -- "Cooper found X% of what metacheck
# missed" and "metacheck found Y% of what Cooper missed" -- for every
# column with a genuine boolean Cooper-vs-metacheck comparison:
#   data_availability, data_archive (overlap-based, handled separately),
#   data_license, data_download, any_readme, code_archived, code_download
#
#
# Two data sources, used for different things:
#   - master_comparison (the FULL corpus, agreements included): used for
#     confusion tables, N's, sensitivity/specificity, McNemar's test.
#   - the disagreement worklist (disagreements only, each with an
#     individually-verified _verdict/_comment): used for the qualitative
#     COOPER_RIGHT/METACHECK_RIGHT/etc. verdict tallies, which only exist
#     for genuine disagreements.

compute_comparison_statistics <- function(master_comparison, disagreement_worklist_review) {
  side_by_side <- master_comparison
  review <- disagreement_worklist_review

  cooper_bool <- function(x, positive, negative) {
    out <- rep(NA, length(x))
    out[x %in% positive] <- TRUE
    out[x %in% negative] <- FALSE
    out
  }

  bool_pairs <- list(
    data_availability = list(cooper = "data_availability", mc = "mc_data_availability",
                             positive = "Yes", negative = c("No", "No, but they are available on request"),
                             label = "Data availability"),
    data_license      = list(cooper = "data_license", mc = "mc_data_license",
                             positive = "Yes", negative = "No",
                             label = "Data licence present"),
    data_download     = list(cooper = "data_download", mc = "mc_data_download",
                             positive = c("Yes", "Yes, but not all data"), negative = "No",
                             label = "Data downloadable and usable"),
    code_archived     = list(cooper = "code_archived", mc = "mc_code_archived",
                             positive = "Yes", negative = "No",
                             label = "Code archived"),
    code_download     = list(cooper = "code_download", mc = "mc_code_download",
                             positive = "Yes", negative = "No",
                             label = "Code downloadable")
  )

  # any_readme is reconstructed the same way cooper_vs_metacheck already
  # does, directly from side_by_side's data_README/code_README +
  # mc_data_README, so the full N (agreements included) is available here.
  data_readme_bool <- cooper_bool(side_by_side$data_README, c("Yes", "Quasi-README"), "No")
  code_readme_bool <- cooper_bool(side_by_side$code_README, c("Yes", "Quasi-README"), "No")
  side_by_side$cooper_any_readme <- ifelse(
    !is.na(data_readme_bool) & data_readme_bool, TRUE,
    ifelse(!is.na(code_readme_bool) & code_readme_bool, TRUE,
    ifelse(is.na(data_readme_bool) & is.na(code_readme_bool), NA, FALSE)))
  side_by_side$mc_any_readme <- side_by_side$mc_data_README

  results <- list()

  for (nm in names(bool_pairs)) {
    p <- bool_pairs[[nm]]
    cb <- cooper_bool(side_by_side[[p$cooper]], p$positive, p$negative)
    mb <- side_by_side[[p$mc]]

    both <- !is.na(cb) & !is.na(mb)
    n_both <- sum(both)
    cb2 <- cb[both]; mb2 <- mb[both]

    tp <- sum(cb2 & mb2)     # Cooper Yes, metacheck Yes
    fn <- sum(cb2 & !mb2)    # Cooper Yes, metacheck No  (metacheck MISSED what Cooper found)
    fp <- sum(!cb2 & mb2)    # Cooper No,  metacheck Yes (metacheck found what Cooper MISSED)
    tn <- sum(!cb2 & !mb2)   # Cooper No,  metacheck No

    sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    ppv <- tp / (tp + fp)
    npv <- tn / (tn + fn)
    accuracy <- (tp + tn) / n_both

    n_disagree <- fn + fp
    pct_cooper_caught_mc_missed <- if (n_disagree > 0) fn / n_disagree * 100 else NA
    pct_mc_caught_cooper_missed <- if (n_disagree > 0) fp / n_disagree * 100 else NA

    mcnemar <- if ((fn + fp) > 0) {
      tryCatch(mcnemar.test(matrix(c(tp, fn, fp, tn), nrow = 2), correct = TRUE),
               error = function(e) NULL)
    } else NULL

    results[[nm]] <- list(
      label = p$label,
      n_cooper_coded = sum(!is.na(cb)),
      n_mc_coded = sum(!is.na(mb)),
      n_both_coded = n_both,
      tp = tp, fn = fn, fp = fp, tn = tn,
      sensitivity = sensitivity, specificity = specificity,
      ppv = ppv, npv = npv, accuracy = accuracy,
      n_agree = tp + tn, n_disagree = n_disagree,
      pct_agree = (tp + tn) / n_both * 100,
      pct_cooper_caught_mc_missed = pct_cooper_caught_mc_missed,
      pct_mc_caught_cooper_missed = pct_mc_caught_cooper_missed,
      mcnemar_stat = if (!is.null(mcnemar)) unname(mcnemar$statistic) else NA,
      mcnemar_df = if (!is.null(mcnemar)) unname(mcnemar$parameter) else NA,
      mcnemar_p = if (!is.null(mcnemar)) mcnemar$p.value else NA
    )
  }

  # any_readme (not in bool_pairs since its columns are named differently)
  {
    cb <- side_by_side$cooper_any_readme
    mb <- side_by_side$mc_any_readme
    both <- !is.na(cb) & !is.na(mb)
    n_both <- sum(both)
    cb2 <- cb[both]; mb2 <- mb[both]
    tp <- sum(cb2 & mb2); fn <- sum(cb2 & !mb2); fp <- sum(!cb2 & mb2); tn <- sum(!cb2 & !mb2)
    sensitivity <- tp / (tp + fn); specificity <- tn / (tn + fp)
    ppv <- tp / (tp + fp); npv <- tn / (tn + fn)
    n_disagree <- fn + fp
    pct_cooper_caught_mc_missed <- if (n_disagree > 0) fn / n_disagree * 100 else NA
    pct_mc_caught_cooper_missed <- if (n_disagree > 0) fp / n_disagree * 100 else NA
    mcnemar <- if ((fn + fp) > 0) tryCatch(mcnemar.test(matrix(c(tp, fn, fp, tn), nrow = 2), correct = TRUE), error = function(e) NULL) else NULL
    results[["any_readme"]] <- list(
      label = "Any README present (data or code, collapsed)",
      n_cooper_coded = sum(!is.na(cb)), n_mc_coded = sum(!is.na(mb)), n_both_coded = n_both,
      tp = tp, fn = fn, fp = fp, tn = tn,
      sensitivity = sensitivity, specificity = specificity, ppv = ppv, npv = npv,
      accuracy = (tp + tn) / n_both,
      n_agree = tp + tn, n_disagree = n_disagree, pct_agree = (tp + tn) / n_both * 100,
      pct_cooper_caught_mc_missed = pct_cooper_caught_mc_missed,
      pct_mc_caught_cooper_missed = pct_mc_caught_cooper_missed,
      mcnemar_stat = if (!is.null(mcnemar)) unname(mcnemar$statistic) else NA,
      mcnemar_df = if (!is.null(mcnemar)) unname(mcnemar$parameter) else NA,
      mcnemar_p = if (!is.null(mcnemar)) mcnemar$p.value else NA
    )
  }

  # data_archive / code_archive: both sides can name several platforms
  # (";"-joined), so they are compared as SETS rather than as a boolean
  # (platform_set_comparison(), R/platforms.R).
  results[["data_archive"]] <- platform_set_comparison(
    side_by_side$data_archive, side_by_side$mc_data_archive,
    "Data archive platform (exact match of platform sets)",
    side_by_side$mc_data_archive_institutional_figshare %in% TRUE)
  results[["code_archive"]] <- platform_set_comparison(
    side_by_side$code_archive, side_by_side$mc_code_archive,
    "Code archive platform (exact match of platform sets)",
    side_by_side$mc_code_archive_institutional_figshare %in% TRUE)

  # Qualitative verdict tallies from the worklist (disagreements only,
  # each individually reviewed and resolved).
  # The worklist itself only keeps a verdict on a column that disagrees for
  # that paper (build_disagreement_worklist()), so these count disagreements.
  verdict_cols <- c("data_availability", "data_archive", "data_license",
                    "data_download", "code_archived", "code_archive",
                    "code_download", "any_readme")
  verdict_tallies <- list()
  for (nm in verdict_cols) {
    vcol <- paste0(nm, "_verdict")
    if (!vcol %in% names(review)) next
    verdict_tallies[[nm]] <- table(review[[vcol]], useNA = "no")
  }

  list(results = results, verdict_tallies = verdict_tallies)
}
