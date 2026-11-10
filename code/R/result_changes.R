# What changed between the last results of the earlier repository and the final run, for
# appendix A.11. The earlier values and the causes are saved in data/review/ by
# code/compare_earlier_results.R; everything else is computed here from the current results.

# TRUE when Cooper et al. and Metacheck agree on one column, NA when one of them has no answer.
# The same rules as build_disagreement_worklist() and platform_set_category().
.agrees_with_cooper <- function(column, cooper, metacheck, inst = FALSE) {
  if (column %in% c("data_archive", "code_archive")) {
    category <- platform_set_category(cooper, metacheck, inst)
    return(ifelse(is.na(category), NA, category == "exact"))
  }
  positive <- switch(column,
                     data_download = c("Yes", "Yes, but not all data"),
                     any_readme = "TRUE",
                     "Yes")
  negative <- switch(column,
                     data_availability = c("No", "No, but they are available on request"),
                     any_readme = "FALSE",
                     "No")
  cooper_true <- rep(NA, length(cooper))
  cooper_true[as.character(cooper) %in% positive] <- TRUE
  cooper_true[as.character(cooper) %in% negative] <- FALSE
  cooper_true == as.logical(metacheck)
}

summarize_result_changes <- function(changes_csv, causes_csv, percentages_csv, other_csv,
                                     master_comparison, comparison_statistics) {
  read <- function(f) utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "NA")
  changes <- read(changes_csv); causes <- read(causes_csv); percentages <- read(percentages_csv); other <- read(other_csv)
  rows <- match(changes$article_id, master_comparison$article_id)

  now <- vapply(seq_len(nrow(changes)), function(i)
    as.character(master_comparison[[paste0("mc_", changes$column[i])]][rows[i]]), "")
  differs <- !(now == changes$final_value | (is.na(now) & is.na(changes$final_value)))
  differs[is.na(differs)] <- TRUE
  if (any(differs)) {
    stop("result_changes_data.csv does not match the current results; run code/compare_earlier_results.R again: ",
         paste(unique(changes$article_id[differs]), collapse = ", "))
  }
  if (!all(changes$cause %in% causes$cause)) stop("result_changes_data.csv has a cause without a description")

  agreement <- function(values) vapply(seq_len(nrow(changes)), function(i) {
    cl <- changes$column[i]
    cooper <- master_comparison[[if (cl == "any_readme") "cooper_any_readme" else cl]][rows[i]]
    inst <- master_comparison[[paste0("mc_", cl, "_institutional_figshare")]]
    inst <- if (is.null(inst)) FALSE else inst[rows[i]] %in% TRUE
    .agrees_with_cooper(cl, cooper, values[i], inst)
  }, NA)
  changes$agreed_before <- agreement(changes$earlier_value)
  changes$agrees_now <- agreement(changes$final_value)
  # A value that could not be compared before, because either side had no answer, is
  # not the same as one that agreed or disagreed before.
  changes$effect <- ifelse(changes$agreed_before %in% FALSE & changes$agrees_now %in% TRUE, "toward",
                    ifelse(changes$agreed_before %in% TRUE & changes$agrees_now %in% FALSE, "away",
                    ifelse(is.na(changes$agreed_before) & changes$agrees_now %in% TRUE, "new_agreement",
                    ifelse(is.na(changes$agreed_before) & changes$agrees_now %in% FALSE, "new_disagreement",
                    ifelse(changes$agreed_before %in% FALSE & changes$agrees_now %in% FALSE, "still_disagrees",
                           "not_comparable")))))

  by_cause <- do.call(rbind, lapply(causes$cause, function(k) {
    x <- changes[changes$cause == k, ]
    data.frame(cause = k, n_papers = length(unique(x$article_id)), n_values = nrow(x),
               n_toward = sum(x$effect == "toward"), n_away = sum(x$effect == "away"),
               n_new_agreement = sum(x$effect == "new_agreement"),
               n_new_disagreement = sum(x$effect == "new_disagreement"),
               n_other = sum(x$effect %in% c("still_disagrees", "not_comparable")))
  }))
  by_cause <- merge(causes, by_cause, by = "cause", sort = FALSE)

  results <- comparison_statistics$results
  percentages$label <- vapply(percentages$column, function(cl) results[[cl]]$label, "")
  percentages$final_pct_agree <- vapply(percentages$column, function(cl) results[[cl]]$pct_agree, 0)
  percentages$final_n_both_coded <- vapply(percentages$column, function(cl) results[[cl]]$n_both_coded, 0)

  list(changes = changes, by_cause = by_cause, percentages = percentages, other_columns = other,
       n_papers = length(unique(changes$article_id)), n_values = nrow(changes))
}
