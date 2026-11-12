library(targets)
library(readxl)
library(dplyr)

# Every object below is a target of the pipeline in _targets.R. Run
# targets::tar_make() to bring them up to date; quarto_render() alone does not.
#
# master_comparison is a strict superset of every other comparison-data
# object this manuscript needs: 
#
#   * Cooper et al.'s own columns, 
#   * metacheck's recreated mc_* columns, 
#   * open_practices/ODDPub's columns, 
#   * the data_open/code_open accuracy_summary attribute, 
#   * recorder_ID
#
# Joined against the DOI-corrected, deduplicated 1861-paper set --
# using this rather than Cooper's raw CSV directly avoids a known
# data-entry bug, see README.md's "known data-entry bug" section, where
# 4 rows' doi field was mistakenly copy-pasted from a different paper's
# row.
#
# TODO (open question, not yet resolved): master_comparison does not
# carry a data_used/code_used column even though Cooper et al. coded
# one -- metacheck has no module that answers "did this paper use
# data/code" as a question distinct from "is there a data/code file
# sitting in a linked repository" (see the Coding section's
# mc_code_used discussion), but Cooper's own coded data_used/code_used
# values could still be joined in here for the papers-that-use-data/
# code denominators used below, rather than reconstructed from
# data_availability/code_archived as a proxy.

# try running targets make if the load fails
tryCatch(tar_load(master_comparison, store = "_targets"),
         error = \(e) {
           tar_make()
           tar_load(master_comparison, store = "_targets")
         })
accuracy_summary <- attr(master_comparison, "accuracy_summary")

# comparison_statistics:
#   * confusion tables, 
#   * sensitivity/specificity, 
#   * the two directional miss rates, 
#   * McNemar's test for every comparison column
# from master_comparison, n = 1861, agreements included, and the
# individually-verified verdicts in the disagreement worklist) and
# disagreement_cause_categories (a reproducible, keyword-based
# categorization of each verified disagreement's root cause, computed
# directly from the verdict comments rather than hand-tallied). Both
# compare the three CORE Metacheck modules' (repo_check/data_check/
# code_check) recreated columns against Cooper et al.'s ground truth --
# see R/comparison_statistics.R and R/disagreement_cause_categories.R
# for the full derivation.

tar_load(comparison_statistics, store = "_targets")
list2env(comparison_statistics, envir = .GlobalEnv)
tar_load(disagreement_cause_categories, store = "_targets")
list2env(disagreement_cause_categories, envir = .GlobalEnv)

# The disagreement worklist itself, for the qualitative review counts
# quoted in the Empirical Comparison section (how many of the n
# disagreements were individually reviewed).
tar_load(disagreement_worklist, store = "_targets")
list2env(disagreement_worklist, envir = .GlobalEnv)

# Where the mistake lies in each disagreement about code. 
# These two columns are kept in the review file by hand, 
# and force(disagreement_worklist) reruns the render when that file changes.
code_categories <- "data/review/disagreement_review_data.xlsx" |>
  read_xlsx() |>
  select(code_category, code_category_reason,code_archived_verdict) |>
  filter(!is.na(code_category))

# functions for in-text reporting
n_code <- function(category) sum(code_categories$code_category == category)
n_code_reason <- function(text) sum(grepl(text, code_categories$code_category_reason, fixed = TRUE))

# create review status sentence
n_review <- nrow(review)
n_reviewed <- sum(review$reviewed, na.rm = TRUE)
review_status <- if (n_reviewed == n_review) {
  sprintf("all %d disagreeing papers carry a verdict for every column on which they disagree", n_review)
} else {
  sprintf("%d of the %d disagreeing papers carry a verdict for every column on which they disagree; the other %d have not yet been reviewed",
          n_reviewed, n_review, n_review - n_reviewed)
}
Review_status <- paste0(toupper(substr(review_status, 1, 1)), substring(review_status, 2))

# side_by_side: master_comparison's own Cooper-vs-metacheck columns,
# under the shorter name several later chunks use.
side_by_side <- master_comparison

# Papers Cooper et al. coded as having code on BOTH GitHub and Zenodo,
# where Metacheck found code on only one of the two: each was reviewed
# by hand against the paper's availability statement and Metacheck's
# repository listing (code/03_comparing_results/
# code_archive_github_zenodo_review_data.csv). The review must cover exactly
# the papers the current comparison puts in that group.
gz_review <- read.csv("data/review/code_archive_github_zenodo_review_data.csv")
gh_or_zenodo <- results$code_archive$github_zenodo_found %in% c("github_only", "zenodo_only")
gz_one_ids <- side_by_side$article_id[gh_or_zenodo]
if (!setequal(gz_review$article_id, gz_one_ids)) {
  stop("code_archive_github_zenodo_review_data.csv does not match the current set of ",
       "papers with code found on only one of GitHub and Zenodo")
}
gz_n <- function(category, missed = c("GitHub", "Zenodo")) {
  sum(gz_review$category == category & gz_review$platform_missed %in% missed)
}

# Papers whose data platforms share nothing between Cooper et al. and
# Metacheck: each was reviewed by hand against the availability statement
# in Cooper et al.'s data, Metacheck's repository listing and DataCite/
# Crossref author records (code/03_comparing_results/
# data_archive_no_shared_platform_review_data.csv). For the deposit Cooper et
# al. coded that Metacheck missed, missed_cause uses the same
# root_cause_category codes as manual_repo_findability_audit_data.csv, so it
# can be sorted into the same five clusters as the audit's table of reasons.
none_review <- read.csv("data/review/data_archive_no_shared_platform_review_data.csv")
none_ids <- side_by_side$article_id[results$data_archive$category %in% "none"]
if (!setequal(none_review$article_id, none_ids)) {
  stop("data_archive_no_shared_platform_review_data.csv does not match the current set of ",
       "papers whose data platforms share nothing")
}
none_n <- function(group) sum(none_review$group %in% group)
none_missed <- none_review[!is.na(none_review$missed_cause), ]
none_cause <- function(causes, fair = NULL) {
  keep <- none_missed$missed_cause %in% causes
  if (!is.null(fair)) keep <- keep & none_missed$fair_machine_findable_in_principle %in% fair
  sum(keep)
}
none_not_fair <- none_cause("DATA_OR_CODE_NOT_IN_FAIR_FORMAT") + none_cause("PLATFORM_NOT_SUPPORTED", "FALSE")
none_unsupported_fair <- none_cause("PLATFORM_NOT_SUPPORTED", "TRUE")
none_unsupported_format <- none_cause(c("CLASSIFIER_EXTENSION_GAP", "CLASSIFIER_MISTYPED"))
none_limitation <- none_cause(c("EXTRACTION_GAP_DOI_GARBLED", "EXTRACTION_GAP_TEXT_MISSING",
                                "SUPPORTED_PLATFORM_BUG", "CLASSIFIER_ARCHIVE_NOT_EXPANDED",
                                "FILES_NOT_ENUMERATED"))
stopifnot(none_not_fair + none_unsupported_fair + none_unsupported_format + none_limitation ==
            nrow(none_missed))
# Papers where Metacheck lists an extra data platform that is GitHub.
n_data_mc_more_github <- sum(results$data_archive$category %in% "mc_more" &
                               grepl("GitHub", side_by_side$mc_data_archive, fixed = TRUE) &
                               !grepl("GitHub", side_by_side$data_archive, fixed = TRUE))

# GitHub repositories of someone other than the paper's authors, and the
# Metacheck answers they alone produced (R/third_party_repositories.R).
targets::tar_load(third_party_repositories, store = "_targets")
tp <- third_party_repositories

# Cause code written into every verdict of the third review (2026-10-15,
# code/03_comparing_results/review_2026-10-15/): COOPER_ERROR,
# METACHECK_BUG, NOT_FAIR, UNSUPPORTED_FAIR_PLATFORM, DIFFERENT_DEFINITION or NONE.
verdict_cause <- function(col) {
  cm <- review[[paste0(col, "_comment")]]
  ifelse(grepl("CAUSE: ", cm, fixed = TRUE), sub("^.*CAUSE: ([A-Z_]+).*$", "\\1", cm), NA_character_)
}
n_cause <- function(col, cause, verdict = c("COOPER_RIGHT", "METACHECK_RIGHT", "BOTH_DEFENSIBLE")) {
  sum(verdict_cause(col) %in% cause & review[[paste0(col, "_verdict")]] %in% verdict)
}
# Coder-right data_download disagreements whose review comment matches a pattern.
n_dl_right <- function(pattern) {
  sum(review$data_download_verdict %in% "COOPER_RIGHT" &
        grepl(pattern, review$data_download_comment, perl = TRUE))
}
# DIFFERENT_DEFINITION occurs only for README: Cooper et al. asked about a README or metadata,
# whereas Metacheck looks for a README file.
stopifnot(!any(unlist(review[paste0(c("data_availability", "data_archive", "data_license", "data_download",
                                      "code_archived", "code_archive", "code_download"),
                                    "_verdict")]) %in% c("DIFFERENT_DEFINITION", "UNCLEAR")),
          !any(review$any_readme_verdict %in% "UNCLEAR"))

# README disagreements by direction, and Cooper et al.'s uncoded README answers.
readme_cooper_yes <- review$cooper_any_readme %in% TRUE
n_readme <- function(cooper_yes, verdict) sum(readme_cooper_yes == cooper_yes & review$any_readme_verdict %in% verdict)
n_readme_description <- sum(readme_cooper_yes & review$any_readme_verdict %in% "DIFFERENT_DEFINITION" &
                              grepl("landing page|description|abstract", review$any_readme_comment, ignore.case = TRUE))
readme_uncoded <- is.na(master_comparison$cooper_any_readme)
n_readme_uncoded <- sum(readme_uncoded)
n_readme_uncoded_unsure <- sum(readme_uncoded & (master_comparison$data_README %in% "Unsure" |
                                                   master_comparison$code_README %in% "Unsure"))
n_readme_uncoded_archived <- sum(readme_uncoded & (master_comparison$data_availability %in% "Yes" |
                                                     master_comparison$code_archived %in% "Yes"))
# The 16 papers whose own R-package repository the R-package-source-tree
# exclusion emptied (RevGadgets, BaBA, m2b, RRPP, actel, scampr, MINOTAUR,
# LadderFuelsR, biomonitoR, EcoEnsemble, fossilsim, frair, tRophicPosition,
# sabinaNSDM, bef, phylomice): how many now have their code found.
rpkg16_ids <- c("10_1111_2041_210x_13750", "10_1111_1365_2664_13806", "10_1111_2041_210x_12989",
                "10_1111_2041_210x_13029", "10_1111_2041_210x_13503", "10_1111_2041_210x_14252",
                "10_1111_2041_210x_12774", "10_1111_2041_210x_14427", "10_1111_1365_2664_13795",
                "10_1111_2041_210x_14148", "10_1111_2041_210x_13170", "10_1111_2041_210x_12784",
                "10_1111_2041_210x_13009", "10_1111_2041_210x_14417", "10_1111_1365_2435_13455",
                "10_1111_1365_2435_14552")
stopifnot(length(unique(rpkg16_ids)) == 16, all(rpkg16_ids %in% master_comparison$article_id))
n_rpkg16_resolved <- sum(master_comparison$mc_code_archived[match(rpkg16_ids, master_comparison$article_id)] %in% TRUE)
n_readme_in_archive <- sum(review$any_readme_verdict %in% "COOPER_RIGHT" &
                             grepl("README is inside|README sits inside|tar archive|\\.rar|\\.7z|ZIP64",
                                   review$any_readme_comment))

# Data download: papers whose data Cooper et al.'s coders downloaded, and
# what Metacheck answered for them (NA = it downloaded no data file at all,
# so the paper drops out of the comparison).
dl_cooper_yes <- master_comparison$data_download %in% c("Yes", "Yes, but not all data")
n_dl_cooper_yes <- sum(dl_cooper_yes)
n_dl_mc_true <- sum(dl_cooper_yes & master_comparison$mc_data_download %in% TRUE)
n_dl_mc_na <- sum(dl_cooper_yes & is.na(master_comparison$mc_data_download))

# How OddPub/open_practices classify the papers EXCLUDED from the
# data_open/code_open accuracy_summary comparison above -- split into
# the two structurally different reasons a paper is excluded (left
# unscored despite the question applying, vs. explicitly given Cooper
# et al.'s own "Maybe" label for needing specific software or being too
# large), since these are not the same kind of missingness and may
# behave differently. There is no human ground truth for either group,
# so this cannot be scored as correct/incorrect, only reported as each
# tool's own claim rate on papers the human coders did not give a clear
# Yes/No for.
#
# "Blank" is restricted to papers where data_open/code_open is NA *and*
# the question should have applied (data_availability/code_archived ==
# "Yes") -- most blank data_open/code_open rows are blank simply
# because data/code was never archived in the first place (the
# question was never asked), which is not "left unscored" and would
# otherwise swamp this count, especially for code (1089 of 1329 blank
# code_open rows have code_archived == "No", only 49 have
# code_archived == "Yes" and are genuinely missing an answer).
blank_data_open <- is.na(side_by_side$data_open) &
  !is.na(side_by_side$data_availability) & side_by_side$data_availability == "Yes"
n_blank_data_open <- sum(blank_data_open)
n_oddpub_open_blank_data <- sum(side_by_side$oddpub_data_open[blank_data_open], na.rm = TRUE)
n_mc_open_blank_data <- sum(side_by_side$mc_data_open[blank_data_open], na.rm = TRUE)

ambig_data_open <- !is.na(side_by_side$data_open) &
  side_by_side$data_open == "Needs specific software or too large"
n_ambig_data_open <- sum(ambig_data_open)
n_oddpub_open_ambig_data <- sum(side_by_side$oddpub_data_open[ambig_data_open], na.rm = TRUE)
n_mc_open_ambig_data <- sum(side_by_side$mc_data_open[ambig_data_open], na.rm = TRUE)

blank_code_open <- is.na(side_by_side$code_open) &
  !is.na(side_by_side$code_archived) & side_by_side$code_archived == "Yes"
n_blank_code_open <- sum(blank_code_open)
n_oddpub_open_blank_code <- sum(side_by_side$oddpub_code_open[blank_code_open], na.rm = TRUE)
n_mc_open_blank_code <- sum(side_by_side$mc_code_open[blank_code_open], na.rm = TRUE)

ambig_code_open <- !is.na(side_by_side$code_open) &
  side_by_side$code_open == "Maybe if I had the right software"
n_ambig_code_open <- sum(ambig_code_open)
n_oddpub_open_ambig_code <- sum(side_by_side$oddpub_code_open[ambig_code_open], na.rm = TRUE)
n_mc_open_ambig_code <- sum(side_by_side$mc_code_open[ambig_code_open], na.rm = TRUE)

# The few columns of the module tables this manuscript reads (code/R/slim_extraction.R).
targets::tar_load(manuscript_data_slim, store = "_targets")
repo_metadata <- manuscript_data_slim$repo_metadata
structure_slim <- manuscript_data_slim$structure_slim
code_table_slim <- manuscript_data_slim$code_table_slim

# Small formatting helpers used throughout the Empirical Comparison
# section, so every percentage/test-statistic there is computed rather
# than typed.
pct1 <- function(x) sprintf("%.2g%%", x * 100)
# format an N and percent from numerator and denominator
npcnt <- function(num, denom) {
  pct <- pct1(num / denom)
  sprintf("%d (%s)", num, pct)
}
# format a p-value < .001
pfmt <- function(p) if (p < .001) "< .001" else sprintf("= %.3f", p)
mcnemar_str <- function(r) {
  if (is.null(r$mcnemar_stat) || is.na(r$mcnemar_stat)) return("not computable")
  sprintf("$\\chi^2$(%d) = %.2g, *p* %s", r$mcnemar_df, r$mcnemar_stat, pfmt(r$mcnemar_p))
}
vt  <- function(tallies, verdict) { n <- unname(tallies[verdict]); if (is.na(n)) 0L else n }
cc  <- function(tab, bucket) { n <- unname(tab[bucket]); if (is.na(n)) 0L else n }