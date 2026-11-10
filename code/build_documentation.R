# Create the codebook next to each data file, and ro-crate-metadata.json.
#
# The labels below are written by hand. The values of each column are read from the data,
# so that a codebook cannot drift from its file. Cooper et al.'s questions are quoted from
# their protocol, documentation/cooper_supplement_documentation.pdf.
#
# Usage, from the repository root: Rscript code/build_documentation.R

# == Codebooks ==========================================================================

# A column's values are listed when there are few; otherwise they are free text or numbers.
describe_values <- function(x, max_values = 12) {
  x <- x[!is.na(x)]
  u <- sort(unique(as.character(x)))
  if (length(u) == 0 || length(u) > max_values) return("")
  paste(u, collapse = "; ")
}

write_codebook <- function(data, labels, path, questions = character()) {
  stopifnot(setequal(names(labels), names(data)))
  out <- data.frame(
    variable_name = names(data),
    label = unname(labels[names(data)]),
    values = vapply(data, describe_values, ""),
    missing_values = "empty cell or NA",
    question = ifelse(names(data) %in% names(questions), unname(questions[names(data)]), "")
  )
  utils::write.csv(out, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

na <- c("NA", "")

## Cooper et al.'s coding -----------------------------------------------------------------
cooper <- utils::read.csv("data/cooper_coded_data.csv", check.names = FALSE, na.strings = na)
write_codebook(cooper, c(
  paper_number = "Number of the paper on the hackathon's spreadsheet",
  doi = "DOI of the paper",
  year_published = "Year of publication",
  journal = "British Ecological Society journal",
  article_type = "Article type",
  data_used = "Whether the paper has data",
  data_availability = "Whether the data are archived, as coded from the Data Availability statement",
  data_availability_text = "Text of the paper's Data Availability statement",
  data_link = "Whether the coder could find the data with the link or instructions given",
  data_archive = "Where the data are archived; several places are separated by a semicolon",
  data_doi = "Whether the dataset has a DOI",
  data_license = "Whether the data have a license",
  data_license_type = "Type of the data's license",
  data_download = "Whether the coder could download the data",
  data_open = "Whether the coder could open the data",
  data_format = "File formats of the data; several are separated by a semicolon",
  data_README = "Whether the data have a README or metadata",
  data_README_scale = "Usefulness of the data's README: 1 is very brief and incomplete, 10 is understood in a few minutes",
  data_completeness = "How complete the archived data are",
  code_used = "Whether the paper uses code",
  code_alert = "Text of the paper that mentions code which is not archived",
  code_archived = "Whether the paper has archived code",
  code_availability = "Whether the code archiving is described in the Data Availability statement",
  code_link = "Whether the coder could find the code with the link or instructions given",
  code_archive = "Where the code is archived; several places are separated by a semicolon",
  code_doi = "Whether the code has a DOI",
  code_license = "Whether the code has a license",
  code_license_type = "Type of the code's license",
  code_CITATION = "Whether the code has a CITATION file",
  code_download = "Whether the coder could download the code",
  code_open = "Whether the coder could open the code",
  code_format = "Format of the code; several are separated by a semicolon",
  code_language = "Programming languages of the code; several are separated by a semicolon",
  code_README = "Whether the code has a README",
  code_README_scale = "Usefulness of the code's README, on a scale of 1 to 10",
  code_annotation_scale = "How well the code is annotated, on a scale of 1 to 10",
  code_vignette = "Whether the code has a separate vignette with examples",
  code_Rpackage_available = "For an R package, whether it is available on CRAN or Bioconductor",
  code_application_cited = "Number of citations of a code application, as entered by the coder",
  code_comments = "Coder's comments on the code",
  country_first = "Country of the first author's main address",
  comments = "Coder's comments on the paper",
  recorder_ID = "Anonymous number of the person who coded the paper"
), "data/cooper_coded_codebook.csv", questions = c(
  data_availability_text = "Please paste the text of the Data Availability statement here",
  data_used = "Does the paper have data?",
  data_archive = "Where are the data archived (select all that apply)?",
  data_doi = "Does the dataset have a DOI?",
  data_download = "Can you download the data?",
  data_open = "Can you open the data?",
  data_format = "What format are the data in?",
  data_README = "Does the data have a README/metadata?",
  data_README_scale = "How useful is the README/metadata?",
  data_completeness = "How complete are the archived data?",
  data_license = "Does the data have a license?",
  data_license_type = "If so, which license?",
  code_used = "Does the paper use code?",
  code_archived = "Does the paper have archived code?",
  code_archive = "Where is the code archived?",
  code_doi = "Does the code have a DOI?",
  code_download = "Can you download the code?",
  code_open = "Can you open the code?",
  code_language = "What language is the code written in?",
  code_format = "What format is the code in?",
  code_README = "Does the code have a README?",
  code_README_scale = "How useful is the README?",
  code_annotation_scale = "How well is the code annotated?",
  code_CITATION = "Does the code have a CITATION file?"
))

## Sample ---------------------------------------------------------------------------------
write_codebook(utils::read.csv("data/papers_data.csv", na.strings = na), c(
  doi = "DOI of the paper",
  journal = "Journal",
  year = "Year of publication",
  paper_number = "Number of the paper in Cooper et al.'s coding (paper_number in cooper_coded_data.csv)",
  article_id = "This project's identifier of the paper, made from its DOI",
  source = "Where the paper's text was obtained",
  ok = "Whether the paper's text was obtained"
), "data/papers_codebook.csv")

## Reviews --------------------------------------------------------------------------------
review <- function(name) utils::read.csv(file.path("data/review", paste0(name, "_data.csv")), check.names = FALSE, na.strings = na)
version_labels <- c(
  metacheck_version = "metacheck commit the row was written for",
  metacheck_version_date = "Date of that commit",
  version_is_exact = "TRUE when the commit was recorded at the time, FALSE when inferred from the date of the row"
)

write_codebook(review("manual_repo_findability_audit"), c(
  paper_doi = "DOI of the paper",
  root_cause_category = "Why metacheck did not find the repository the coders found",
  manual_repo_access_status = "Whether the repository could be reached by hand",
  manual_repo_license = "License found by hand",
  manual_why_metacheck_misses_it = "Explanation of the miss, written by the reviewer",
  link_matches_coder_verbatim = "Whether the link found matches the coders' text exactly",
  additional_links_found = "Further links found by the reviewer",
  version_labels,
  doi_only_in_reference_list = "Whether the DOI appears only in the paper's reference list"
), "data/review/manual_repo_findability_audit_codebook.csv")

write_codebook(review("manual_fair_api_findability_audit"), c(
  paper_doi = "DOI of the paper",
  root_cause_category = "Why metacheck did not find the repository the coders found",
  fair_has_persistent_id = "Whether the resource has a persistent identifier",
  fair_has_documented_api = "Whether the host has a documented application programming interface",
  fair_api_evidence = "Evidence for the answer on the interface",
  fair_publicly_accessible_now = "Whether the resource was publicly accessible when checked",
  fair_machine_findable_in_principle = "Whether a machine could find the resource, in the reviewer's judgement",
  fair_reasoning = "Reasoning for that judgement",
  version_labels
), "data/review/manual_fair_api_findability_audit_codebook.csv")

write_codebook(review("repo_not_detected_categorization"), c(
  article_id = "This project's identifier of the paper",
  column = "Comparison column the row belongs to",
  comment = "Reviewer's comment",
  category = "Category of the reason the repository was not detected"
), "data/review/repo_not_detected_categorization_codebook.csv")

write_codebook(review("code_archive_github_zenodo_review"), c(
  article_id = "This project's identifier of the paper",
  platform_missed = "Platform on which metacheck did not find the code",
  category = "Category of the reason",
  comment = "Reviewer's comment"
), "data/review/code_archive_github_zenodo_review_codebook.csv")

write_codebook(review("data_archive_no_shared_platform_review"), c(
  article_id = "This project's identifier of the paper",
  group = "Group of the disagreement",
  missed_cause = "Cause code for a deposit metacheck missed, as in root_cause_category of the audit",
  fair_machine_findable_in_principle = "Whether a machine could find the deposit, in the reviewer's judgement",
  cooper_data_archive = "Where Cooper et al.'s coders recorded the data",
  mc_data_archive = "Where metacheck found the data",
  verdict = "Reviewer's verdict",
  comment = "Reviewer's comment"
), "data/review/data_archive_no_shared_platform_review_codebook.csv")

write_codebook(review("github_repository_ownership_review"), c(
  article_id = "This project's identifier of the paper",
  repo_url = "Address of the GitHub repository",
  owner = "Owner of the repository",
  repository = "Name of the repository",
  named_in_cooper_statement = "Whether the statement in Cooper et al.'s data names it",
  owner_is_author = "Whether the owner is an author of the paper",
  how_judged = "How that was judged",
  why_not_authors = "Why the repository is not the authors', when it is not",
  n_files = "Number of files metacheck listed in it",
  n_data_files = "Number of data files among them",
  n_code_files = "Number of code files among them",
  crossref_authors = "The paper's authors according to Crossref"
), "data/review/github_repository_ownership_review_codebook.csv")

write_codebook(review("result_changes"), c(
  article_id = "This project's identifier of the paper",
  column = "Column compared with Cooper et al. that changed",
  earlier_value = "Metacheck's value in the earlier repository's results of 2026-10-18",
  final_value = "Metacheck's value in the final results",
  cause = "Cause of the change, described in result_change_causes_data.csv"
), "data/review/result_changes_codebook.csv")

write_codebook(review("result_change_causes"), c(
  cause = "Name of the cause",
  metacheck_commit = "metacheck commit that made the change, or why none is given",
  mechanism = "What changed in metacheck",
  consequence = "What that did to the results"
), "data/review/result_change_causes_codebook.csv")

write_codebook(review("result_change_percentages"), c(
  column = "Column compared with Cooper et al.",
  earlier_pct_agree = "Percentage agreement in the earlier repository's results of 2026-10-18",
  earlier_n_both_coded = "Number of papers both coded in those results"
), "data/review/result_change_percentages_codebook.csv")

write_codebook(review("result_change_other_columns"), c(
  column = "Column of the comparison table that is not compared with Cooper et al.",
  n_papers = "Number of papers whose value changed",
  metacheck_commit = "metacheck commit that made the change, or why none is given",
  mechanism = "What changed"
), "data/review/result_change_other_columns_codebook.csv")

### The disagreement review has five columns for each comparison, and the values compared.
worklist <- openxlsx::read.xlsx("data/review/disagreement_review_data.xlsx")
compared <- c("data_availability", "data_archive", "data_license", "data_download",
              "code_archived", "code_archive", "code_download", "any_readme")
per <- unlist(lapply(compared, function(cl) stats::setNames(c(
  sprintf("Verdict on the disagreement about %s", cl),
  "Evidence for that verdict, ending in a cause code in the last review",
  "metacheck commit the verdict was written for",
  "Date of that commit",
  "TRUE when the commit was recorded at the time, FALSE when inferred"
), paste0(cl, c("_verdict", "_comment", "_metacheck_version", "_metacheck_version_date", "_version_is_exact")))))
values <- unlist(lapply(compared, function(cl) {
  cooper_name <- if (cl == "any_readme") "cooper_any_readme" else cl
  stats::setNames(c(sprintf("Cooper et al.'s value for %s", cl), sprintf("metacheck's value for %s", cl)),
                  c(cooper_name, paste0("mc_", cl)))
}))
write_codebook(worklist, c(
  row_id = "Number of the row",
  article_id = "This project's identifier of the paper",
  disagreements = "Comparison columns on which Cooper et al. and metacheck disagree",
  reviewed = "Whether every disagreement of this paper has a verdict",
  per, values,
  code_category = "Where the mistake lies in a disagreement about code: not_fair (the code was not shared in a FAIR way), metacheck (Metacheck's limitation or error), or coders_wrong (the manual coding was wrong)",
  code_category_reason = "The specific reason within that category"
), "data/review/disagreement_review_codebook.csv")

## Why scripts did not run: the two files the pipeline writes (code/R/reproducibility_review.R)
actor_label <- paste(
  "Who could make the script run, from its cause: authors (FILE_NOT_SHARED, CODE_ERROR,",
  "PACKAGE_SOURCE_UNKNOWN, NEEDS_PERSON), software_changed (PACKAGE_CHANGED, R_DEFAULT_CHANGED,",
  "PYTHON2), docker_image (PACKAGE_UNAVAILABLE, IMAGE_LACKS_LIBRARY, ENVIRONMENT), run_settings",
  "(TIME_LIMIT, NOT_DOWNLOADED, NEEDS_INTERNET), metacheck (PATH_NOT_MAPPED, METACHECK_BUG),",
  "unclear (FILE_NOT_FOUND, OTHER, or a code not listed); empty for a script that ran through or",
  "is not counted")
if (file.exists("data/reproducibility_failures_scripts_data.csv")) {
  write_codebook(utils::read.csv("data/reproducibility_failures_scripts_data.csv", na.strings = na), c(
    article_id = "This project's identifier of the paper",
    file_name = "Name of the script",
    file_path = "Path of the script in the deposit (empty for runs before metacheck 0.4.3)",
    outcome = "What reproducibility_check did with the script; ran_ok is ran through, and run_by_source, run_by_import, jags_model, not_r_content, defines_functions, not_analysis and docker_failed are not counted",
    reason = "metacheck's own reason why the script stopped",
    auto_cause = "The cause classified from the outcome, the reason and the error line; the codes are those of cause in data/review/reproducibility_notes_codebook.csv",
    error_line = "The last error line of the script's output",
    error_lines = "Every error line of the script's output, separated by ' | '",
    not_downloaded_file = "The deposit's files the script needs that were not downloaded, separated by '; '",
    not_downloaded_mb = "Size of those files together, in MB",
    run = "Folder of the run the script's result comes from",
    run_metacheck_version = "First 8 characters of the metacheck commit of that run",
    cause = "The cause: that of the note on the script in data/review/reproducibility_notes_data.csv, otherwise auto_cause",
    status = "What is to be done: none (ran through or not counted), not_reviewed (no note), or the note's status: open (metacheck can be improved), fixed, authors, decision",
    comment = "The note's evidence",
    note_metacheck_version = "metacheck commit the note was written for",
    note_date = "Date the note was written or last checked",
    outdated = "TRUE when the note was written for another metacheck commit than the run's",
    actor = actor_label
  ), "data/reproducibility_failures_scripts_codebook.csv")
}
if (file.exists("data/reproducibility_failures_papers_data.csv")) {
  write_codebook(utils::read.csv("data/reproducibility_failures_papers_data.csv", na.strings = na), c(
    article_id = "This project's identifier of the paper",
    scripts_n = "Number of scripts counted: tried, and not merely sourced, imported, a model or function file",
    ran_ok_n = "Number of counted scripts that ran through",
    not_tried_n = "Number of scripts not tried, as a file they need was not downloaded",
    largest_not_downloaded_mb = "The download limit (MB per file) under which every script could have been tried",
    verdict = "ALL_RAN, PARTLY_RAN, NONE_RAN, or NOTHING_TO_RUN (no script counted)",
    main_cause = "The most frequent cause among the counted scripts that did not run through; of equally frequent ones the first in alphabetical order",
    causes = "Every cause among those scripts, with its number of scripts",
    authors_n = "Number of those scripts whose actor is authors (see actor in data/reproducibility_failures_scripts_codebook.csv)",
    software_changed_n = "Number of those scripts whose actor is software_changed",
    docker_image_n = "Number of those scripts whose actor is docker_image",
    run_settings_n = "Number of those scripts whose actor is run_settings",
    metacheck_n = "Number of those scripts whose actor is metacheck",
    unclear_n = "Number of those scripts whose actor is unclear",
    open_n = "Number of those scripts whose note says metacheck can be improved (status open)",
    not_reviewed_n = "Number of counted scripts without a note",
    outdated_notes_n = "Number of scripts whose note was written for another metacheck commit",
    run = "Folder of the run the paper's result comes from",
    metacheck_version = "First 8 characters of the metacheck commit of that run"
  ), "data/reproducibility_failures_papers_codebook.csv")
}

## Results of the run: every table and column, read from the file ---------------------------
describe_results <- function(path) {
  r <- readRDS(path)
  one <- function(tbl, name) data.frame(
    table_name = name, variable_name = names(tbl),
    type = vapply(tbl, function(x) class(x)[1], ""),
    n_rows = nrow(tbl), n_missing = vapply(tbl, function(x) sum(is.na(x)), 0L),
    values = vapply(tbl, function(x) if (is.list(x)) "" else describe_values(x, 8), ""))
  parts <- list(one(r$summary_table, "summary_table"))
  for (m in setdiff(names(r), c("run_info", "summary_table", "errors"))) {
    for (t in names(r[[m]])) {
      if (is.data.frame(r[[m]][[t]]) && ncol(r[[m]][[t]])) parts[[length(parts) + 1]] <- one(r[[m]][[t]], paste0(m, "$", t))
    }
  }
  do.call(rbind, parts)
}
if (file.exists("data/metacheck_results.rds")) {
  utils::write.csv(describe_results("data/metacheck_results.rds"), "data/metacheck_results_codebook.csv",
                   row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# == RO-Crate ===========================================================================
# Lists every file of the repository, so a machine can find each without reading the README.
kind_of <- function(f) {
  if (grepl("_codebook[.]csv$", f)) return("Codebook of the data file with the same name")
  if (grepl("^data/review/", f)) return("Review by hand of metacheck's output")
  if (grepl("^data/", f)) return("Data")
  if (grepl("^code/", f)) return("Code")
  if (grepl("^tests/", f)) return("Code: tests of the pipeline")
  if (grepl("^documentation/", f)) return("Documentation")
  "File that applies to the whole repository"
}
files <- list.files(".", recursive = TRUE, all.files = TRUE)
files <- files[!grepl("^([.]git|[.]Rproj[.]user|_targets|[.]quarto|renv/library)(/|$)|^data/bes[.]rds$|^documentation/cooper_original_paper[.]pdf$|^ro-crate-metadata[.]json$", files)]
ro <- list(
  `@context` = "https://w3id.org/ro/crate/1.1/context",
  `@graph` = c(
    list(
      list(`@id` = "ro-crate-metadata.json", `@type` = "CreativeWork",
           conformsTo = list(`@id` = "https://w3id.org/ro/crate/1.1"), about = list(`@id` = "./")),
      list(`@id` = "./", `@type` = "Dataset",
           name = "Validating Metacheck against the BES data and code archiving corpus",
           description = paste("Results of running metacheck on 1861 papers from British Ecological Society journals,",
                               "the comparison with the coding by Cooper et al., and the code that creates the manuscript from them."),
           author = list(list(`@id` = "https://orcid.org/0000-0002-8393-5316")),
           hasPart = lapply(files, function(f) list(`@id` = f))),
      list(`@id` = "https://orcid.org/0000-0002-8393-5316", `@type` = "Person", name = "Daniel Lakens")
    ),
    lapply(files, function(f) list(`@id` = f, `@type` = "File", name = basename(f), description = kind_of(f),
                                   contentSize = as.character(file.size(f))))
  )
)
jsonlite::write_json(ro, "ro-crate-metadata.json", auto_unbox = TRUE, pretty = TRUE)
cat(length(files), "files listed.\n")
