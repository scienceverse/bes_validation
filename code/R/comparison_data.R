# Recreate every column of Cooper et al.'s coded dataset, as far as
# possible, purely from metacheck's own repo_check/data_check/code_check
# output -- NOT open_practices/ODDPub, which measure a different,
# judgement-based question. This is a direct, mechanical column-by-column
# reconstruction, built from an explicit specification (each mc_* column's
# derivation rule was given, not inferred) so every choice below is
# traceable back to that rule rather than to a guess about what "counts."
#
#
# Cooper's own column, and what mc_* recreates it from:
#   data_availability -> repo_check found at least one linked repository
#     with at least one file of any type (summary_table$files_n > 0)
#   data_archive      -> WHERE the data lives (platform name, from the
#     repo_url, e.g. "Dryad") for whichever repository holds the data
#   data_doi          -> the DOI metacheck resolved for that repository
#     (repo_check's repo_metadata)
#   data_license      -> whether repo_check found a licence for that
#     repository (repo_metadata$license present)
#   data_download     -> whether the tabular data file(s) actually
#     downloaded AND could be opened with standard software (i.e.
#     data_check's own tabular_usable flag, not just file_location
#     being non-NA -- a file can download and still fail to parse)
#   data_format       -> file extensions of every tabular data file
#   data_README/code_README -> collapsed into one any_readme signal:
#     metacheck records a single README signal per paper (repo_check's
#     doc_role == "readme"), not separately for data vs. code, so it is
#     compared against cooper_any_readme (TRUE if Cooper's coders said
#     "Yes"/"Quasi-README" to EITHER of their two questions) rather than
#     double-counting one signal against two Cooper columns.
#   code_used         -> NOT classified by metacheck -- left NA
#   code_archived     -> whether repo_check-listed files for this paper
#     include at least one file code_check classified as code
#   code_download     -> whether that code file's file_location is
#     non-NA
#   code_language     -> code_check's own `language` column

recreate_cooper_columns <- function(res_repo_check, res_data_check, res_code_check) {

  structure_table <- res_data_check$structure |>
    .ensure_cols(list(paper_id = character(), repo_url = character(),
                      file_location = character(), data_type = character(),
                      data_format = character(), tabular_usable = logical()))

  repo_table <- res_repo_check$table |>
    .ensure_cols(list(paper_id = character(), repo_url = character(),
                      doc_role = character()))

  .first_non_na <- function(x) {
    x <- stats::na.omit(x)
    if (length(x) == 0) NA_character_ else x[[1]]
  }
  repo_metadata <- if ("repo_metadata" %in% names(res_repo_check))
    res_repo_check$repo_metadata |>
      dplyr::summarise(doi = .first_non_na(doi), license = .first_non_na(license),
                .by = repo_url) else
    data.frame(repo_url = character(0), doi = character(0), license = character(0))

  # One row per paper's DATA-holding repository: the repo_url that has at
  # least one tabular-data-classified file, per paper.
  data_repos <- structure_table |>
    dplyr::filter(data_type == "data") |>
    dplyr::distinct(paper_id, repo_url)

  # "Does a data repository exist at all" uses repo_check's own file count
  # (files_n > 0 on the summary_table), not data_check's narrower
  # data-classification -- a repository existing is a repo_check question.
  mc_data_availability <- res_repo_check$summary_table |>
    dplyr::summarise(mc_data_availability = any(files_n > 0, na.rm = TRUE), .by = paper_id)

  # Platform names from repo_url_platform() (R/platforms.R).
  mc_data_archive <- data_repos |>
    dplyr::mutate(platform = repo_url_platform(repo_url)) |>
    dplyr::summarise(mc_data_archive = paste(sort(unique(platform)), collapse = ";"), .by = paper_id)

  # TRUE when every Figshare deposit holding data is an institutional
  # Figshare (is_institutional_figshare(), R/platforms.R); the platform
  # comparison then also accepts Cooper's "Other repo/database" for it.
  institutional_figshare_flag <- function(repos, out_col) {
    repos |>
      dplyr::filter(repo_url_platform(repo_url) == "Figshare") |>
      dplyr::summarise(flag = all(is_institutional_figshare(repo_url)), .by = paper_id) |>
      dplyr::rename(!!out_col := flag)
  }
  mc_data_archive_inst <- institutional_figshare_flag(data_repos, "mc_data_archive_institutional_figshare")

  mc_data_doi <- data_repos |>
    dplyr::left_join(repo_metadata |> dplyr::select(repo_url, doi), by = "repo_url") |>
    dplyr::filter(!is.na(doi)) |>
    dplyr::summarise(mc_data_doi = paste(sort(unique(doi)), collapse = ";"), .by = paper_id)

  mc_data_license <- data_repos |>
    dplyr::left_join(repo_metadata |> dplyr::select(repo_url, license), by = "repo_url") |>
    dplyr::summarise(mc_data_license = any(!is.na(license)), .by = paper_id)

  # "Downloaded AND opened successfully with standard software" (Cooper's
  # own data_download wording) requires file_location non-NA (genuinely
  # fetched) AND tabular_usable TRUE (parsed successfully) together --
  # tabular_usable alone is not a usable signal on its own. Computed
  # directly over structure_table's own per-file rows, not joined through
  # data_repos by repo_url -- a file's own file_location/tabular_usable
  # already carries everything this needs.
  mc_data_download <- structure_table |>
    dplyr::filter(data_type == "data", !is.na(file_location), nzchar(file_location)) |>
    dplyr::summarise(mc_data_download = any(tabular_usable %in% TRUE), .by = paper_id)

  # File extensions of every tabular file -- NOT data_check's own
  # `data_format` column, which despite the name is a coarse
  # raw-vs-tabular structural category (values are only
  # "raw"/"tabular"/NA, never an extension). Extract the extension from
  # file_name directly instead, restricted to files data_check itself
  # judged tabular_usable (matching mc_data_download's own "genuinely
  # usable tabular data" scope, not just anything data-typed).
  mc_data_format <- structure_table |>
    dplyr::filter(data_type == "data", tabular_usable %in% TRUE) |>
    dplyr::mutate(ext = tolower(sub("^.*(\\.[A-Za-z0-9]+)$", "\\1", file_name))) |>
    dplyr::filter(nzchar(ext), startsWith(ext, ".")) |>
    dplyr::summarise(mc_data_format = paste(sort(unique(ext)), collapse = ";"), .by = paper_id)

  mc_readme <- repo_table |>
    dplyr::summarise(mc_readme_present = any(doc_role == "readme", na.rm = TRUE), .by = paper_id)

  code_table <- res_code_check$table |>
    .ensure_cols(list(paper_id = character(), repo_url = character(),
                      file_location = character(), language = character()))

  mc_code_archived <- code_table |>
    dplyr::summarise(mc_code_archived = dplyr::n() > 0, .by = paper_id)

  # Platform(s) of every repository holding at least one code_check code
  # file -- the code counterpart of mc_data_archive, compared against
  # Cooper's own code_archive column.
  mc_code_archive <- code_table |>
    dplyr::filter(!is.na(repo_url), nzchar(repo_url)) |>
    dplyr::distinct(paper_id, repo_url) |>
    dplyr::mutate(platform = repo_url_platform(repo_url)) |>
    dplyr::summarise(mc_code_archive = paste(sort(unique(platform)), collapse = ";"), .by = paper_id)
  mc_code_archive_inst <- institutional_figshare_flag(
    code_table |> dplyr::filter(!is.na(repo_url), nzchar(repo_url)) |> dplyr::distinct(paper_id, repo_url),
    "mc_code_archive_institutional_figshare")

  mc_code_download <- code_table |>
    dplyr::summarise(mc_code_download = any(!is.na(file_location) & nzchar(file_location)),
              .by = paper_id)

  mc_code_language <- code_table |>
    dplyr::filter(!is.na(language), nzchar(language)) |>
    dplyr::summarise(mc_code_language = paste(sort(unique(language)), collapse = ";"), .by = paper_id)

  # summary_table's paper_id (not the three per-file tables) is the full
  # corpus by construction -- a paper repo_check found zero files for has
  # no row at all in any per-file table, and using only those tables here
  # would silently drop such papers from the comparison entirely instead
  # of correctly scoring them FALSE.
  all_paper_ids <- unique(c(res_repo_check$summary_table$paper_id,
                            structure_table$paper_id, code_table$paper_id, repo_table$paper_id))

  recreated <- data.frame(paper_id = all_paper_ids, stringsAsFactors = FALSE) |>
    dplyr::left_join(mc_data_availability, by = "paper_id") |>
    dplyr::left_join(mc_data_archive,      by = "paper_id") |>
    dplyr::left_join(mc_data_archive_inst, by = "paper_id") |>
    dplyr::left_join(mc_data_doi,          by = "paper_id") |>
    dplyr::left_join(mc_data_license,      by = "paper_id") |>
    dplyr::left_join(mc_data_download,     by = "paper_id") |>
    dplyr::left_join(mc_data_format,       by = "paper_id") |>
    dplyr::left_join(mc_readme |> dplyr::rename(mc_data_README = mc_readme_present), by = "paper_id") |>
    dplyr::mutate(mc_code_used = NA) |>
    dplyr::left_join(mc_code_archived,     by = "paper_id") |>
    dplyr::left_join(mc_code_archive,      by = "paper_id") |>
    dplyr::left_join(mc_code_archive_inst, by = "paper_id") |>
    dplyr::left_join(mc_code_download,     by = "paper_id") |>
    dplyr::left_join(mc_code_language,     by = "paper_id") |>
    dplyr::left_join(mc_readme |> dplyr::rename(mc_code_README = mc_readme_present), by = "paper_id")

  # "No repository/file found at all" has a definitive FALSE for existence
  # columns (repo_check ran and found nothing) -- left NA for every other
  # mc_* column (license, DOI, download, format, language), which are
  # legitimately not answerable when there is no repository to ask about.
  for (col in c("mc_data_availability", "mc_code_archived", "mc_data_README", "mc_code_README")) {
    recreated[[col]][is.na(recreated[[col]])] <- FALSE
  }

  recreated
}

build_cooper_vs_metacheck <- function(recreated_cooper_columns, cooper_csv_file, sample_csv_file) {
  # Cooper's CSV is read from a fixed, versioned local snapshot rather
  # than fetched fresh from GitHub on every run, so this target has no
  # network dependency and is reproducible from that snapshot; DOI
  # corrections and article_id from read_cooper_coded() (R/cooper_data.R).
  cooper <- read_cooper_coded(cooper_csv_file, sample_csv_file)

  side_by_side <- cooper |>
    dplyr::select(article_id, data_availability, data_archive, data_doi, data_license,
           data_download, data_format, data_README,
           code_used, code_archived, code_archive, code_download, code_language, code_README) |>
    dplyr::left_join(recreated_cooper_columns, by = c("article_id" = "paper_id"))

  # any_readme: collapsed data_README OR code_README vs. the one repo_check
  # readme signal metacheck actually has.
  cooper_bool <- function(x, positive, negative) {
    out <- rep(NA, length(x))
    out[x %in% positive] <- TRUE
    out[x %in% negative] <- FALSE
    out
  }
  data_readme_bool <- cooper_bool(side_by_side$data_README, c("Yes", "Quasi-README"), "No")
  code_readme_bool <- cooper_bool(side_by_side$code_README, c("Yes", "Quasi-README"), "No")
  side_by_side$cooper_any_readme <- ifelse(
    !is.na(data_readme_bool) & data_readme_bool, TRUE,
    ifelse(!is.na(code_readme_bool) & code_readme_bool, TRUE,
    ifelse(is.na(data_readme_bool) & is.na(code_readme_bool), NA, FALSE)))
  side_by_side$mc_any_readme <- side_by_side$mc_data_README

  side_by_side
}
