# Why papers Cooper et al. coded as archiving data drop out of the
# data_archive comparison.
#
# compute_comparison_statistics() only compares papers where BOTH
# Cooper's data_archive and mc_data_archive are non-NA. mc_data_archive
# is built only from repositories in which data_check classified at
# least one file as data_type == "data"; every other paper gets NA and
# silently leaves the comparison. This decomposes the papers Cooper
# coded data_availability == "Yes" by why mc_data_archive is NA, and,
# for the papers where repo_check DID find a repository with files,
# describes what those files are and what Cooper's coders recorded as
# the data (their data_archive platform and data_format file types).
#

summarize_data_archive_uncoded_papers <- function(master_comparison, res_data_check) {

  # 1. Decompose Cooper's "archived data" papers.
  cooper_yes <- master_comparison$data_availability %in% "Yes"
  mc_na      <- is.na(master_comparison$mc_data_archive)

  n_cooper_yes          <- sum(cooper_yes)
  n_cooper_archive_na   <- sum(cooper_yes & is.na(master_comparison$data_archive))
  n_mc_archive_na       <- sum(cooper_yes & mc_na)
  n_no_repo_found       <- sum(cooper_yes & mc_na & master_comparison$mc_data_availability %in% FALSE)
  n_repo_but_no_data    <- sum(cooper_yes & mc_na & master_comparison$mc_data_availability %in% TRUE)
  n_both_coded          <- sum(!is.na(master_comparison$data_archive) & !mc_na)

  # 2. The 'repository found, no data file' papers.
  ids <- master_comparison$article_id[cooper_yes & mc_na &
                                        master_comparison$mc_data_availability %in% TRUE]

  files <- res_data_check$structure
  files <- files[files$paper_id %in% ids, ]

  file_ext <- function(f) {
    out <- rep("(none)", length(f))
    has <- !is.na(f) & grepl("[.][A-Za-z0-9]{1,6}$", f)
    out[has] <- tolower(regmatches(f[has], regexpr("[.][A-Za-z0-9]{1,6}$", f[has])))
    out
  }
  files$ext <- file_ext(files$file_name)

  ext_by_data_type <- lapply(split(files$ext, files$data_type),
                             function(e) sort(table(e), decreasing = TRUE))
  data_type_table <- table(files$data_type, useNA = "ifany")

  # Platform of the repositories Metacheck fetched, with the same labels as
  # mc_data_archive (repo_url_platform(), R/platforms.R).
  platform <- repo_url_platform

  # Extension groups used to classify each paper. Archive members carry
  # the archive's address in archive_url, so an archive counts as opened
  # when at least one member row points back to it.
  archive_ext  <- c(".zip", ".gz", ".7z", ".rar", ".tar", ".tgz", ".bz2")
  text_ext     <- c(".txt", ".rtf", ".doc", ".docx", ".pdf", ".csv", ".xls", ".xlsx")
  nontab_ext   <- c(".jpg", ".jpeg", ".png", ".tif", ".tiff", ".wav", ".mp3", ".mp4",
                    ".dbf", ".shp", ".shx", ".cpg", ".prj", ".tab", ".ply", ".stl",
                    ".tfw", ".ovr", ".nc", ".asc", ".grd", ".gri")
  readme_regex <- "readme|licen[cs]e|citation|changelog"
  # Repository housekeeping files (.gitignore, .Rbuildignore, LICENSE,
  # CRAN-SUBMISSION, ...) that data_check labels "materials"/"unknown"
  # but that say nothing about whether a repository holds data.
  housekeeping_regex <- "^[.]|^licen[cs]e|^cran-|^news|^copying"

  per_paper <- do.call(rbind, lapply(split(files, files$paper_id), function(x) {
    is_archive   <- x$ext %in% archive_ext & is.na(x$archive_member)
    opened_urls  <- unique(x$archive_url[!is.na(x$archive_member)])
    archive_urls <- dplyr::coalesce(x$file_url, x$file_location)
    unopened     <- is_archive & !(archive_urls %in% opened_urls)
    text_as_doc  <- x$ext %in% text_ext & x$data_type %in% c("documentation", "unknown") &
                    !grepl(readme_regex, x$file_name, ignore.case = TRUE)
    nontab       <- x$ext %in% nontab_ext & x$data_type %in% c("materials", "unknown")
    housekeeping  <- grepl(housekeeping_regex, x$file_name, ignore.case = TRUE)
    code_doc_only <- all(x$data_type %in% c("code", "documentation") | housekeeping)

    data.frame(
      paper_id = x$paper_id[1],
      n_files = nrow(x),
      mc_platform = paste(sort(unique(platform(x$repo_url))), collapse = ";"),
      has_unopened_archive = any(unopened),
      has_opened_archive = length(opened_urls) > 0,
      has_text_file_not_data = any(text_as_doc),
      has_nontabular_file = any(nontab),
      code_and_documentation_only = code_doc_only,
      example_files = paste(head(unique(x$file_name[unopened | text_as_doc | nontab]), 5), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))

  # One primary reason per paper, checked in this order: the first
  # reason that applies wins (a paper can meet several).
  per_paper$primary_reason <- dplyr::case_when(
    per_paper$has_unopened_archive        ~ "Compressed archive never opened",
    per_paper$has_text_file_not_data      ~ "Text/office/PDF file not classified as data",
    per_paper$has_nontabular_file         ~ "Image, audio, GIS or 3D file not classified as data",
    per_paper$code_and_documentation_only ~ "Only code and documentation files fetched",
    per_paper$has_opened_archive          ~ "Archive opened, contents not classified as data",
    TRUE                                  ~ "File in a format data_check does not recognise"
  )

  m <- match(per_paper$paper_id, master_comparison$article_id)
  per_paper$cooper_data_archive <- master_comparison$data_archive[m]
  per_paper$cooper_data_format  <- master_comparison$data_format[m]
  per_paper$cooper_platform_fetched <- mapply(function(cp, mp) {
    if (is.na(cp)) return(NA)
    any(trimws(strsplit(cp, ";")[[1]]) %in% strsplit(mp, ";")[[1]])
  }, per_paper$cooper_data_archive, per_paper$mc_platform)

  primary_reason_table   <- sort(table(per_paper$primary_reason), decreasing = TRUE)
  flag_counts <- colSums(per_paper[, c("has_unopened_archive", "has_opened_archive",
                                       "has_text_file_not_data", "has_nontabular_file",
                                       "code_and_documentation_only")])
  platform_crosstab      <- table(cooper = per_paper$cooper_data_archive, metacheck = per_paper$mc_platform)
  n_cooper_platform_fetched <- sum(per_paper$cooper_platform_fetched %in% TRUE)
  cooper_format_split <- trimws(unlist(strsplit(na.omit(per_paper$cooper_data_format), ";")))
  cooper_format_table <- sort(table(cooper_format_split), decreasing = TRUE)

  list(
    n_cooper_yes = n_cooper_yes,
    n_cooper_archive_na = n_cooper_archive_na,
    n_mc_archive_na = n_mc_archive_na,
    n_no_repo_found = n_no_repo_found,
    n_repo_but_no_data = n_repo_but_no_data,
    n_both_coded = n_both_coded,
    data_type_table = data_type_table,
    ext_by_data_type = ext_by_data_type,
    n_cooper_platform_fetched = n_cooper_platform_fetched,
    platform_crosstab = platform_crosstab,
    primary_reason_table = primary_reason_table,
    flag_counts = flag_counts,
    cooper_format_table = cooper_format_table,
    per_paper = per_paper
  )
}
