# Reads Cooper et al.'s coded CSV and attaches this project's article_id.
#
# One function, used by every target that reads the CSV, so the DOI
# correction below cannot be applied in one place and forgotten in another
# (until 2026-10-12, compute_comment_quality_correlation() read the CSV
# without it, which mapped two Cooper rows onto each of 4 article_ids).
#
# Cooper et al.'s coded CSV has 4 DOIs each mistakenly copy-pasted onto TWO
# different papers (confirmed by cross-referencing paper_number -- unique,
# never duplicated -- against the master metadata list: for all 8 affected
# rows the coder's own recorded journal matches that paper_number's TRUE
# journal, so only the doi field was wrong on one row of each pair).
# sample.csv already reflects the corrected DOIs, so the DOI-keyed lookup
# below needs the same correction to give each Cooper row its own paper.
cooper_doi_corrections <- function() {
  data.frame(
    paper_number = c("57", "1275", "47", "7176"),
    correct_doi  = c("10.1002/2688-8319.12136", "10.1111/1365-2656.13512",
                     "10.1111/1365-2656.13145", "10.1111/1365-2435.12981"),
    stringsAsFactors = FALSE
  )
}

read_cooper_coded <- function(cooper_csv_file, sample_csv_file) {
  cooper <- read.csv(cooper_csv_file, na.strings = "NA", stringsAsFactors = FALSE)
  cooper <- .fix_invalid_utf8(cooper)

  doi_corrections <- cooper_doi_corrections()
  m <- match(as.character(cooper$paper_number), doi_corrections$paper_number)
  fix <- !is.na(m)
  if (any(fix)) cooper$doi[fix] <- doi_corrections$correct_doi[m[fix]]

  sampled <- read.csv(sample_csv_file, colClasses = "character")
  doi_to_article_id <- setNames(sampled$article_id, tolower(sampled$doi))
  cooper$article_id <- unname(doi_to_article_id[tolower(cooper$doi)])

  # Every later join is keyed on article_id: a missing or repeated id would
  # silently drop or multiply rows there.
  stopifnot(!anyNA(cooper$article_id), !anyDuplicated(cooper$article_id))
  cooper
}
