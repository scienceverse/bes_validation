# Flags manual audit rows whose explanatory comment may no longer match
# metacheck's current behaviour.
#
# data/review/manual_repo_findability_audit_data.csv and
# manual_fair_api_findability_audit_data.csv contain free-text prose written by
# reading metacheck's output for one specific paper and explaining why it
# behaved that way (e.g. "peek_zips defaults to FALSE in data_check()").
# That explanation is only correct for the metacheck version installed at
# the time it was written, but targets has no way to see a version
# dependency hidden inside English prose -- it can only track the CSV file
# itself, not what install of metacheck it describes. A metacheck update
# can silently make an old row's reasoning wrong (as happened with the
# Morford et al. paper's peek_zips explanation after metacheck's own
# default changed) with nothing in the pipeline flagging it.
#
# Each row records, in metacheck_version, the metacheck commit
# at the time the row was last written, and in version_is_exact, whether
# that SHA was actually captured (TRUE) or merely inferred from the git
# commit date of the row itself (FALSE, for rows written before this
# tracking existed). This target reruns whenever either audit CSV changes
# or the results come from another metacheck commit, and returns the rows that
# were written for another version -- a worklist of rows to
# re-verify, not an error to fix automatically, since only a human
# re-reading the paper can confirm whether the old explanation still holds.

.audit_staleness <- function(audit_csv_path, current_sha) {
  df <- read.csv(audit_csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = "NA")
  stopifnot(all(c("metacheck_version", "version_is_exact") %in% names(df)))

  is_stale <- is.na(current_sha) |
    df$version_is_exact != TRUE |
    (df$version_is_exact == TRUE & df$metacheck_version != current_sha)

  data.frame(
    paper_doi = df$paper_doi,
    metacheck_version = df$metacheck_version,
    metacheck_version_date = df$metacheck_version_date,
    version_is_exact = df$version_is_exact,
    stale = is_stale,
    stringsAsFactors = FALSE
  )
}

# Combined staleness check across both manual audit files, for a single
# targets node. current_sha is the metacheck commit recorded in the result file. Returns a named list of the two per-file data frames
# so a caller can inspect either one
# (tar_read(audit_staleness)$repo_findability).
check_audit_staleness <- function(manual_repo_findability_audit_csv,
                                   manual_fair_api_findability_audit_csv,
                                   current_sha) {
  list(
    repo_findability = .audit_staleness(manual_repo_findability_audit_csv, current_sha),
    fair_api_findability = .audit_staleness(manual_fair_api_findability_audit_csv, current_sha),
    current_metacheck_sha = current_sha
  )
}
