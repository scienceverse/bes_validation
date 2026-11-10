# Categorizes each reviewed disagreement's verdict comment into a small,
# reproducible set of root-cause buckets, by keyword pattern over the
# comment text -- so the manuscript's qualitative "N of M disagreements
# were caused by X" prose is computed from the actual review comments,
# not hand-counted or hand-typed. A comment is assigned to the FIRST
# bucket (in priority order) whose pattern it matches; anything matching
# none of them falls into "other" rather than being forced into a bucket
# it doesn't clearly belong to.
#

categorize_disagreement_causes <- function(disagreement_worklist_review) {
  review <- disagreement_worklist_review

  # COOPER_RIGHT cause buckets (data_availability, code_archived, any_readme).
  # Priority order matters: a comment mentioning both a gated-fetch error
  # AND "never found" should count as the more specific gated-fetch-error
  # cause, not the generic detection-miss bucket.
  cooper_right_patterns <- c(
    # "links to a GitHub account": metacheck 0.3.3.9 reports an account or
    # organisation address this way (before: "invalid or inaccessible GitHub
    # repository"); it still does not list the account's repositories.
    gated_fetch_error  = "the fetch errored|arguments imply differing number of rows|seems to be offline|invalid or inaccessible GitHub|links to a GitHub account",
    zip_not_peeked     = "zip-peek|never open(ed)?[^.]{0,20}zip|zip[^.]{0,20}never opened|GitHub-integration snapshot|GitHub-Zenodo integration|data_type\\s*[=is]{1,3}\\s*.{0,3}unknown|does not peek inside|never peek",
    repo_not_detected  = paste0(
      "never (located|detected|found)|found/listed nothing|repo_n\\s*=?\\s*0|",
      "zero (files (examined|for)|rows for this paper)|never checked|",
      "was(n.t| not) ever (fetched|attempted)|not (a platform |)repo_check (recognizes|recognized)|",
      "not repo_check-recognized|not among (metacheck|repo_check)|URL-recognition gap|",
      "extraction[- ]?(gap|corrupt)|space-broken|",
      "repo_check[^.]{0,25}(never had|found zero|has zero rows)|cannot resolve|",
      "bare GitHub username|no file extension"
    )
  )
  # code_archived-only bucket: repo_check DID find the repository (often
  # because it also holds the paper's data), but code_check classified
  # zero files there as code, despite the paper's own text stating code
  # is bundled in that same deposit -- a distinct cross-module gap from
  # "the repository was never found at all".
  code_bundled_pattern <- paste0(
    "but neither repo_check nor code_check found any file classified as code|",
    "found (zero|no) files? classified as code|",
    "code_check[^.]{0,25}(never|did ?not|didn.t)[^.]{0,15}(find|classify)"
  )
  # code_archived-only bucket: repo_check DID find the paper's own
  # GitHub/CRAN R-package repository, but the R-package-source-tree
  # exclusion (a fix applied and later corrected) stripped the package's
  # real R/ source directory from the listing along with the
  # bundled-dependency scaffolding it was meant to remove.
  r_package_regression_pattern <- "R-package-source-tree[- ]exclusion"

  categorize <- function(comments, patterns) {
    cat_out <- rep("other", length(comments))
    for (nm in names(patterns)) {
      hit <- cat_out == "other" & grepl(patterns[[nm]], comments, ignore.case = TRUE, perl = TRUE)
      cat_out[hit] <- nm
    }
    cat_out[is.na(comments)] <- NA_character_
    cat_out
  }

  cooper_right_causes <- list()
  for (col in c("data_availability", "code_archived", "any_readme")) {
    vcol <- paste0(col, "_verdict"); ccol <- paste0(col, "_comment")
    idx <- review[[vcol]] %in% "COOPER_RIGHT"
    patterns <- cooper_right_patterns
    if (col == "code_archived") patterns <- c(patterns,
      code_bundled_not_classified = code_bundled_pattern,
      r_package_source_excluded = r_package_regression_pattern)
    cats <- categorize(review[[ccol]][idx], patterns)
    tab <- table(factor(cats, levels = c(names(patterns), "other")))
    cooper_right_causes[[col]] <- tab
  }

  # any_readme DIFFERENT_DEFINITION cause buckets (platform metadata-field mismatch)
  any_readme_dd_patterns <- c(
    dryad_abstract              = "Dryad.{0,20}abstract",
    zenodo_figshare_description = "(Zenodo|Figshare)[^.]{0,60}description",
    archive_only_zip            = "Archive-only|peek_zips"
  )
  idx <- review$any_readme_verdict %in% "DIFFERENT_DEFINITION"
  ar_dd_cats <- categorize(review$any_readme_comment[idx], any_readme_dd_patterns)
  any_readme_dd_causes <- table(factor(ar_dd_cats, levels = c(names(any_readme_dd_patterns), "other")))

  # data_availability DIFFERENT_DEFINITION cause buckets (no checkable external repo)
  da_dd_patterns <- c(
    on_request        = "on request|upon request",
    embedded_in_article = "Supporting Information|Supplementary|supplementary materials|included in the manuscript|manuscript text|embedded",
    confidential       = "confidential|not publicly available|not publically available|protect (respondent|participant)",
    named_repo_no_link = "no (captured|downloadable) URL|no URL/DOI|with no URL|stated commitment"
  )
  idx <- review$data_availability_verdict %in% "DIFFERENT_DEFINITION"
  da_dd_cats <- categorize(review$data_availability_comment[idx], da_dd_patterns)
  data_availability_dd_causes <- table(factor(da_dd_cats, levels = c(names(da_dd_patterns), "other")))

  # code_archived METACHECK_RIGHT: two distinct sub-patterns worth
  # separating -- (a) code_check actually found real, named code files
  # and Cooper coded No, (b) no code/script is mentioned anywhere in the
  # paper's own text at all.
  code_archived_mr_patterns <- c(
    code_check_found_files = "code_check found \\d+ real code file",
    no_code_mentioned       = "no (code|mention of code)|no basis|not mentioned|never mentions|no code, script"
  )
  idx <- review$code_archived_verdict %in% "METACHECK_RIGHT"
  ca_mr_cats <- categorize(review$code_archived_comment[idx], code_archived_mr_patterns)
  code_archived_mr_causes <- table(factor(ca_mr_cats, levels = c(names(code_archived_mr_patterns), "other")))

  list(
    cooper_right_causes = cooper_right_causes,
    any_readme_dd_causes = any_readme_dd_causes,
    data_availability_dd_causes = data_availability_dd_causes,
    code_archived_mr_causes = code_archived_mr_causes
  )
}
