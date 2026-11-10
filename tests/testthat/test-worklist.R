# The disagreement worklist (R/disagreement_worklist.R): which disagreements
# it lists, how verdicts are carried from one build to the next, and the
# version check on verdicts.

fake_side_by_side <- function(mc_archive = c("Zenodo", "Dryad", "Dryad"),
                              mc_avail = c(TRUE, TRUE, FALSE)) {
  data.frame(
    article_id = c("p1", "p2", "p3"),
    data_availability = c("Yes", "Yes", "Yes"), mc_data_availability = mc_avail,
    data_license = NA_character_, mc_data_license = NA,
    data_download = NA_character_, mc_data_download = NA,
    code_archived = NA_character_, mc_code_archived = NA,
    code_download = NA_character_, mc_code_download = NA,
    data_archive = c("Dryad", "Dryad;Other repo/database", NA), mc_data_archive = mc_archive,
    code_archive = NA_character_, mc_code_archive = NA_character_,
    cooper_any_readme = NA, mc_any_readme = NA,
    stringsAsFactors = FALSE)
}

build <- function(sbs, xlsx) {
  wl <- build_disagreement_worklist(sbs, xlsx, "no_such_target_in_any_store")
  write_disagreement_worklist_xlsx(wl, xlsx)
  wl
}

set_verdict <- function(xlsx, row_id, col, verdict) {
  x <- openxlsx::read.xlsx(xlsx)
  x[x$row_id == row_id, paste0(col, "_verdict")] <- verdict
  openxlsx::write.xlsx(x, xlsx, overwrite = TRUE)
}

test_that("the worklist lists exactly the disagreements the statistics count", {
  xlsx <- tempfile(fileext = ".xlsx")
  wl <- build(fake_side_by_side(), xlsx)
  # p1: platforms differ (Dryad vs Zenodo); p2: Metacheck lists fewer
  # platforms (not an exact match, so a disagreement); p3: no repository found.
  expect_equal(wl$review$article_id, c("p1", "p2", "p3"))
  expect_equal(unname(wl$flagged[, "data_archive"]), c(TRUE, TRUE, FALSE))
  expect_equal(unname(wl$flagged[, "data_availability"]), c(FALSE, FALSE, TRUE))
})

test_that("a verdict is kept while its values are unchanged, and dropped when they change", {
  xlsx <- tempfile(fileext = ".xlsx")
  build(fake_side_by_side(), xlsx)
  set_verdict(xlsx, "p1", "data_archive", "COOPER_RIGHT")
  set_verdict(xlsx, "p3", "data_availability", "UNCLEAR")

  same <- build(fake_side_by_side(), xlsx)$review
  expect_equal(same$data_archive_verdict[same$row_id == "p1"], "COOPER_RIGHT")
  expect_equal(same$data_availability_verdict[same$row_id == "p3"], "UNCLEAR")

  # p1's Metacheck platform changes but still disagrees: the verdict was
  # about the old value, so it is dropped. p3 now agrees: no longer listed.
  changed <- build(fake_side_by_side(mc_archive = c("Figshare", "Dryad", "Dryad"),
                                     mc_avail = c(TRUE, TRUE, TRUE)), xlsx)$review
  expect_true(is.na(changed$data_archive_verdict[changed$row_id == "p1"]))
  expect_false("p3" %in% changed$row_id)
})

test_that("a verdict never sits on a column that agrees, even when the xlsx says otherwise", {
  xlsx <- tempfile(fileext = ".xlsx")
  build(fake_side_by_side(), xlsx)
  # A stale verdict on p2's data_availability, where Cooper and Metacheck agree.
  set_verdict(xlsx, "p2", "data_availability", "COOPER_RIGHT")
  wl <- build(fake_side_by_side(), xlsx)
  expect_true(is.na(wl$review$data_availability_verdict[wl$review$row_id == "p2"]))
})

test_that("carrying verdicts forward does not depend on the pipeline's previous run", {
  # The values each verdict is about are stored in the xlsx with it.
  xlsx <- tempfile(fileext = ".xlsx")
  wl <- build(fake_side_by_side(), xlsx)
  x <- openxlsx::read.xlsx(xlsx)
  expect_true(all(wl$value_cols %in% names(x)))
})

test_that("an unchanged worklist is not rewritten, and the file reads back the same", {
  xlsx <- tempfile(fileext = ".xlsx")
  wl <- build(fake_side_by_side(), xlsx)
  h1 <- tools::md5sum(xlsx)
  write_disagreement_worklist_xlsx(wl, xlsx)
  expect_equal(unname(tools::md5sum(xlsx)), unname(h1))
  x <- openxlsx::read.xlsx(xlsx)
  expect_equal(x$row_id, wl$review$row_id)
})

test_that("a verdict is out of date unless its exact version is the installed one", {
  review <- data.frame(row_id = c("a", "b", "c", "d"),
                       data_license_verdict = c("COOPER_RIGHT", "UNCLEAR", "METACHECK_RIGHT", NA),
                       data_license_metacheck_version = c("sha1", "sha2", NA, NA),
                       data_license_metacheck_version_date = "2026-10-12",
                       data_license_version_is_exact = c("TRUE", "TRUE", "FALSE", NA))
  out <- check_worklist_staleness(list(review = review, verdict_cols = "data_license"), "sha1")$verdicts
  expect_equal(out$row_id, c("a", "b", "c"))
  expect_equal(out$stale, c(FALSE, TRUE, TRUE))
  expect_true(all(check_worklist_staleness(list(review = review, verdict_cols = "data_license"), NA)$verdicts$stale))
})

test_that("in the real worklist, verdicts sit only on disagreeing columns, and every disagreement is listed", {
  skip_if_no_store()
  wl <- tar("disagreement_worklist")
  stats <- tar("comparison_statistics")$results
  rev <- wl$review
  for (col in wl$verdict_cols) {
    v <- rev[[paste0(col, "_verdict")]]
    has <- !is.na(v) & nzchar(v)
    expect_equal(sum(has & !wl$flagged[, col]), 0, info = col)
    expect_equal(sum(wl$flagged[, col]), stats[[col]]$n_disagree, info = col)
  }
  expect_false(anyDuplicated(rev$row_id) > 0)
})
