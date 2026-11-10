# Sorting reviewed disagreements into causes by the wording of their review
# comments (categorize_disagreement_causes(), R/disagreement_cause_categories.R).

fake_review <- function(verdicts, comments, col = "data_availability") {
  r <- data.frame(row_id = paste0("r", seq_along(verdicts)))
  for (c in c("data_availability", "code_archived", "any_readme")) {
    r[[paste0(c, "_verdict")]] <- NA_character_
    r[[paste0(c, "_comment")]] <- NA_character_
  }
  r[[paste0(col, "_verdict")]] <- verdicts
  r[[paste0(col, "_comment")]] <- comments
  r
}

test_that("each COOPER_RIGHT cause pattern catches its own example", {
  out <- categorize_disagreement_causes(fake_review(
    rep("COOPER_RIGHT", 4),
    c("repo_check never detected the repository",
      "the fetch errored with an R-level error",
      "the zip-peek step did not run",
      "something else entirely")))
  expect_equal(as.integer(out$cooper_right_causes$data_availability[
    c("gated_fetch_error", "zip_not_peeked", "repo_not_detected", "other")]), c(1, 1, 1, 1))
})

test_that("a comment matching two causes counts once, for the first cause in order", {
  out <- categorize_disagreement_causes(fake_review(
    "COOPER_RIGHT", "the fetch errored, so the repository was never found"))
  tab <- out$cooper_right_causes$data_availability
  expect_equal(sum(tab), 1)
  expect_equal(unname(tab["gated_fetch_error"]), 1)
})

test_that("only verdicts of the right kind are counted", {
  out <- categorize_disagreement_causes(fake_review(
    c("COOPER_RIGHT", "METACHECK_RIGHT", NA),
    c("never detected", "never detected", "never detected")))
  expect_equal(sum(out$cooper_right_causes$data_availability), 1)
})

test_that("code_archived has its own two extra causes", {
  out <- categorize_disagreement_causes(fake_review(
    c("COOPER_RIGHT", "COOPER_RIGHT"),
    c("found zero files classified as code", "the R-package-source-tree exclusion removed it"),
    col = "code_archived"))
  tab <- out$cooper_right_causes$code_archived
  expect_equal(as.integer(tab[c("code_bundled_not_classified", "r_package_source_excluded")]), c(1L, 1L))
})

test_that("in the real worklist, each cause table counts each verdict of its kind exactly once", {
  skip_if_no_store()
  rev <- tar("disagreement_worklist")$review
  cats <- tar("disagreement_cause_categories")
  for (col in c("data_availability", "code_archived", "any_readme"))
    expect_equal(sum(cats$cooper_right_causes[[col]]),
                 sum(rev[[paste0(col, "_verdict")]] %in% "COOPER_RIGHT"), info = col)
  expect_equal(sum(cats$any_readme_dd_causes), sum(rev$any_readme_verdict %in% "DIFFERENT_DEFINITION"))
  expect_equal(sum(cats$data_availability_dd_causes), sum(rev$data_availability_verdict %in% "DIFFERENT_DEFINITION"))
  expect_equal(sum(cats$code_archived_mr_causes), sum(rev$code_archived_verdict %in% "METACHECK_RIGHT"))
})
