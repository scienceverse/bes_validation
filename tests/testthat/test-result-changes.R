# Appendix A.11: what changed since the earlier results.

test_that("agreement with Cooper et al. is worked out as in the worklist", {
  expect_true(.agrees_with_cooper("code_archived", "Yes", "TRUE"))
  expect_false(.agrees_with_cooper("code_archived", "No", "TRUE"))
  expect_true(is.na(.agrees_with_cooper("code_archived", NA, "TRUE")))
  expect_true(is.na(.agrees_with_cooper("data_license", "No", NA)))
  expect_false(.agrees_with_cooper("data_availability", "No, but they are available on request", "TRUE"))
  expect_true(.agrees_with_cooper("data_download", "Yes, but not all data", "TRUE"))
  expect_true(.agrees_with_cooper("any_readme", TRUE, "TRUE"))
  expect_false(.agrees_with_cooper("any_readme", FALSE, "TRUE"))
  expect_true(.agrees_with_cooper("code_archive", "Zenodo", "Zenodo"))
  expect_false(.agrees_with_cooper("code_archive", "Zenodo", "Dryad"))
})

test_that("the summary stops when the saved values differ from the current results", {
  dir <- withr::local_tempdir()
  f <- function(name, x) { p <- file.path(dir, name); utils::write.csv(x, p, row.names = FALSE); p }
  changes <- f("c.csv", data.frame(article_id = "a", column = "code_archived", earlier_value = "FALSE",
                                   final_value = "TRUE", cause = "X"))
  causes <- f("k.csv", data.frame(cause = "X", metacheck_commit = "abc", mechanism = "m", consequence = "n"))
  pct <- f("p.csv", data.frame(column = "code_archived", earlier_pct_agree = 80, earlier_n_both_coded = 10))
  other <- f("o.csv", data.frame(column = "z", n_papers = 1, metacheck_commit = "abc", mechanism = "m"))
  stats <- list(results = list(code_archived = list(label = "Code", pct_agree = 90, n_both_coded = 10)))

  ok <- data.frame(article_id = "a", code_archived = "Yes", mc_code_archived = TRUE)
  out <- summarize_result_changes(changes, causes, pct, other, ok, stats)
  expect_equal(out$changes$effect, "toward")
  expect_equal(out$by_cause$n_toward, 1)
  expect_equal(out$percentages$final_pct_agree, 90)

  stale <- data.frame(article_id = "a", code_archived = "Yes", mc_code_archived = FALSE)
  expect_error(summarize_result_changes(changes, causes, pct, other, stale, stats), "does not match")
})

test_that("the appendix covers the current results", {
  skip_if_no_store()
  rc <- tar("result_changes")
  expect_true(all(rc$changes$cause %in% rc$by_cause$cause))
  expect_equal(sum(rc$by_cause$n_values), rc$n_values)
  expect_equal(sum(rc$by_cause$n_toward + rc$by_cause$n_away + rc$by_cause$n_new_agreement +
                     rc$by_cause$n_new_disagreement + rc$by_cause$n_other), rc$n_values)
  # every cause listed explains at least one changed value
  expect_true(all(rc$by_cause$n_values > 0))
})

test_that("the numbers typed in appendix A.11 still hold", {
  skip_if_no_store()
  res <- tar("comparison_statistics")$results
  expect_equal(sprintf("%.1f", res$code_archived$pct_agree), "87.9")
  expect_equal(sprintf("%.1f", res$code_archive$pct_agree), "78.0")
  expect_equal(sprintf("%.1f", res$data_archive$pct_agree), "90.3")
})
