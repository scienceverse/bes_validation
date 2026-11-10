# Numbers and claims typed into manuscript.qmd's text rather than computed,
# checked against the computed results.

pct_text <- function(x) sprintf("%.1f%%", x)

test_that("the abstract's percentages are the computed agreement rates", {
  skip_if_no_store()
  res <- tar("comparison_statistics")$results
  abstract <- grep("^abstract:", readLines(proj("documentation/manuscript.qmd"), warn = FALSE), value = TRUE)
  expect_length(abstract, 1)
  expect_match(abstract, paste0("data could be downloaded and used was ", pct_text(res$data_download$pct_agree)), fixed = TRUE)
  expect_match(abstract, paste0("code could be downloaded was ", pct_text(res$code_download$pct_agree)), fixed = TRUE)
  expect_match(abstract, paste0("could be found at all (", pct_text(res$data_availability$pct_agree), ")"), fixed = TRUE)
  expect_match(abstract, paste0("whether code was archived (", pct_text(res$code_archived$pct_agree), ")"), fixed = TRUE)
  expect_match(abstract, paste0("whether a README was present (", pct_text(res$any_readme$pct_agree), ")"), fixed = TRUE)
  expect_match(abstract, "in 1861 papers", fixed = TRUE)
  expect_match(abstract, "145 participants", fixed = TRUE)
})

test_that("the typed claims about precision and agreement still hold", {
  skip_if_no_store()
  res <- tar("comparison_statistics")$results
  # "positive predictive value was above 96% for five of the six boolean
  # constructs ... code_archived is the exception"
  expect_match(manuscript_text(), "positive predictive value was above 96% for five of the six", fixed = TRUE)
  for (nm in c("data_availability", "data_license", "data_download", "code_download", "any_readme"))
    expect_gt(res[[nm]]$ppv, 0.96)
  expect_lt(res$code_archived$ppv, 0.96)
  # This sentence was false (data_download has Cooper-No cases) and is now
  # computed; it must not come back.
  expect_false(grepl("has no codeable Cooper-`No` case at all", manuscript_text(), fixed = TRUE))
  # "the largest raw disagreement count of any construct" (any_readme).
  count <- vapply(res, function(r) r$n_disagree, numeric(1))
  expect_equal(names(which.max(count)), "any_readme")
  # A claim that code_archived has the largest or highest disagreement
  # RATE was false (data_availability and any_readme are higher) and was
  # removed; it must not come back unless it becomes true.
  rate <- vapply(res, function(r) 100 - r$pct_agree, numeric(1))
  if (grepl("(largest|highest) disagreement rate", manuscript_text()))
    expect_equal(names(which.max(rate)), "code_archived")
})

test_that("the typed portability and annotation claims still hold", {
  skip_if_no_store()
  fe <- tar("free_extras_summary")
  # "more than half of the papers whose code we could inspect at all"
  expect_gt(fe$pct_papers_portability, 50)
  cq <- tar("comment_quality_correlation")
  expect_match(manuscript_text(), paste0("coded for ", cq$n_cooper_coded, " of the 1861 papers"), fixed = TRUE)
})

test_that("no sentence still says the remaining code-download disagreements are failed archive downloads", {
  txt <- manuscript_text()
  expect_false(grepl("nearly all of them archive-member downloads that failed", txt, fixed = TRUE))
  expect_false(grepl("share one cause: code inside zip archives", txt, fixed = TRUE))
  expect_false(grepl("are archive members whose download failed during the corpus run but succeeds when rerun", txt, fixed = TRUE))
  expect_false(grepl("the SAPFLUXNET repository is refused outright rather than downloaded", txt, fixed = TRUE))
})
