# Cooper et al.'s coded data, and its link to this project's article ids.

test_that("the DOI correction changes exactly the 4 listed papers' DOIs", {
  csv <- tempfile(fileext = ".csv"); smp <- tempfile(fileext = ".csv")
  write.csv(data.frame(paper_number = c(57, 1275, 47, 7176, 1, 2),
                       doi = c("10.x/dup1", "10.x/dup2", "10.x/dup3", "10.x/dup4", "10.x/a", "10.x/b"),
                       data_availability = "Yes"), csv, row.names = FALSE)
  correct <- cooper_doi_corrections()$correct_doi
  write.csv(data.frame(article_id = c("p57", "p1275", "p47", "p7176", "pa", "pb"),
                       doi = c(correct, "10.x/a", "10.x/b")), smp, row.names = FALSE)
  out <- read_cooper_coded(csv, smp)
  expect_equal(out$doi, c(correct, "10.x/a", "10.x/b"))
  expect_equal(out$article_id, c("p57", "p1275", "p47", "p7176", "pa", "pb"))
})

test_that("a Cooper row without a matching paper, or two rows on one paper, stops the reader", {
  csv <- tempfile(fileext = ".csv"); smp <- tempfile(fileext = ".csv")
  write.csv(data.frame(paper_number = c(1, 2), doi = c("10.x/a", "10.x/a")), csv, row.names = FALSE)
  write.csv(data.frame(article_id = "pa", doi = "10.x/a"), smp, row.names = FALSE)
  expect_error(read_cooper_coded(csv, smp))
  write.csv(data.frame(paper_number = c(1, 2), doi = c("10.x/a", "10.x/zzz")), csv, row.names = FALSE)
  expect_error(read_cooper_coded(csv, smp))
})

test_that("every Cooper row gets exactly one paper, and all 1,861 papers are covered", {
  cooper <- read_cooper_coded(cooper_csv(), sample_csv())
  expect_equal(nrow(cooper), N_PAPERS)
  expect_false(anyNA(cooper$article_id))
  expect_false(anyDuplicated(cooper$article_id) > 0)
  smp <- read.csv(sample_csv(), colClasses = "character")
  expect_setequal(cooper$article_id, smp$article_id)
})

test_that("Cooper et al.'s CSV reproduces the numbers their paper reports", {
  cooper <- read_cooper_coded(cooper_csv(), sample_csv())
  # Cooper et al. (2026), pp. 1958-1960, and the manuscript's Introduction.
  expect_equal(sum(cooper$data_used %in% "Yes"), 1735)
  expect_equal(sum(cooper$code_used %in% "Yes"), 1670)
  expect_equal(sum(cooper$data_availability %in% "Yes"), 1690)
  expect_equal(sum(cooper$code_archived %in% "Yes"), 577)
  expect_equal(sum(cooper$data_README %in% c("Yes", "Quasi-README")), 1114)
  expect_equal(length(unique(cooper$recorder_ID)), 145)
  expect_equal(sum(!is.na(suppressWarnings(as.numeric(cooper$code_annotation_scale)))), 543)
  # Two published counts differ slightly from Cooper et al.'s own CSV; the
  # manuscript quotes the published numbers. Fixed here so a change to the
  # CSV snapshot is noticed.
  expect_equal(sum(cooper$data_license %in% "Yes"), 1437)                          # published: 1,438
  expect_equal(sum(cooper$code_README %in% c("Yes", "Quasi-README")), 353)         # published: 351
})

test_that("joining Metacheck's and the open-practices columns keeps one row per paper", {
  skip_if_no_store()
  cvm <- tar("cooper_vs_metacheck")
  mc <- tar("master_comparison")
  expect_equal(nrow(cvm), N_PAPERS)
  expect_equal(nrow(mc), N_PAPERS)
  expect_false(anyNA(mc$article_id))
  expect_false(anyDuplicated(mc$article_id) > 0)
})
