# Agreement statistics (compute_comparison_statistics(), R/comparison_statistics.R)
# and the platform-set comparison (R/platforms.R).

# 12 papers: 3 Cooper-Yes/MC-Yes, 2 Cooper-Yes/MC-No, 1 Cooper-No/MC-Yes,
# 4 Cooper-No/MC-No (two of them "No, but ... on request", which counts as
# No), and 2 not comparable (Cooper "Unsure"; Metacheck missing).
fake_master <- function() {
  n <- 12
  na_chr <- rep(NA_character_, n); na_lgl <- rep(NA, n)
  data.frame(
    article_id = paste0("p", 1:n),
    data_availability = c(rep("Yes", 5), "No", "No", "No",
                          rep("No, but they are available on request", 2), "Unsure", "Yes"),
    mc_data_availability = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, NA),
    data_license = na_chr, mc_data_license = na_lgl,
    data_download = c("Yes", "Yes, but not all data", "No", rep(NA, 9)),
    mc_data_download = c(TRUE, FALSE, FALSE, rep(NA, 9)),
    code_archived = na_chr, mc_code_archived = na_lgl,
    code_download = na_chr, mc_code_download = na_lgl,
    data_README = na_chr, code_README = na_chr, mc_data_README = na_lgl,
    data_archive = na_chr, mc_data_archive = na_chr,
    code_archive = na_chr, mc_code_archive = na_chr,
    stringsAsFactors = FALSE)
}

test_that("the 2x2 statistics match a hand-worked table", {
  r <- compute_comparison_statistics(fake_master(), data.frame())$results$data_availability
  expect_equal(c(r$tp, r$fn, r$fp, r$tn), c(3, 2, 1, 4))
  expect_equal(r$n_both_coded, 10)
  expect_equal(r$sensitivity, 3 / 5)
  expect_equal(r$specificity, 4 / 5)
  expect_equal(r$ppv, 3 / 4)
  expect_equal(r$npv, 4 / 6)
  expect_equal(r$n_agree, 7)
  expect_equal(r$n_disagree, 3)
  expect_equal(r$pct_agree, 70)
  expect_equal(r$pct_cooper_caught_mc_missed, 2 / 3 * 100)
  expect_equal(r$pct_mc_caught_cooper_missed, 1 / 3 * 100)
  expect_equal(r$pct_cooper_caught_mc_missed + r$pct_mc_caught_cooper_missed, 100)
  ref <- mcnemar.test(matrix(c(3, 2, 1, 4), nrow = 2), correct = TRUE)
  expect_equal(r$mcnemar_stat, unname(ref$statistic))
  expect_equal(r$mcnemar_p, ref$p.value)
})

test_that("Cooper's answers are sorted into yes, no, or not comparable", {
  r <- compute_comparison_statistics(fake_master(), data.frame())$results
  # "Yes, but not all data" counts as Yes for data_download.
  expect_equal(c(r$data_download$tp, r$data_download$fn, r$data_download$tn), c(1, 1, 1))
  expect_equal(r$data_availability$n_cooper_coded, 11)  # "Unsure" excluded
})

test_that("platform sets are compared on Cooper et al.'s label scale", {
  cv <- c("Dryad", "Dryad;Other repo/database", "Zenodo", "Dryad;Zenodo", "Dryad",
          "Other repo/database", "CRAN", "Supplementary materials", "Supplementary materials;Dryad",
          " dryad ", NA, "Dryad")
  mv <- c("Dryad", "Dryad", "GitHub, GitLab, Codeberg or similar platform;Zenodo", "Zenodo;Figshare",
          "Zenodo", "OSF", "Other repo/database", "Dryad", "Dryad", "Dryad", "Dryad", NA)
  expect_equal(platform_set_category(cv, mv),
               c("exact", "mc_fewer", "mc_more", "partial", "none", "exact", "exact",
                 NA, "exact", "exact", NA, NA))
  r <- platform_set_comparison(cv, mv, "test")
  expect_equal(r$n_both_coded, 9)
  expect_equal(as.integer(r$categories), c(5, 1, 1, 1, 1))
  expect_equal(r$n_agree, 5)
  expect_equal(r$n_disagree, 4)
  expect_equal(r$n_any_overlap, 8)
  expect_equal(r$n_cooper_only_non_repository, 1)
  expect_equal(r$n_cooper_multi, 2)
  expect_equal(r$n_mc_multi, 2)
})

test_that("an institutional Figshare matches Cooper's Figshare or Other label", {
  expect_true(all(is_institutional_figshare(c("https://doi.org/10.25375/uct.14618526.v1",
                                              "https://smithsonian.figshare.com/articles/1",
                                              "https://doi.org/10.5522/04/23217572.v1"))))
  expect_false(any(is_institutional_figshare(c("https://doi.org/10.6084/m9.figshare.9906092",
                                               "https://figshare.com/articles/x/1", NA))))
  cv <- c("Other repo/database", "Other repo/database", "Figshare", "Dryad")
  mv <- c("Figshare", "Figshare", "Figshare", "Figshare")
  # Only with the flag set, and only when Cooper chose Other rather than Figshare.
  expect_equal(platform_set_category(cv, mv, c(TRUE, FALSE, TRUE, TRUE)),
               c("exact", "none", "exact", "none"))
})

test_that("papers coded as GitHub plus Zenodo are split by what metacheck found", {
  gh <- "GitHub, GitLab, Codeberg or similar platform"
  cv <- rep(paste0(gh, ";Zenodo"), 6)
  cv[6] <- "Zenodo"
  mv <- c(paste0(gh, ";Zenodo"), gh, "Zenodo", "Dryad", NA, "Zenodo")
  r <- platform_set_comparison(cv, mv, "test")
  expect_equal(r$github_zenodo_found,
               c("both", "github_only", "zenodo_only", "neither", NA, NA))
  expect_equal(as.integer(r$github_zenodo), c(1, 1, 1, 1))
  expect_equal(r$n_cooper_github_zenodo, 4)
})

test_that("the real statistics add up for every measure", {
  skip_if_no_store()
  res <- tar("comparison_statistics")$results
  for (nm in names(res)) {
    r <- res[[nm]]
    expect_equal(r$n_agree + r$n_disagree, r$n_both_coded, info = nm)
    if (!is.null(r$categories)) {
      expect_equal(sum(r$categories), r$n_both_coded, info = nm)
    } else {
      expect_equal(r$tp + r$fn + r$fp + r$tn, r$n_both_coded, info = nm)
      expect_equal(r$pct_agree, 100 * (r$tp + r$tn) / r$n_both_coded, info = nm)
    }
  }
})
