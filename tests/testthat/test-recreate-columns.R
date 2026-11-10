# How each mc_* column is derived from repo_check/data_check/code_check
# output (recreate_cooper_columns(), R/comparison_data.R), checked against
# the rules in the manuscript's Coding section on a small invented corpus.
#
#   p1: one Dryad repository: a downloaded, readable data file, a README,
#       and a downloaded R script; the repository has a licence and DOI.
#   p2: one OSF repository holding only a documentation file; that
#       repository has a licence, but no data.
#   p3: repo_check found nothing.
#   p4: a Zenodo and a GitHub repository, both holding data: one data file
#       downloaded but unreadable as a table, one readable but not
#       downloaded; an R and a Python script, neither downloaded. Only the
#       GitHub repository has a licence.

write_fake_corpus <- function(dir) {
  dryad <- "https://doi.org/10.5061/dryad.abc"; osf <- "https://osf.io/xyz"
  zen <- "https://doi.org/10.5281/zenodo.1"; gh <- "https://github.com/a/b"
  res_repo_check <- list(
    summary_table = data.frame(paper_id = c("p1", "p2", "p3", "p4"), files_n = c(3, 1, 0, 4)),
    table = data.frame(
      paper_id = c("p1", "p1", "p1", "p2", "p4", "p4", "p4", "p4"),
      repo_url = c(dryad, dryad, dryad, osf, zen, gh, gh, zen),
      doc_role = c(NA, "readme", NA, NA, NA, NA, NA, NA)),
    repo_metadata = data.frame(
      paper_id = c("p1", "p2", "p4", "p4"),
      repo_url = c(dryad, osf, zen, gh),
      doi = c("10.5061/dryad.abc", "10.17605/osf.io/xyz", "10.5281/zenodo.1", NA),
      license = c("CC0", "CC-BY", NA, "MIT")))
  res_data_check <- list(structure = data.frame(
    paper_id = c("p1", "p1", "p2", "p4", "p4"),
    repo_url = c(dryad, dryad, osf, zen, gh),
    file_name = c("data.CSV", "README.md", "notes.pdf", "raw.csv", "b.TXT"),
    file_location = c("/c/data.csv", "/c/README.md", NA, "/c/raw.csv", NA),
    data_type = c("data", "documentation", "documentation", "data", "data"),
    data_format = NA_character_,
    tabular_usable = c(TRUE, NA, NA, FALSE, TRUE)))
  res_code_check <- list(table = data.frame(
    paper_id = c("p1", "p4", "p4"),
    repo_url = c(dryad, gh, zen),
    file_location = c("/c/a.R", NA, ""),
    language = c("R", "R", "Python")))
  list(res_repo_check, res_data_check, res_code_check)
}

recreated_fake <- function() {
  f <- write_fake_corpus(tempdir())
  r <- recreate_cooper_columns(f[[1]], f[[2]], f[[3]])
  r[match(c("p1", "p2", "p3", "p4"), r$paper_id), ]
}

test_that("every paper gets a row, including one where repo_check found nothing", {
  r <- recreated_fake()
  expect_equal(r$paper_id, c("p1", "p2", "p3", "p4"))
})

test_that("mc_data_availability is TRUE exactly when repo_check listed at least one file", {
  expect_equal(recreated_fake()$mc_data_availability, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("mc_data_archive lists the platform of every repository holding a data file, sorted", {
  expect_equal(recreated_fake()$mc_data_archive,
               c("Dryad", NA, NA, "GitHub, GitLab, Codeberg or similar platform;Zenodo"))
})

test_that("licence and DOI come only from repositories that hold a data file", {
  r <- recreated_fake()
  expect_equal(r$mc_data_license, c(TRUE, NA, NA, TRUE))
  expect_equal(r$mc_data_doi, c("10.5061/dryad.abc", NA, NA, "10.5281/zenodo.1"))
})

test_that("mc_data_download needs a data file that was both downloaded and readable as a table", {
  expect_equal(recreated_fake()$mc_data_download, c(TRUE, NA, NA, FALSE))
})

test_that("mc_data_format lists the lower-case extensions of readable data files", {
  expect_equal(recreated_fake()$mc_data_format, c(".csv", NA, NA, ".txt"))
})

test_that("the README signal is any README-named file in any of the paper's repositories", {
  r <- recreated_fake()
  expect_equal(r$mc_data_README, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(r$mc_code_README, r$mc_data_README)
})

test_that("code archived needs one code file; code download needs one with a download location", {
  r <- recreated_fake()
  expect_equal(r$mc_code_archived, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(r$mc_code_download, c(TRUE, NA, NA, FALSE))
  expect_equal(r$mc_code_language, c("R", NA, NA, "Python;R"))
  expect_equal(r$mc_code_archive,
               c("Dryad", NA, NA, "GitHub, GitLab, Codeberg or similar platform;Zenodo"))
  expect_true(all(is.na(r$mc_code_used)))
})

test_that("repository URLs get the right platform label", {
  expect_equal(
    repo_url_platform(c("https://osf.io/abc", "https://github.com/a/b", "https://gitlab.com/a/b",
                        "https://zenodo.org/records/1", "https://doi.org/10.5281/zenodo.2",
                        "https://datadryad.org/stash/dataset/x", "https://doi.org/10.5061/dryad.y",
                        "https://figshare.com/articles/x", "https://doi.org/10.6084/m9.figshare.1",
                        "https://example.org/data", NA)),
    c("OSF", rep("GitHub, GitLab, Codeberg or similar platform", 2), "Zenodo", "Zenodo",
      "Dryad", "Dryad", "Figshare", "Figshare", "Other repo/database", NA))
  # Every DOI prefix and institutional host Metacheck itself knows for Dryad
  # and Figshare gets that label, not just the most common one.
  for (p in metacheck:::.dryad_doi_prefixes())
    expect_equal(repo_url_platform(paste0("https://doi.org/", p, "/dryad.x")), "Dryad", info = p)
  for (h in metacheck:::.figshare_vanity_hosts())
    expect_equal(repo_url_platform(paste0("https://", h, "/articles/1")), "Figshare", info = h)
  for (p in names(metacheck:::.figshare_doi_prefix_hosts()))
    expect_equal(repo_url_platform(paste0("https://doi.org/", p, "/m9.x")), "Figshare", info = p)
})

test_that("Cooper's combined README answer follows the Coding section's rule", {
  csv <- tempfile(fileext = ".csv"); smp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    paper_number = 1:6, doi = paste0("10.x/", 1:6),
    data_availability = NA, data_archive = NA, data_doi = NA, data_license = NA,
    data_download = NA, data_format = NA, code_used = NA, code_archived = NA,
    code_archive = NA, code_download = NA, code_language = NA,
    data_README = c("Yes", "Quasi-README", "No", "No", NA, NA),
    code_README = c(NA, "No", "Yes", "No", "No", NA)), csv, row.names = FALSE)
  write.csv(data.frame(article_id = paste0("p", 1:6), doi = paste0("10.x/", 1:6)), smp, row.names = FALSE)
  recreated <- data.frame(paper_id = paste0("p", 1:6), mc_data_README = FALSE)
  out <- build_cooper_vs_metacheck(recreated, csv, smp)
  expect_equal(out$cooper_any_readme, c(TRUE, TRUE, TRUE, FALSE, FALSE, NA))
})

test_that("Metacheck's answers in the real comparison are consistent with each other", {
  skip_if_no_store()
  m <- tar("master_comparison")
  slim <- tar("manuscript_data_slim")
  with_data <- unique(slim$structure_slim$paper_id[slim$structure_slim$data_type == "data"])
  with_code <- unique(slim$code_table_slim$paper_id)
  repo_found <- m$article_id[m$mc_data_availability %in% TRUE]
  # A data or code file can only be found in a repository that was found.
  expect_true(all(with_data %in% repo_found))
  expect_true(all(with_code %in% repo_found))
  # Downloaded code is archived code; downloaded data is data.
  expect_true(all(m$mc_code_archived[m$mc_code_download %in% TRUE]))
  expect_true(all(m$article_id[m$mc_data_download %in% TRUE] %in% with_data))
  expect_true(all(!is.na(m$mc_data_archive[m$article_id %in% with_data])))
  expect_true(all(m$mc_code_archived[m$article_id %in% with_code]))
})
