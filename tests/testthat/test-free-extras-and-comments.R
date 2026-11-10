# "Results you get for free" (summarize_free_extras()) and the comment-count
# versus comment-quality correlation (compute_comment_quality_correlation()).

# Two papers, five code files:
#   p1: a.R  (missing file, setwd, parse error, 10 comment / 90 code lines)
#       b.R  (absolute path, not downloaded, never parsed)
#       c.py (zero comments, has a docstring, scattered imports)
#   p2: d.R  (parsed fine, 0 comments, no issues)
#       e.do (Stata, 5 comment / 5 code lines)
write_fake_extras <- function(dir) {
  res_code_check <- list(
    table = data.frame(
      paper_id = c("p1", "p1", "p1", "p2", "p2"),
      file_name = c("a.R", "b.R", "c.py", "d.R", "e.do"),
      language = c("R", "R", "Python", "R", "Stata"),
      file_location = c("/a.R", NA, "/c.py", "/d.R", "/e.do"),
      loaded_files_missing = c(1, 0, 0, 0, NA),
      code_abs_path = c(FALSE, TRUE, FALSE, FALSE, FALSE),
      code_setwd = c(TRUE, FALSE, FALSE, FALSE, FALSE),
      comment_lines = c(10, NA, 0, 0, 5),
      code_lines = c(90, NA, 50, 20, 5),
      percentage_comment = c(10, NA, 0, 0, 50),
      has_docstring = c(NA, NA, TRUE, NA, NA),
      library_max_between = c(1, NA, 10, 0, NA),
      parse_error = c(TRUE, NA, NA, FALSE, NA)),
    summary_table = data.frame(paper_id = c("p1", "p2", "p3"), code_n = c(3, 2, 0),
                               code_version_pinned = c(TRUE, FALSE, FALSE)))
  res_data_check <- list(
    structure = data.frame(paper_id = c("p1", "p3")),
    findings = data.frame(check = c("Personal info (column name)", "Merged cells", "Merged cells"),
                          source_file = c("x.csv", "x.csv", "y.xlsx")))
  res_repo_check <- list(
    table = data.frame(paper_id = c("p1", "p2", "p3")),
    summary_table = data.frame(paper_id = c("p1", "p2", "p3"), files_n = c(3, 2, 1)),
    naming_issues = data.frame(paper_id = c("p1", "p1", "p3"), rule = c("space", "unclassifiable", "space"),
                               severity = c("bad", "bad", "suggestion")))
  list(res_repo_check, res_data_check, res_code_check)
}

test_that("each free-results number uses the denominator the manuscript names", {
  f <- write_fake_extras(tempdir())
  fe <- summarize_free_extras(f[[1]], f[[2]], f[[3]])
  expect_equal(fe$n_code_files, 5)
  expect_equal(fe$n_code_papers, 2)
  expect_equal(c(fe$n_missing_files, fe$n_abs_path, fe$n_setwd), c(1, 1, 1))
  expect_equal(fe$pct_missing_files, 100 * 1 / 5)
  expect_equal(fe$n_papers_portability, 1)
  expect_equal(fe$pct_papers_portability, 50)
  # Comment share is lines-weighted over files with line counts.
  expect_equal(fe$pct_comment_corpus, 100 * (10 + 0 + 0 + 5) / (10 + 90 + 0 + 50 + 0 + 20 + 5 + 5))
  expect_equal(fe$n_zero_comment, 2)
  expect_equal(c(fe$n_py_zero_comment, fe$n_py_zero_comment_has_docstring), c(1, 1))
  # Scattered: more than 3 lines apart, over files with a value.
  expect_equal(c(fe$n_scattered, fe$n_scattered_denom), c(1, 3))
  # Parse errors: R files that were downloaded AND reached parse().
  expect_equal(c(fe$n_parse_error, fe$n_parse_denom), c(1, 2))
  # Version pinning: over papers with at least one code file.
  expect_equal(c(fe$n_pin_papers_pinned, fe$n_pin_papers), c(1, 2))
  expect_equal(c(sum(fe$spreadsheet_findings_table), fe$n_spreadsheet_files_flagged), c(3, 2))
  expect_equal(c(fe$n_naming_issues, fe$n_naming_bad, fe$n_naming_suggestion, fe$n_papers_naming), c(3, 2, 1, 2))
  expect_equal(fe$pct_papers_naming, 100 * 2 / 3)
})

test_that("comment share per paper is weighted by lines, with one row per paper", {
  dir <- tempdir()
  res_code_check <- list(table = data.frame(
    paper_id = c("p1", "p1", "p2", "p3", "p4", "p5"),
    file_name = paste0("f", 1:6),
    comment_lines = c(10, 0, 5, 1, 30, 2), code_lines = c(90, 100, 5, 9, 70, 98),
    percentage_comment = c(10, 0, 50, 10, 30, 2)))
  csv <- tempfile(fileext = ".csv"); smp <- tempfile(fileext = ".csv")
  write.csv(data.frame(paper_number = 1:5, doi = paste0("10.x/", 1:5),
                       code_annotation_scale = c(3, 8, 5, 6, 2)), csv, row.names = FALSE)
  write.csv(data.frame(article_id = paste0("p", 1:5), doi = paste0("10.x/", 1:5)), smp, row.names = FALSE)
  cq <- compute_comment_quality_correlation(res_code_check, csv, smp)
  expect_equal(cq$n_both, 5)
  expect_false(anyDuplicated(cq$data$article_id) > 0)
  # p1: (10 + 0) / (10 + 0 + 90 + 100), not the mean of 10% and 0%.
  expect_equal(cq$data$mc_percentage_comment[cq$data$article_id == "p1"], 10 / 200)
  expect_equal(cq$data$mc_code_files_n[cq$data$article_id == "p1"], 2)
  pct <- cq$data$mc_percentage_comment[match(paste0("p", 1:5), cq$data$article_id)]
  expect_equal(cq$pearson_r, cor(c(3, 8, 5, 6, 2), pct))
})

test_that("the real comment-quality data has one row per paper and uses all rated papers", {
  skip_if_no_store()
  cq <- tar("comment_quality_correlation")
  expect_false(anyDuplicated(cq$data$article_id) > 0)
  expect_equal(cq$n_cooper_coded, 543)
})
