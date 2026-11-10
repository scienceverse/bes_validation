# The modules' results in data/metacheck_results.rds.

# A file row's identity: the same file of the same repository for the same
# paper. Two rows that agree on all of these are the same file listed twice.
# The download location is part of it: two same-named files in different
# folders of one repository unpack to different locations.
file_identity <- c("paper_id", "repo_url", "file_path", "file_name", "file_url",
                   "archive_url", "archive_member", "file_location")
copies <- function(tbl, listed_twice = character(0)) {
  cols <- intersect(file_identity, names(tbl))
  dup <- duplicated(do.call(paste, c(lapply(tbl[cols], as.character), sep = "\r")))
  # A repository's own listing can hold a file twice (Dryad lists some files,
  # archives included, under two file numbers, e.g. doi:10.5061/dryad.2r5k78d);
  # a table without a file_url column then shows that file, or a file
  # unpacked from that archive, as two identical rows. That is the
  # repository's listing, faithfully reported, not a copy.
  dup & !paste(tbl$paper_id, tbl$repo_url) %in% listed_twice
}
# Repositories whose own listing (in repo_check) holds a file name more than
# once under different file URLs.
listed_twice <- function(repo_table) {
  k <- paste(repo_table$paper_id, repo_table$repo_url, basename(repo_table$file_path))
  u <- tapply(repo_table$file_url, k, function(x) length(unique(x)))
  twice <- names(u)[u > 1]
  unique(paste(repo_table$paper_id, repo_table$repo_url)[k %in% twice])
}

papers <- function() read.csv(sample_csv(), colClasses = "character")$article_id

test_that("the repo_check results list every file once and count files correctly", {
  r <- load_corpus("repo")
  st <- r$summary_table
  expect_equal(nrow(st), N_PAPERS)
  expect_setequal(st$paper_id, papers())
  expect_null(r$paper)
  expect_false(any(grepl("_by_batch_by_batch", names(r))))
  expect_equal(sum(copies(r$table)), 0)
  expect_true(all(r$table$paper_id %in% st$paper_id))
  # files_n is the number of files repo_check listed for the paper. Metacheck
  # 109a9aa leaves it NA for a paper that cites no repository (repo_n 0),
  # which has no files.
  rows <- as.integer(table(factor(r$table$paper_id, levels = st$paper_id)))
  expect_true(all(st$repo_n[is.na(st$files_n)] == 0))
  expect_equal(ifelse(is.na(st$files_n), 0L, as.integer(st$files_n)), rows)
})

test_that("the data_check results list every file once", {
  d <- load_corpus("data")
  expect_equal(nrow(d$summary_table), N_PAPERS)
  expect_setequal(d$summary_table$paper_id, papers())
  expect_null(d$paper)
  expect_false(any(grepl("_by_batch_by_batch", names(d))))
  expect_equal(sum(copies(d$structure, listed_twice(load_corpus("repo")$table))), 0)
  expect_true(all(d$structure$paper_id %in% d$summary_table$paper_id))
})

test_that("the code_check results list every file once", {
  k <- load_corpus("code")
  expect_equal(nrow(k$summary_table), N_PAPERS)
  expect_setequal(k$summary_table$paper_id, papers())
  expect_null(k$paper)
  expect_false(any(grepl("_by_batch_by_batch", names(k))))
  expect_equal(sum(copies(k$table, listed_twice(load_corpus("repo")$table))), 0)
  expect_true(all(k$table$paper_id %in% k$summary_table$paper_id))
})
