# Runs every test in tests/testthat/, the way devtools::test() would for a
# package. From the project root:
#   Rscript tests/testthat.R
# Every failure is reported (testthat otherwise stops
# listing failures after the 10th).
options(testthat.progress.max_fails = Inf)
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
