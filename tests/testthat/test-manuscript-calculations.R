# The manuscript's own calculations, run from manuscript.qmd's own chunks
# (manuscript_env(), helper-load.R), checked against what they must be by
# definition.

skip_if_no_manuscript_inputs <- function() {
  skip_if_no_store()
  testthat::skip_if_not(file.exists(proj("data/bes.rds")), "data/bes.rds not present")
}

test_that("the counts of papers with data or code fit together", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  expect_equal(e$n_both_yes_either + e$n_cooper_yes_mc_no, e$n_cooper_either)
  expect_equal(e$n_both_yes_either + e$n_mc_yes_cooper_no, e$n_mc_either)
  expect_equal(e$n_either_total_disagree, e$n_cooper_yes_mc_no + e$n_mc_yes_cooper_no)
  expect_lte(e$n_mc_either, e$n_mc_repo_found)
  expect_lte(max(e$n_mc_data, e$n_mc_code), e$n_mc_either)
  expect_equal(e$n_repo_not_found_of_gap + e$n_repo_found_but_no_data_or_code, e$n_cooper_yes_mc_no)
})

test_that("the audit file covers exactly the papers where Cooper found data or code and Metacheck found none", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  smp <- read.csv(sample_csv(), colClasses = "character")
  audit_ids <- smp$article_id[match(tolower(e$full_audit$paper_doi), tolower(smp$doi))]
  expect_false(anyNA(audit_ids))
  expect_false(anyDuplicated(audit_ids) > 0)
  expect_setequal(audit_ids, e$gap_ids)
  expect_identical(e$full_audit$paper_doi, e$full_fair_audit$paper_doi)
})

test_that("every audit category belongs to exactly one of the five clusters, and the clusters add up", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  clusters <- list(no_repo = e$no_repo_cited_cat, not_classified = e$not_classified_cats,
                   limitation = e$metacheck_limitation_cats, not_fair = e$not_fair_cats,
                   unsupported_or_not_live = c("PLATFORM_NOT_SUPPORTED", "REPO_NOT_ACTUALLY_LIVE_OR_ACCESSIBLE"))
  all_cats <- unlist(clusters)
  expect_false(anyDuplicated(all_cats) > 0)
  expect_true(all(e$full_audit$root_cause_category %in% all_cats))
  # Unsupported platforms split by whether they are FAIR, which must be recorded.
  unsupported <- e$full_audit$root_cause_category == "PLATFORM_NOT_SUPPORTED"
  expect_true(all(e$full_audit$fair_machine_findable_in_principle[unsupported] %in% c("TRUE", "FALSE", TRUE, FALSE)))
  expect_equal(e$n_no_repo_cited + e$n_not_classified + e$n_metacheck_limitation +
                 e$n_unsupported_but_fair + e$n_nonfair, e$n_cooper_yes_mc_no)
  expect_equal(e$n_not_fair_format + e$n_unsupported_format, e$n_not_classified)
  expect_equal(nrow(e$a10_tab), e$n_metacheck_limitation)
  expect_equal(nrow(e$a10_nc_tab), e$n_not_classified)
})

test_that("the FAIR-first classification is complete and consistent between the two audit files", {
  a <- read.csv(proj("data/review/manual_repo_findability_audit_data.csv"), stringsAsFactors = FALSE)
  f <- read.csv(proj("data/review/manual_fair_api_findability_audit_data.csv"), stringsAsFactors = FALSE)
  # The configuration-gap category was dissolved on 2026-10-13: none of its
  # repositories could be supported by adding a host name.
  expect_false("SUPPORTED_PLATFORM_CONFIG_GAP" %in% a$root_cause_category)
  expect_identical(a$root_cause_category, f$root_cause_category)
  # A not-FAIR category means the FAIR file says not machine-findable.
  not_fair <- grepl("^NOT_FAIR", a$root_cause_category) | a$root_cause_category == "PLATFORM_URL_NOT_A_SINGLE_REPO"
  expect_true(all(f$fair_machine_findable_in_principle[not_fair] %in% c(FALSE, "FALSE")))
  # Every row reclassified on 2026-10-13 says why.
  moved <- grepl("RECLASSIFIED 2026-10-13", a$manual_why_metacheck_misses_it, fixed = TRUE)
  expect_gt(sum(moved), 0)
  expect_true(all(a$version_is_exact[moved] %in% c(TRUE, "TRUE")))
})

test_that("the 'repository not detected' file lists only papers Cooper coded and Metacheck found nothing for", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  cats <- read.csv(proj("data/review/repo_not_detected_categorization_data.csv"), stringsAsFactors = FALSE)
  expect_false(anyDuplicated(cats$article_id) > 0)
  no_repo <- e$side_by_side$article_id[e$side_by_side$mc_data_availability %in% FALSE]
  expect_setequal(cats$article_id, intersect(e$gap_ids, no_repo))
})

test_that("the licence paragraph counts each repository once", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  expect_false(anyDuplicated(e$rmeta$repo_url) > 0)
  expect_lte(e$n_dryad_cc0, e$n_dryad_licensed)
  expect_lte(e$n_dryad_licensed, e$n_dryad_total)
  expect_equal(e$n_nondryad_cc0, sum(e$rmeta$license %in% e$cc0_url & !e$is_dryad_url(e$rmeta$repo_url)))
})

test_that("the count of supported repositories counts every host once", {
  skip_if_no_manuscript_inputs()
  e <- manuscript_env()
  hosts <- c(e$dataverse_hosts, e$dspace7_hosts, e$dspace_legacy_hosts, e$dataone_hosts, e$figshare_hosts)
  expect_true(all(nzchar(hosts)))
  expect_equal(e$n_unique_repos, length(unique(hosts)) + e$n_dedicated_platforms)
})

test_that("the uncoded-data-papers summary uses the same platform labels as the comparison", {
  skip_if_no_store()
  u <- tar("data_archive_uncoded_papers")
  labels <- c("OSF", "GitHub, GitLab, Codeberg or similar platform", "Zenodo", "Dryad",
              "Figshare", "Other repo/database")
  used <- unlist(strsplit(u$per_paper$mc_platform, ";"))
  expect_true(all(used %in% labels))
  expect_equal(u$n_no_repo_found + u$n_repo_but_no_data, u$n_mc_archive_na)
})
