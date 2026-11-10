# The pipeline from the Metacheck results to the manuscript, as a dependency graph.
#
# code/run_metacheck.R creates data/metacheck_results.rds, and code/match_reported_results.R
# creates data/match_reported_results.rds. The first takes many hours and depends on
# repositories on the internet, and the second reads the private corpus, so both stay
# outside this pipeline. It starts from their two files and needs no internet access or
# Docker.
#
# Usage, from the repository root:
#   Rscript -e "targets::tar_make()"        # bring everything up to date
#   Rscript -e "targets::tar_outdated()"    # what is out of date, without running it
#   Rscript -e "targets::tar_visnetwork()"  # the dependency graph
#   Rscript -e "targets::tar_read(master_comparison)"  # one result

library(targets)

tar_option_set(packages = c("dplyr", "metacheck", "openxlsx"), format = "rds")

tar_source("code/R")

list(
  # == Inputs ========================================================================
  tar_target(metacheck_results_file, "data/metacheck_results.rds", format = "file"),
  tar_target(match_reported_results_file, "data/match_reported_results.rds", format = "file"),
  tar_target(cooper_csv_file, "data/cooper_coded_data.csv", format = "file"),
  tar_target(sample_csv_file, "data/papers_data.csv", format = "file"),

  # Reviews done by hand; tracked for changes, never regenerated here.
  tar_target(manual_repo_findability_audit_csv, "data/review/manual_repo_findability_audit_data.csv", format = "file"),
  tar_target(manual_fair_api_findability_audit_csv, "data/review/manual_fair_api_findability_audit_data.csv", format = "file"),
  tar_target(repo_not_detected_categorization_csv, "data/review/repo_not_detected_categorization_data.csv", format = "file"),
  tar_target(code_archive_github_zenodo_review_csv, "data/review/code_archive_github_zenodo_review_data.csv", format = "file"),
  tar_target(data_archive_no_shared_platform_review_csv, "data/review/data_archive_no_shared_platform_review_data.csv", format = "file"),
  tar_target(github_repository_ownership_review_csv, "data/review/github_repository_ownership_review_data.csv", format = "file"),
  tar_target(result_changes_csv, "data/review/result_changes_data.csv", format = "file"),
  tar_target(result_change_causes_csv, "data/review/result_change_causes_data.csv", format = "file"),
  tar_target(result_change_percentages_csv, "data/review/result_change_percentages_data.csv", format = "file"),
  tar_target(result_change_other_columns_csv, "data/review/result_change_other_columns_data.csv", format = "file"),
  tar_target(reproducibility_notes_csv, "data/review/reproducibility_notes_data.csv", format = "file"),

  # == The Metacheck results, per module ==============================================
  tar_target(metacheck_results, readRDS(metacheck_results_file)),
  tar_target(res_repo_check, module_results(metacheck_results, "repo_check")),
  tar_target(res_data_check, module_results(metacheck_results, "data_check")),
  tar_target(res_code_check, module_results(metacheck_results, "code_check")),
  tar_target(res_open_practices, module_results(metacheck_results, "open_practices")),
  tar_target(res_oddpub, module_results(metacheck_results, "oddpub")),
  tar_target(res_reproducibility_check, module_results(metacheck_results, "reproducibility_check")),
  # The reproducibility review: each script of every paper whose code ran, why it did
  # not reproduce, with the notes written by hand (code/R/reproducibility_review.R).
  # A note written for another metacheck commit than the run's is marked outdated.
  tar_target(reproducibility_review_scripts, repro_review_join(
    repro_review_scripts(metacheck_results),
    read.csv(reproducibility_notes_csv, stringsAsFactors = FALSE))),
  tar_target(reproducibility_review_papers, repro_review_papers(reproducibility_review_scripts)),
  # The same two tables as files, for the discussion of the causes in the manuscript
  # and for finding what metacheck could improve (codebooks: code/build_documentation.R).
  tar_target(reproducibility_failures_scripts_csv, write_repro_review_csv(
    reproducibility_review_scripts, "data/reproducibility_failures_scripts_data.csv"), format = "file"),
  tar_target(reproducibility_failures_papers_csv, write_repro_review_csv(
    reproducibility_review_papers, "data/reproducibility_failures_papers_data.csv"), format = "file"),

  # The reviews explain Metacheck's behaviour on specific papers, which holds only for
  # the version that produced the results. These two targets list the rows and verdicts
  # written for another version, to be checked again by a person.
  tar_target(metacheck_sha, metacheck_results$run_info$metacheck_sha),
  tar_target(audit_staleness, check_audit_staleness(
    manual_repo_findability_audit_csv, manual_fair_api_findability_audit_csv, metacheck_sha)),

  # == Columns the manuscript reads directly ==========================================
  tar_target(structure_slim_obj, extract_structure_slim(res_data_check)),
  tar_target(code_table_slim_obj, extract_code_table_slim(res_code_check)),
  tar_target(manuscript_data_slim, assemble_manuscript_data_slim(
    res_repo_check, structure_slim_obj, code_table_slim_obj)),

  # == Comparison with Cooper et al. ==================================================
  tar_target(recreated_cooper_columns, recreate_cooper_columns(
    res_repo_check, res_data_check, res_code_check)),
  tar_target(cooper_vs_metacheck, build_cooper_vs_metacheck(
    recreated_cooper_columns, cooper_csv_file, sample_csv_file)),

  # == Disagreement review ============================================================
  # The spreadsheet is both an input, so that a verdict entered by hand reruns what
  # depends on it, and an output: new disagreements are added as empty rows, and
  # verdicts already entered are carried forward.
  tar_target(disagreement_worklist_xlsx_input, "data/review/disagreement_review_data.xlsx", format = "file"),
  tar_target(disagreement_worklist, build_disagreement_worklist(
    cooper_vs_metacheck, disagreement_worklist_xlsx_input, "disagreement_worklist")),
  tar_target(disagreement_worklist_xlsx_written,
             write_disagreement_worklist_xlsx(disagreement_worklist, disagreement_worklist_xlsx_input),
             format = "file"),
  tar_target(worklist_staleness, check_worklist_staleness(disagreement_worklist, metacheck_sha)),

  tar_target(master_comparison, build_master_comparison(
    cooper_vs_metacheck, res_open_practices, res_oddpub, cooper_csv_file, sample_csv_file)),

  # == Statistics and summaries the manuscript reads ==================================
  tar_target(comparison_statistics, compute_comparison_statistics(
    master_comparison, disagreement_worklist$review)),
  tar_target(disagreement_cause_categories, categorize_disagreement_causes(
    disagreement_worklist$review)),
  tar_target(comment_quality_correlation, compute_comment_quality_correlation(
    res_code_check, cooper_csv_file, sample_csv_file)),
  tar_target(free_extras_summary, summarize_free_extras(
    res_repo_check, res_data_check, res_code_check)),
  tar_target(data_archive_uncoded_papers, summarize_data_archive_uncoded_papers(
    master_comparison, res_data_check)),
  tar_target(third_party_repositories, summarize_third_party_repositories(
    master_comparison, res_repo_check, res_data_check, res_code_check,
    github_repository_ownership_review_csv)),
  tar_target(result_changes, summarize_result_changes(
    result_changes_csv, result_change_causes_csv, result_change_percentages_csv,
    result_change_other_columns_csv, master_comparison, comparison_statistics)),
  tar_target(match_table, readRDS(match_reported_results_file)$match_table),
  tar_target(reproducibility_results, repro_results(res_reproducibility_check, match_table)),
  tar_target(reproducibility_detail, extract_reproducibility_detail(res_reproducibility_check, match_table)),

  # == Manuscript and appendix ========================================================
  # The file targets make an edit of the text rerun the render. The force() calls list
  # what each document reads with tar_load(), because targets cannot see inside it.
  # The appendix runs three chunks of the manuscript, so it reads all of the same.
  tar_target(manuscript_qmd_file, "documentation/manuscript.qmd", format = "file"),
  tar_target(appendix_qmd_file, "documentation/appendix.qmd", format = "file"),
  tar_target(
    manuscript_render,
    {
      force(manuscript_qmd_file)
      force(manuscript_data_slim); force(master_comparison)
      force(comparison_statistics); force(disagreement_cause_categories)
      force(comment_quality_correlation); force(free_extras_summary)
      force(data_archive_uncoded_papers); force(disagreement_worklist)
      force(third_party_repositories); force(res_reproducibility_check); force(reproducibility_detail)
      force(res_code_check)
      force(manual_repo_findability_audit_csv); force(manual_fair_api_findability_audit_csv)
      force(repo_not_detected_categorization_csv); force(code_archive_github_zenodo_review_csv)
      force(data_archive_no_shared_platform_review_csv)
      quarto::quarto_render("documentation/manuscript.qmd")
      "documentation/manuscript.pdf"
    },
    format = "file"
  ),
  tar_target(
    appendix_render,
    {
      force(appendix_qmd_file); force(manuscript_qmd_file)
      force(manuscript_data_slim); force(master_comparison)
      force(comparison_statistics); force(disagreement_cause_categories)
      force(comment_quality_correlation); force(free_extras_summary)
      force(data_archive_uncoded_papers); force(disagreement_worklist)
      force(third_party_repositories); force(reproducibility_results); force(reproducibility_detail)
      force(manual_repo_findability_audit_csv); force(manual_fair_api_findability_audit_csv)
      force(repo_not_detected_categorization_csv); force(code_archive_github_zenodo_review_csv)
      force(data_archive_no_shared_platform_review_csv); force(result_changes)
      quarto::quarto_render("documentation/appendix.qmd")
      "documentation/appendix.pdf"
    },
    format = "file"
  )
)
