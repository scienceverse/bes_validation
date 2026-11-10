# The small subset of columns the manuscript reads from the module results.

extract_structure_slim <- function(res_data_check) {
  res_data_check$structure[, c("paper_id", "data_type", "repo_url", "file_name")]
}

extract_code_table_slim <- function(res_code_check) {
  res_code_check$table[, c("paper_id", "repo_url", "file_name", "language")]
}

assemble_manuscript_data_slim <- function(res_repo_check, structure_slim, code_table_slim) {
  list(
    repo_metadata = res_repo_check$repo_metadata,
    structure_slim = structure_slim,
    code_table_slim = code_table_slim
  )
}
