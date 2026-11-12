# Computed from repo_metadata (loaded via data_load.R, from
# manuscript_data_slim) to check a pattern visible to anyone
# inspecting repo_metadata directly: many Dryad-hosted repositories
# report the identical license string. `license` here is read verbatim
# from each platform's own API response (see mc_data_doi/mc_data_license
# below) -- nothing here rewrites or standardises it.
#
# is_dryad_url() reads its prefix list from metacheck's own
# .dryad_doi_prefixes() rather than hardcoding only the single most
# common prefix (10.5061) -- an earlier version of this chunk did exactly
# that and, as a result, miscounted 14 genuine Dryad repositories (real
# datadryad.org deposits under one of Dryad's other institutional DOI
# prefixes, e.g. 10.25338, 10.7291, 10.6078 -- confirmed live, each
# resolves to datadryad.org) as "non-Dryad", which in turn made the
# prose below wrongly claim the CC0-1.0 string "never appears for a
# non-Dryad repository". This is the same multi-prefix detection gap
# Appendix A.6 documents and fixes for repo_check's own Dataverse
# DOI-prefix allowlist and for 03_recreate_cooper_columns.R's platform
# labelling -- this chunk had an independent copy of the same narrow,
# single-prefix bug that neither of those fixes touched, since it lives
# only here in the manuscript's own license-value-check chunk.
# One row per repository: repo_metadata has one row per (paper,
# repository), so a repository cited by two papers would otherwise be
# counted twice in "licensed Dryad repositories".
rmeta <- repo_metadata |>
  summarise(license = first(license, na_rm = TRUE), .by = repo_url)

dryad_prefix_pattern <- paste(gsub("\\.", "\\\\.", metacheck:::.dryad_doi_prefixes()), collapse = "|")
is_dryad_url <- function(x) grepl(paste0("datadryad\\.org|(", dryad_prefix_pattern, ")/dryad|doi\\.org/(", dryad_prefix_pattern, ")"), x, ignore.case = TRUE)
cc0_url <- "https://spdx.org/licenses/CC0-1.0.html"
n_dryad_total <- sum(is_dryad_url(rmeta$repo_url))
n_dryad_licensed <- sum(is_dryad_url(rmeta$repo_url) & !is.na(rmeta$license))
n_dryad_cc0 <- sum(rmeta$license %in% cc0_url & is_dryad_url(rmeta$repo_url), na.rm = TRUE)
n_cc0_total <- sum(rmeta$license %in% cc0_url, na.rm = TRUE)
n_nondryad_cc0 <- n_cc0_total - n_dryad_cc0