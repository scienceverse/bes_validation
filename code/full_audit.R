# The unified, single audit file covering ALL n_cooper_yes_mc_no
# "Cooper found data/code, Metacheck missed it" papers -- superseding
# the three separate, partially-overlapping audits (the
# repo_not_detected_categorization_data.csv population, the first
# agent-reviewed manual_repo_findability_audit_data.csv pass, and the later
# classification-gap extension of it) an earlier version of this section
# reported one at a time. Built across two rounds: (1) every paper
# where repo_check found no repository at all was independently reviewed
# by one AI agent (Claude, Anthropic) per paper, each instructed to read
# the paper's full extracted text, locate the citation independently of
# both Cooper's recorded excerpt and Metacheck's own detector output,
# query the live repository page or API directly, and check Metacheck's
# source code directly for a backend; (2) every paper where repo_check
# DID find a repository but neither data_check nor code_check classified
# any of its files as data or code was reviewed the same way, but with
# the file listing already in hand, so the agent's task was to determine
# whether the unclassified files are genuinely data or code, and if so,
# why Metacheck's classifier missed them. A handful of round-(1) papers
# were instead checked directly by the manuscript's author, as a manual
# pilot before the agent-based pass began. No human review of each
# individual agent's live-fetch result or classification was performed
# afterward.
full_audit <- read.csv("data/review/manual_repo_findability_audit_data.csv")
full_fair_audit <- read.csv("data/review/manual_fair_api_findability_audit_data.csv")
stopifnot(nrow(full_audit) == n_cooper_yes_mc_no)
stopifnot(identical(full_audit$paper_doi, full_fair_audit$paper_doi))

full_audit$fair_machine_findable_in_principle <- full_fair_audit$fair_machine_findable_in_principle

# Five clusters, all mutually exclusive and summing to n_cooper_yes_mc_no.
# (An earlier "repository contains no genuine data or code" cluster is now
# empty: on opening each of its files, every one held data or code, and
# those rows moved to the not-classified cluster below.)
# CORRECTLY_DETECTABLE_NO_GAP_FOUND no longer appears: every row this
# manuscript's own review had assigned to it was independently re-verified
# against the live pipeline (res_repo_check.RData, and for most, a fresh
# uncached repo_check() rerun) and found to be either a real, misclassified
# Metacheck-side gap, or a paper that genuinely resolved once the pipeline
# itself was refreshed -- in which case it was removed from this population
# entirely rather than retained under a "no gap" label, the same treatment
# given to every other genuinely-resolved paper in this review.
no_repo_cited_cat <- "NO_REPOSITORY_CITED"
# The repository was found and holds data or code, but Metacheck classified
# none of its files as such: either the format is one Metacheck does not yet
# support (unsupported_format_cats), or the data or code were not shared in a
# FAIR form that identifies them as data or code (a table inside a PDF or
# Word document, a bare .txt file, code saved without its language's
# extension).
unsupported_format_cats <- c("CLASSIFIER_EXTENSION_GAP", "CLASSIFIER_MISTYPED")
not_fair_format_cat <- "DATA_OR_CODE_NOT_IN_FAIR_FORMAT"
not_classified_cats <- c(unsupported_format_cats, not_fair_format_cat)
# Technical limitations: the repository is FAIR and on a platform Metacheck
# supports, but something stops Metacheck from using it. Two kinds:
#   - text extraction: the PDF-to-text step corrupted or dropped the
#     identifier before Metacheck sees the text;
#   - Metacheck's own code: a defect or a deliberate design limit.
extraction_cats <- c("EXTRACTION_GAP_DOI_GARBLED", "EXTRACTION_GAP_TEXT_MISSING")
metacheck_code_cats <- c("SUPPORTED_PLATFORM_BUG",
                         "CLASSIFIER_ARCHIVE_NOT_EXPANDED", "FILES_NOT_ENUMERATED")
metacheck_limitation_cats <- c(extraction_cats, metacheck_code_cats)
# Not FAIR, besides an unsupported platform without an API and a repository
# that is not live (both counted below): the citation is a private reviewer
# or share link, or a bare organisation/user page that identifies no specific
# deposit; the repository blocks automated access; or it offers no API
# through which the data can be retrieved (classified 2026-10-13, FAIR-first).
not_fair_cats <- c("NOT_FAIR_PRIVATE_LINK", "PLATFORM_URL_NOT_A_SINGLE_REPO",
                   "NOT_FAIR_BLOCKS_AUTOMATED_ACCESS", "NOT_FAIR_NO_DATA_API")

n_no_repo_cited <- sum(full_audit$root_cause_category == no_repo_cited_cat)
n_not_classified <- sum(full_audit$root_cause_category %in% not_classified_cats)
n_unsupported_format <- sum(full_audit$root_cause_category %in% unsupported_format_cats)
n_not_fair_format <- sum(full_audit$root_cause_category == not_fair_format_cat)
n_metacheck_limitation <- sum(full_audit$root_cause_category %in% metacheck_limitation_cats)

is_platform_not_supported <- full_audit$root_cause_category == "PLATFORM_NOT_SUPPORTED"
is_not_live <- full_audit$root_cause_category == "REPO_NOT_ACTUALLY_LIVE_OR_ACCESSIBLE"
n_unsupported_but_fair <- sum(is_platform_not_supported & full_audit$fair_machine_findable_in_principle == "TRUE")
n_unsupported_not_fair <- sum(is_platform_not_supported & full_audit$fair_machine_findable_in_principle == "FALSE")
n_not_live <- sum(is_not_live)
n_nf <- function(cat) sum(full_audit$root_cause_category == cat)
n_nonfair_private_link <- n_nf("NOT_FAIR_PRIVATE_LINK")
n_nonfair_no_deposit <- n_nf("PLATFORM_URL_NOT_A_SINGLE_REPO")
n_nonfair_blocked <- n_nf("NOT_FAIR_BLOCKS_AUTOMATED_ACCESS")
n_nonfair_no_api <- n_nf("NOT_FAIR_NO_DATA_API")
n_nonfair <- n_unsupported_not_fair + n_not_live + sum(full_audit$root_cause_category %in% not_fair_cats)

stopifnot(n_no_repo_cited + n_not_classified + n_metacheck_limitation +
            n_unsupported_but_fair + n_nonfair == n_cooper_yes_mc_no)

# Misses explained by data and code not shared following the FAIR
# principles: no repository linked, data or code in a non-FAIR format, or
# a non-FAIR link or repository. Every other paper Cooper's coders found
# to share data or code shared it in a FAIR way, and Metacheck's own
# finds (n_both_yes_either) are a subset of those.
n_not_fair_total <- n_no_repo_cited + n_not_fair_format + n_nonfair
n_cooper_fair <- n_cooper_either - n_not_fair_total
stopifnot(n_both_yes_either + n_cooper_yes_mc_no == n_cooper_either)
pct_fair_found <- sprintf("%.1f%%", 100 * n_both_yes_either / n_cooper_fair)

# The former STALE_CACHE_FALSE_NEGATIVE papers were rerun one at a time
# against metacheck 109a9aa on 2026-10-13: those that now have files left
# the audit, and the cause of every other one is a specific code or
# extraction limitation, so that category no longer exists.
stopifnot(!"STALE_CACHE_FALSE_NEGATIVE" %in% full_audit$root_cause_category)

# Breakdown within the Metacheck-limitation cluster, for the detailed
# subsection below.
n_lim_archive_not_expanded <- sum(full_audit$root_cause_category == "CLASSIFIER_ARCHIVE_NOT_EXPANDED")
n_lim_files_not_enumerated <- sum(full_audit$root_cause_category == "FILES_NOT_ENUMERATED")
n_lim_platform_bug <- sum(full_audit$root_cause_category == "SUPPORTED_PLATFORM_BUG")
n_lim_doi_garbled <- sum(full_audit$root_cause_category == "EXTRACTION_GAP_DOI_GARBLED")
n_lim_text_missing <- sum(full_audit$root_cause_category == "EXTRACTION_GAP_TEXT_MISSING")
n_lim_extraction <- n_lim_doi_garbled + n_lim_text_missing
n_lim_code <- sum(full_audit$root_cause_category %in% metacheck_code_cats)
# Within the code limitations: the repository DOI appears only in the
# reference list of the extracted text, which Metacheck does not search,
# either because the authors cited their data only through a reference
# ("data citation") or because the PDF-to-text step moved the DOI out of
# the data statement or scrambled it ("text conversion").
n_lim_ref_only <- sum(!is.na(full_audit$doi_only_in_reference_list))
n_lim_ref_citation <- sum(full_audit$doi_only_in_reference_list %in% "data citation")
n_lim_ref_conversion <- sum(full_audit$doi_only_in_reference_list %in% "text conversion")
stopifnot(n_lim_ref_citation + n_lim_ref_conversion == n_lim_ref_only,
          all(full_audit$root_cause_category[!is.na(full_audit$doi_only_in_reference_list)] %in% metacheck_code_cats))
# The FAIR repositories on an unsupported platform reclassified on
# 2026-10-13 include the Environmental Data Initiative (EDI) papers.
n_unsupported_edi <- sum(is_platform_not_supported & grepl("EDI is a DataONE member node",
                                                           full_audit$manual_why_metacheck_misses_it, fixed = TRUE))

