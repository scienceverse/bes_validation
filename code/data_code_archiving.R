# Computed from side_by_side (for the Cooper-side and
# unsupported-repository counts) and structure_slim/code_table_slim
# (for the metacheck-side counts -- both loaded via data_load.R, 
# from manuscript_data_slim.RData), and from the host lists the
# installed metacheck package itself uses to recognise a repository
# platform (metacheck:::.dataverse_hosts(), etc.) -- nothing in this
# chunk is typed by hand.
#
# TODO (open question, not yet resolved): accuracy_summary (OddPub vs.
# open_practices accuracy against Cooper's data_open/code_open) is
# computed by 02_create_comparison_data/03_build_master_comparison.R
# and attached to master_comparison.rds as an R attribute rather than
# as ordinary columns/a separate object. An attribute is easy to lose
# silently (it does not survive an unguarded subset or a naive re-save,
# and does not show up in a plain View()/str() of the data frame's
# columns) -- consider having that script return accuracy_summary as
# its own named list element, or its own small .rds, instead.

n_cooper_data    <- sum(side_by_side$data_availability %in% "Yes")
n_cooper_code    <- sum(side_by_side$code_archived %in% "Yes")
n_cooper_either  <- sum(side_by_side$data_availability %in% "Yes" |
                          side_by_side$code_archived %in% "Yes")

# mc_data_availability's own scope: at least one linked repository with
# at least one file of any type found in the main text.
n_mc_repo_found  <- sum(side_by_side$mc_data_availability, na.rm = TRUE)

# "Contained data"/"contained code": at least one file data_check/
# code_check classified as data/code respectively -- broader than
# mc_data_format's own tabular_usable-only scope, since "contained data"
# in this sentence means any data-classified file, not only a file that
# parsed successfully as a usable table.
papers_with_data <- structure_slim |>
  filter(data_type == "data") |>
  pull(paper_id) |> unique()
papers_with_code <- unique(code_table_slim$paper_id)
n_mc_data   <- length(papers_with_data)
n_mc_code   <- length(papers_with_code)
n_mc_either <- length(union(papers_with_data, papers_with_code))

# Unique repositories (repo_url) holding at least one data- or
# code-classified file, across the whole corpus.
repo_urls_with_data <- structure_slim |>
  filter(data_type == "data") |>
  pull(repo_url) |> unique()
repo_urls_with_code <- unique(code_table_slim$repo_url)
n_repos_either <- length(union(repo_urls_with_data, repo_urls_with_code))

# The two DIRECTIONAL disagreement counts between Cooper's coding and
# metacheck's "has data or code" judgement, computed directly from the
# two id sets rather than as a naive difference of their sizes
# (n_cooper_either - n_mc_either is NOT the same as
# length(setdiff(cooper_ids, mc_ids)) whenever mc finds papers cooper
# didn't -- it silently nets the two directions against each other).
cooper_either_ids <- side_by_side |>
  filter(data_availability %in% "Yes" |
           code_archived %in% "Yes") |>
  pull(article_id)
mc_either_ids <- union(papers_with_data, papers_with_code)
n_cooper_yes_mc_no <- length(setdiff(cooper_either_ids, mc_either_ids))
n_mc_yes_cooper_no <- length(setdiff(mc_either_ids, cooper_either_ids))
n_both_yes_either <- length(intersect(cooper_either_ids, mc_either_ids))
n_either_total_disagree <- n_cooper_yes_mc_no + n_mc_yes_cooper_no

# Of the n_cooper_yes_mc_no papers (Cooper says data/code exists,
# metacheck found none), how many trace to repo_check finding no
# repository at all versus repo_check finding a real repository whose
# files data_check/code_check simply did not classify as data or code
# (an emptier, but not zero, disagreement -- the reverse of "repository
# not detected"). Both counts below are restricted to gap_ids itself
# (the n_cooper_yes_mc_no set), so they are mutually exclusive subsets
# of it and sum to at most n_cooper_yes_mc_no. A paper where Cooper's own
# data_availability/code_archived columns are both NA (e.g.
# 10_1002_2688_8319_12029) has no "Yes" judgement to disagree with at all,
# so it is correctly excluded here.
a6_cats_ids_check <- read.csv("data/review/repo_not_detected_categorization_data.csv")
a6_cats_ids_check <- unique(a6_cats_ids_check$article_id)
gap_ids <- setdiff(cooper_either_ids, mc_either_ids)
n_repo_not_found_of_gap <- sum(gap_ids %in% a6_cats_ids_check)
n_repo_found_but_no_data_or_code <- sum(
  gap_ids %in% side_by_side$article_id[side_by_side$mc_data_availability %in% TRUE]
)

# n_mc_no_repo_corpus_wide is EVERY paper repo_check found nothing for,
# out of the full 1861-paper corpus, regardless of what Cooper coded --
# this is NOT the same number as n_mc_missed_reviewed (the row count of
# repo_not_detected_categorization_data.csv, computed further below), which is
# restricted to the subset of those "found nothing" papers that were ALSO
# a disagreement with Cooper (i.e. Cooper coded that paper as having data
# or code). Every row in the categorization file is a Cooper=Yes
# disagreement -- the remaining n_mc_no_repo_corpus_wide -
# n_mc_missed_reviewed papers are cases where repo_check found nothing AND Cooper agreed there
# was nothing to find, so they were correctly never disagreements and
# never entered the categorization file at all. (This file's row count
# has changed more than once over the course of this project as further
# fixes and rechecks were applied -- re-verify this count directly
# against the CSV rather than trusting a number written here, since this
# comment itself went stale once already.)
#
# The categorization file was cleaned once (2026-09-29) to remove 38
# rows that had gone stale: they described a "repository not detected"
# state from before this session's metacheck platform fixes (4TU,
# Dataverse DOI-prefix, Dryad, Figshare, DSpace7, DataOne) were rerun,
# but repo_check now genuinely finds a repository for all 38 --
# confirmed directly against current mc_data_availability for every row
# in the file, not just the ones already known to be affected. A random
# sample of the remaining rows was independently spot-checked against
# the current repo_check$table output and found to still accurately
# describe "repo_check finds nothing" for each one.
n_mc_no_repo_corpus_wide <- sum(!side_by_side$mc_data_availability, na.rm = TRUE)

# Unique repositories reachable through metacheck's shared-API backends
# (Dataverse, DSpace 7, DSpace 6/legacy, DataOne, Figshare), each
# individually confirmed against a real host rather than guessed from a
# generic pattern, plus its dedicated single-platform integrations.
# n_dedicated_platforms counts each distinct _links()-style detector
# function in the vendored source (code/data/reproducibility_check/vendor/
# metacheck/R/archive-*.R) that recognizes and queries one specific
# platform: 4TU, AsPredicted, Dryad, FSD, GitHub, GitLab, Mendeley Data,
# OSF, ResearchBox, ReShare, and Zenodo (11 platforms). This deliberately
# excludes archive-local.R (local files, not a repository platform) and
# archive-zenodo-upload.R (an upload helper, not a detection integration),
# and excludes PsychArchives, whose host (www.psycharchives.org) is one of
# the DSpace-legacy hosts counted in n_dspace_legacy_hosts below rather
# than a separate dedicated integration.
dataverse_hosts <- union(metacheck:::.dataverse_hosts(), 
                         names(metacheck:::.dataverse_doi_prefix_hosts()))
dspace7_hosts <- metacheck:::.dspace7_hosts()
dspace_legacy_hosts <- metacheck:::.dspace_legacy_hosts()
dataone_hosts <- vapply(metacheck:::.dataone_hosts(), `[[`, "", "host")
# The Figshare table maps a DOI prefix to a host, so the hosts are its values (the
# Dataverse table above runs the other way, host to prefix, so there the names are hosts).
figshare_hosts <- union(metacheck:::.figshare_vanity_hosts(), unlist(metacheck:::.figshare_doi_prefix_hosts(), use.names = FALSE))
n_dataverse_hosts <- length(dataverse_hosts)
n_dspace7_hosts   <- length(dspace7_hosts)
n_dspace_legacy_hosts <- length(dspace_legacy_hosts)
n_dataone_hosts   <- length(dataone_hosts)
n_figshare_hosts  <- length(figshare_hosts)
n_dedicated_platforms <- 11L
# A host can be in more than one list (dspace.ut.ee is queried through both
# DSpace APIs), so the total counts each host once rather than summing the
# per-family counts.
n_unique_repos <- c(dataverse_hosts, 
                    dspace7_hosts, 
                    dspace_legacy_hosts,
                    dataone_hosts, 
                    figshare_hosts) |>
  unique() |> length() + n_dedicated_platforms

# Papers whose ONLY Cooper-coded data_archive value(s) fall outside
# metacheck's supported-platform categories (Cooper's own "Other
# repo/database" catch-all, or "Personal website"/"Supplementary
# materials", neither of which is an externally checkable repository at
# all) -- and, of those, how many metacheck's repo_check consequently
# found nothing at all for.
unsupported_labels <- c("Other repo/database", 
                        "Personal website", 
                        "Supplementary materials")
is_unsupported_only <- !is.na(side_by_side$data_archive) &
  vapply(strsplit(side_by_side$data_archive, ";"),
         function(v) all(trimws(v) %in% unsupported_labels), logical(1))
n_unsupported_repo <- sum(is_unsupported_only)
n_unsupported_repo_and_mc_missed <- sum(is_unsupported_only & !side_by_side$mc_data_availability, na.rm = TRUE)

# Three-way breakdown of those n_unsupported_repo papers by Cooper's own
# data_archive label. "Personal website" and "Supplementary materials"
# are not FAIR by this paper's own definition regardless of what
# metacheck supports (a machine cannot reliably find/access data at an
# arbitrary URL or embedded in the article's own supplementary file).
# "Other repo/database" is Cooper's catch-all label and is NOT the same
# thing as "a repository metacheck genuinely does not support" -- her
# coders used it for any repository outside their own named platform
# list, which includes real OSF/Dryad/Figshare/Zenodo deposits their
# coding scheme simply had no more specific box for, and metacheck
# supports every one of those. A subset uses more than one of these
# labels together (e.g. "Supplementary materials;Other repo/database"),
# so the two counts below do not sum to n_unsupported_repo.
split_labels <- strsplit(side_by_side$data_archive[is_unsupported_only], ";")
n_unsupported_personal_web <- sum(vapply(split_labels, function(v) "Personal website" %in% trimws(v), logical(1)))
n_unsupported_supp_mat     <- sum(vapply(split_labels, function(v) "Supplementary materials" %in% trimws(v), logical(1)))

# 03_comparing_results/repo_not_detected_categorization_data.csv (see A.6's own
# fuller loading comment below, where it is reused as a6_cats) is the
# only place in this project a repository's platform was actually
# verified one paper at a time (reading the paper's own text and, where
# relevant, querying the cited platform's own API) -- rather than
# inferred from Cooper's own coded label the way the Other-repo/database
# count above was. UNSUPPORTED_REPO is that verified count: a paper whose
# individually-checked citation genuinely names a platform metacheck has
# no backend for at all, as opposed to a real OSF/Dryad/Zenodo/Figshare
# repository that only *looked* unsupported because of how Cooper's
# coders labelled it, or a paper metacheck simply failed to detect for
# an unrelated reason (a truncated DOI, an embargo, a text-extraction
# artefact -- see A.6's own category table).
a6_cats_preview <- read.csv("data/review/repo_not_detected_categorization_data.csv", stringsAsFactors = FALSE)
a6_cats_preview <- a6_cats_preview[!duplicated(a6_cats_preview$article_id), ]
n_genuinely_unsupported <- length(unique(a6_cats_preview$article_id[a6_cats_preview$category == "UNSUPPORTED_REPO"]))

# Breakdown of the hard, repo_check-outcome-based count -- every paper
# with at least one "repository not detected" disagreement (the same
# population the table of reasons below summarises; a6_cats_preview here is the
# identical CSV that table's own a6_cats loads later, read once more here so
# this earlier reconciling paragraph has it available). This replaces an
# earlier version of this paragraph that was anchored to
# n_unsupported_repo (a softer count built only from Cooper's own
# data_archive label text -- a paper counted if EVERY label she gave it
# was "Other repo/database"/"Personal website"/"Supplementary
# materials" -- which said nothing about what repo_check actually did
# and, being label-based rather than outcome-based, never summed to
# that table's own total). nrow(a6_cats_preview) IS its total.
n_mc_missed_reviewed <- nrow(a6_cats_preview)
n_mc_missed_unsupported_repo <- sum(a6_cats_preview$category == "UNSUPPORTED_REPO")
n_mc_missed_genbank <- sum(a6_cats_preview$category == "NCBI_GENBANK")
n_mc_missed_supported_bug <- sum(a6_cats_preview$category %in% c("SUPPORTED_PLATFORM_BUG", "SUPPORTED_PLATFORM_MISSED"))
n_mc_missed_other_named <- n_mc_missed_reviewed - n_mc_missed_unsupported_repo - n_mc_missed_genbank - n_mc_missed_supported_bug