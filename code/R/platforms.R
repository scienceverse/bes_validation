# Platform labels for repository URLs, and the set comparison of Cooper et
# al.'s coded platform(s) with Metacheck's. Shared by the comparison data,
# the statistics, the disagreement worklist and the uncoded-papers summary,
# so all four label and compare platforms the same way.

# Platform-name matching: matches BOTH the platform's own domain and its DOI
# prefix reached via doi.org (repo_url stores the doi.org form far more often
# than the platform's own domain). Dryad and Figshare match every alternate
# DOI prefix and institutional host metacheck itself recognises
# (.dryad_doi_prefixes(), .figshare_vanity_hosts(),
# .figshare_doi_prefix_hosts()), read from metacheck's own lists so this
# classification cannot drift out of sync with what it recognises.
repo_url_platform <- function(url) {
  dryad_url_regex <- paste0(
    "datadryad\\.org|(?:",
    paste(gsub("\\.", "\\\\.", metacheck:::.dryad_doi_prefixes()), collapse = "|"),
    ")/"
  )
  figshare_url_regex <- paste0(
    "figshare\\.com|10\\.6084/m9\\.figshare|(?:",
    paste(gsub("\\.", "\\\\.", metacheck:::.figshare_vanity_hosts()), collapse = "|"),
    ")|(?:",
    paste(gsub("\\.", "\\\\.", names(metacheck:::.figshare_doi_prefix_hosts())), collapse = "|"),
    ")/"
  )
  dplyr::case_when(
    is.na(url) ~ NA_character_,
    grepl("osf\\.io", url, ignore.case = TRUE) ~ "OSF",
    grepl("github\\.com|gitlab\\.com", url, ignore.case = TRUE) ~ "GitHub, GitLab, Codeberg or similar platform",
    grepl("zenodo\\.org|10\\.5281/zenodo", url, ignore.case = TRUE) ~ "Zenodo",
    grepl(dryad_url_regex, url, ignore.case = TRUE, perl = TRUE) ~ "Dryad",
    grepl(figshare_url_regex, url, ignore.case = TRUE, perl = TRUE) ~ "Figshare",
    TRUE ~ "Other repo/database"
  )
}

# TRUE for an address of a university or organisation repository that runs on
# Figshare's software (an institutional Figshare): a *.figshare.com subdomain
# other than figshare.com itself, one of metacheck's institutional hosts, or a
# DOI prefix metacheck maps to one. repo_url_platform() labels these
# "Figshare"; Cooper et al.'s coders labelled them "Figshare" in some papers and
# an institutional repository ("Other repo/database") in others, so the
# comparison accepts either label for them (align_institutional_figshare()).
is_institutional_figshare <- function(url) {
  u <- tolower(url)
  hosts <- unique(c(metacheck:::.figshare_vanity_hosts(), unlist(metacheck:::.figshare_doi_prefix_hosts())))
  prefixes <- names(metacheck:::.figshare_doi_prefix_hosts())
  !is.na(u) & (
    (grepl("[a-z0-9-]+\\.figshare\\.com", u) & !grepl("(^|//)(www\\.)?figshare\\.com", u)) |
      grepl(paste(gsub(".", "\\.", hosts, fixed = TRUE), collapse = "|"), u) |
      grepl(paste0("(", paste(gsub(".", "\\.", prefixes, fixed = TRUE), collapse = "|"), ")/"), u))
}

# Metacheck's platform set b, aligned with Cooper et al.'s set a: when every
# Figshare deposit Metacheck found is an institutional Figshare (inst TRUE) and
# Cooper et al. coded "Other repo/database" but not "Figshare", the deposit is
# counted under Cooper et al.'s label.
align_institutional_figshare <- function(a, b, inst) {
  if (isTRUE(inst) && "figshare" %in% b && !"figshare" %in% a && "other repo/database" %in% a) {
    b <- unique(replace(b, b == "figshare", "other repo/database"))
  }
  b
}

# One side's ";"-joined platform answer as a set on Cooper et al.'s label
# scale. Cooper's coding form listed OSF separately, but their cleaned data
# merge it into "Other repo/database", so metacheck's "OSF" is mapped to
# that label;
# Cooper's "CRAN" (code only) is likewise an "Other repo/database" in
# metacheck's scheme. Cooper's "Supplementary materials" and "Personal
# website" are not repositories metacheck checks at all, so they are dropped.
platform_set <- function(x, side) {
  if (is.null(x) || is.na(x)) return(character(0))
  s <- unique(tolower(trimws(strsplit(x, ";")[[1]])))
  s <- s[!is.na(s) & nzchar(s)]
  other <- "other repo/database"
  if (side == "mc") s[s == "osf"] <- other
  if (side == "cooper") {
    s[s == "cran"] <- other
    s <- setdiff(s, c("supplementary materials", "personal website"))
  }
  unique(s)
}

# Per paper: NA when either side has no platform left (not comparable), else
# exactly one of
#   exact    - the same set of platforms
#   mc_fewer - metacheck's platforms are a strict subset of Cooper's
#   mc_more  - Cooper's platforms are a strict subset of metacheck's
#   partial  - some shared platforms, but each side has one the other lacks
#   none     - no shared platform
# mv_inst: per paper, TRUE when every Figshare deposit Metacheck found is an
# institutional Figshare (see align_institutional_figshare()).
platform_set_category <- function(cv, mv, mv_inst = rep(FALSE, length(mv))) {
  cs <- lapply(cv, platform_set, side = "cooper")
  ms <- mapply(align_institutional_figshare, cs, lapply(mv, platform_set, side = "mc"),
               mv_inst, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  mapply(function(a, b) {
    if (length(a) == 0 || length(b) == 0) NA_character_
    else if (setequal(a, b)) "exact"
    else if (all(b %in% a)) "mc_fewer"
    else if (all(a %in% b)) "mc_more"
    else if (any(a %in% b)) "partial"
    else "none"
  }, cs, ms, USE.NAMES = FALSE)
}

# n_agree/pct_agree use the exact match; any_overlap keeps the more lenient
# "at least one shared platform" rule for comparison.
platform_set_comparison <- function(cv, mv, label, mv_inst = rep(FALSE, length(mv))) {
  cs <- lapply(cv, platform_set, side = "cooper")
  ms <- mapply(align_institutional_figshare, cs, lapply(mv, platform_set, side = "mc"),
               mv_inst, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  cooper_raw_coded <- !is.na(cv) & nzchar(cv)
  cat_all <- platform_set_category(cv, mv, mv_inst)
  both <- !is.na(cat_all)
  cat_of <- cat_all[both]
  categories <- table(factor(cat_of, levels = c("exact", "mc_fewer", "mc_more", "partial", "none")))
  # Papers Cooper coded as BOTH GitHub and Zenodo (the usual "development
  # repository plus archived release" pair): which of the two metacheck
  # found. One value per input row, NA unless comparable and coded both.
  gh <- "github, gitlab, codeberg or similar platform"
  github_zenodo_found <- mapply(function(a, b, ok) {
    if (!ok || !all(c(gh, "zenodo") %in% a)) return(NA_character_)
    if (all(c(gh, "zenodo") %in% b)) "both"
    else if (gh %in% b) "github_only"
    else if ("zenodo" %in% b) "zenodo_only"
    else "neither"
  }, cs, ms, both, USE.NAMES = FALSE)
  github_zenodo_tab <- table(factor(github_zenodo_found,
                                    levels = c("both", "github_only", "zenodo_only", "neither")))
  list(
    label = label,
    n_cooper_coded = sum(cooper_raw_coded),
    n_mc_coded = sum(lengths(ms) > 0),
    n_cooper_only_non_repository = sum(cooper_raw_coded & lengths(cs) == 0),
    n_cooper_multi = sum(lengths(cs[both]) > 1),
    n_mc_multi = sum(lengths(ms[both]) > 1),
    n_both_coded = sum(both),
    categories = categories,
    n_agree = unname(categories["exact"]),
    n_disagree = sum(both) - unname(categories["exact"]),
    pct_agree = unname(categories["exact"]) / sum(both) * 100,
    n_any_overlap = sum(cat_of != "none"),
    pct_any_overlap = mean(cat_of != "none") * 100,
    category = cat_all,
    n_cooper_github_zenodo = sum(github_zenodo_tab),
    github_zenodo = github_zenodo_tab,
    github_zenodo_found = github_zenodo_found
  )
}
