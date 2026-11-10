# How much repositories of someone other than the paper's authors change
# the comparison with Cooper et al.
#
# repo_check follows every GitHub/GitLab address in a paper's text, so it
# also lists the repositories of software or data collections the paper
# merely USED (an R package, a demultiplexing tool, a text corpus). Files
# in those repositories then count as the paper's own data or code.
# data/review/github_repository_ownership_review_data.csv records,
# for every GitHub/GitLab repository repo_check listed, whether its owner
# is an author (or the authors' lab, project or organisation account),
# judged against the paper's Crossref author list. This recomputes each
# affected Metacheck answer with those third-party repositories removed,
# and reports which answers they alone produced.

summarize_third_party_repositories <- function(master_comparison, res_repo_check,
                                               res_data_check, res_code_check,
                                               ownership_csv) {
  own <- read.csv(ownership_csv, stringsAsFactors = FALSE)

  files <- res_repo_check$table
  data_files <- res_data_check$structure
  data_files <- data_files[data_files$data_type %in% "data", ]
  code_files <- res_code_check$table
  tp <- own[!own$owner_is_author, c("article_id", "repo_url")]
  is_tp <- function(pid, url) paste(pid, url) %in% paste(tp$article_id, tp$repo_url)
  platforms <- function(urls) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    if (length(urls) == 0) NA_character_ else paste(sort(unique(repo_url_platform(urls))), collapse = ";")
  }

  corpus <- own[!own$owner_is_author & grepl("text corpus", own$why_not_authors, fixed = TRUE), ]
  links <- unique(files[, c("paper_id", "repo_url")])
  reviewed <- paste(links$paper_id, links$repo_url) %in% paste(own$article_id, own$repo_url)

  papers <- unique(tp$article_id)
  per_paper <- do.call(rbind, lapply(papers, function(p) {
    f <- files[files$paper_id == p, ]
    d <- data_files[data_files$paper_id == p, ]
    k <- code_files[code_files$paper_id == p, ]
    i <- match(p, master_comparison$article_id)
    data.frame(
      article_id = p,
      cooper_data_availability = master_comparison$data_availability[i],
      mc_data_availability = master_comparison$mc_data_availability[i],
      mc_data_availability_without = any(!is_tp(p, f$repo_url)),
      cooper_code_archived = master_comparison$code_archived[i],
      mc_code_archived = master_comparison$mc_code_archived[i],
      mc_code_archived_without = any(!is_tp(p, k$repo_url)),
      cooper_data_archive = master_comparison$data_archive[i],
      mc_data_archive = master_comparison$mc_data_archive[i],
      mc_data_archive_without = platforms(d$repo_url[!is_tp(p, d$repo_url)]),
      cooper_code_archive = master_comparison$code_archive[i],
      mc_code_archive = master_comparison$mc_code_archive[i],
      mc_code_archive_without = platforms(k$repo_url[!is_tp(p, k$repo_url)]),
      stringsAsFactors = FALSE)
  }))
  for (col in c("data_archive", "code_archive")) {
    inst <- master_comparison[[paste0("mc_", col, "_institutional_figshare")]][
      match(per_paper$article_id, master_comparison$article_id)] %in% TRUE
    per_paper[[paste0(col, "_category")]] <- platform_set_category(
      per_paper[[paste0("cooper_", col)]], per_paper[[paste0("mc_", col)]], inst)
    per_paper[[paste0(col, "_category_without")]] <- platform_set_category(
      per_paper[[paste0("cooper_", col)]], per_paper[[paste0("mc_", col, "_without")]], inst)
  }

  only_tp <- function(with, without) with %in% TRUE & !without
  data_only <- only_tp(per_paper$mc_data_availability, per_paper$mc_data_availability_without)
  code_only <- only_tp(per_paper$mc_code_archived, per_paper$mc_code_archived_without)
  changed <- function(col) {
    a <- per_paper[[paste0(col, "_category")]]
    b <- per_paper[[paste0(col, "_category_without")]]
    !is.na(a) & (is.na(b) | a != b)
  }

  list(
    per_paper = per_paper,
    n_github_repos = nrow(own),
    n_github_papers = length(unique(own$article_id)),
    n_third_party_repos = nrow(tp),
    n_third_party_papers = length(papers),
    n_third_party_in_statement = sum(!own$owner_is_author & own$named_in_cooper_statement),
    n_corpus_papers = length(unique(corpus$article_id)),
    # Links to repositories of any platform, and those the review did not judge.
    n_links = nrow(links),
    n_links_not_reviewed = sum(!reviewed),
    # Metacheck's "data shared" / "code archived" answer rests ONLY on
    # third-party repositories, split by Cooper et al.'s answer.
    data_availability_only_third_party = table(factor(per_paper$cooper_data_availability[data_only],
                                                      levels = c("Yes", "No"))),
    code_archived_only_third_party = table(factor(per_paper$cooper_code_archived[code_only],
                                                  levels = c("Yes", "No")), useNA = "ifany"),
    n_code_archived_only_third_party = sum(code_only),
    # Platform comparisons whose category the third-party repositories changed.
    data_archive_changed = table(per_paper$data_archive_category[changed("data_archive")],
                                 per_paper$data_archive_category_without[changed("data_archive")],
                                 useNA = "ifany"),
    code_archive_changed = table(per_paper$code_archive_category[changed("code_archive")],
                                 per_paper$code_archive_category_without[changed("code_archive")],
                                 useNA = "ifany"),
    n_data_archive_changed = sum(changed("data_archive")),
    n_code_archive_changed = sum(changed("code_archive")),
    n_data_archive_mc_more_to_exact = sum(per_paper$data_archive_category %in% "mc_more" &
                                            per_paper$data_archive_category_without %in% "exact"),
    n_code_archive_mc_more_to_exact = sum(per_paper$code_archive_category %in% "mc_more" &
                                            per_paper$code_archive_category_without %in% "exact"),
    n_code_archive_exact_only_third_party = sum(per_paper$code_archive_category %in% "exact" &
                                                  is.na(per_paper$code_archive_category_without))
  )
}
