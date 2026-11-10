# Exploratory analysis: does Metacheck's automated, mechanical comment
# COUNT (comment_lines / code_lines / percentage_comment, from
# code_check()'s per-file table) correlate with Cooper et al.'s
# human-judged comment QUALITY rating (code_annotation_scale, 1-10)?
#
# This is a genuinely different question from the rest of this
# manuscript's comparison: every other construct asks "did Metacheck
# detect the same THING a human coder detected" (a repository, a
# license, a download). This asks whether a cheap, fully automatable
# proxy (how much of the code is comments) tracks a construct Metacheck
# cannot compute at all (whether those comments are actually helpful).

compute_comment_quality_correlation <- function(res_code_check, cooper_csv_file, sample_csv_file) {
  # Same reader as every other target: without its DOI correction, two
  # Cooper rows map onto each of 4 article_ids.
  cooper_raw <- read_cooper_coded(cooper_csv_file, sample_csv_file)

  cooper_annot <- data.frame(
    article_id = cooper_raw$article_id,
    code_annotation_scale = suppressWarnings(as.numeric(cooper_raw$code_annotation_scale)),
    stringsAsFactors = FALSE
  )
  cooper_annot <- cooper_annot[!is.na(cooper_annot$article_id), ]

  cf <- res_code_check$table

  stopifnot(all(c("paper_id", "comment_lines", "code_lines", "percentage_comment") %in% names(cf)))

  has_stats <- !is.na(cf$comment_lines) & !is.na(cf$code_lines)

  agg <- aggregate(
    cbind(comment_lines, code_lines) ~ paper_id,
    data = cf[has_stats, ],
    FUN = sum
  )
  # Weighted (by total code lines), not an average of per-file percentages --
  # a 500-line file and a 5-line file should not count equally toward "how
  # commented is this paper's code overall".
  agg$mc_percentage_comment <- agg$comment_lines / (agg$comment_lines + agg$code_lines)
  names(agg)[names(agg) == "comment_lines"] <- "mc_comment_lines"
  names(agg)[names(agg) == "code_lines"]    <- "mc_code_lines"

  # Files-analysed count per paper (all code_check rows, not just those
  # with stats) -- useful context: a paper with 1 tiny file is a much
  # noisier comment-percentage estimate than one with 20.
  n_files <- aggregate(file_name ~ paper_id, data = cf, FUN = length)
  names(n_files)[2] <- "mc_code_files_n"
  agg <- merge(agg, n_files, by = "paper_id", all.x = TRUE)

  joined <- merge(cooper_annot, agg, by.x = "article_id", by.y = "paper_id", all = FALSE)
  both_coded <- joined[!is.na(joined$code_annotation_scale) & !is.na(joined$mc_percentage_comment), ]

  cor_pearson  <- cor.test(both_coded$code_annotation_scale, both_coded$mc_percentage_comment, method = "pearson")
  cor_spearman <- cor.test(both_coded$code_annotation_scale, both_coded$mc_percentage_comment, method = "spearman")
  cor_lines    <- cor.test(both_coded$code_annotation_scale, both_coded$mc_comment_lines, method = "spearman")
  cor_codelines_control <- cor.test(both_coded$mc_code_lines, both_coded$mc_percentage_comment, method = "spearman")

  list(
    n_cooper_coded = sum(!is.na(cooper_annot$code_annotation_scale)),
    n_mc_has_stats = nrow(agg),
    n_both = nrow(both_coded),
    pearson_r = unname(cor_pearson$estimate),
    pearson_ci = cor_pearson$conf.int,
    pearson_p = cor_pearson$p.value,
    spearman_rho = unname(cor_spearman$estimate),
    spearman_p = cor_spearman$p.value,
    spearman_rho_lines = unname(cor_lines$estimate),
    spearman_p_lines = cor_lines$p.value,
    codelines_vs_pct_rho = unname(cor_codelines_control$estimate),
    codelines_vs_pct_p = cor_codelines_control$p.value,
    data = both_coded
  )
}
