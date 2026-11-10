# Shared helpers used by several pipeline targets.

# Cooper et al.'s raw CSV (BES-data-code-hackathon-cleaned) contains a
# handful of byte sequences that are not valid UTF-8 (a garbled accented
# author name or en-dash in a few free-text cells). read.csv()'s default
# fileEncoding reads these as a few extra garbled characters rather than
# failing outright, but the resulting string is not valid UTF-8, and any
# later code that calls nchar()/View() on it throws "invalid multibyte
# string". Forcing fileEncoding = "UTF-8" is not a safe fix instead --
# R's scan()-based reader aborts entirely at the first invalid byte. This
# repairs only the specific invalid byte sequences, in place, leaving
# every valid character untouched.
.fix_invalid_utf8 <- function(df) {
  fix_col <- function(x) {
    if (!is.character(x)) return(x)
    bad <- !validEnc(x)
    if (any(bad)) x[bad] <- iconv(x[bad], from = "UTF-8", to = "UTF-8", sub = "byte")
    x
  }
  df[] <- lapply(df, fix_col)
  df
}

# Add missing columns to a data frame with the right empty type, so
# downstream select()/bind_rows()/filter() calls don't fail with
# "undefined columns selected" when a batch produced zero rows for a
# given module, or NULL outright.
.ensure_cols <- function(df, cols) {
  if (is.null(df)) df <- as.data.frame(cols)
  for (nm in names(cols)) {
    if (!nm %in% names(df)) df[[nm]] <- cols[[nm]][0][seq_len(nrow(df))]
  }
  df
}
