# Record what changed between the last results of the earlier repository and the final run,
# and why, in four files in data/review/ that appendix A.11 of the manuscript reads:
#   result_changes_data.csv               every value that changed in a compared column
#   result_change_causes_data.csv         the cause of each kind of change
#   result_change_percentages_data.csv    the earlier agreement percentages
#   result_change_other_columns_data.csv  changes in columns not compared with Cooper et al.
#
# NEEDS: the pipeline store of the earlier repository, _targets/objects/ as of 2026-10-18, and
# this repository's store. The earlier repository is not part of this one, so the files above
# are saved here and the pipeline reads them. The causes were traced by hand, from the listings,
# metacheck's commits and its NEWS.md, and are written below.
#
# Usage, from the repository root:
#   Rscript code/compare_earlier_results.R --earlier "path/to/bes-validation-manuscript"

args <- commandArgs(TRUE)
earlier <- args[match("--earlier", args) + 1]
store <- file.path(earlier, "_targets/objects")
stopifnot(dir.exists(store))

cause_of_paper <- c(
  "10_1111_2041_210x_14062" = "FILE_TYPE_TABLE", "10_1111_2041_210x_13709" = "FILE_TYPE_TABLE",
  "10_1111_2041_210x_13669" = "FILE_TYPE_TABLE", "10_1002_2688_8319_12310" = "FILE_TYPE_TABLE",
  "10_1111_2041_210x_13787" = "FILE_TYPE_TABLE", "10_1111_2041_210x_13437" = "FILE_TYPE_TABLE",
  "10_1111_2041_210x_13114" = "ADDRESS_PUNCTUATION", "10_1002_pan3_10551" = "ADDRESS_PUNCTUATION",
  "10_1111_2041_210x_14387" = "DATAONE_IDENTIFIERS", "10_1111_1365_2656_13504" = "DATAONE_IDENTIFIERS",
  "10_1111_1365_2435_14022" = "DRYAD_SHARING_LINK",
  "10_1111_2041_210x_14427" = "ROOT_R_PACKAGE_KEPT", "10_1111_2041_210x_12774" = "ROOT_R_PACKAGE_KEPT",
  "10_1111_1365_2745_14340" = "ROOT_R_PACKAGE_KEPT",
  "10_1111_2041_210x_13748" = "COMPRESSED_TAR_UNPACKED", "10_1111_1365_2435_14305" = "COMPRESSED_TAR_UNPACKED",
  "10_1111_2041_210x_13120" = "COMPRESSED_TAR_UNPACKED",
  "10_1111_1365_2435_14431" = "OSF_LISTING", "10_1002_2688_8319_12402" = "OSF_LICENSE",
  "10_1111_1365_2664_14751" = "DSPACE_LOOKUP", "10_1111_2041_210x_13308" = "DSPACE_LOOKUP",
  "10_1111_1365_2656_13159" = "DSPACE_LOOKUP",
  "10_1111_1365_2435_14173" = "DUPLICATE_OF_OSF_LISTING",
  "10_1111_1365_2656_12978" = "NOW_DOWNLOADED", "10_1111_1365_2656_13222" = "NOW_DOWNLOADED",
  "10_1111_1365_2435_13921" = "NOW_DOWNLOADED", "10_1111_1365_2745_14312" = "NOW_DOWNLOADED",
  "10_1111_1365_2435_13192" = "NOW_DOWNLOADED", "10_1111_1365_2435_14492" = "NOW_DOWNLOADED",
  "10_1111_1365_2664_14383" = "NOT_DOWNLOADED_ON_PURPOSE"
)

causes <- data.frame(rbind(
  c("FILE_TYPE_TABLE", "d52b3fd9; 9509af14",
    paste("metacheck's table of file types (file_types.rda) changed: .json counts as data only (it was code and data),",
          ".js, .ts and .tsx as code only, .html as a web page and no longer as code; shapefile parts and .ply count as data.",
          "The earlier results of these papers were made before this change."),
    paste("Pages of a documentation website no longer count as code, and JavaScript no longer counts as data. JSON files",
          "count as data, also configuration files such as package.json, so software repositories can now be listed as holding data.")),
  c("ADDRESS_PUNCTUATION", "5c4feb1e",
    "repo_check removes a final . , ; or : from a repository address. With the full stop that ended a sentence the address led nowhere, and the deposit was listed without files.",
    "The deposit is found and its files are listed."),
  c("DATAONE_IDENTIFIERS", "5c4feb1e",
    "A DataONE file identifier with slashes is kept whole, it was cut at its last slash; a file with no usable identifier is found by name in the node's search, which works again on KNB.",
    "Files that failed to download are fetched and read."),
  c("DRYAD_SHARING_LINK", "f7826893",
    "repo_check follows a Dryad sharing link (datadryad.org/stash/share/...), the link made for reviewers, to the dataset it leads to. It carries no DOI, so the paper had no repository.",
    "The deposit is found, so data, license, download and README all change."),
  c("ROOT_R_PACKAGE_KEPT", "9509af14",
    "repo_check keeps the files of an R package at a repository's root when the only files outside it configure services such as Travis, codecov or GitHub Actions. These files have the extension .yml, which counts as code, so the package looked like a bundled dependency and its files were removed.",
    "The GitHub repository's R files are listed, so GitHub joins the platforms for code, and for data when the package holds data files."),
  c("COMPRESSED_TAR_UNPACKED", "bd44cc56; not separated from other changes to archives",
    "data_check unpacks a compressed tar file (.tar.gz) inside a deposit and classifies the files in it, also when it lies inside a zip file. The earlier results list it as one file of unknown type.",
    "R files and data files inside the archive are found."),
  c("OSF_LISTING", "d52b3fd9",
    "osf_info() lists a project's whole tree of components and folders. The earlier listing of this project held 96 files, the final listing 131.",
    "Scripts and a README in folders that were not listed are found."),
  c("OSF_LICENSE", "f7826893",
    "With osf_license = TRUE, repo_check reads the license of an OSF project through its registration when the project address does not answer. The final run sets this argument.",
    "The license is found."),
  c("DSPACE_LOOKUP", "d52b3fd9; 3bfb2ed7; not separated",
    "DSpace repositories are recognised from doi.org's handle service and by host name, and a repository's former address is recognised.",
    "The deposit, or its license, is found."),
  c("DUPLICATE_OF_OSF_LISTING", "d52b3fd9; abc2e2a8",
    "An OSF project with a linked GitHub repository is listed through GitHub, so its rows have the same address and path as the GitHub repository's own rows. repo_check keeps each file once, under OSF, which comes first.",
    "No file is lost, but GitHub leaves the list of platforms although the paper names it."),
  c("NOW_DOWNLOADED", "not established",
    "The final run holds a saved copy of these files, and the earlier results do not. For a .txt file, data_check reads the content of a saved copy and calls it data when it is a table (txt_classify_content(), since e1b5aa17, 2026-08-12), which explains the three papers with .txt data. Why the earlier runs have no copy was not established; no change in metacheck was found that explains it.",
    "Data files are read, and data_download becomes TRUE."),
  c("NOT_DOWNLOADED_ON_PURPOSE", "none; setting of this run",
    "The final run does not download the SAPFLUXNET deposit of 10.1111/1365-2664.14383, on the authors' decision, because of its size; code/run_metacheck.R lists the paper in no_download.",
    "data_download is missing for this paper instead of TRUE.")
))
names(causes) <- c("cause", "metacheck_commit", "mechanism", "consequence")

compared <- c("data_availability", "data_archive", "data_license", "data_download",
              "code_archived", "code_archive", "code_download", "any_readme")

old <- readRDS(file.path(store, "master_comparison"))
new <- targets::tar_read(master_comparison)
old <- old[match(new$article_id, old$article_id), ]

changed <- function(col) {
  a <- as.character(old[[col]]); b <- as.character(new[[col]])
  d <- !(a == b | (is.na(a) & is.na(b))); d[is.na(d)] <- TRUE
  data.frame(article_id = new$article_id[d], earlier_value = a[d], final_value = b[d])
}
changes <- do.call(rbind, lapply(compared, function(cl) cbind(column = cl, changed(paste0("mc_", cl)))))
changes <- changes[, c("article_id", "column", "earlier_value", "final_value")]
changes <- changes[order(changes$article_id, match(changes$column, compared)), ]
missing_cause <- setdiff(changes$article_id, names(cause_of_paper))
if (length(missing_cause)) stop("No cause recorded for: ", paste(missing_cause, collapse = ", "))
if (length(setdiff(names(cause_of_paper), changes$article_id))) stop("A cause is recorded for a paper that did not change")
changes$cause <- unname(cause_of_paper[changes$article_id])

old_stats <- readRDS(file.path(store, "comparison_statistics"))$results
percentages <- data.frame(
  column = compared,
  earlier_pct_agree = vapply(compared, function(cl) old_stats[[cl]]$pct_agree, 0),
  earlier_n_both_coded = vapply(compared, function(cl) old_stats[[cl]]$n_both_coded, 0)
)

other <- data.frame(rbind(
  c("mc_on_request", "5deacb37", "Every paper now gets a row in open_practices, so a paper with no flagged sentence reads FALSE instead of missing. No answer changed."),
  c("mc_code_open", "5deacb37", "Only a capital R counts as a word for code, so a statistic such as r(68) = .58 no longer makes a sentence a code statement."),
  c("mc_data_open", "not established", "The statements of these papers offer data only on request, and they no longer count as open data."),
  c("mc_data_format", "e985e2af; 358074ae; d52b3fd9", "Operating system files such as .DS_Store and __MACOSX folders are left out, and the file types of cause FILE_TYPE_TABLE apply."),
  c("mc_data_doi", "follows from the compared columns", "The DOI of a deposit is read only for a repository that holds data, so it changes with data_archive."),
  c("mc_code_language", "follows from the compared columns", "The languages are read from the code files found."),
  c("mc_data_README", "follows from the compared columns", "Same value as any_readme."),
  c("mc_code_README", "follows from the compared columns", "Same value as any_readme.")
))
names(other) <- c("column", "metacheck_commit", "mechanism")
other$n_papers <- vapply(other$column, function(cl) nrow(changed(cl)), 0L)
other <- other[other$n_papers > 0, c("column", "n_papers", "metacheck_commit", "mechanism")]

write_csv <- function(x, name) utils::write.csv(x, file.path("data/review", name), row.names = FALSE, fileEncoding = "UTF-8")
write_csv(changes, "result_changes_data.csv")
write_csv(causes, "result_change_causes_data.csv")
write_csv(percentages, "result_change_percentages_data.csv")
write_csv(other, "result_change_other_columns_data.csv")
cat(nrow(changes), "changed values in", length(unique(changes$article_id)), "papers.\n")
