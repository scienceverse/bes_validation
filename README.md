# Validating Metacheck against the BES data and code archiving corpus

This repository holds the results of running [metacheck](https://github.com/scienceverse/metacheck)
on 1861 papers from British Ecological Society journals, the comparison with the human
coding by Cooper et al., and the code that creates the manuscript from them.

The repository follows the
[guide on machine-readable FAIR data and code](https://github.com/scienceverse/ms_FAIR_data_and_code).
`ro-crate-metadata.json` lists every file for machines.

## What is where

```
README.md                     this file
ro-crate-metadata.json        every file, for machines (made by code/build_documentation.R)
_targets.R                    the pipeline from the results to the manuscript
_quarto.yml                   settings for rendering the manuscript
renv.lock                     the R packages, at the versions used
Dockerfile                    a Docker image with R, RStudio, the packages, Quarto and LaTeX
docker/
  check_reproduction.R        rebuilds everything and compares the manuscript's numbers with the published PDF
code/
  run_metacheck.R             runs metacheck on all papers; creates data/metacheck_results.rds
  match_reported_results.R    creates data/match_reported_results.rds
  splice_reproducibility.R    puts the reproducibility_check results of a later run into data/metacheck_results.rds
  build_documentation.R       creates the codebooks and ro-crate-metadata.json
  compare_earlier_results.R   records what changed since the earlier repository, for appendix A.11
  modules/oddpub.R            the ODDPub module, run in the chain of modules
  R/                          the functions of the pipeline
data/
  metacheck_results.rds       all tables of all papers from the final run
  match_reported_results.rds  reported results compared with the output of each paper's code
  cooper_coded_data.csv       the coding by Cooper et al.
  papers_data.csv             the papers, with this project's identifier for each
  reproducibility_failures_scripts_data.csv  why each script did not run through (made by the pipeline)
  reproducibility_failures_papers_data.csv   the same per paper, with who could make the scripts run
  review/                     reviews of metacheck's output, done by hand
  *_codebook.csv              the codebook of the data file with the same name
documentation/
  manuscript.qmd              the manuscript, with all its calculations
  manuscript.pdf              the rendered manuscript
  appendix.qmd                the appendix: every change to metacheck during the project
  appendix.pdf                the rendered appendix
  manuscript_verification.qmd  the source of every number and a check of every factual statement
  manuscript_verification.pdf  the rendered verification
  session_info_documentation.txt  the software used for the last render
  cooper_supplement_documentation.pdf  Cooper et al.'s coding protocol
  _extensions/                the apaquarto extension (version 7.0.2) for APA style
tests/                        tests of the pipeline
```

`data/bes.rds` holds the full text of the papers. It is copyrighted, so it is not shared.
It is hosted on the private repository `scienceverse/papers_private`, release `bes-2026-08-29`.

## From the results to the manuscript

This needs no internet access, no Docker and not the papers' full text. From the root of
the repository:

```r
renv::restore()
targets::tar_make()
```

`tar_make()` runs only what is out of date, and renders the manuscript to
`documentation/manuscript.pdf` and the appendix to `documentation/appendix.pdf`.
This needs R 4.5.3, Quarto 1.9.37 (which includes Typst, for the manuscript) and LaTeX
(for the appendix and the verification document).

### With Docker

The `Dockerfile` builds an image with all of this: R 4.5.3, RStudio Server, the packages
of `renv.lock`, Quarto 1.9.37, LaTeX and this repository. GitHub builds and publishes it
on every change to `main` (`.github/workflows/docker-image.yml`), as
`ghcr.io/scienceverse/bes_validation` (`latest`, a version such as `1.0`, or the short
code of a commit). To download it instead of building it, replace `bes_validation` by
`ghcr.io/scienceverse/bes_validation:latest` in the commands below. To build it yourself,
from the root of the repository:

```
docker build -t bes_validation .
```

The build rebuilds the whole paper once, from the data, and stops with an error if any
number in the rebuilt manuscript differs from the published `documentation/manuscript.pdf`.
It takes about 15 minutes; the image is about 6 GB. To work in RStudio, start the image
and open <http://localhost:8787> in a browser (user name `rstudio`):

```
docker run --rm -p 8787:8787 -e PASSWORD=choose_a_password bes_validation
```

The project is in `~/bes_validation`; `targets::tar_make()` reruns the pipeline there.
To repeat the check without RStudio, which also works without internet access:

```
docker run --rm --network none bes_validation Rscript docker/check_reproduction.R
```

It deletes every stored result, rebuilds the pipeline, the manuscript, the appendix and the
verification, and reports whether every number in the rebuilt manuscript (627 on
10 November 2026) is identical to the published version. The image has no Times New
Roman, a font that may not be redistributed, so its PDF uses another font and its line and
page breaks can differ; the check therefore compares the numbers, not the layout.

## From the papers to the results

This takes many hours. It needs Docker, API keys in `.Renviron`, the private `data/bes.rds`,
and preferably the downloads saved by an earlier run.

```bash
Rscript code/run_metacheck.R
Rscript code/match_reported_results.R
```

For this project, 22 papers were then run again with the fixes for problems found during the run,
listed in `documentation/rerun_paper_ids_documentation.txt`; `run_info$rerun` in the result file
records which, and the version of the earlier run:

```bash
Rscript code/run_metacheck.R --ids documentation/rerun_paper_ids_documentation.txt \
  --base D:/bes_validation_run/final --out D:/bes_validation_run/final_rerun
```

The first command runs one chain of modules for each paper:
`repo_check`, `data_check`, `code_check`, `open_practices`, ODDPub and `reproducibility_check`.
Every paper is saved as it finishes, so running the same command again after an interruption
continues with the unfinished papers.

`code/match_reported_results.R` compares each paper's reported statistics with the statistics
its code printed. `reproducibility_check` does this itself, but in the version used for this
run that step failed without a message (see the issues document), so this script does it on
the saved output, without running any paper's code again.

### Fixed versions

- metacheck 0.4.2.1, commit e62da2eab6a45aebc993336c21e176b5cebe569a, for the run:
  `remotes::install_github("scienceverse/metacheck@e62da2e")`
- metacheck 0.4.2.1, commit f7949c05f9bf6dfa277be234b8023bf7021172c6, for the 22 papers run again,
  `code/match_reported_results.R` and the analysis:
  `remotes::install_github("scienceverse/metacheck@f7949c0")`
- oddpub 7.3.0
- Docker images, recorded by digest in `run_info` inside the result file:
  - `ghcr.io/scienceverse/metacheck_r_large@sha256:2937ccf9d2127cd1e6095b2c5c8dd1980402ab2a95b00954bb9df88d5fbb0b50`
  - `ghcr.io/scienceverse/metacheck_py_small@sha256:18a0c9a47e750995e97c2e8740a1b573ee57a6cd8ef37a77970bceebf2543990`

### What the run reuses

Repository files and listings come from earlier runs. Listings were fetched between
September and October 2026 and are not refreshed. Before this run:

- Dryad listings saved before metacheck's paging fix were given the fields
  `storage_size`, `files_total` and `files_incomplete`, computed from the listing,
  for deposits with fewer than 20 files. For 35 deposits the result matched a new
  request to Dryad field by field.
- 20 OSF listings saved as "unfound" during the OSF outage of 17 October 2026 were
  replaced by their September versions; the OSF answered all 20 normally.

## Reviews done by hand

The files in `data/review/` explain metacheck's behaviour on specific papers, so they hold
only for the version they were written for. Each row records that version. The pipeline
targets `audit_staleness` and `worklist_staleness` list what was written for another version:
`targets::tar_read(worklist_staleness)`.

## Tests

```bash
Rscript tests/testthat.R
```
