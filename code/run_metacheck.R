# Run metacheck over the corpus of 1861 BES papers and save all results in one
# file. This is the only script that creates the results the analysis reads.
#
# Each paper goes through one chain of modules, each building on the output of
# the ones before it (see run_paper() below):
#   repo_check |> data_check |> code_check |> open_practices |> ODDPub |>
#   reproducibility_check
# ODDPub is not part of metacheck; it is a module file in code/modules/.
#
# Usage (from the repository root):
#   Rscript code/run_metacheck.R                 # the whole corpus
#   Rscript code/run_metacheck.R --first 30 --out E:/bes_validation_run/test_first30_chain
#   Rscript code/run_metacheck.R --only <paper_id> --out E:/bes_validation_run/test_one
# Optional: --workers N (papers at the same time, default 5).
#
# RERUNNING SOME PAPERS: with --ids FILE (one paper id per line) and --base DIR (the folder
# of the earlier run), only those papers run, in the --out folder, and the result file holds
# these papers from --out and all others from --base. run_info$rerun records which.
#
# NEEDS: metacheck at the version fixed in README.md, the keys in .Renviron,
# Docker running, the files downloaded earlier on E: (.metacheck_repo_cache and
# its neighbours), and the private corpus at data/bes.rds.
#
# RESUMING: every paper is saved as it finishes. After any interruption, run the
# same command again and it continues with the unfinished papers. It refuses to
# continue with a different metacheck version.
#
# FILES (on E:, outside this repository, in the --out folder):
#   papers/<id>.rds       every module's full result for one paper, without the
#                         data previews and without a copy of the paper's text
#   previews/<id>.rds     data_check's previews, which a later codebook_check needs
#   manifests/            every file's download outcome, including failures
#   logs/, timing/, stage/  one log, one table of times, and the current module
#   metacheck_results.rds the single result file: all tables of all papers
# A run of the whole corpus also copies metacheck_results.rds to data/.

args <- commandArgs(TRUE)
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) default else args[i + 1]
}
first_n <- as.integer(arg_value("--first", NA))
only_id <- arg_value("--only", NA)
n_workers <- as.integer(arg_value("--workers", 5))
# Time limits of reproducibility_check, per script and for all scripts of a paper
# together, in seconds; shorter ones give quicker test runs of a few papers.
repro_timeout <- as.integer(arg_value("--timeout", 3000))
repro_total <- as.integer(arg_value("--total-timeout", 3600))
# The time for installing a paper's packages, apart from the scripts' limits: with
# short script limits the install ran out of time, and every script after it stopped
# on a missing package (metacheck#568).
repro_install <- as.integer(arg_value("--install-timeout", 1200))
# Whether scripts may use the internet during the run (metacheck's `internet`), within
# the same file-size limits as the deposit's files. Off by default: the code is not ours.
repro_internet <- tolower(arg_value("--internet", "false")) %in% c("true", "yes", "1")
# The largest single file downloaded, in MB (metacheck's default is 100): a script
# that needs a larger file is not tried (outcome not_downloaded).
max_file_mb <- as.numeric(arg_value("--max-file-size", 300))
out_dir <- arg_value("--out", "E:/bes_validation_run/final")
ids_file <- arg_value("--ids", NA)
base_dir <- arg_value("--base", NA)

CACHE_ROOT <- "E:/"
CACHE_CHECK <- file.path(CACHE_ROOT, ".metacheck_repo_cache")
MIN_FREE_GB <- 10
# Temporary files go to E: as well: each paper unpacks archives, copies its
# repository and installs packages there, and a full C: crashed this computer.
# Each paper runs in a new R process, which takes its temporary folder from these.
# Without the slash of "E:/": "E://..." made every folder below it fail.
TEMP_ROOT <- file.path(sub("/+$", "", CACHE_ROOT), "bes_validation_run", "tmp")
dir.create(TEMP_ROOT, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = TEMP_ROOT, TMP = TEMP_ROOT, TEMP = TEMP_ROOT)

# A paper is stopped as stalled when its log, processor use and data transfer
# all stand still this long. reproducibility_check is silent while Docker runs a
# script, so it gets more time: one hour of scripts (total_timeout) plus
# installing packages.
HANG_MINUTES <- 15
HANG_MINUTES_REPRO <- 120

# metacheck downloads a Dryad archive whole (one request) only when it fits this
# budget, otherwise file by file (two requests each), which runs into Dryad's
# hourly limit. These two archives are larger than the usual budget.
BUDGET_MB <- 2000
BIG_BUDGET <- c("10_1111_2041_210x_12774" = 10000, "10_1111_2041_210x_13206" = 25000)
# Known to stall or to take many hours: last, so they do not hold up the others.
RUN_LAST <- c("10_1111_1365_2664_13374", "10_1111_2041_210x_12774",
              "10_1111_2041_210x_13206", "10_1111_1365_2664_14383")
# SAPFLUXNET (Zenodo 3971689, 3983 files, 23 GB): reading its data files would take
# over a day. Its files are listed and classified by name, and nothing is fetched or
# read. max_files_per_repo does not prevent this: for files inside an archive, only the
# download budget applies. 27 other repositories over 1000 files are read in full.
NO_DOWNLOAD <- "10_1111_1365_2664_14383"
# Pairs of papers citing the same repository never run together, so they cannot
# write the same download at once.
SAME_REPOSITORY <- list(
  c("10_1111_1365_2664_12885", "10_1111_1365_2664_13929"),
  c("10_1111_1365_2656_13663", "10_1111_1365_2656_13995"),
  c("10_1111_1365_2664_13369", "10_1111_1365_2664_13535"),
  c("10_1111_1365_2656_12912", "10_1111_1365_2656_13639"),
  c("10_1002_pan3_10256", "10_1002_pan3_10551"))

status <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sprintf(...)))
  utils::flush.console()
}

# == What runs for one paper, in its own R process ===========================
# Its own process so that a request that never returns can be stopped without
# losing the other papers. Everything it needs comes in through its arguments.
run_paper <- function(id, paper_file, budget_mb, no_download, final_attempt, cfg) {
  suppressMessages(library(metacheck))
  options(warn = 1, metacheck.cache.dir = cfg$cache_root,
          metacheck.docker_resource_limits = cfg$limits)
  # Some of metacheck's processes fall back to the working directory for
  # their downloads, so work from the folder that holds them.
  setwd(cfg$cache_root)

  # module_run(), with the time of each module recorded. After a module fails,
  # the rest of the chain is passed over, and what was finished is kept.
  timing <- list()
  failed <- FALSE
  run <- function(prev, module, ...) {
    force(prev)
    if (failed) return(prev)
    name <- sub("[.]R$", "", basename(module))
    writeLines(name, file.path(cfg$out_dir, "stage", paste0(id, ".txt")))
    cat(sprintf("[%s] start %s\n", format(Sys.time()), name))
    t0 <- Sys.time()
    res <- tryCatch(module_run(prev, module, ...), error = function(e) e)
    failed <<- inherits(res, "error")
    timing[[name]] <<- data.frame(
      paper_id = id, module = name, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      error = if (failed) conditionMessage(res) else NA_character_)
    if (failed) prev else res
  }

  download_data <- if (no_download) "none" else "data"
  # The code will run, so the first module that downloads also fetches what the
  # run needs, in the same request to each host (metacheck#568)
  # and Python runs in the large image the metacheck server uses (torch, tensorflow)
  prev_for_run <- options(metacheck.download_for_run = !no_download,
                          metacheck.docker_image_py = "ghcr.io/scienceverse/metacheck_py_large:latest")
  on.exit(options(prev_for_run), add = TRUE)
  mo <- readRDS(paper_file) |>
    run("repo_check", osf_license = TRUE, cache = TRUE, skip_on_api_limit = FALSE) |>
    run("data_check", manifest = cfg$manifest_dir, cache = TRUE, skip_on_api_limit = FALSE,
        max_file_size = cfg$max_file_mb, max_download_size = budget_mb, download = download_data) |>
    run("code_check", manifest = cfg$manifest_dir, cache = TRUE, skip_on_api_limit = FALSE,
        max_file_size = cfg$max_file_mb, max_download_size = budget_mb, download = !no_download) |>
    run("open_practices") |>
    run(cfg$oddpub_module) |>
    run("reproducibility_check", execute = TRUE, sandbox = "docker", install_missing = TRUE,
        timeout = cfg$repro_timeout, total_timeout = cfg$repro_total, install_timeout = cfg$repro_install, internet = cfg$repro_internet, cache = TRUE, skip_on_api_limit = FALSE,
        max_file_size = cfg$max_file_mb, max_download_size = budget_mb, download = download_data)

  write.csv(do.call(rbind, timing), file.path(cfg$out_dir, "timing", paste0(id, ".csv")), row.names = FALSE)
  errors <- Filter(function(t) !is.na(t$error), timing)
  # A paper with an error is tried once more; only then are its partial results kept.
  if (length(errors) && !final_attempt) return(invisible(FALSE))
  if (!inherits(mo, "metacheck_module_output")) stop("The first module failed: ", errors[[1]]$error)

  save_rds <- function(x, path) {   # never leaves a half-written file behind
    saveRDS(x, paste0(path, ".tmp"))
    file.rename(paste0(path, ".tmp"), path)
  }
  # The chain's last output holds the outputs of the modules before it.
  modules <- mo$prev_outputs
  modules[[mo$module]] <- mo
  names(modules) <- sub("[.]R$", "", basename(names(modules)))
  # Previews are kept apart: they are large, and only a later codebook_check
  # reads them.
  if (length(modules$data_check$previews)) {
    save_rds(modules$data_check$previews, file.path(cfg$out_dir, "previews", paste0(id, ".rds")))
  }
  modules <- lapply(modules, function(r) {
    r[setdiff(names(r), c("paper", "prev_outputs", "previews", "summary_table", "module", "title", "section"))]
  })
  save_rds(list(paper_id = id, generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                errors = if (length(errors)) do.call(rbind, errors),
                summary_table = mo$summary_table, modules = modules),
           file.path(cfg$out_dir, "papers", paste0(id, ".rds")))
  invisible(TRUE)
}

# == Watching the papers that are running ====================================
io_of <- function(pid) {
  out <- suppressWarnings(system2("powershell", c("-NoProfile", "-Command",
    shQuote(sprintf("$p = Get-CimInstance Win32_Process -Filter 'ProcessId=%d'; if ($p) { $p.ReadTransferCount + $p.OtherTransferCount }", pid))),
    stdout = TRUE, stderr = FALSE))
  suppressWarnings(as.numeric(out[length(out)]))
}

start_paper <- function(id, final_attempt, cfg, bes) {
  paper_file <- file.path(tempdir(), paste0(id, "_paper.rds"))
  saveRDS(bes[id], paper_file)
  budget_mb <- if (id %in% names(BIG_BUDGET)) BIG_BUDGET[[id]] else BUDGET_MB
  log_file <- file.path(cfg$out_dir, "logs", paste0(id, ".log"))
  proc <- callr::r_bg(run_paper, args = list(id, paper_file, budget_mb, id %in% NO_DOWNLOAD, final_attempt, cfg),
                      stdout = log_file, stderr = "2>&1", supervise = TRUE)
  st <- new.env()
  st$id <- id; st$proc <- proc; st$log_file <- log_file; st$t0 <- Sys.time()
  st$last_change <- Sys.time(); st$last_check <- Sys.time()
  st$last_size <- -1; st$last_cpu <- -1; st$last_io <- -1
  st
}

# One check of a running paper. Real work shows at least one of: a growing log
# (a host's rate-limit wait prints a countdown), 6 seconds of processor time per
# minute, or 1 MB transferred per minute. A stalled request still trickles a
# little, which must count as stalled.
check_paper <- function(st, done_file, cfg) {
  if (!st$proc$is_alive()) return(if (file.exists(done_file)) "ok" else "failed")
  sz <- if (file.exists(st$log_file)) file.info(st$log_file)$size else 0
  if (!identical(sz, st$last_size)) { st$last_size <- sz; st$last_change <- Sys.time() }
  if (difftime(Sys.time(), st$last_check, units = "secs") >= 60) {
    st$last_check <- Sys.time()
    cpu <- tryCatch(sum(st$proc$get_cpu_times()[c("user", "system")], na.rm = TRUE), error = function(e) NA_real_)
    io <- io_of(st$proc$get_pid())
    if (!is.na(cpu) && st$last_cpu >= 0 && cpu - st$last_cpu >= 6) st$last_change <- Sys.time()
    if (length(io) == 1 && !is.na(io) && st$last_io >= 0 && io - st$last_io >= 1048576) st$last_change <- Sys.time()
    if (!is.na(cpu)) st$last_cpu <- cpu
    if (length(io) == 1 && !is.na(io)) st$last_io <- io
  }
  stage_file <- file.path(cfg$out_dir, "stage", paste0(st$id, ".txt"))
  now <- if (file.exists(stage_file)) readLines(stage_file, warn = FALSE)[1] else ""
  limit <- if (identical(now, "reproducibility_check")) HANG_MINUTES_REPRO else HANG_MINUTES
  if (difftime(Sys.time(), st$last_change, units = "mins") > limit) {
    tryCatch(st$proc$kill_tree(), error = function(e) st$proc$kill())
    return("stalled")
  }
  "running"
}

free_gb <- function(drive) {
  out <- suppressWarnings(system2("powershell", c("-NoProfile", "-Command",
    shQuote(sprintf("(Get-PSDrive %s).Free", drive))), stdout = TRUE, stderr = FALSE))
  suppressWarnings(as.numeric(out[length(out)])) / 1e9
}

# A process that crashes in a container makes WSL save a copy of its memory on C:, about
# 3 GB each, and up to 10 of them. They have no use here and filled C: during the run, so
# each is deleted once it has stopped growing for 2 minutes.
delete_crash_records <- function() {
  records <- list.files(file.path(Sys.getenv("LOCALAPPDATA"), "Temp", "wsl-crashes"),
                        pattern = "^wsl-crash-.*[.]dmp$", full.names = TRUE)
  finished <- records[difftime(Sys.time(), file.mtime(records), units = "mins") > 2]
  if (length(finished)) unlink(finished)
}

# Containers of a stopped paper are not stopped with it. Removed only when no
# paper is running, so no paper loses a container it is using.
remove_left_containers <- function() {
  left <- suppressWarnings(system2("docker", c("ps", "-aq", "--filter", "name=repro_"), stdout = TRUE, stderr = FALSE))
  if (length(left)) {
    status("Removing %d container(s) left by stopped papers.", length(left))
    system2("docker", c("rm", "-f", left), stdout = FALSE, stderr = FALSE)
  }
  # and the volumes that hold their package libraries (metacheck#568)
  vols <- suppressWarnings(system2("docker", c("volume", "ls", "-q", "--filter", "name=metacheck_lib_"),
                                   stdout = TRUE, stderr = FALSE))
  if (length(vols)) system2("docker", c("volume", "rm", "-f", vols), stdout = FALSE, stderr = FALSE)
}

# Run all papers without a saved result, n_workers at a time, and then once more
# those that failed or stalled.
run_all <- function(ids, cfg, bes) {
  done_file <- function(i) file.path(cfg$out_dir, "papers", paste0(i, ".rds"))
  partner <- function(i) setdiff(unlist(Filter(function(pair) i %in% pair, SAME_REPOSITORY)), i)
  for (attempt in 1:2) {
    queue <- ids[!file.exists(done_file(ids))]
    if (length(queue) == 0) break
    queue <- c(setdiff(queue, RUN_LAST), intersect(queue, RUN_LAST))
    status("Attempt %d: %d paper(s), %d at a time.", attempt, length(queue), n_workers)
    total <- length(queue); finished <- 0; running <- list(); stopped_any <- FALSE
    last_disk <- Sys.time() - 3600; low_disk <- FALSE
    while (length(queue) > 0 || length(running) > 0) {
      # The drive holding the downloads can disconnect under heavy load; without
      # it, results would silently differ. Stop; a new run resumes.
      if (!dir.exists(CACHE_CHECK)) {
        for (s in running) tryCatch(s$proc$kill_tree(), error = function(e) s$proc$kill())
        stop(sprintf("%s disappeared at %s. Reconnect the drive and run this command again. ",
                     CACHE_CHECK, format(Sys.time())),
             "Papers that finished in the minutes before this time may need checking.", call. = FALSE)
      }
      # Every minute: when space runs low, start no new papers, let the running ones
      # finish, and stop. E: holds the temporary files, where a single Python paper's
      # packages can take several GB, the downloads, results and Docker's data; C:
      # is still watched, since a full C: crashed this computer. Far too low stops
      # the papers at once.
      if (difftime(Sys.time(), last_disk, units = "mins") >= 1) {
        last_disk <- Sys.time()
        delete_crash_records()
        free_now <- c(free_gb("C"), free_gb("E"))
        low_disk <- isTRUE(any(free_now < MIN_FREE_GB))
        if (isTRUE(any(free_now < MIN_FREE_GB / 2))) {
          for (s in running) tryCatch(s$proc$kill_tree(), error = function(e) s$proc$kill())
          remove_left_containers()
          stop(sprintf("Only %.1f GB free on C: and %.1f GB on E:. Running papers were stopped. ", free_now[1], free_now[2]),
               "Free space and run this command again.", call. = FALSE)
        }
      }
      if (low_disk && length(running) == 0) {
        stop("Less than ", MIN_FREE_GB, " GB free on C: or E:. Free space and run this command again.", call. = FALSE)
      }
      while (!low_disk && length(running) < n_workers && length(queue) > 0) {
        pick <- which(!vapply(queue, function(i) any(partner(i) %in% names(running)), logical(1)))
        if (length(pick) == 0) break
        id <- queue[pick[1]]; queue <- queue[-pick[1]]
        running[[id]] <- start_paper(id, attempt == 2, cfg, bes)
      }
      Sys.sleep(3)
      for (id in names(running)) {
        res <- check_paper(running[[id]], done_file(id), cfg)
        if (res == "running") next
        finished <- finished + 1
        if (res == "stalled") stopped_any <- TRUE
        status("Attempt %d, %d/%d %s: %s (%.1f min).", attempt, finished, total, id, res,
               as.numeric(difftime(Sys.time(), running[[id]]$t0, units = "mins")))
        running[[id]] <- NULL
      }
    }
    if (stopped_any) remove_left_containers()
  }
}

# == Combining all papers into one file ======================================
# bind_rows() refuses a column that is, say, text in one paper and TRUE/FALSE in
# another; such a column becomes text. Whole and decimal numbers, and columns
# that are empty in one paper, combine as they are.
bind_tables <- function(dfs) {
  dfs <- Filter(function(d) nrow(d) > 0, dfs)
  if (length(dfs) == 0) return(data.frame())
  kind <- function(x) {
    if (is.numeric(x)) "number" else if (is.logical(x) && all(is.na(x))) NA_character_ else class(x)[1]
  }
  cols <- unique(unlist(lapply(dfs, names)))
  mixed <- cols[vapply(cols, function(cl) {
    kinds <- unlist(lapply(dfs, function(d) if (cl %in% names(d)) kind(d[[cl]])))
    length(unique(kinds[!is.na(kinds)])) > 1
  }, logical(1))]
  dfs <- lapply(dfs, function(d) {
    for (cl in intersect(mixed, names(d))) {
      if (!is.list(d[[cl]])) d[[cl]] <- as.character(d[[cl]])
    }
    d
  })
  dplyr::bind_rows(dfs)
}

combine_results <- function(paper_files, out_file, run_info) {
  res <- lapply(paper_files, readRDS)
  names(res) <- vapply(res, `[[`, "", "paper_id")
  out <- list(run_info = run_info, summary_table = bind_tables(lapply(res, `[[`, "summary_table")))
  for (m in unique(unlist(lapply(res, function(r) names(r$modules))))) {
    parts <- lapply(res, function(r) r$modules[[m]])
    tables <- list()
    for (f in unique(unlist(lapply(parts, names)))) {
      dfs <- Filter(is.data.frame, lapply(names(parts), function(id) {
        x <- parts[[id]][[f]]
        # Some tables have the column but leave it empty.
        if (is.data.frame(x) && nrow(x) > 0) {
          if (!"paper_id" %in% names(x)) x$paper_id <- id
          x$paper_id[is.na(x$paper_id)] <- id
        }
        x
      }))
      if (length(dfs)) tables[[f]] <- bind_tables(dfs)
    }
    out[[m]] <- tables
  }
  # Reports hold the names of packages that failed to install, in text only.
  out$reproducibility_check$report_by_paper <- lapply(res, function(r) r$modules$reproducibility_check$report)
  out$errors <- bind_tables(Filter(is.data.frame, lapply(res, `[[`, "errors")))
  saveRDS(out, out_file)
}

# == The run ==================================================================
suppressMessages(library(metacheck))
for (key in c("GITHUB_PAT", "DRYAD_PAT", "OSF_PAT", "ZENODO_PAT")) {
  if (!nzchar(Sys.getenv(key))) stop(key, " is not set; check .Renviron.")
}
stopifnot(dir.exists(CACHE_CHECK))
docker <- metacheck::repro_docker_available()
if (!docker$ok) stop(docker$msg)

# metacheck's own division of processors and memory over containers running together.
module_env <- new.env(parent = asNamespace("metacheck"))
sys.source(system.file("modules", "reproducibility_check.R", package = "metacheck"), envir = module_env)
limits <- module_env$.repro_docker_resource_limits(n_workers)
# The most memory a container may take, in GB: all containers together must stay
# within what Windows can give, or Docker and the computer itself crash (2026-11-05).
max_memory_gb <- as.numeric(arg_value("--memory-gb", NA))
if (!is.na(max_memory_gb)) limits$memory_gb <- min(limits$memory_gb, max_memory_gb)

for (d in c("papers", "previews", "manifests", "logs", "timing", "stage")) {
  dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
}
out_dir <- normalizePath(out_dir, winslash = "/")
cfg <- list(cache_root = CACHE_ROOT, out_dir = out_dir, limits = limits, repro_timeout = repro_timeout, repro_total = repro_total, repro_install = repro_install, repro_internet = repro_internet,
            max_file_mb = max_file_mb,
            manifest_dir = file.path(out_dir, "manifests"),
            oddpub_module = normalizePath("code/modules/oddpub.R", winslash = "/"))

images <- suppressWarnings(system2("docker", c("images", "--digests", "--format", shQuote("{{.Repository}}:{{.Tag}} {{.Digest}}")), stdout = TRUE))
run_info <- list(
  metacheck_version = as.character(packageVersion("metacheck")),
  metacheck_sha = utils::packageDescription("metacheck")[["RemoteSha"]],
  oddpub_version = as.character(packageVersion("oddpub")),
  r_version = R.version.string,
  docker_images = grep("metacheck_(r_large|py_small|py_large)", images, value = TRUE),
  settings = list(budget_mb = BUDGET_MB, big_budget = BIG_BUDGET, no_download = NO_DOWNLOAD,
                  timeout = repro_timeout, total_timeout = repro_total, install_timeout = repro_install, internet = repro_internet, max_file_mb = max_file_mb,
                  workers = n_workers, limits = limits))
info_file <- file.path(out_dir, "run_info.rds")
if (file.exists(info_file)) {
  old <- readRDS(info_file)
  if (!identical(old$metacheck_sha, run_info$metacheck_sha)) {
    stop("This folder was started with metacheck ", old$metacheck_sha, ", and ", run_info$metacheck_sha,
         " is installed. One run uses one version.")
  }
  run_info$started <- old$started
} else {
  run_info$started <- format(Sys.time())
  saveRDS(run_info, info_file)
}
status("metacheck %s (%s), %d at a time, %.1f processors and %d GB per container, output in %s.",
       run_info$metacheck_version, run_info$metacheck_sha, n_workers, limits$cpus, limits$memory_gb, out_dir)

bes <- readRDS("data/bes.rds")
ids <- names(bes)
if (!is.na(first_n)) ids <- ids[seq_len(first_n)]
if (!is.na(only_id)) ids <- only_id
if (!is.na(ids_file)) ids <- readLines(ids_file)
status("%d papers.", length(ids))

run_all(ids, cfg, bes)

paper_files <- file.path(out_dir, "papers", paste0(ids, ".rds"))
missing <- ids[!file.exists(paper_files)]
if (length(missing)) status("No result for: %s", paste(missing, collapse = ", "))
status("%d of %d papers have a saved result.", length(ids) - length(missing), length(ids))

timing_dirs <- file.path(out_dir, "timing")
if (!is.na(base_dir)) {
  # All other papers come from the earlier run.
  rerun_ids <- ids[!ids %in% missing]
  ids <- names(bes)
  paper_files <- ifelse(ids %in% rerun_ids, file.path(out_dir, "papers", paste0(ids, ".rds")),
                        file.path(base_dir, "papers", paste0(ids, ".rds")))
  missing <- ids[!file.exists(paper_files)]
  timing_dirs <- c(file.path(base_dir, "timing"), timing_dirs)
  run_info$rerun <- list(paper_ids = rerun_ids, base = readRDS(file.path(base_dir, "run_info.rds"))[c("metacheck_version", "metacheck_sha", "started")])
}
timing_files <- list.files(timing_dirs, full.names = TRUE)
timing_files <- timing_files[!duplicated(basename(timing_files), fromLast = TRUE)]
run_info$timing <- do.call(rbind, lapply(timing_files, read.csv))
run_info$finished <- format(Sys.time())
run_info$n_papers <- length(ids) - length(missing)
result_file <- file.path(out_dir, "metacheck_results.rds")
combine_results(paper_files[file.exists(paper_files)], result_file, run_info)
status("Saved %s (%.1f MB).", result_file, file.size(result_file) / 1e6)
if (is.na(first_n) && is.na(only_id) && (is.na(ids_file) || !is.na(base_dir))) {
  file.copy(result_file, "data/metacheck_results.rds", overwrite = TRUE)
  status("Copied to data/metacheck_results.rds.")
}
status("Done.")
