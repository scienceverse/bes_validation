#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggsankey")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsankey)
library(patchwork)

# get side_by_side table if not running from qmd
if (!exists("side_by_side")) {
  targets::tar_load(master_comparison, store = "_targets")
  side_by_side <- master_comparison
}

# data progression ----
df <- side_by_side %>%
  filter(!is.na(data_availability)) |>
  select(article_id, starts_with("data_")) |>
  mutate(across(everything(), ~ tidyr::replace_na(as.character(.x), "NA"))) |>
  mutate(data_availability = replace_values(
    data_availability, 
    from = "No, but they are available on request", 
    to = "On request")) |>
  mutate(data_doi = replace_values(
    data_doi, 
    from = c("Yes, but DOI not found/incorrect", 
             "Yes, but not for all data archived"),
    to = c("Incorrect", "Partial"))) |>
  mutate(data_archive = case_when(
    grepl(";", data_archive) ~ "Multiple",
    grepl("^GitHub", data_archive) ~ "Git Repos",
    grepl("^Other", data_archive) ~ "Other",
    grepl("^Suppl", data_archive) ~ "SuppMats",
    .default = data_archive
  )) |>
  mutate(data_license = replace_values(
    data_license, 
    from = "Yes, but not for all data archived", 
    to = "Partial")) |>
  mutate(data_download = replace_values(
    data_download, 
    from = c("No, because the data are embargoed", 
             "Yes, but not all data"),
    to = c("Embargoed", "Partial"))) |>
  mutate(data_README = replace_values(
    data_README, 
    from = "Quasi-README", 
    to = "Quasi")) |>
  pivot_longer(cols = starts_with("data_")) |>
  mutate(.by = c(name, value),
         name = gsub("data_", "", name),
         value = sprintf("%s (%i)", value, n())) |>
  ungroup() |>
  pivot_wider() |>
  make_long(availability, archive, download, doi, license, README)

nodes <- unique(df$node)
order <- case_when(
  startsWith(nodes, "NA (")  ~ 1,
  startsWith(nodes, "No (")  ~ 2,
  startsWith(nodes, "On request (")  ~ 3,
  startsWith(nodes, "Yes (") ~ 5,
  startsWith(nodes, "Zenodo (") ~ 5,
  startsWith(nodes, "Dryad (") ~ 5,
  startsWith(nodes, "Figshare (") ~ 5,
  startsWith(nodes, "Git") ~ 5,
  startsWith(nodes, "Other") ~ 5,
  startsWith(nodes, "Multiple") ~ 5,
  TRUE                   ~ 4
)
nodes <- nodes[order(order, nodes)]
colours <- case_when(
  grepl("Yes \\(", nodes) ~ "darkgreen",
  grepl("No \\(", nodes) ~ "firebrick",
  grepl("On request \\(", nodes) ~ "firebrick",
  grepl("NA \\(", nodes) ~ "grey30",
  grepl("Zenodo \\(", nodes) ~ "darkgreen",
  grepl("^Git", nodes) ~ "darkgreen",
  grepl("Figshare \\(", nodes) ~ "darkgreen",
  grepl("Dryad \\(", nodes) ~ "darkgreen",
  grepl("Other \\(", nodes) ~ "darkgreen",
  grepl("Multiple \\(", nodes) ~ "darkgreen",
  .default = "goldenrod"
)
names(colours) <- nodes # just for manual checking

df |>
  mutate(node = factor(node, nodes),
         next_node = factor(next_node, nodes)) |>
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = node,
             label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1, 
              width = .1, space = 50) +
  geom_sankey_text(size = 3.5, space = 50,
                   color = 1, 
                   #fill = "white", alpha = 0.25, 
                   position = position_nudge(x = .075),
                   vjust = 0.5, hjust = 0) +
  scale_fill_manual(values = colours) +
  scale_x_discrete(position = "top",
                   expand = expansion(add = c(0.25, 0.75))) +
  theme_sankey(base_size = 14) +
  labs(x = NULL, title = "Data") +
  theme(legend.position = "none")

ggsave("documentation/figs/data_sankey.png", width = 10, height = 8)


# code progression ----
df <- side_by_side %>%
  select(article_id, starts_with("code_")) |>
  mutate(across(everything(), ~ tidyr::replace_na(as.character(.x), "NA"))) |>
  mutate(code_used = replace_values(
    code_used, 
    from = c("X"),
    to = c("X"))) |>
  mutate(code_archive = case_when(
    grepl(";", code_archive) ~ "Multiple",
    grepl("^GitHub", code_archive) ~ "Git Repos",
    grepl("^Other", code_archive) ~ "Other",
    grepl("^Suppl", code_archive) ~ "SuppMats",
    .default = code_archive
  )) |>
  mutate(code_README = replace_values(
    code_README, 
    from = "Quasi-README", 
    to = "Quasi")) |>
  pivot_longer(cols = starts_with("code_")) |>
  mutate(.by = c(name, value),
         name = gsub("code_", "", name),
         value = sprintf("%s (%i)", value, n())) |>
  ungroup() |>
  pivot_wider() |>
  make_long(used, archived, archive, download, README)

nodes <- unique(df$node)
order <- case_when(
  startsWith(nodes, "NA (")  ~ 1,
  startsWith(nodes, "No (")  ~ 2,
  startsWith(nodes, "On request (")  ~ 3,
  startsWith(nodes, "Yes (") ~ 5,
  startsWith(nodes, "Zenodo (") ~ 5,
  startsWith(nodes, "Dryad (") ~ 5,
  startsWith(nodes, "Figshare (") ~ 5,
  startsWith(nodes, "Git") ~ 5,
  startsWith(nodes, "CRAN (") ~ 5,
  startsWith(nodes, "Other") ~ 5,
  startsWith(nodes, "Multiple") ~ 5,
  TRUE                   ~ 4
)
nodes <- nodes[order(order, nodes)]
colours <- case_when(
  grepl("Yes \\(", nodes) ~ "darkgreen",
  grepl("No \\(", nodes) ~ "firebrick",
  grepl("On request \\(", nodes) ~ "firebrick",
  grepl("NA \\(", nodes) ~ "grey30",
  grepl("Zenodo \\(", nodes) ~ "darkgreen",
  grepl("^Git", nodes) ~ "darkgreen",
  grepl("Figshare \\(", nodes) ~ "darkgreen",
  grepl("Dryad \\(", nodes) ~ "darkgreen",
  grepl("Other \\(", nodes) ~ "darkgreen",
  grepl("Multiple \\(", nodes) ~ "darkgreen",
  grepl("CRAN \\(", nodes) ~ "darkgreen",
  .default = "goldenrod"
)
names(colours) <- nodes

df |>
  mutate(node = factor(node, nodes),
         next_node = factor(next_node, nodes)) |>
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = node,
             label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1, 
              width = .1, space = 50) +
  geom_sankey_text(size = 3.5, space = 50,
                   color = 1, 
                   #fill = "white", alpha = 0.25, 
                   position = position_nudge(x = .075),
                   vjust = 0.5, hjust = 0) +
  scale_fill_manual(values = colours) +
  scale_x_discrete(position = "top",
                   expand = expansion(add = c(0.25, 0.75))) +
  theme_sankey(base_size = 14) +
  labs(x = NULL, title = "Code") +
  theme(legend.position = "none")

ggsave("documentation/figs/code_sankey.png", width = 10, height = 8)


# comparisons ----

comp_figs <- function(title, bes_col, mc_col, 
                      bes_from = c(T, F), 
                      bes_to = c("Yes", "No"),
                      mc_from = c(T, F), 
                      mc_to = c("Yes", "No"),
                      na.rm = FALSE) {
  
  # recode variables ----
  df <- side_by_side |>
    filter(!na.rm | !is.na(.data[[bes_col]]) | !is.na(.data[[mc_col]])) |>
    select(article_id, all_of(c(bes_col, mc_col))) |>
    mutate(bes = as.character(.data[[bes_col]]),
           bes = replace_values(bes, from = as.character(bes_from), to = bes_to)) |>
    mutate(mc = as.character(.data[[mc_col]]),
           mc =  replace_values(mc, from = as.character(mc_from), to = mc_to)) |>
    select(article_id, bes, mc) |>
    pivot_longer(cols = -article_id) |>
    mutate(.by = c(name, value),
           value = sprintf("%s (%i)", value, n())) |>
    ungroup() |>
    pivot_wider() |>
    make_long(bes, mc)
  
  # sort node order and colours----
  nodes <- unique(df$node)
  order <- case_when(
    startsWith(nodes, "NA (")  ~ 1,
    # red
    startsWith(nodes, "No (")  ~ 2,
    startsWith(nodes, "On request (")  ~ 3,
    # green
    startsWith(nodes, "Yes (") ~ 5,
    startsWith(nodes, "Zenodo (") ~ 5,
    startsWith(nodes, "Dryad (") ~ 5,
    startsWith(nodes, "Figshare (") ~ 5,
    startsWith(nodes, "Git") ~ 5,
    startsWith(nodes, "Other") ~ 5,
    startsWith(nodes, "Multiple") ~ 5,
    # everything else yellow
    TRUE                   ~ 4
  )
  nodes <- nodes[order(order, nodes)]
  colours <- case_when(
    grepl("NA \\(", nodes) ~ "grey60",
    grepl("No \\(", nodes) ~ "firebrick",
    grepl("On request \\(", nodes) ~ "firebrick",
    grepl("Yes \\(", nodes) ~ "darkgreen",
    grepl("Zenodo \\(", nodes) ~ "darkgreen",
    grepl("^Git", nodes) ~ "darkgreen",
    grepl("Figshare \\(", nodes) ~ "darkgreen",
    grepl("Dryad \\(", nodes) ~ "darkgreen",
    grepl("Other \\(", nodes) ~ "darkgreen",
    grepl("Multiple \\(", nodes) ~ "darkgreen",
    .default = "goldenrod"
  )
  names(colours) <- nodes
  
  # create plot ----
  fig <- df |>
    mutate(node = factor(node, nodes),
           next_node = factor(next_node, nodes)) |>
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = node,
               label = node)) +
    geom_sankey(flow.alpha = 0.5, 
                node.color = 1, 
                width = 0.1, 
                linewidth = 0,
                space = 50) +
    geom_sankey_label(
      mapping = aes(hjust = ifelse(x == "bes", 1, 0)),
      size = 3.5, space = 50,
      color = "white", 
      #fill = "white", alpha = 0.25, 
      #position = position_nudge(x = .075),
      linewidth = 0,
      vjust = 0.5) +
    scale_fill_manual(values = colours) +
    theme_sankey(base_size = 14) +
    labs(x = NULL) +
    ggtitle(title) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  widefig <- fig +
    scale_x_discrete(labels = c("Cooper", "Metacheck"),
                     position = "top",
                     expand = expansion(add = c(.3, .3)))
  
  # save plot ----
  figname <- tolower(title) |> gsub(" ", "_", x = _) |> paste0(".png")
  figpath <- file.path("documentation", "figs", figname)
  ggsave(figpath, width = 10, height = 7, plot = widefig)
  
  # return with different expansion & title
  fig  +
    scale_x_discrete(labels = c("Cooper", "Metacheck"),
                         position = "top",
                         expand = expansion(add = c(1, 1)))
}

# data plots ----

datafigs <- list()

## data_availability ----

datafigs$data_availability <- comp_figs(
  title    = "Data Availability",
  bes_col  = "data_availability",
  mc_col   = "mc_data_availability",
  bes_from = "No, but they are available on request",
  bes_to   = "On request"
)


## data_license ----

datafigs$data_license <- comp_figs(
  title    = "Data License",
  bes_col  = "data_license",
  mc_col   = "mc_data_license",
  bes_from = "Yes, but not for all data archived",
  bes_to   = "Partial"
)

## data_doi ----

side_by_side$mc_data_doi_recoded <- !is.na(side_by_side$mc_data_doi)

datafigs$data_doi <- comp_figs(
  title    = "Data DOI",
  bes_col  = "data_doi",
  mc_col   = "mc_data_doi_recoded",
  bes_from = c("Yes, but DOI not found/incorrect", 
           "Yes, but not for all data archived"),
  bes_to = c("Incorrect", "Partial")
)

## data_download ----

datafigs$data_download <- comp_figs(
  title    = "Data Download",
  bes_col  = "data_download",
  mc_col   = "mc_data_download",
  bes_from = c("No, because the data are embargoed", 
               "Yes, but not all data"),
  bes_to = c("Embargoed", "Partial")
)

## data_README ----

datafigs$data_readme <- comp_figs(
  title    = "Data README",
  bes_col  = "data_README",
  mc_col   = "mc_data_README",
  bes_from = "Quasi-README", 
  bes_to = "Quasi"
)

## data_archive ----

values <- side_by_side$data_archive |>
  c(side_by_side$mc_data_archive) |>
  c(side_by_side$code_archive) |>
  c(side_by_side$mc_code_archive) |>
  unique() |> 
  setdiff(c(NA_character_))

# plot name --- side_by_side name
archives <- c(
  "Dryad" = "Dryad", 
  "Zenodo" = "Zenodo",
  "Figshare" = "Figshare",
  "Other" = "Other repo/database", 
  "Personal site" = "Personal website",
  "SuppMats" = "Supplementary materials",
  "GitHub+" = "GitHub, GitLab, Codeberg or similar platform"
)
multi <- setdiff(values, archives)
names(multi) <- rep("Multiple", length(multi))

datafigs$data_archive <- comp_figs(
  title    = "Data Archive",
  bes_col  = "data_archive",
  mc_col   = "mc_data_archive",
  bes_from = unname(c(archives, multi)),
  bes_to   = names(c(archives, multi)),
  mc_from = unname(c(archives, multi)),
  mc_to   = names(c(archives, multi))
)


## show all data plots ----
wrap_plots(datafigs[1:4], nrow = 1)
ggsave("documentation/figs/data_all1.png", width = 16, height = 8)
browseURL("documentation/figs/data_all1.png")

wrap_plots(datafigs[5:6], nrow = 1)
ggsave("documentation/figs/data_all2.png", width = 7, height = 7)
browseURL("documentation/figs/data_all2.png")


# code plots ----


codefigs <- list()

## code_archived ----

codefigs$code_archived <- comp_figs(
  title    = "Code Archived",
  bes_col  = "code_archived",
  mc_col   = "mc_code_archived"
)

## code_download ----

codefigs$code_download <- comp_figs(
  title    = "Code Download",
  bes_col  = "code_download",
  mc_col   = "mc_code_download",
  na.rm = TRUE
)


## code_README ----

side_by_side$mc_code_README_recoded <- ifelse(
  side_by_side$mc_code_archived,
  side_by_side$mc_code_README, NA)

codefigs$code_readme <- comp_figs(
  title    = "Code README",
  bes_col  = "code_README",
  mc_col   = "mc_code_README_recoded",
  bes_from = "Quasi-README", 
  bes_to = "Quasi",
  na.rm = TRUE
)



## code_archive ----

codefigs$code_archive <- comp_figs(
  title    = "Code Archive",
  bes_col  = "code_archive",
  mc_col   = "mc_code_archive",
  bes_from = unname(c(archives, multi)),
  bes_to   = names(c(archives, multi)),
  mc_from = unname(c(archives, multi)),
  mc_to   = names(c(archives, multi)),
  na.rm = TRUE
)


## show all code plots ----
wrap_plots(codefigs, nrow = 1)
ggsave("documentation/figs/code_all.png", width = 12, height = 6)
browseURL("documentation/figs/code_all.png")

