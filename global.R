# shARed mInotAur Database exploratioN Environment : ARIADNE
# Version: 4.2
# Author:
# Francesco Vitali §
# Mocali Stefano §
# Sara Del Duca §
# Elena Tondini §
# Vivianne Yayende +
# Antonio Bispo +
# Mario Adam +
# Rajasekaran Murugan @
#
# Author e-mail:
# francesco.vitali@crea.gov.it
# stefano.mocali@crea.gov.it
# sara.delduca@crea.gov.it
# elena.tondini@crea.gov.it
# vivianne.koyao-yayende@inrae.fr
# antonio.bispo@inrae.fr
# mario.adam@inrae.fr
# rajasekaran.murugan@boku.ac.at
#
# Author Affiliation:
# § Research Centre for Agriculture and Environment, Council for Agricultural Research and Economics (CREA-AA)
# + National Research Institute for Agriculture, Food and Environment (INRAE)
# @ BOKU University (BOKU)


######################################################################################################
## --- Loading libraries, install if necessary ---##
######################################################################################################


packages <- c("shiny", "shinydashboard", "tidyverse", "plotly", "rstatix", "maps", "shinythemes",
              "ggnewscale")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing)
}

lapply(packages, library, character.only = TRUE)

# coin is required by rstatix::wilcox_effsize() but must NOT be attached with
# library(): it defines its own wilcox_test() that would mask rstatix::wilcox_test()
# used throughout server.R. Install only; rstatix calls coin:: internally.
if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin")
}


######################################################################################################
## --- Loading data, QC, formatting ---##
######################################################################################################

# setting folder for raw data

MINOTAUR_rawdata_source <- "next_release/data_source/"

# Preparing different metadata

# general metadata: assembly of a "curated" subset of all the metadata to include
# variables for features of the app, corrected for values

read.table(file = paste0(MINOTAUR_rawdata_source, "tr_metadata_study_mstu.csv"), sep = ";", header = T) -> metadata_study
read.table(file = paste0(MINOTAUR_rawdata_source, "tr_metadata_soil_msoil.csv"), sep = ";", header = T) -> metadata_soil
read.table(file = paste0(MINOTAUR_rawdata_source, "tr_metadata_scope_msco.csv"), sep = ";", header = T) -> metadata_scope
read.table(file = paste0(MINOTAUR_rawdata_source, "tr_metadata_agri_magri.csv"), sep = ";", header = T) -> metadata_agri

metadata_study[, colnames(metadata_study) %in% colnames(metadata_study)[c(1, 4, 13, 17, 18, 19, 20, 24, 26, 27)]] -> metadata_study_selected
metadata_soil[, colnames(metadata_soil) %in% colnames(metadata_soil)[c(1, 17, 20, 24, 25, 26, 27, 29, 31, 35, 36, 37, 39, 41, 43, 44, 45, 46, 47)]] -> metadata_soil_selected
metadata_scope[, colnames(metadata_scope) %in% colnames(metadata_scope)[c(14, 16, 17, 18)]] -> metadata_scope_selected
metadata_agri[, colnames(metadata_agri) %in% colnames(metadata_agri)[c(28, 1, 2, 3, 10, 11, 12, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27)]] -> metadata_agri_selected


metadata_MINOTAUR_selected <- metadata_study_selected %>%
  full_join(metadata_soil_selected, by = "id_sampling_point") %>%
  full_join(metadata_scope_selected, by = "id_sampling_point") %>%
  full_join(metadata_agri_selected, by = "id_sampling_point")


summary(unique(metadata_study_selected$id_sampling_point))

# curation of metadata

metadata_MINOTAUR_selected <- metadata_MINOTAUR_selected %>%
  mutate(
    mean_temperature_of_the_day = as.numeric(mean_temperature_of_the_day),
    year_precipitation = as.numeric(year_precipitation),
    caco3 = as.numeric(caco3)
  ) %>%
  mutate(across(where(is.character), ~na_if(., "na")))


# Preparing different set of data
# bacteria
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_bacteria_dbac.csv"), sep = ";", header = T) -> bacteria_data
bacteria_data <- bacteria_data[, colSums(is.na(bacteria_data)) < nrow(bacteria_data)] # remove all NA variables

# fungi
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_fungi_dfun.csv"), sep = ";", header = T) %>%
    select(-c(2:12, 16:18, )) -> fungi_data
fungi_data <- fungi_data[, colSums(is.na(fungi_data)) < nrow(fungi_data)] # remove all NA variables
# macro
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_macro_in_row_dmirow.csv"), sep = ";", header = T) %>%
    select(c(1, 4, 16:19)) -> macrof_data
# meso
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_mesofauna_dmes.csv"), sep = ";", header = T) %>%
    select(-c(1, 3, 11:13, 22, 50, 51, 58:60)) -> mesof_data_community
mesof_data_community <- mesof_data_community[, colSums(is.na(mesof_data_community)) < nrow(mesof_data_community)] # remove all NA variables
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_index_mesofauna_dimes.csv"), sep = ";", header = T) %>%
    select(c(1, 15, 16, 19, 20)) -> mesof_data_index
mesof_data_index <- mesof_data_index[, colSums(is.na(mesof_data_index)) < nrow(mesof_data_index)] # remove all NA variables
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_observation_dobs.csv"), sep = ";", header = T) %>%
    select(c(6, 8, 12, 13, 14, 19)) -> enchit_data
# micro
read.table(file = paste0(MINOTAUR_rawdata_source, "t_data_microfauna_dmic.csv"), sep = ";", header = T) %>%
    select(c(1:4, 6, 7, 11, 12, 13, 15, 16, 19, 21, 25, 26, 29)) -> microf_data


# setup of some variables or list for dropdown or selection tools

get_levels <- function(x) levels(factor(x))

farming_systems <- get_levels(metadata_MINOTAUR_selected$farming_system)
land_uses <- get_levels(metadata_MINOTAUR_selected$study_landuse)
country_codes <- get_levels(metadata_MINOTAUR_selected$country_code)

macro_taxon_codes <- get_levels(macrof_data$taxon)
macro_taxon_codes <- macro_taxon_codes[-1]
macro_taxon_codes <- c(macro_taxon_codes, "All macrofauna")


sample_list <- list(
    bact = bacteria_data$id_sampling_point,
    fung = fungi_data$id_sampling_point,
    micro = microf_data$id_sampling_point,
    meso = mesof_data_community$id_sampling_point,
    macro = unique(macrof_data$id_sampling_point)
)

common_vars <- c(
  "Land use" = "study_landuse",
  "WRB soil type" = "soil_type_wrb",
  "Soil taxonomy" = "soil_type_in_soil_taxonomy",
  "Soil texture" = "texture",
  "Management" = "farming_system",
  "Cropping system" = "cropping_system",
  "Crop" = "crop_1",
  "Rotation" = "crop_rotation",
  "Tillage" = "tillage_system",
  "Fertilization" = "fertilizer_type",
  "% sand" = "sand",
  "% silt" = "silt",
  "pH" = "ph_mean",
  "Humidity" = "soil_humidity",
  "CaCO3" = "caco3",
  "CEC" = "cec_mean",
  "C/N" = "carbon_azote_ratio_mean",
  "SOC" = "soc_mean",
  "SOM" = "som_mean",
  "Bulk density" = "bulk_density",
  "P total" = "phosphorus_total",
  "P avail" = "phosphorus_available",
  "K avail" = "potassium_available",
  "N" = "nitrogen",
  "OC" = "organic_carbon"
)

######################################################################################################
## --- SML scenario testing: variable definitions ---##
######################################################################################################
# Single source of truth for the "Scenario Testing" tab (SML Annex I thresholds), kept in
# config/sml_scenario_variables.csv rather than hardcoded here so it can be reviewed/updated
# (e.g. by colleagues, in Excel) without touching app code. Both the input UI and the radar
# plot read from this table, so adding a future axis is a matter of adding one row to the CSV.
# See the CSV's own header comment for column meanings.

sml_scenario_variables <- read.csv(
  "config/sml_scenario_variables.csv",
  comment.char = "#", na.strings = c("", "NA"), stringsAsFactors = FALSE
)

# National-legislation precedent for the 10 Annex I Part A/B rows shown in the SML "General
# Information" table (ui.R) - a 2026-09/10 AI-assisted first-pass scan, UNVERIFIED, kept in a CSV
# for the same reason as above. Deliberately has no numeric thresholds (see the CSV's own header
# comment): the legislative landscape is too heterogeneous across states to present as comparable
# figures yet. `variable_id` values here are independent of sml_scenario_variables$id - this table
# has one row per Annex I Part A/B descriptor (10, including "soc_organic", which has no
# corresponding Scenario Testing variable) rather than per scenario-testing axis (9).
sml_national_precedent <- read.csv(
  "config/sml_national_precedent.csv",
  comment.char = "#", na.strings = c("", "NA"), stringsAsFactors = FALSE
)

# Clickable "<found>/27" cell for the Annex I table (ui.R). Denominator is always 27 (EU member
# states), not the row count for this variable - a variable with 3 "found" + 2 "uncertain" rows
# still has 22 states nobody checked, not 5.
sml_precedent_link <- function(var_id) {
  found_n <- sum(sml_national_precedent$variable_id == var_id & sml_national_precedent$status == "found")
  tags$a(href = "#", `data-toggle` = "modal", `data-target` = paste0("#precedent-modal-", var_id),
         style = "font-weight:bold; text-decoration:underline; cursor:pointer;",
         sprintf("%d/27", found_n))
}

# Bootstrap modal (pure client-side, no server round-trip) listing every state found/uncertain
# for one Annex I variable, with legislation name, legal category and source link - deliberately
# no numeric thresholds, see config/sml_national_precedent.csv's own header comment.
sml_precedent_modal <- function(var_id, title) {
  rows <- sml_national_precedent[sml_national_precedent$variable_id == var_id, ]
  rows <- rows[order(rows$status != "found", rows$country), ]
  n_found <- sum(rows$status == "found")
  n_uncertain <- sum(rows$status == "uncertain")

  body_rows <- if (nrow(rows) == 0) {
    list(tags$tr(tags$td(colspan = 4, em("No candidate legislation identified in this scan."))))
  } else {
    lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      status_style <- if (r$status == "found") "color:#2e7d32; font-weight:bold;" else "color:#b8860b; font-weight:bold;"
      link_cell <- if (!is.na(r$source_link) && nzchar(r$source_link)) {
        tags$a(href = r$source_link, target = "_blank", rel = "noopener", "source")
      } else {
        tags$span(style = "color:#999;", "no link retrieved")
      }
      tags$tr(
        tags$td(r$country),
        tags$td(tags$span(style = status_style, if (r$status == "found") "Found" else "Uncertain")),
        tags$td(r$instrument, tags$br(), tags$span(style = "color:#777; font-size:0.85em;", r$legal_category)),
        tags$td(link_cell)
      )
    })
  }

  tags$div(class = "modal fade", id = paste0("precedent-modal-", var_id), tabindex = "-1", role = "dialog",
    tags$div(class = "modal-dialog modal-lg", role = "document",
      tags$div(class = "modal-content",
        tags$div(class = "modal-header",
          tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                      tags$span(`aria-hidden` = "true", HTML("&times;"))),
          tags$h4(class = "modal-title", title)
        ),
        tags$div(class = "modal-body",
          p(em(sprintf(
            "%d of 27 EU Member States found with existing legislation touching this variable (any legal domain, not necessarily framed as soil law) - %d more flagged as uncertain, the rest not confirmed either way. Unverified first-pass scan, 2026-09/10. No numeric thresholds recorded - see the note below the main table.",
            n_found, n_uncertain
          ))),
          tags$div(style = "overflow-x:auto;",
            tags$table(class = "table table-condensed table-striped",
              tags$thead(tags$tr(tags$th("Country"), tags$th("Status"), tags$th("Legislation / legal category"), tags$th("Source"))),
              tags$tbody(body_rows)
            )
          )
        )
      )
    )
  )
}

# Index of the 10 Annex I Part A/B rows this precedent research covers, used by ui.R to generate
# one modal per variable (see sml_precedent_modal() above) without repeating the title 10 times.
sml_annex_precedent_index <- data.frame(
  id = c("ec", "soc_clay", "soc_organic", "bulk_density", "phosphorus",
         "erosion", "heavy_metals", "water_holding", "ksat_air_capacity", "soc_stock"),
  title = c("Electrical conductivity (EC)", "SOC concentration - Mineral soils",
            "SOC concentration - Organic soils", "Bulk density (subsoil)",
            "Extractable phosphorus", "Soil erosion rate",
            "Heavy metals and selected organic contaminants", "Soil water holding capacity",
            "Saturated hydraulic conductivity and air capacity", "SOC stocks"),
  stringsAsFactors = FALSE
)

# Okabe-Ito colorblind-safe qualitative palette (already used elsewhere in this app
# for the macrofauna ecological-group plots) - 8 colors, so scenarios are capped at 8.
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                        "#0072B2", "#D55E00", "#CC79A7", "#000000")

# ggplot2 has no built-in radar/spider coordinate system; this is the standard
# recipe (coord_polar with straight, not curved, grid lines between axes).
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r,
          start = start, direction = sign(direction),
          is_linear = function(coord) TRUE)
}

