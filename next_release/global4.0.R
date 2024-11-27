# shARed mInotAur Database exploratioN Environment : ARIADNE
# Version: 4.0.0
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
## --- Loading libraries ---##
######################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, maps, ggplot2, tidyverse, plotly, shinythemes, rstatix)


######################################################################################################
## --- Loading data, QC, formatting ---##
######################################################################################################

# setting folder for raw data

"./data_source/" -> MINOTAUR_rawdata_source

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

full_join(
    full_join(
        full_join(metadata_study_selected, metadata_soil_selected, by = "id_sampling_point"),
        metadata_scope_selected,
        by = "id_sampling_point"
    ),
    metadata_agri_selected,
    by = "id_sampling_point"
) -> metadata_MINOTAUR_selected

summary(unique(metadata_study_selected$id_sampling_point))

# curation of metadata

# summary(metadata_MINOTAUR_selected)

# 1) mean temp is character
metadata_MINOTAUR_selected$mean_temperature_of_the_day <- as.numeric(metadata_MINOTAUR_selected$mean_temperature_of_the_day)
# 2) year prec is character
metadata_MINOTAUR_selected$year_precipitation <- as.numeric(metadata_MINOTAUR_selected$year_precipitation)
# 3) year prec is character
metadata_MINOTAUR_selected$caco3 <- as.numeric(metadata_MINOTAUR_selected$caco3)
# 4) convert na character in real NA
metadata_MINOTAUR_selected[metadata_MINOTAUR_selected == "na"] <- NA

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

levels(as.factor(metadata_MINOTAUR_selected$farming_system)) -> farming_systems
levels(as.factor(metadata_MINOTAUR_selected$study_landuse)) -> land_uses
levels(as.factor(metadata_MINOTAUR_selected$country_code)) -> country_codes

levels(as.factor(macrof_data$taxon)) -> macro_taxon_codes
macro_taxon_codes <- macro_taxon_codes[-1]
macro_taxon_codes <- c(macro_taxon_codes, "All macrofauna")


sample_list <- list(
    bact = bacteria_data$id_sampling_point,
    fung = fungi_data$id_sampling_point,
    micro = microf_data$id_sampling_point,
    meso = mesof_data_community$id_sampling_point,
    macro = unique(macrof_data$id_sampling_point)
)




