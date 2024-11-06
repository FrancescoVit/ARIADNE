# shARed mInotAur Database exploratioN Environment : ARIADNE
# Version: 3.0.0
# Author:
# Francesco Vitali §
# Mocali Stefano §
# Sara Del Duca §
# Elena Tondini §
# Vivianne Yayende +
# Antonio Bispo +
# MArio Adam +
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
# + INRAE
# @ BOKU


######################################################################################################
## --- Loading libraries ---##
######################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, maps, ggplot2, tidyverse, plotly, shinythemes, rstatix)


######################################################################################################
## --- Loading data, QC, formatting ---##
######################################################################################################

# Setting working directory -> TO DO: set it to be automatically the location of R file

setwd("/home/fvitali/Documenti/CREA/MINOTAUR/ShinyApp/ShinyApp_ARIADNE")

# reading files from "data_source" folder

list.files("./data_source") -> MINOTAUR_DB_csv

# Preparing different metadata

# general metadata: assembly of a "curated" subset of all the metadata to include
# variables for features of the app, corrected for values

read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[16]), sep = ";", header = T) -> metadata_study
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[15]), sep = ";", header = T) -> metadata_soil
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[14]), sep = ";", header = T) -> metadata_scope
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[8]), sep = ";", header = T) -> metadata_agri

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
# fungi
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[3]), sep = ";", header = T) %>%
  select(-c(2:12, 16:18, )) -> fungi_data
fungi_data <- fungi_data[, colSums(is.na(fungi_data)) < nrow(fungi_data)] # remove all NA variables
# macro
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[4]), sep = ";", header = T) %>%
  select(c(1, 4, 16:19)) -> macrof_data
# meso
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[5]), sep = ";", header = T) %>%
  select(-c(1, 3, 11:13, 22, 50, 51, 58:60)) -> mesof_data_community
mesof_data_community <- mesof_data_community[, colSums(is.na(mesof_data_community)) < nrow(mesof_data_community)] # remove all NA variables
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[2]), sep = ";", header = T) %>%
  select(c(1, 15, 16, 19, 20)) -> mesof_data_index
mesof_data_index <- mesof_data_index[, colSums(is.na(mesof_data_index)) < nrow(mesof_data_index)] # remove all NA variables
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[7]), sep = ";", header = T) %>%
  select(c(6, 8, 12, 13, 14, 19)) -> enchit_data
# micro
read.table(file = paste0("./data_source/", MINOTAUR_DB_csv[6]), sep = ";", header = T) %>%
  select(c(1:4, 6, 7, 11, 12, 13, 15, 16, 19, 21, 25, 26, 29)) -> microf_data


# setup of some variables or list for dropdown or selection tools

levels(as.factor(metadata_MINOTAUR_selected$farming_system)) -> farming_systems
levels(as.factor(metadata_MINOTAUR_selected$study_landuse)) -> land_uses
levels(as.factor(metadata_MINOTAUR_selected$country_code)) -> country_codes

levels(as.factor(macrof_data$taxon)) -> macro_taxon_codes
macro_taxon_codes <- macro_taxon_codes[-1]
macro_taxon_codes <- c(macro_taxon_codes, "All macrofauna")


sample_list <- list(
  fung = fungi_data$id_sampling_point,
  micro = microf_data$id_sampling_point,
  meso = mesof_data_community$id_sampling_point,
  macro = unique(macrof_data$id_sampling_point)
)

######################################################################################################
## --- Setting ui ---##
######################################################################################################

ui <- dashboardPage(
  #### --- Define some appearance details ---####

  ## --- Define dashboard header ---##
  dashboardHeader(
    title = "ARIADNE",
    titleWidth = 120
  ),

  ## --- Define dashboard sidebar ---##
  dashboardSidebar(
    width = 120,
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("magnifying-glass-chart"))
    )
  ),

  ## --- Define dashboard body ---##
  dashboardBody(
    tags$head(
      tags$style(HTML(" 
                      /* navbar */
                      .skin-blue .main-header .navbar {
                      background-color: #ba9a71;
                      }
                      /* logo */
                      .skin-blue .main-header .logo {
                      background-color: #ba9a71;
                      }
                      /* logo hovered*/
                      .skin-blue .main-header .logo:hover {
                      background-color: #9cb533;
                      }
                      /* sidebar */
                      .skin-blue .main-sidebar {
                      background-color: #000000;
                      }
                      "))
    ), # Customization of header, logo and sidebar
    tags$style(HTML("
                      /* primary text and background */
                      .box.box-solid.box-primary>.box-header {
                      color:#000000;
                      background:#87c8d7
                      }
                      /* primary borders */
                      .box.box-solid.box-primary{
                      border-bottom-color:#000000;
                      border-left-color:#000000;
                      border-right-color:#000000;
                      border-top-color:#000000;
                      }
                      ")), # Customization of primary status box
    
    #### --- Define tabs content ---####
    tabItems(
      #### --- First tab content: Overview tab ---####
      tabItem(
        tabName = "Overview",

        ## --- Filtering using dropdown ---##
        fluidRow(
          box(
            width = 12,
            title = "Sample selection",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE, # open box
            column(
              width = 6,
              selectInput("type",
                label = "Land use:",
                c(
                  "All samples" = "all",
                  "Agroforestry" = land_uses[1],
                  "Arable land" = land_uses[2],
                  "Arable" = land_uses[3],
                  "Fallow bare" = land_uses[4],
                  "Fallow green" = land_uses[5],
                  "Fallow" = land_uses[6],
                  "Forest land" = land_uses[7],
                  "Grass land" = land_uses[8],
                  "Permanent crop" = land_uses[10],
                  "Wet land" = land_uses[11]
                )
              )
            ),
            column(
              width = 6,
              selectInput("manag",
                label = "Management:",
                c(
                  "All samples" = "all",
                  "Agroecology" = farming_systems[1],
                  "Conventional farming" = farming_systems[2],
                  "Organic farming" = farming_systems[4]
                )
              )
            ),

            ## --- Multiple checkbox for samples and biological levels ---##
            fluidRow(
              box(
                width = 6,
                checkboxGroupInput("biolevel_select",
                  label = h3("Select biota group to show"),
                  choices = list( # "Bacteria" = "bact",
                    "Fungi" = "fung",
                    "Microfauna" = "micro",
                    "Mesofauna" = "meso",
                    "Macrofauna" = "macro"
                  ),
                  selected = c("fung", "micro", "meso", "macro"), inline = T
                )
              ),
              box(
                width = 6,
                selectInput("state_select",
                  "Select State",
                  country_codes,
                  multiple = TRUE,
                  selectize = TRUE
                )
              )
            ),
          )
        ), # close box

        ## --- Text to report n of selected samples ---##

        fluidRow(
          box(
            width = 6,
            uiOutput("info_box_total")
          ),
          box(
            width = 6,
            uiOutput("info_box_selected")
          )
        ),

        ## --- Select map visualization range ---##

        fluidRow(
          box(
            width = 12,
            title = "Adjust map visualization range",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(
              width = 6,
              sliderInput("long_min", "Longitude range:",
                min = -25, max = 80,
                value = c(-11, 30)
              )
            ),
            column(
              width = 6,
              sliderInput("lat_min", "Latitude range:",
                min = 25, max = 80,
                value = c(37, 71)
              )
            )
          )
        ),

        ## --- Overview plots: map and lollipop ---##

        fluidRow(
          box(
            width = 8,
            plotOutput("plot_map_overview",
              width = "100%"
            )
          ),
          box(
            width = 4,
            plotlyOutput("plot_country_overview")
          )
        )
      ), # close first tabItem

      #### --- Second tab content: analysis tab ---####
      tabItem(
        tabName = "Analysis",
        fluidRow(
          box(
            width = 12,
            tabsetPanel(
              type = "tabs", id = "group_tab",
              #### --- Biodiversity indices panel ---####
              tabPanel(
                title = "Biodiversity - Indices", value = "t_biodiv_index",
                #### --- Fungi row ---####
                fluidRow( # Fungi
                  box(
                    title = "Fungi",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12,
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("fungi_index_choice",
                          label = "Select index to plot:",
                          c(
                            "Chao1" = "fungi_chao1_index",
                            "Fisher alpha" = "fungi_fisher_alpha",
                            "Shannon index" = "fungi_shannon_index",
                            "Simpson index" = "fungi_simpson_index",
                            "Evenness" = "fungi_evenness_index",
                            "Richness" = "fungi_richness_index",
                            "Inverse Simpson index" = "fungi_inverse_simpson_index"
                          )
                        )
                      ),
                      column(
                        width = 6,
                        selectInput("fungi_var_choice",
                          label = "Select variable to plot:",
                          c(
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
                        )
                      ),
                    ),
                    numericInput("treshold_line_fung",
                      "Optionally set value to draw a line:  (only for numerical variables)",
                      value = 0,
                      min = 0,
                      max = Inf,
                      step = 0.001
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        plotlyOutput("plot_biodiv_index_fungi",
                          width = "100%"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12, 
                        align = "center",
                        dataTableOutput("fungi_index_table")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 6, align = "left",
                        tableOutput("table_fungi_correl_or_kruskal")
                      ),
                      column(
                        width = 6, align = "left",
                        plotOutput("tile_plot_fungi")
                      )
                    )
                  )
                ),
                #### --- Microfauna row ---####
                fluidRow( # Microfauna
                  box(
                    title = "Microfauna",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12,
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("micro_index_choice",
                          label = "Select index to plot:",
                          c(
                            "CI index" = "ci",
                            "EI index" = "ei",
                            "MI index" = "mi",
                            "Genera richness" = "nematode_genera_richness"
                          )
                        )
                      ),
                      column(
                        width = 6,
                        selectInput("micro_var_choice",
                          label = "Select variable to plot:",
                          c(
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
                        )
                      )
                    ),
                    numericInput("treshold_line_micro",
                                 "Optionally set value to draw a line: (only for numerical variables)",
                                 value = 0,
                                 min = 0,
                                 max = Inf,
                                 step = 0.001
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        plotlyOutput("plot_biodiv_index_micro",
                          width = "100%"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        dataTableOutput("micro_index_table")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 6, align = "left",
                        tableOutput("table_micro_correl_or_kruskal")
                      ),
                      column(
                        width = 6, align = "left",
                        plotOutput("tile_plot_micro")
                      )
                    )
                  )
                ),
                #### --- Mesofauna row ---####
                fluidRow( # Mesofauna
                  box(
                    title = "Mesofauna",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12,
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("meso_var_choice",
                                    label = "Select variable:",
                                    c(
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
                        )
                      )
                    ),
                    numericInput("treshold_line_meso",
                                 "Optionally set value to draw a line:  (only for numerical variables)",
                                 value = 0,
                                 min = 0,
                                 max = Inf,
                                 step = 0.001
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        plotlyOutput("plot_biodiv_index_meso",
                                     width = "100%"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        dataTableOutput("meso_index_table")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 6, align = "left",
                        tableOutput("table_meso_correl_or_kruskal")
                      ),
                      column(
                        width = 6, align = "left",
                        plotOutput("tile_plot_meso")
                      )
                    )
                  )
                )
              ),
              #### --- Biodiversity community panel ---####
              tabPanel(
                title = "Biodiversity - Community", value = "t_biodiv_comm",
                #### --- Fungi row ---####
                fluidRow( # Fungi
                  box(
                    title = "Fungi",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12
                  )
                ),
                #### --- Microfauna row ---####
                fluidRow( # Microfauna
                  box(
                    title = "Microfauna",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12
                  )
                ),
                #### --- Mesofauna row ---####
                fluidRow( # Mesofauna
                  box(
                    title = "Mesofauna",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12,
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("meso_choice2",
                          label = "Select Categorical variable:",
                          c(
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
                        )
                      ),
                      column(
                        width = 6,
                        selectInput("meso_choice3",
                          label = "Select biological form:",
                          c(
                            "Acari 20" = "acari_20",
                            "Aranae 01" = "araneae_01",
                            "Aranae 05" = "araneae_05",
                            "Aranae tot" = "ab_araneae",
                            "Chilopoda 10" = "chilopoda_10",
                            "Chilopoda 20" = "chilopoda_20",
                            "Chilopoda tot" = "ab_chilopoda",
                            "Coleoptera 01" = "coleoptera_01",
                            "Coleoptera 05" = "coleoptera_05",
                            "Coleoptera 10" = "coleoptera_10",
                            "Coleoptera 15" = "coleoptera_15",
                            "Coleoptera 20" = "coleoptera_20",
                            "Coleoptera (larvae) 10" = "coleoptera_l10",
                            "Coleoptera tot" = "ab_coleoptera",
                            "Collembola 01" = "collembola_01",
                            "Collembola 02" = "collembola_02",
                            "Collembola 04" = "collembola_04",
                            "Collembola 06" = "collembola_06",
                            "Collembola 08" = "collembola_08",
                            "Collembola 10" = "collembola_10",
                            "Collembola 20" = "collembola_20",
                            "Collembola tot" = "ab_collembola",
                            "Dermaptera 01" = "dermaptera_01",
                            "Diplopoda 10" = "diplopoda_10",
                            "Diplopoda 20" = "diplopoda_20",
                            "Diplopoda tot" = "ab_diplopoda",
                            "Diplura 20" = "diplura_20",
                            "Diptera 01" = "diptera_01",
                            "Diptera (larvae) 10" = "diptera_l10",
                            "Embioptera 10" = "embioptera_10",
                            "Hemiptera 01" = "hemiptera_01",
                            "Hymenoptera 01" = "hymenoptera_01",
                            "Hymenoptera 05" = "hymenoptera_05",
                            "Hymenoptera (larvae) 10" = "hymenoptera_l10",
                            "Hymenoptera tot" = "ab_hymenoptera",
                            "Isopoda 10" = "isopoda_10",
                            "Opiliones 10" = "opiliones_10",
                            "Palpigradi 20" = "palpigradi_20",
                            "Pauropoda 20" = "pauropoda_20",
                            "Protura 20" = "protura_20",
                            "Pseudoscorpionida 20" = "pseudoscorp_20",
                            "Psocoptera 01" = "psocoptera_01",
                            "Symphyla 20" = "symphyla_20",
                            "Thysanoptera 01" = "thysanoptera_01"
                          )
                        )
                      )
                    ),
                  )
                ),
                #### --- Macrofauna row ---####
                fluidRow( # Macrofauna
                  box(
                    title = "Macrofauna",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    width = 12,
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("macro_var_choice_abb",
                          label = "Select Categorical variable:",
                          c(
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
                        )
                      ),
                      column(
                        width = 6,
                        selectInput("macro_taxon_choice",
                          selected = macro_taxon_codes[78],
                          "Select Taxon",
                          macro_taxon_codes,
                          multiple = F,
                          selectize = TRUE
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 12, align = "center",
                        plotlyOutput("plot_biodiv_abb_macro",
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              )

              #### --- Close previously open ---####
            ) # close Tabsetpanel
          ) # close box
        ) # close fluidrow
      ) # close second tab content
    ) # close tabitem
  ) # close dashboardBody
) # close dashboardPage



######################################################################################################
## --- Setting server ---##
######################################################################################################

server <- function(input, output) {
  #### --- Text or notification box indicating n of samples selected by filters ---####

  output$info_box_total <- renderUI({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    sample_to_keep <- c()
    biota_lvls <- c("fung", "micro", "meso", "macro")
    for (i in 1:4) {
      biota_lvl <- biota_lvls[i]
      as.character(unlist(sample_list[biota_lvl])) -> samples_loop
      sample_to_keep <- c(sample_to_keep, samples_loop)
    }
    sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)

    infoBox("Total selected records:",
      width = "100%",
      nrow(sites_selected),
      icon = icon("vial"),
      fill = TRUE
    )
  })

  output$info_box_selected <- renderUI({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected <- data.frame()
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    infoBox("Biota group selected records:",
      width = "100%",
      nrow(sites_selected),
      icon = icon("vial-circle-check"),
      fill = TRUE
    )
  })


  #### --- Overview on map of samples selected by filters ---####
  output$plot_map_overview <- renderPlot({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected <- data.frame()
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    world <- map_data(map = "world")
    europe <- subset(world, region %in% c(
      "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
      "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
      "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
      "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland",
      "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein",
      "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro",
      "Macedonia", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
      "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain",
      "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican"
    ))
    plt1 <- ggplot() +
      geom_polygon(
        data = europe,
        aes(x = long, y = lat, group = group),
        colour = "black",
        fill = "gray90",
        alpha = 0.6
      ) +
      xlab("Latitude") +
      ylab("Longitude") +
      coord_fixed(
        ylim = c(input$lat_min, input$lat_max),
        xlim = c(input$long_min, input$long_max)
      ) +
      theme_bw()



    if (nrow(sites_selected) > 0) {
      plt1 <- plt1 + geom_point(
        data = sites_selected,
        aes(x = longitude, y = latitude),
        colour = "red"
      )
    }

    plot(plt1)
  })

  #### --- Overview of countries selected by filters ---####
  output$plot_country_overview <- renderPlotly({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected <- data.frame()
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }


    as.data.frame(summary(as.factor(na.omit(sites_selected$country_code)))) %>%
      rownames_to_column() %>%
      rename("Count" = "summary(as.factor(na.omit(sites_selected$country_code)))") %>%
      rename("Country" = "rowname") %>%
      arrange(-Count) %>%
      mutate(Country = factor(Country, levels = Country)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_segment(aes(x = Country, xend = Country, y = 0, yend = Count)) +
      geom_point(size = 3) +
      coord_flip() +
      theme_classic() -> plt2

    plt2 <- ggplotly(plt2)
  })

  #### --- Biodiversity index tab:  ---####
  #### --- Fungi ---####

  output$plot_biodiv_index_fungi <- renderPlotly({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    fungi_data %>%
      select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> fungi_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% fungi_data_index_selected$id_sampling_point) -> meta_fungi_selected
    full_join(meta_fungi_selected, fungi_data_index_selected, by = "id_sampling_point") -> meta_fungi_selected
    
    # clean data
    meta_fungi_selected[meta_fungi_selected == 9999] <- NA

    # plot
    if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == F) {
      if (input$treshold_line_fung == 0 | is.na(input$treshold_line_fung) == T) {
        
        validate(
          need(is.na(meta_fungi_selected[, input$fungi_var_choice]) == F, "No data for selected variable")
        )
        
        meta_fungi_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$fungi_var_choice, y = input$fungi_index_choice)) +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$fungi_var_choice) +
          ylab(input$fungi_index_choice) +
          theme_bw() -> plt6

        ggplotly(plt6) -> plt6
        plt6$x$data[[1]]$hoverinfo <- "none"

        ggplotly(plt6)
      } else if (input$treshold_line_fung != 0) {
        
        validate(
          need(is.na(meta_fungi_selected[, input$fungi_var_choice]) == F, "No data for selected variable")
        )
        
        meta_fungi_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$fungi_var_choice, y = input$fungi_index_choice)) +
          geom_vline(xintercept = input$treshold_line_fung, colour = "red", linetype = "dotdash") +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$fungi_var_choice) +
          ylab(input$fungi_index_choice) +
          theme_bw() -> plt6

        ggplotly(plt6) -> plt6
        plt6$x$data[[1]]$hoverinfo <- "none"

        ggplotly(plt6)
      }
    } else if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == T) {
      meta_fungi_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$fungi_var_choice) != "") ->  meta_fungi_selected
      
      validate(
        need(nrow(meta_fungi_selected) != 0, "No data for selected variable")
      )
      
      meta_fungi_selected %>% 
        ggplot(aes_string(x =  input$fungi_var_choice, y = input$fungi_index_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab(input$fungi_index_choice) +
        theme_bw() +
        coord_flip() -> plt6

      ggplotly(plt6) -> plt6
      plt6$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt6)
    }
  })
  
  output$fungi_index_table <- renderDataTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    fungi_data %>%
      select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> fungi_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% fungi_data_index_selected$id_sampling_point) -> meta_fungi_selected
    full_join(meta_fungi_selected, fungi_data_index_selected, by = "id_sampling_point") -> meta_fungi_selected

    # clean data
    meta_fungi_selected[meta_fungi_selected == 9999] <- NA
    meta_fungi_selected %>% 
    filter(!!as.symbol(input$fungi_var_choice) != "") -> meta_fungi_selected
    
    validate(
      need(nrow(meta_fungi_selected) != 0, "")
    )
    

    if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == F) {
     
        data.frame(
          Index = input$fungi_index_choice,
          t(round(quantile(meta_fungi_selected[, input$fungi_index_choice],
            probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
            na.rm = T
          ), digits = 3)),
          Min = round(min(meta_fungi_selected[, input$fungi_index_choice],
                          na.rm = T
                          ), 
                      digits = 3),
          Max = round(max(meta_fungi_selected[, input$fungi_index_choice],
                          na.rm = T
                          ), 
                      digits = 3),
          Avg = round(mean(meta_fungi_selected[, input$fungi_index_choice],
                          na.rm = T
          ), 
          digits = 3),
          N = length(na.omit(meta_fungi_selected[, input$fungi_index_choice]))
        ) -> table_data_fungi_index
        colnames(table_data_fungi_index) <- c(
          "Variable",
          "10th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "90th perc.",
          "Minimum value",
          "Maximum value",
          "Avg",
          "N"
        )
        
        data.table::as.data.table(table_data_fungi_index)
        # to do, insert here a way to count samples over or below the threshold
  
    } else if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == T) {
      levels(as.factor(meta_fungi_selected[, input$fungi_var_choice])) -> levels_fungi_table

      # table

      df_out_loop <- data.frame()

      for (i in 1:length(levels_fungi_table)) {
        filter(
          meta_fungi_selected,
          meta_fungi_selected[, input$fungi_var_choice] == levels_fungi_table[i]
        ) -> df_loop

        data.frame(
          Index = levels_fungi_table[i],
          t(round(quantile(df_loop[, input$fungi_index_choice],
            probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
            na.rm = T
          ), digits = 3)),
          Min = round(min(df_loop[, input$fungi_index_choice], na.rm = T), digits = 3),
          Max = round(max(df_loop[, input$fungi_index_choice], na.rm = T), digits = 3),
          Avg = round(mean(df_loop[, input$fungi_index_choice], na.rm = T), digits = 3),
          N = length(na.omit(df_loop[, input$fungi_index_choice]))
        ) -> df_out_cycle

        colnames(df_out_cycle) <- c(
          "Variable",
          "10th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "90th perc.",
          "Minimum value",
          "Maximum value",
          "Avg",
          "N"
        )

        df_out_loop <- rbind(df_out_loop, df_out_cycle)
      }

      table_data_fungi_index <- df_out_loop

      data.table::as.data.table(table_data_fungi_index)
    }
  })
  
  output$table_fungi_correl_or_kruskal <- renderTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    fungi_data %>%
      select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> fungi_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% fungi_data_index_selected$id_sampling_point) -> meta_fungi_selected
    full_join(meta_fungi_selected, fungi_data_index_selected, by = "id_sampling_point") -> meta_fungi_selected
    
    
    # clean data
    meta_fungi_selected[meta_fungi_selected == 9999] <- NA
    meta_fungi_selected %>% 
      filter(!!as.symbol(input$fungi_var_choice) != "") -> meta_fungi_selected
    
    validate(
      need(nrow(meta_fungi_selected) != 0, "")
    )
    
    if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == F) {
      # Numerical variable: display different correlation or other like lm
      
      meta_fungi_selected %>% 
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>% 
        drop_na() %>% 
        cor_test(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice),method = "pearson") %>% 
        as.data.frame() -> fungi_pearson
      
      meta_fungi_selected %>% 
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>% 
        drop_na() %>% 
        cor_test(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice),method = "spearman") %>% 
        as.data.frame() -> fungi_spearman
      
      rbind(fungi_pearson[,-c(1,2,6,7)], fungi_spearman[,-c(1,2)]) -> df_correlation_kruskal_fungi
      
      df_correlation_kruskal_fungi
      
     
    } else if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == T) {
      # Categorical variable: display results of Kruskal wallis
      
      fungi_var_choice <- input$fungi_var_choice
      fungi_index_choice <- input$fungi_index_choice
      
      meta_fungi_selected %>% 
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>% 
        drop_na() %>% 
        kruskal_test(as.formula(paste(fungi_index_choice, paste("~", fungi_var_choice)))) %>% 
        as.data.frame() -> df_correlation_kruskal_fungi
      
      df_correlation_kruskal_fungi[,-c(1,2)]
    }
  })
  
  output$tile_plot_fungi <- renderPlot({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    fungi_data %>%
      select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> fungi_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% fungi_data_index_selected$id_sampling_point) -> meta_fungi_selected
    full_join(meta_fungi_selected, fungi_data_index_selected, by = "id_sampling_point") -> meta_fungi_selected
    
    
    # clean data
    meta_fungi_selected[meta_fungi_selected == 9999] <- NA
    meta_fungi_selected %>% 
      filter(!!as.symbol(input$fungi_var_choice) != "") -> meta_fungi_selected
    
    validate(
      need(nrow(meta_fungi_selected) != 0, "")
    )
    
    if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == T) {
      # Categorical variable
      
      fungi_var_choice <- input$fungi_var_choice
      fungi_index_choice <- input$fungi_index_choice
      
      meta_fungi_selected %>% 
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>% 
        drop_na() %>% 
        wilcox_test(as.formula(paste(fungi_index_choice, paste("~", fungi_var_choice))), p.adjust.method = "bonferroni") -> fungi_wilcox
      
      fungi_wilcox %>% 
        as.data.frame() %>% 
        ggplot(aes(x = group1, 
                   y = group2, 
                   fill = p)) +
        geom_tile(color = "black", 
                  lwd = 0.8,
                  linetype = 1) +
        geom_text(aes(label = p), 
                  color = "black", 
                  size = 4) +
        xlab("") + 
        ylab("") + 
        scale_fill_gradient(low = "grey80", 
                            high = "white") + 
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(),
              panel.grid.major.x= element_line(colour = "black",  linetype = "dotted"),
              #panel.grid.major = element_blank(), 
              #panel.grid.minor = element_blank(), 
              panel.background = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))-> plt9
      
      plt9
       
    }
    
    
    
    
  })
  
  #### --- Mesofauna ---####

  output$plot_biodiv_index_meso <- renderPlotly({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }


    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) -> meta_meso_selected
    full_join(meta_meso_selected, mesof_data_index_selected, by = "id_sampling_point") -> meta_meso_selected
    
    diversity_index_value <- "diversity_index_value"

    # make average QBS between replicates of the sample, which have the same QBS value
    # the problem is that the meta_meso_selected has also metadata, so I can't really average over the replicates....
    # meta_meso_selected %>%
    #   mutate(Sample_ID_avg = substr(id_sampling_point,1,nchar(id_sampling_point)-2)) %>%
    #   group_by(Sample_ID_avg) -> meta_meso_selected
    #

    if (is.character(meta_meso_selected[, input$meso_var_choice]) == F) {
      if (input$treshold_line_meso == 0 | is.na(input$treshold_line_meso) == T) {
        
        validate(
          need(is.na(meta_meso_selected[, input$meso_var_choice]) == F, "No data for selected variable")
        )
        
      meta_meso_selected %>%
        as_tibble() %>%
        ggplot(aes_string(x = input$meso_var_choice, y = diversity_index_value)) +
        geom_point() +
        geom_smooth(method = "loess") +
        xlab(input$meso_var_choice) +
        ylab("QBS-ar") +
        theme_bw() -> plt5

      ggplotly(plt5) -> plt5
      plt5$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt5)
      } else if (input$treshold_line_meso != 0) {
        
        validate(
          need(is.na(meta_meso_selected[, input$meso_var_choice]) == F, "No data for selected variable")
        )
        
        meta_meso_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$meso_var_choice, y = diversity_index_value)) +
          geom_vline(xintercept = input$treshold_line_meso, colour = "red", linetype = "dotdash") +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$meso_var_choice) +
          ylab("QBS-ar") +
          theme_bw() -> plt5
        
        ggplotly(plt5) -> plt5
        plt5$x$data[[1]]$hoverinfo <- "none"
        
        ggplotly(plt5)
      }
    } else if (is.character(meta_meso_selected[, input$meso_var_choice]) == T) {
      meta_meso_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected
      
      validate(
        need(nrow(meta_meso_selected) != 0, "No data for selected variable")
      )
      
      meta_meso_selected %>% 
        ggplot(aes_string(x = input$meso_var_choice, y = diversity_index_value)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab("QBS-ar") +
        theme_bw() +
        coord_flip() -> plt5

      ggplotly(plt5) -> plt5
      plt5$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt5)
    }
  })

  output$meso_index_table <- renderDataTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) -> meta_meso_selected
    full_join(meta_meso_selected, mesof_data_index_selected, by = "id_sampling_point") -> meta_meso_selected

    # clean data
    meta_meso_selected[meta_meso_selected == 9999] <- NA
    meta_meso_selected %>% 
    filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected

    if (is.character(meta_meso_selected[, input$meso_var_choice]) == F) {
      
      validate(
        need(is.na(meta_meso_selected[, input$meso_var_choice]) == F, "")
      )
      
      # table
      data.frame(
        Index = "QBS-ar",
        t(round(quantile(meta_meso_selected[, "diversity_index_value"],
          probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
          na.rm = T
        ), digits = 3)),
        Min = round(min(meta_meso_selected[, "diversity_index_value"],
          na.rm = T
        ), digits = 3),
        Max = round(max(meta_meso_selected[, "diversity_index_value"],
          na.rm = T
        ), digits = 3),
        Avg = round(mean(meta_meso_selected[, "diversity_index_value"],
                        na.rm = T
        ), digits = 3),
        N = length(na.omit(meta_meso_selected[, "diversity_index_value"]))
      ) -> table_data_meso_index
      colnames(table_data_meso_index) <- c(
        "Variable",
        "10th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "90th perc.",
        "Minimum value",
        "Maximum value",
        "Avg",
        "N"
      )
      
      data.table::as.data.table(table_data_meso_index)
    
      } else if (is.character(meta_meso_selected[, input$meso_var_choice]) == T) {
      levels(as.factor(meta_meso_selected[, input$meso_var_choice])) -> levels_meso_table
     
        validate(
          need(length(levels_meso_table) != 0, "")
        )
        
      # table

      df_out_loop <- data.frame()

      for (i in 1:length(levels_meso_table)) {
        filter(
          meta_meso_selected,
          meta_meso_selected[, input$meso_var_choice] == levels_meso_table[i]
        ) -> df_loop

        data.frame(
          Index = levels_meso_table[i],
          t(round(quantile(df_loop[, "diversity_index_value"],
            probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
            na.rm = T
          ), digits = 3)),
          Min = round(min(df_loop[, "diversity_index_value"], na.rm = T), digits = 3),
          Max = round(max(df_loop[, "diversity_index_value"], na.rm = T), digits = 3),
          Avg = round(mean(df_loop[, "diversity_index_value"], na.rm = T), digits = 3),
          N = length(na.omit(df_loop[, "diversity_index_value"]))
        ) -> df_out_cycle

        colnames(df_out_cycle) <- c(
          "Variable",
          "10th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "90th perc.",
          "Minimum value",
          "Maximum value",
          "Avg",
          "N"
        )

        df_out_loop <- rbind(df_out_loop, df_out_cycle)
      }

      table_data_meso_index <- df_out_loop

      data.table::as.data.table(table_data_meso_index)
    }
  })
  
  output$table_meso_correl_or_kruskal <- renderTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) -> meta_meso_selected
    full_join(meta_meso_selected, mesof_data_index_selected, by = "id_sampling_point") -> meta_meso_selected
    
    # clean data
    meta_meso_selected[meta_meso_selected == 9999] <- NA
    meta_meso_selected %>% 
      filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected
    
    if (is.character(meta_meso_selected[, input$meso_var_choice]) == F) {
      # Numerical variable: display different correlation or other like lm
     
      validate(
        need(is.na(meta_meso_selected[, input$meso_var_choice]) == F, "")
      )
      
      meta_meso_selected %>% 
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>% 
        drop_na() %>% 
        cor_test(diversity_index_value, !!as.symbol(input$meso_var_choice),method = "pearson") %>% 
        as.data.frame() -> meso_pearson
      
      meta_meso_selected %>% 
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>% 
        drop_na() %>% 
        cor_test(diversity_index_value, !!as.symbol(input$meso_var_choice), method = "spearman") %>% 
        as.data.frame() -> meso_spearman
      
      rbind(meso_pearson[,-c(1,2,6,7)], meso_spearman[,-c(1,2)]) -> df_correlation_kruskal_meso
      
      df_correlation_kruskal_meso
      
    } else if (is.character(meta_meso_selected[, input$meso_var_choice]) == T) {
      
      meta_meso_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected
      
      validate(
        need(nrow(meta_meso_selected) != 0, "")
      )
      
      # Categorical variable: display results of Kruskal wallis
      
      meso_var_choice <- input$meso_var_choice
      
      meta_meso_selected %>% 
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>% 
        drop_na() %>% 
        kruskal_test(as.formula(paste("diversity_index_value ~", meso_var_choice))) %>% 
        as.data.frame() -> df_correlation_kruskal_meso
      
      df_correlation_kruskal_meso[,-c(1,2)]
    }
  })
  
  output$tile_plot_meso <- renderPlot({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) -> meta_meso_selected
    full_join(meta_meso_selected, mesof_data_index_selected, by = "id_sampling_point") -> meta_meso_selected
    
    # clean data
    meta_meso_selected[meta_meso_selected == 9999] <- NA
    meta_meso_selected %>% 
      filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected
    
    if (is.character(meta_meso_selected[, input$meso_var_choice]) == T) {
      # Categorical variable
      
      meso_var_choice <- input$meso_var_choice
      
      validate(
        need(is.na(meta_meso_selected[, input$meso_var_choice]) == F, "")
      )
      
      meta_meso_selected %>% 
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>% 
        drop_na() %>% 
        wilcox_test(as.formula(paste("diversity_index_value", paste("~", meso_var_choice))), p.adjust.method = "bonferroni") -> meso_wilcox
      
      meso_wilcox %>% 
        as.data.frame() %>% 
        ggplot(aes(x = group1, 
                   y = group2, 
                   fill = p)) +
        geom_tile(color = "black", 
                  lwd = 0.8,
                  linetype = 1) +
        geom_text(aes(label = p), 
                  color = "black", 
                  size = 4) +
        xlab("") + 
        ylab("") + 
        scale_fill_gradient(low = "grey80", 
                            high = "white") + 
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(),
              panel.grid.major.x= element_line(colour = "black",  linetype = "dotted"),
              #panel.grid.major = element_blank(), 
              #panel.grid.minor = element_blank(), 
              panel.background = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))-> plt10
      
      plt10
      
    }
    
    
    
    
  })

  #### --- Microfauna ####

  output$plot_biodiv_index_micro <- renderPlotly({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    microf_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(c(1:3, 13, 16)) -> microf_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% microf_data_index_selected$id_sampling_point) -> meta_micro_selected
    full_join(meta_micro_selected, microf_data_index_selected, by = "id_sampling_point") -> meta_micro_selected

    if (is.character(meta_micro_selected[, input$micro_var_choice]) == F) {
      if (input$treshold_line_micro == 0 | is.na(input$treshold_line_micro) == T) {
      meta_micro_selected %>%
        as_tibble() %>%
        ggplot(aes_string(x = input$micro_var_choice, y = input$micro_index_choice)) +
        geom_point() +
        geom_smooth(method = "loess") +
        xlab(input$micro_var_choice) +
        ylab(input$micro_index_choice) +
        theme_bw() -> plt7

      ggplotly(plt7) -> plt7
      plt7$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt7)
      } else if (input$treshold_line_micro != 0) {
        meta_micro_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$micro_var_choice, y = input$micro_index_choice)) +
          geom_vline(xintercept = input$treshold_line_micro, colour = "red", linetype = "dotdash") +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$micro_var_choice) +
          ylab(input$micro_index_choice) +
          theme_bw() -> plt7
        
        ggplotly(plt7) -> plt7
        plt7$x$data[[1]]$hoverinfo <- "none"
        
        ggplotly(plt7)
      }
    } else if (is.character(meta_micro_selected[, input$micro_var_choice]) == T) {
      meta_micro_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$micro_var_choice) != "") %>%
        ggplot(aes_string(x = input$micro_var_choice, y = input$micro_index_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab(input$micro_index_choice) +
        theme_bw() +
        coord_flip() -> plt7

      ggplotly(plt7) -> plt7
      plt7$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt7)
    }
  })

  output$micro_index_table <- renderDataTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    microf_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(c(1:3, 13, 16)) -> microf_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% microf_data_index_selected$id_sampling_point) -> meta_micro_selected
    full_join(meta_micro_selected, microf_data_index_selected, by = "id_sampling_point") -> meta_micro_selected

    # clean data
    meta_micro_selected[meta_micro_selected == 9999] <- NA
    meta_micro_selected %>% 
    filter(!!as.symbol(input$micro_var_choice) != "") -> meta_micro_selected

    if (is.character(meta_micro_selected[, input$micro_var_choice]) == F) {
      # table
      data.frame(
        Index = input$micro_index_choice,
        t(round(quantile(meta_micro_selected[, input$micro_index_choice],
          probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
          na.rm = T
        ), digits = 3)),
        Min = round(min(meta_micro_selected[, input$micro_index_choice],
          na.rm = T
        ), digits = 3),
        Max = round(max(meta_micro_selected[, input$micro_index_choice],
          na.rm = T
        ), digits = 3),
        Avg = round(mean(meta_micro_selected[, input$micro_index_choice],
                        na.rm = T
        ), digits = 3),
        N = na.omit(length(meta_micro_selected[, input$micro_index_choice]))
      ) -> table_data_micro_index
      colnames(table_data_micro_index) <- c(
        "Variable",
        "10th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "90th perc.",
        "Minimum value",
        "Maximum value",
        "Avg",
        "N"
      )
      
      data.table::as.data.table(table_data_micro_index)
      
    } else if (is.character(meta_micro_selected[, input$micro_var_choice]) == T) {
      levels(as.factor(meta_micro_selected[, input$micro_var_choice])) -> levels_micro_table

      # table

      df_out_loop <- data.frame()

      for (i in 1:length(levels_micro_table)) {
        filter(
          meta_micro_selected,
          meta_micro_selected[, input$micro_var_choice] == levels_micro_table[i]
        ) -> df_loop

        data.frame(
          Index = levels_micro_table[i],
          t(round(quantile(df_loop[, input$micro_index_choice],
            probs = c(0.10, 0.25, 0.50, 0.75, 0.9),
            na.rm = T
          ), digits = 3)),
          Min = round(min(df_loop[, input$micro_index_choice], na.rm = T), digits = 3),
          Max = round(max(df_loop[, input$micro_index_choice], na.rm = T), digits = 3),
          Avg = round(mean(df_loop[, input$micro_index_choice], na.rm = T), digits = 3),
          N = length(na.omit(df_loop[, input$micro_index_choice]))
        ) -> df_out_cycle

        colnames(df_out_cycle) <- c(
          "Variable",
          "10th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "90th perc.",
          "Minimum value",
          "Maximum value",
          "Avg",
          "N"
        )

        df_out_loop <- rbind(df_out_loop, df_out_cycle)
      }

      table_data_micro_index <- df_out_loop

      data.table::as.data.table(table_data_micro_index)
    }
  })
  
  output$table_micro_correl_or_kruskal <- renderTable({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    # prepare data
    microf_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(c(1:3, 13, 16)) -> microf_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% microf_data_index_selected$id_sampling_point) -> meta_micro_selected
    full_join(meta_micro_selected, microf_data_index_selected, by = "id_sampling_point") -> meta_micro_selected
    
    # clean data
    meta_micro_selected[meta_micro_selected == 9999] <- NA
    meta_micro_selected %>% 
      filter(!!as.symbol(input$micro_var_choice) != "") -> meta_micro_selected
    
    
    if (is.character(meta_micro_selected[, input$micro_var_choice]) == F) {
      # Numerical variable: display different correlation or other like lm
      
      meta_micro_selected %>% 
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>% 
        drop_na() %>% 
        cor_test(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice),method = "pearson") %>% 
        as.data.frame() -> micro_pearson
      
      meta_micro_selected %>% 
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>% 
        drop_na() %>% 
        cor_test(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice),method = "spearman") %>% 
        as.data.frame() -> micro_spearman
      
      rbind(micro_pearson[,-c(1,2,6,7)], micro_spearman[,-c(1,2)]) -> df_correlation_kruskal_micro
      
      df_correlation_kruskal_micro
      
      
    } else if (is.character(meta_micro_selected[, input$micro_var_choice]) == T) {
      # Categorical variable: display results of Kruskal wallis
      
      micro_var_choice <- input$micro_var_choice
      micro_index_choice <- input$micro_index_choice
      
      meta_micro_selected %>% 
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>% 
        drop_na() %>% 
        kruskal_test(as.formula(paste(micro_index_choice, paste("~", micro_var_choice)))) %>% 
        as.data.frame() -> df_correlation_kruskal_micro
      
      df_correlation_kruskal_micro[,-c(1,2)]
    }
  })
  
  output$tile_plot_micro <- renderPlot({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }
    
    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }
    
    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }
    
    # prepare data
    microf_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(c(1:3, 13, 16)) -> microf_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% microf_data_index_selected$id_sampling_point) -> meta_micro_selected
    full_join(meta_micro_selected, microf_data_index_selected, by = "id_sampling_point") -> meta_micro_selected
    
    # clean data
    meta_micro_selected[meta_micro_selected == 9999] <- NA
    meta_micro_selected %>% 
      filter(!!as.symbol(input$micro_var_choice) != "") -> meta_micro_selected
    
    validate(
      need(nrow(meta_micro_selected) != 0, "")
    )
    
    if (is.character(meta_micro_selected[, input$micro_var_choice]) == T) {
      # Categorical variable
      
      micro_var_choice <- input$micro_var_choice
      micro_index_choice <- input$micro_index_choice
      
      meta_micro_selected %>% 
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>% 
        drop_na() %>% 
        wilcox_test(as.formula(paste(micro_index_choice, paste("~", micro_var_choice))), p.adjust.method = "bonferroni") -> micro_wilcox
      
      micro_wilcox %>% 
        as.data.frame() %>% 
        ggplot(aes(x = group1, 
                   y = group2, 
                   fill = p)) +
        geom_tile(color = "black", 
                  lwd = 0.8,
                  linetype = 1) +
        geom_text(aes(label = p), 
                  color = "black", 
                  size = 4) +
        xlab("") + 
        ylab("") + 
        scale_fill_gradient(low = "grey80", 
                            high = "white") + 
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(),
              panel.grid.major.x= element_line(colour = "black",  linetype = "dotted"),
              #panel.grid.major = element_blank(), 
              #panel.grid.minor = element_blank(), 
              panel.background = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))-> plt11
      
      plt11
      
    }
    
    
    
    
  })

  #### --- Biodiversity community tab:  ---####
  #### --- Fungi ---####
  #### --- Microfauna ---####
  #### --- Mesofauna ---####
  #### --- Macrofauna ---####
  
  output$plot_biodiv_abb_macro <- renderPlotly({
    if (input$type == "all" & input$manag == "all") {
      sites_selected <- metadata_MINOTAUR_selected
    } else if (input$type == "all" & input$manag != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, farming_system == input$manag)
    } else if (input$manag == "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type)
    } else if (input$manag != "all" & input$type != "all") {
      sites_selected <- subset(metadata_MINOTAUR_selected, study_landuse == input$type & farming_system == input$manag)
    }

    if (is.null(input$state_select) == T) {
      sites_selected -> sites_selected
    } else if (is.null(input$state_select) == F) {
      sites_selected %>%
        filter(country_code %in% input$state_select) -> sites_selected
    }

    if (length(input$biolevel_select) == 0) {
      sites_selected -> sites_selected
    } else {
      sample_to_keep <- c()
      for (i in 1:length(input$biolevel_select)) {
        biota_lvl <- input$biolevel_select[i]
        as.character(unlist(sample_list[biota_lvl])) -> samples_loop
        sample_to_keep <- c(sample_to_keep, samples_loop)
      }
      sites_selected <- subset(sites_selected, id_sampling_point %in% sample_to_keep)
    }

    # prepare data
    macrof_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(-2) -> macro_data_selected

    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% macro_data_selected$id_sampling_point) -> meta_macro_abb_selected
    full_join(meta_macro_abb_selected, macro_data_selected, by = "id_sampling_point") -> meta_macro_abb_selected
    
    abundance <- "abundance"


    if (is.character(meta_macro_abb_selected[, input$macro_var_choice_abb]) == F) {
      if (input$macro_taxon_choice == "All macrofauna") {

        meta_macro_abb_selected %>%
          select(c("id_sampling_point","abundance")) %>%
          group_by(id_sampling_point) %>%
          summarise_all(mean) %>% 
          ungroup() %>% 
          as.data.frame() -> meta_macro_abb_average
        
        metadata_MINOTAUR_selected %>% 
          filter(id_sampling_point %in% meta_macro_abb_average$id_sampling_point) -> metadata_macro_average
        
        full_join(meta_macro_abb_average, metadata_macro_average, by = "id_sampling_point") -> meta_macro_abb_average
        
        meta_macro_abb_average %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$macro_var_choice_abb, y = abundance)) +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$macro_var_choice_abb) +
          ylab(paste0(input$macro_taxon_choice, " Abundance")) +
          theme_bw() -> plt8
        
        ggplotly(plt8) -> plt8
        plt8$x$data[[1]]$hoverinfo <- "none"
        
        ggplotly(plt8)


      } else if (input$macro_taxon_choice != "All macrofauna") {

      filter(
        meta_macro_abb_selected,
        meta_macro_abb_selected$taxon == input$macro_taxon_choice
      ) -> df_macro_abb

      df_macro_abb %>%
        as_tibble() %>%
        ggplot(aes_string(x = input$macro_var_choice_abb, y = abundance)) +
        geom_point() +
        geom_smooth(method = "loess") +
        xlab(input$macro_var_choice_abb) +
        ylab(paste0(input$macro_taxon_choice, " Abundance")) +
        theme_bw() -> plt8

      ggplotly(plt8) -> plt8
      plt8$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt8)
      }
    } else if (is.character(meta_macro_abb_selected[, input$macro_var_choice_abb]) == T) {
      if (input$macro_taxon_choice == "All macrofauna") {
        
        meta_macro_abb_selected %>%
          select(c("id_sampling_point","abundance")) %>%
          group_by(id_sampling_point) %>%
          summarise_all(mean) %>% 
          ungroup() %>% 
          as.data.frame()  -> meta_macro_abb_average
        
        metadata_MINOTAUR_selected %>% 
          filter(id_sampling_point %in% meta_macro_abb_average$id_sampling_point) -> metadata_macro_average
        
        full_join(meta_macro_abb_average, metadata_macro_average, by = "id_sampling_point") -> meta_macro_abb_average
        
        meta_macro_abb_average %>%
          as_tibble() %>%
          filter(!!as.symbol(input$macro_var_choice_abb) != "") %>% 
          ggplot(aes_string(x = input$macro_var_choice_abb, y = abundance)) +
          geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
          geom_boxplot(alpha = 0.5) +
          xlab(input$macro_var_choice_abb) +
          ylab(paste0(input$macro_taxon_choice, " Abundance")) +
          theme_bw() +
          coord_flip() -> plt8
        
        ggplotly(plt8) -> plt8
        plt8$x$data[[1]]$hoverinfo <- "none"
        
        ggplotly(plt8)
        
        
      } else if (input$macro_taxon_choice != "All macrofauna") {
      filter(
        meta_macro_abb_selected,
        meta_macro_abb_selected$taxon == input$macro_taxon_choice
      ) -> df_macro_abb


      df_macro_abb %>%
        as_tibble() %>%
        filter(!!as.symbol(input$macro_var_choice_abb) != "") %>% 
        ggplot(aes_string(x = input$macro_var_choice_abb, y = abundance)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab(input$macro_var_choice_abb) +
        ylab(paste0(input$macro_taxon_choice, " Abundance")) +
        theme_bw() +
        coord_flip() -> plt8

      ggplotly(plt8) -> plt8
      plt8$x$data[[1]]$hoverinfo <- "none"

      ggplotly(plt8)
      }
    }
  })
  
}

######################################################################################################
## --- Create Shiny app ---##
shinyApp(ui = ui, server = server)
