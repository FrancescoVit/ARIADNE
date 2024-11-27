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

###################################################################

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
