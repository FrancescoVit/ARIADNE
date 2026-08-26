# shARed mInotAur Database exploratioN Environment : ARIADNE
# Version: 4.1.1
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
## --- Setting UI ---##
######################################################################################################


ui <- dashboardPage(
    #### --- Define some appearance details ---####

    ## --- Define dashboard header ---##
    dashboardHeader(
        title = "ARIADNE",
        titleWidth = 230
    ),

    ## --- Define dashboard sidebar ---##
    dashboardSidebar(
        width = 230,
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("house")),
            menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
            menuItem("Analysis", tabName = "Analysis", icon = icon("magnifying-glass-chart")),
            menuItem("SML", tabName = "SML", icon = icon("gavel"))
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
            #### --- Home tab content: landing/home page ---####
            tabItem(
              tabName = "Home",
              fluidRow(
                box(
                  width = 12,
                  align = "center",
                  h1("Welcome to ARIADNE version 4.1.1"),
                  br(),
                  br(),
                  h4("Welcome to the shARed mInotAur Database exploratioN Environment, or in short ARIADNE."),
                  br(),
                  h4("This application was developed as part of the actions of MINOTAUR project, under the EJP Soil program. 
                     The objective of ARIADNE is to provide an easy to use user interface for the exploration of Europena soil 
                     biodiversity data that were collected and harmonized during the MINOTAUR project"), # maybe not proper as it is using HTML header
                  br(),
                  br(),
                  br(),
                  h4("There are three main modules in ARIADNE: The Overview, the Analysis, and the SML module."),
                  h4("The Overview module is used to filter the database records on the basis of different metadata (i.e. land use, management,
                     country, and biota group) and to visualize the number of selected records, their geographical distribution, and their
                     distribution per Member State. Records that are filtered in the Overview module, are used to display analysis in the Analysis module"),
                  h4("The Analysis module is used to show the actual analysis on the selected records. Here analysis are organized based on different indices
                     and on different level of soil biota"),
                  h4("The SML module provides background information on the EU Soil Monitoring Law (Directive (EU) 2025/2360) and its Annex I soil
                     descriptors, together with a Scenario Testing tool where you can define and compare different sets of national thresholds and,
                     optionally, test them against your own uploaded sample data")
                  )
                ),
              fluidRow(
                box(
                  width = 12,
                  align = "center",
                  uiOutput("logo_MINOTAUR",width = "50%")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  align = "center",
                  #h6 ("For further information visit the"),
                  fluidRow(
                  a(
                    "For further information on ARIADNE app visit the wiki page",
                    href = "https://github.com/FrancescoVit/ARIADNE/wiki"
                    )),
                  fluidRow(
                    a(
                      "For leaving feedbacks, comments or opinion, you can use the issue track system in GitHub",
                      href = "https://github.com/FrancescoVit/ARIADNE/issues"
                    )),
                  fluidRow(
                  a(
                    "For further information on the MINOTAUR project check this",
                    href = "https://projects.au.dk/ejpsoil/soil-research/minotaur"
                  )
                  )
                  )
                )
              ),
          
            #### --- Overview tab content ---####
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
                                    choices = list( 
                                        "Bacteria" = "bact",
                                        "Fungi" = "fung",
                                        "Microfauna" = "micro",
                                        "Mesofauna" = "meso",
                                        "Macrofauna" = "macro"
                                    ),
                                    selected = c("bact","fung", "micro", "meso", "macro"), inline = T
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
                            #### --- Biodiversity and Ecological indices panel ---####
                            tabPanel(
                                title = "Biodiversity and Ecological indices", value = "t_biodiv_index",
                                #### --- Bacteria row ---####
                                fluidRow( # Bacteria
                                  box(
                                    title = "Bacteria (Metabarcoding)",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    collapsed = T,
                                    width = 12,
                                    fluidRow(
                                      column(
                                        width = 6,
                                        selectInput("bacteria_index_choice",
                                                    label = "Select index to plot:",
                                                    c(
                                                      "Chao1" = "bacteria_chao1_index",
                                                      "Fisher alpha" = "bacteria_fisher_alpha",
                                                      "Shannon index" = "bacteria_shannon_index",
                                                      "Simpson index" = "bacteria_simpson_index",
                                                      "Evenness" = "bacteria_evenness_index",
                                                      "Richness" = "bacteria_richness_index",
                                                      "Inverse Simpson index" = "bacteria_inverse_simpson_index"
                                                    )
                                        )
                                      ),
                                      column(
                                        width = 6,
                                        selectInput("bacteria_var_choice", "Select variable to plot:", common_vars)
                                        ),
                                    ),
                                    numericInput("treshold_line_bact",
                                                 "Optionally set value to draw a line:  (only for numerical variables)",
                                                 value = 0,
                                                 min = 0,
                                                 max = Inf,
                                                 step = 0.001
                                    ),
                                    fluidRow(
                                      column(
                                        width = 12, align = "center",
                                        plotOutput("plot_biodiv_index_bacteria",
                                                     width = "100%"
                                        )
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width = 12,
                                        align = "center",
                                        dataTableOutput("bacteria_index_table")
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width = 3, align = "left",
                                        tableOutput("table_bacteria_correl_or_kruskal")
                                      ),
                                      column(
                                        width = 9, align = "left",
                                        plotOutput("tile_plot_bacteria")
                                      )
                                    )
                                  )
                                ),
                                #### --- Fungi row ---####
                                fluidRow( # Fungi
                                    box(
                                        title = "Fungi (Metabarcoding)",
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
                                                        "Inverse Simpson index" = "fungi_inverse_simpson_index"))
                                            ),
                                            column(
                                              width = 6,
                                              selectInput("fungi_var_choice", "Select variable to plot:", common_vars))
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
                                                plotOutput("plot_biodiv_index_fungi",
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
                                                width = 3, align = "left",
                                                tableOutput("table_fungi_correl_or_kruskal")
                                            ),
                                            column(
                                                width = 9, align = "left",
                                                plotOutput("tile_plot_fungi")
                                            )
                                        )
                                    )
                                ),
                                #### --- Microfauna row ---####
                                fluidRow( # Microfauna
                                    box(
                                        title = "Microfauna (Nematodes)",
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
                                              selectInput("micro_var_choice", "Select variable to plot:", common_vars)
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
                                                plotOutput("plot_biodiv_index_micro",
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
                                                width = 3, align = "left",
                                                tableOutput("table_micro_correl_or_kruskal")
                                            ),
                                            column(
                                                width = 9, align = "left",
                                                plotOutput("tile_plot_micro")
                                            )
                                        )
                                    )
                                ),
                                #### --- Mesofauna row ---####
                                fluidRow( # Mesofauna
                                    box(
                                        title = "Mesofauna (Microarthropods)",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = T,
                                        width = 12,
                                        fluidRow(
                                          column(
                                            width = 6,
                                            selectInput("meso_index_choice",
                                                        label = "Select index to plot:",
                                                        c(
                                                          "QBS-ar" = "diversity_index_value",
                                                          "Biological form richness" = "BF_richness"
                                                        )
                                            )
                                          ),
                                          column(
                                            width = 6,
                                            selectInput("meso_var_choice", "Select variable to plot:", common_vars)
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
                                                plotOutput("plot_biodiv_index_meso",
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
                                                width = 3, align = "left",
                                                tableOutput("table_meso_correl_or_kruskal")
                                            ),
                                            column(
                                                width = 9, align = "left",
                                                plotOutput("tile_plot_meso")
                                            )
                                        )
                                    )
                                ),
                                
                                #### --- Macrofauna ####
                                
                                fluidRow( # Macrofauna
                                  box(
                                    title = "Macrofauna (Earthworms)",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    collapsed = T,
                                    width = 12,
                                    fluidRow(
                                      column(
                                        width = 6,
                                        selectInput("macro_var_choice", "Select variable to plot:", common_vars)
                                      )
                                      ),
                                    numericInput("treshold_line_macro",
                                                 "Optionally set value to draw a line:  (only for numerical variables)",
                                                 value = 0,
                                                 min = 0,
                                                 max = Inf,
                                                 step = 0.001
                                    ),
                                    fluidRow(
                                      column(
                                        width = 12, align = "center",
                                        plotOutput("plot_biodiv_index_macro",
                                                     width = "100%"
                                        )
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width = 12, align = "center",
                                        plotOutput("barplot_biodiv_index_macro",
                                                     width = "100%"
                                        )
                                      )
                                    )
                                    )
                                  )
                                ),

                            #### --- Close previously open ---####
                        ) # close Tabsetpanel
                    ) # close box
                ) # close fluidrow
            ), # close second tab content

            #### --- Third tab content: SML tab ---####
            tabItem(
                tabName = "SML",
                fluidRow(
                    box(
                        width = 12,
                        tabsetPanel(
                            type = "tabs", id = "sml_tab",
                            #### --- General Information panel ---####
                            tabPanel(
                                title = "General Information", value = "t_sml_info",
                                br(),
                                h4("Sources"),
                                tags$ul(
                                    tags$li(tags$a(href = "https://eur-lex.europa.eu/eli/dir/2025/2360/oj/eng",
                                                   target = "_blank",
                                                   "Directive (EU) 2025/2360 - official text (EUR-Lex)")),
                                    tags$li(tags$a(href = "https://mission-soil-platform.ec.europa.eu/news-events/latest-news/soil-monitoring-law-published-eu-official-journal",
                                                   target = "_blank",
                                                   "European Commission - Mission Soil Platform: the Soil Monitoring Law is published"))
                                ),
                                p("The EU Soil Monitoring Law (Directive (EU) 2025/2360) is the first Union-wide legal
                                   framework for monitoring, assessing and managing soil health. It was approved by the
                                   European Parliament and the Council on 12 November 2025, published in the Official
                                   Journal on 26 November 2025, and entered into force on 16 December 2025. Member States
                                   have three years from entry into force to transpose it into national law."),
                                p("The Directive was proposed by the European Commission in July 2023 as part of the EU
                                   Soil Strategy for 2030 and the European Green Deal, in response to evidence that a
                                   large share of European soils are in an unhealthy condition. It sets out a common list
                                   of soil descriptors (Annex I), covering physical, chemical and biological properties,
                                   grouped into four parts depending on how their assessment criteria are set. For Part A
                                   descriptors, the non-binding sustainable target values are fixed directly in Annex I
                                   (e.g. electrical conductivity below 4 dS/m); for Part B, and for the Operational
                                   Trigger Values that apply across Parts A and B, each Member State sets the actual
                                   numeric value within the framework the Directive establishes. The long-term,
                                   non-binding ambition is healthy soils across the EU by 2050."),
                                br(),
                                h4("Annex I descriptors"),
                                p(em("Transcribed directly from Annex I of Directive (EU) 2025/2360. The last column
                                      indicates whether an equivalent variable is available in the MINOTAUR database
                                      used by this app.")),
                                tags$table(
                                    class = "table table-bordered table-striped",
                                    style = "border-spacing: 0 6px; border-collapse: separate;",
                                    tags$thead(
                                        tags$tr(
                                            tags$th("Variable"), tags$th("Description"),
                                            tags$th("Threshold"), tags$th("Coverage in MINOTAUR DB")
                                        )
                                    ),
                                    tags$tbody(
                                        tags$tr(tags$td(colspan = 4, style = "background-color:#dbe9f6; padding-top:14px; padding-bottom:14px;", tags$strong(
                                            "Part A - descriptors with criteria for healthy soil condition established at Union level"
                                        ))),
                                        tags$tr(tags$td("Electrical conductivity (EC)"), tags$td("Salinisation"),
                                                tags$td("< 4 dS/m (saturated soil paste extract) or equivalent. Exempt: naturally saline land, areas with regular marine flooding, sea-spray areas"),
                                                tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td(style = "padding-left: 28px;", "SOC concentration - Mineral soils"), tags$td("Loss of soil organic carbon"),
                                                tags$td("SOC/clay ratio > 1/13"),
                                                tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓†"))),
                                        tags$tr(tags$td(style = "padding-left: 28px;", "SOC concentration - Organic soils"), tags$td("Loss of soil organic carbon"),
                                                tags$td("National targets set under Regulation (EU) 2024/1991"),
                                                tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Bulk density (subsoil)"), tags$td("Subsoil compaction"),
                                                tags$td("< 1.47-1.80 g/cm3 depending on soil texture class (5 classes defined). Optional: saturated hydraulic conductivity ≥ 10 cm/day, air capacity ≥ 5%"),
                                                tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓*"))),

                                        tags$tr(tags$td(colspan = 4, style = "background-color:#e2f0e3; padding-top:14px; padding-bottom:14px;", tags$strong(
                                            "Part B - descriptors with criteria for healthy soil condition established at Member State level"
                                        ))),
                                        tags$tr(tags$td("Extractable phosphorus"), tags$td("Excess nutrient content"),
                                                tags$td("< maximum value set by each Member State"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Soil erosion rate"), tags$td("Soil erosion"),
                                                tags$td("< maximum value set by each Member State"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Heavy metals (As, Sb, Cd, Co, Cr, Cu, Hg, Pb, Ni, Tl, V, Zn) and selected organic contaminants"), tags$td("Soil contamination"),
                                                tags$td("Risk-based: no unacceptable risk to human health/environment (assessed by Member States)"),
                                                tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Soil water holding capacity"), tags$td("Reduced water retention"),
                                                tags$td("Above minimal threshold set by Member State"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Saturated hydraulic conductivity and air capacity"), tags$td("Reduced water infiltration"),
                                                tags$td("Above minimal threshold set by Member State"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("SOC stocks"), tags$td("Loss of SOC (stock, not concentration)"),
                                                tags$td("Contribute to national LULUCF targets; > minimum value by soil texture, set by Member State"),
                                                tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),

                                        tags$tr(tags$td(colspan = 4, style = "background-color:#faf0dc; padding-top:14px; padding-bottom:14px;", tags$strong(
                                            "Part C - descriptors without criteria"
                                        ))),
                                        tags$tr(tags$td("Total nitrogen content / SOC-to-nitrogen ratio"), tags$td("Excess nutrient content"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓†"))),
                                        tags$tr(tags$td("Soil pH (optional: base saturation)"), tags$td("Acidification"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Bulk density (topsoil)"), tags$td("Topsoil compaction"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓*"))),
                                        tags$tr(tags$td("DNA metabarcoding for fungi and bacteria (mandatory); optional: nematode/earthworm/springtail/ant abundance and diversity, QBS-ar, PLFA, archaea/protist/animal metabarcoding, basal respiration, invasive species"),
                                                tags$td("Loss of soil biodiversity"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓✓✓"))),
                                        tags$tr(tags$td("PFAS-21/PFAS-43, pesticide active substances and metabolites"), tags$td("Soil contamination (emerging)"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),

                                        tags$tr(tags$td(colspan = 4, style = "background-color:#e8e6f0; padding-top:14px; padding-bottom:14px;", tags$strong(
                                            "Part D - soil sealing and soil removal indicators"
                                        ))),
                                        tags$tr(tags$td("Sealed soil and soil-removal area (km2, % of Member State surface)"), tags$td("Soil sealing and removal"),
                                                tags$td("No numeric threshold - tracked over time"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Settlement area and land-use change to/from settlement (km2, % of Member State surface)"), tags$td("Land take"),
                                                tags$td("No numeric threshold - tracked over time"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗")))
                                    )
                                ),
                                p(em("* MINOTAUR records a single bulk density value that does not distinguish
                                      topsoil from subsoil layers.")),
                                p(em("† MINOTAUR has the underlying variables (SOC and clay content; total nitrogen
                                      and the C/N ratio) but does not compute the exact SOC-to-clay or
                                      SOC-to-nitrogen ratios used by the Directive's criteria.")),
                                p(em("For Part B, and for the water/erosion/contamination criteria, the Directive
                                      requires each Member State to set the actual numeric maximum, minimum or
                                      threshold value; for Part A, the numeric criteria shown above are fixed
                                      directly in Annex I of the Directive itself."))
                            ), # close General Information tabPanel

                            #### --- Scenario Testing panel ---####
                            tabPanel(
                                title = "Scenario Testing", value = "t_sml_scenario",
                                fluidRow(
                    box(
                        width = 12,
                        title = "Part A - fixed EU-level criteria",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        p(em("Identical in every scenario - set directly by Annex I of Directive (EU) 2025/2360.
                              Bulk density uses a single representative soil texture class (sand/loamy sand/sandy
                              loam/loam) for this first version.")),
                        tags$table(
                            class = "table table-bordered table-striped",
                            tags$thead(tags$tr(
                                tags$th("Variable"), tags$th("Unit"), tags$th("EU value"), tags$th("Direction")
                            )),
                            tags$tbody(
                                tags$tr(tags$td("Electrical conductivity"), tags$td("dS/m"), tags$td("4"), tags$td("Healthy ≤")),
                                tags$tr(tags$td("SOC/clay ratio"), tags$td("ratio"), tags$td("0.077 (1/13)"), tags$td("Healthy ≥")),
                                tags$tr(tags$td("Bulk density (subsoil)"), tags$td("g/cm3"), tags$td("1.80"), tags$td("Healthy ≤"))
                            )
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Part B - your threshold scenarios",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        actionButton("add_scenario", "+ Add scenario", icon = icon("plus")),
                        p(em("Up to 8 scenarios (one color each). Fill in all six values for a scenario for it to
                              appear on the plot below.")),
                        uiOutput("scenario_boxes")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Upload your own samples (session-only, never saved)",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        p(em("Optional. Upload a CSV of your own samples to plot them as points against the
                              scenarios above. The file is only held in your browser session - it is never
                              written to disk or shared, and disappears when the session ends.")),
                        tags$a(href = "sml_scenario_template.csv", download = NA,
                               icon("download"), " Download CSV template"),
                        br(), br(),
                        fileInput("sample_upload", "Upload CSV", accept = ".csv"),
                        uiOutput("sample_upload_warnings")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Healthy soil space",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        radioButtons("scenario_view_mode", label = NULL,
                                     choices = c("Overlay" = "overlay", "Side by side" = "facet"),
                                     selected = "overlay", inline = TRUE),
                        plotOutput("scenario_radar_plot", height = "600px")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Sample classification per scenario",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        p(em("For each uploaded sample and each scenario: green = meets that variable's threshold,
                              red = fails it, grey = variable not available for that sample. The last column
                              counts healthy/unhealthy/undetermined variables for that sample under that scenario.")),
                        uiOutput("sample_classification_table")
                    )
                )
                            ) # close Scenario Testing tabPanel
                        ) # close Tabsetpanel
                    ) # close box
                ) # close fluidrow
            ) # close third tab content
        ) # close tabitem
    ) # close dashboardBody
) # close dashboardPage
