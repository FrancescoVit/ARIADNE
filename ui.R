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
                  h4("There are two main modules in ARIADNE: The Overview and the Analysis."),
                  h4("The Overview module is used to filter the database records on the basis of different metadata (i.e. land use, management, 
                     country, and biota group) and to visualize the number of selected records, their geographical distribution, and their
                     distribution per Member State. Records that are filtered in the Overview module, are used to display analysis in the Analysis module"),
                  h4("The Analysis module is used to show the actual analysis on the selected records. Here analysis are organized based on different indices
                     and on different level of soil biota")
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
                                   have until 17 December 2028 to transpose it into national law."),
                                p("The Directive was proposed by the European Commission in July 2023 as part of the EU
                                   Soil Strategy for 2030 and the European Green Deal, in response to evidence that a
                                   large share of European soils are in an unhealthy condition. It sets out a common list
                                   of soil descriptors (Annex I), covering physical, chemical and biological properties,
                                   grouped into four parts depending on how their assessment criteria are set. For
                                   descriptors with defined criteria, Member States - not the Directive itself - establish
                                   non-binding Sustainable Target Values and Operational Trigger Values. The long-term,
                                   non-binding ambition is healthy soils across the EU by 2050."),
                                br(),
                                h4("Annex I descriptors and their coverage in MINOTAUR"),
                                p(em("The last column indicates whether an equivalent variable is available in the
                                      MINOTAUR database used by this app.")),
                                tags$table(
                                    class = "table table-bordered table-striped",
                                    tags$thead(
                                        tags$tr(
                                            tags$th("Variable"), tags$th("Description"),
                                            tags$th("Threshold"), tags$th("In MINOTAUR")
                                        )
                                    ),
                                    tags$tbody(
                                        tags$tr(tags$td(colspan = 4, tags$strong(
                                            "Part A - descriptors with criteria established at EU level (salinisation, loss of SOC, subsoil compaction)"
                                        ))),
                                        tags$tr(tags$td("Electrical conductivity (EC)"), tags$td("Salinisation"),
                                                tags$td("EU-mandated descriptor; STV/OTV value set nationally"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Soil organic carbon (SOC) concentration"), tags$td("Loss of soil organic carbon"),
                                                tags$td("EU-mandated descriptor; STV/OTV value set nationally"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Bulk density (subsoil)"), tags$td("Subsoil compaction"),
                                                tags$td("EU-mandated descriptor; STV/OTV value set nationally"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓*"))),

                                        tags$tr(tags$td(colspan = 4, tags$strong(
                                            "Part B - descriptors with criteria established by Member States (phosphorus, erosion, contamination, water retention/infiltration, air capacity, SOC stock)"
                                        ))),
                                        tags$tr(tags$td("Available phosphorus"), tags$td("Nutrient/fertility status"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Soil erosion rate"), tags$td("Water/wind erosion"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Soil contamination (heavy metals)"), tags$td("Chemical contamination"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Water holding capacity"), tags$td("Water regulation"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Saturated hydraulic conductivity"), tags$td("Water infiltration"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Air capacity"), tags$td("Soil aeration"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Soil organic carbon stock"), tags$td("Loss of organic carbon (stock, not concentration)"),
                                                tags$td("Set by Member States"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),

                                        tags$tr(tags$td(colspan = 4, tags$strong(
                                            "Part C - descriptors without criteria, monitored only (excess nutrients, acidification, topsoil compaction, biodiversity, PFAS/pesticide contamination)"
                                        ))),
                                        tags$tr(tags$td("Total nitrogen"), tags$td("Nutrient excess/deficiency"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Carbon/nitrogen ratio (C/N)"), tags$td("Nutrient cycling, organic matter quality"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("pH"), tags$td("Acidification"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Soil biodiversity (at least one descriptor: metabarcoding, PLFA, organism abundance)"),
                                                tags$td("Loss of soil biodiversity"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓"))),
                                        tags$tr(tags$td("Bulk density (topsoil)"), tags$td("Topsoil compaction"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#2e7d32; font-weight:bold;", "✓*"))),
                                        tags$tr(tags$td("Soil contamination (PFAS, pesticides and metabolites)"), tags$td("Emerging contaminants"),
                                                tags$td("No threshold"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),

                                        tags$tr(tags$td(colspan = 4, tags$strong(
                                            "Part D - soil sealing and soil removal indicators"
                                        ))),
                                        tags$tr(tags$td("Soil sealing indicator"), tags$td("Soil imperviousness"),
                                                tags$td("Annual monitoring, no numeric threshold"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗"))),
                                        tags$tr(tags$td("Soil removal / land take indicator"), tags$td("Land consumption"),
                                                tags$td("Annual monitoring, no numeric threshold"), tags$td(tags$span(style = "color:#c62828; font-weight:bold;", "✗")))
                                    )
                                ),
                                p(em("* MINOTAUR records a single bulk density value that does not distinguish
                                      topsoil from subsoil layers.")),
                                p(em("For Part A, \"established at Union level\" means the Directive makes the
                                      descriptor and its measurement methodology (Annex II) mandatory for every
                                      Member State - not that the EU fixes a single numeric threshold. As with
                                      Part B, the actual Sustainable Target Value and Operational Trigger Value
                                      for each descriptor are still set nationally; the final Directive text does
                                      not specify EU-wide numeric thresholds for any descriptor."))
                            )
                        )
                    )
                )
            ) # close third tab content
        ) # close tabitem
    ) # close dashboardBody
) # close dashboardPage
