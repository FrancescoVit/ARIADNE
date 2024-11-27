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

#################################################################################################################


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
        filter(!!as.symbol(input$fungi_var_choice) != "") -> meta_fungi_selected

      validate(
        need(nrow(meta_fungi_selected) != 0, "No data for selected variable")
      )

      meta_fungi_selected %>%
        ggplot(aes_string(x = input$fungi_var_choice, y = input$fungi_index_choice)) +
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
        Min = round(
          min(meta_fungi_selected[, input$fungi_index_choice],
            na.rm = T
          ),
          digits = 3
        ),
        Max = round(
          max(meta_fungi_selected[, input$fungi_index_choice],
            na.rm = T
          ),
          digits = 3
        ),
        Avg = round(
          mean(meta_fungi_selected[, input$fungi_index_choice],
            na.rm = T
          ),
          digits = 3
        ),
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
        cor_test(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice), method = "pearson") %>%
        as.data.frame() -> fungi_pearson

      meta_fungi_selected %>%
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>%
        drop_na() %>%
        cor_test(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice), method = "spearman") %>%
        as.data.frame() -> fungi_spearman

      rbind(fungi_pearson[, -c(1, 2, 6, 7)], fungi_spearman[, -c(1, 2)]) -> df_correlation_kruskal_fungi

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

      df_correlation_kruskal_fungi[, -c(1, 2)]
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
        ggplot(aes(
          x = group1,
          y = group2,
          fill = p
        )) +
        geom_tile(
          color = "black",
          lwd = 0.8,
          linetype = 1
        ) +
        geom_text(aes(label = p),
          color = "black",
          size = 4
        ) +
        xlab("") +
        ylab("") +
        scale_fill_gradient(
          low = "grey80",
          high = "white"
        ) +
        theme(
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank()
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt9

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
        cor_test(diversity_index_value, !!as.symbol(input$meso_var_choice), method = "pearson") %>%
        as.data.frame() -> meso_pearson

      meta_meso_selected %>%
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>%
        drop_na() %>%
        cor_test(diversity_index_value, !!as.symbol(input$meso_var_choice), method = "spearman") %>%
        as.data.frame() -> meso_spearman

      rbind(meso_pearson[, -c(1, 2, 6, 7)], meso_spearman[, -c(1, 2)]) -> df_correlation_kruskal_meso

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

      df_correlation_kruskal_meso[, -c(1, 2)]
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
        ggplot(aes(
          x = group1,
          y = group2,
          fill = p
        )) +
        geom_tile(
          color = "black",
          lwd = 0.8,
          linetype = 1
        ) +
        geom_text(aes(label = p),
          color = "black",
          size = 4
        ) +
        xlab("") +
        ylab("") +
        scale_fill_gradient(
          low = "grey80",
          high = "white"
        ) +
        theme(
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank()
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt10

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
        cor_test(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice), method = "pearson") %>%
        as.data.frame() -> micro_pearson

      meta_micro_selected %>%
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>%
        drop_na() %>%
        cor_test(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice), method = "spearman") %>%
        as.data.frame() -> micro_spearman

      rbind(micro_pearson[, -c(1, 2, 6, 7)], micro_spearman[, -c(1, 2)]) -> df_correlation_kruskal_micro

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

      df_correlation_kruskal_micro[, -c(1, 2)]
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
        ggplot(aes(
          x = group1,
          y = group2,
          fill = p
        )) +
        geom_tile(
          color = "black",
          lwd = 0.8,
          linetype = 1
        ) +
        geom_text(aes(label = p),
          color = "black",
          size = 4
        ) +
        xlab("") +
        ylab("") +
        scale_fill_gradient(
          low = "grey80",
          high = "white"
        ) +
        theme(
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank()
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt11

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
          select(c("id_sampling_point", "abundance")) %>%
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
          select(c("id_sampling_point", "abundance")) %>%
          group_by(id_sampling_point) %>%
          summarise_all(mean) %>%
          ungroup() %>%
          as.data.frame() -> meta_macro_abb_average

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
