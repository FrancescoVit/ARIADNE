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
## --- Setting server ---##
######################################################################################################


server <- function(input, output) {
  
  #### --- Import some images for logo and similar ---####

  output$logo_MINOTAUR <- renderUI({
    tags$div(
      style = "text-align:center;",
      tags$img(
        src = "https://raw.githubusercontent.com/FrancescoVit/ARIADNE/main/src/MINOTAUR_Logo_PNG.png",
        style = "max-width:100%; height:auto;"
      )
    )
  })
  
  #### --- Setting up a reactive filtering ---####

  sites_selected_reactive <- reactive({
    
    df <- metadata_MINOTAUR_selected
    
    if (input$type != "all") {
      df <- df[df$study_landuse == input$type, ]
    }
    if (input$manag != "all") {
      df <- df[df$farming_system == input$manag, ]
    }
    
    if (!is.null(input$state_select)) {
      df <- dplyr::filter(df, country_code %in% input$state_select)
    }

    biota_lvls <- input$biolevel_select
    
    if (is.null(biota_lvls) || length(biota_lvls) == 0) {
      return(data.frame())
    }
    
    sample_to_keep <- unlist(sample_list[biota_lvls])
    
    df <- df[df$id_sampling_point %in% sample_to_keep, ]
    
  })
  
  
  #### --- Text or notification box indicating n of samples selected by filters ---####

  output$info_box_total <- renderUI({
    
    sites_selected <- sites_selected_reactive()

    infoBox("Total selected records:",
      width = "100%",
      nrow(sites_selected),
      icon = icon("vial"),
      fill = TRUE
    )
  })

  output$info_box_selected <- renderUI({
    
    sites_selected <- sites_selected_reactive()


    infoBox("Biota group selected records:",
      width = "100%",
      nrow(sites_selected),
      icon = icon("vial-circle-check"),
      fill = TRUE
    )
  })

  #### --- Overview on map of samples selected by filters ---####
  output$plot_map_overview <- renderPlot({
    
    sites_selected <- sites_selected_reactive()
    
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
   
    sites_selected <- sites_selected_reactive()

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
  #### --- Bacteria ---####
  
  output$plot_biodiv_index_bacteria <- renderPlot({
  
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    bacteria_data %>%
      #select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> bact_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% bact_data_index_selected$id_sampling_point) -> meta_bact_selected
    full_join(meta_bact_selected, bact_data_index_selected, by = "id_sampling_point") -> meta_bact_selected
    
    # clean data
    meta_bact_selected[meta_bact_selected == 9999] <- NA
    
    # plot
    if (is.character(meta_bact_selected[, input$bacteria_var_choice]) == F) {
      if (input$treshold_line_bact == 0 | is.na(input$treshold_line_bact) == T) {
        validate(
          need(is.na(meta_bact_selected[, input$bacteria_var_choice]) == F, "No data for selected variable")
        )
        
        meta_bact_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$bacteria_var_choice, y = input$bacteria_index_choice)) +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$bacteria_var_choice) +
          ylab(input$bacteria_index_choice) +
          theme_bw() -> plt12

        plt12
      } else if (input$treshold_line_bact != 0) {
        validate(
          need(is.na(meta_bact_selected[, input$bacteria_var_choice]) == F, "No data for selected variable")
        )
        
        meta_bact_selected %>%
          as_tibble() %>%
          ggplot(aes_string(x = input$bacteria_var_choice, y = input$bacteria_index_choice)) +
          geom_vline(xintercept = input$treshold_line_bact, colour = "red", linetype = "dotdash") +
          geom_point() +
          geom_smooth(method = "loess") +
          xlab(input$bacteria_var_choice) +
          ylab(input$bacteria_index_choice) +
          theme_bw() -> plt12

        plt12
      }
    } else if (is.character(meta_bact_selected[, input$bacteria_var_choice]) == T) {
      meta_bact_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$bacteria_var_choice) != "") -> meta_bact_selected
      
      validate(
        need(nrow(meta_bact_selected) != 0, "No data for selected variable")
      )
      
      meta_bact_selected %>%
        ggplot(aes_string(x = input$bacteria_var_choice, y = input$bacteria_index_choice, fill = input$bacteria_var_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab(input$bacteria_index_choice) +
        theme_bw() +
        coord_flip() -> plt12

      plt12
    }
  })

  output$bacteria_index_table <- renderDataTable({
   
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    bacteria_data %>%
      #select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> bacteria_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% bacteria_data_index_selected$id_sampling_point) -> meta_bacteria_selected
    full_join(meta_bacteria_selected, bacteria_data_index_selected, by = "id_sampling_point") -> meta_bacteria_selected
    
    # clean data
    meta_bacteria_selected[meta_bacteria_selected == 9999] <- NA
    meta_bacteria_selected %>%
      filter(!!as.symbol(input$bacteria_var_choice) != "") -> meta_bacteria_selected
    
    validate(
      need(nrow(meta_bacteria_selected) != 0, "")
    )
    
    
    if (is.character(meta_bacteria_selected[, input$bacteria_var_choice]) == F) {
      data.frame(
        Index = input$bacteria_index_choice,
        t(round(quantile(meta_bacteria_selected[, input$bacteria_index_choice],
                         probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
                         na.rm = T
        ), digits = 3)),
        Min = round(
          min(meta_bacteria_selected[, input$bacteria_index_choice],
              na.rm = T
          ),
          digits = 3
        ),
        Max = round(
          max(meta_bacteria_selected[, input$bacteria_index_choice],
              na.rm = T
          ),
          digits = 3
        ),
        Avg = round(
          mean(meta_bacteria_selected[, input$bacteria_index_choice],
               na.rm = T
          ),
          digits = 3
        ),
        N = length(na.omit(meta_bacteria_selected[, input$bacteria_index_choice]))
      ) -> table_data_bacteria_index
      colnames(table_data_bacteria_index) <- c(
        "Variable",
        "12.5th perc.",
        "10th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "87.5th perc.",
        "90th perc.",
        "Minimum value",
        "Maximum value",
        "Avg",
        "N"
      )
      
      data.table::as.data.table(table_data_bacteria_index)
      # to do, insert here a way to count samples over or below the threshold, if inserted
      
    } else if (is.character(meta_bacteria_selected[, input$bacteria_var_choice]) == T) {
      levels(as.factor(meta_bacteria_selected[, input$bacteria_var_choice])) -> levels_bacteria_table
      
      # table
      
      df_out_loop <- data.frame()
      
      for (i in 1:length(levels_bacteria_table)) {
        filter(
          meta_bacteria_selected,
          meta_bacteria_selected[, input$bacteria_var_choice] == levels_bacteria_table[i]
        ) -> df_loop
        
        data.frame(
          Index = levels_bacteria_table[i],
          t(round(quantile(df_loop[, input$bacteria_index_choice],
                           probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
                           na.rm = T
          ), digits = 3)),
          Min = round(min(df_loop[, input$bacteria_index_choice], na.rm = T), digits = 3),
          Max = round(max(df_loop[, input$bacteria_index_choice], na.rm = T), digits = 3),
          Avg = round(mean(df_loop[, input$bacteria_index_choice], na.rm = T), digits = 3),
          N = length(na.omit(df_loop[, input$bacteria_index_choice]))
        ) -> df_out_cycle
        
        colnames(df_out_cycle) <- c(
          "Variable",
          "10th perc.",
          "12.5th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "87.5th perc.",
          "90th perc.",
          "Minimum value",
          "Maximum value",
          "Avg",
          "N"
        )
        
        df_out_loop <- rbind(df_out_loop, df_out_cycle)
      }
      
      table_data_bacteria_index <- df_out_loop
      
      data.table::as.data.table(table_data_bacteria_index)
    }
  })
  
  output$table_bacteria_correl_or_kruskal <- renderTable({
   
    sites_selected <- sites_selected_reactive()
    
    bacteria_data %>%
      #select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> bacteria_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% bacteria_data_index_selected$id_sampling_point) -> meta_bacteria_selected
    full_join(meta_bacteria_selected, bacteria_data_index_selected, by = "id_sampling_point") -> meta_bacteria_selected
    
    
    # clean data
    meta_bacteria_selected[meta_bacteria_selected == 9999] <- NA
    meta_bacteria_selected %>%
      filter(!!as.symbol(input$bacteria_var_choice) != "") -> meta_bacteria_selected
    
    validate(
      need(nrow(meta_bacteria_selected) != 0, "")
    )
    
    if (is.character(meta_bacteria_selected[, input$bacteria_var_choice]) == F) {
      # Numerical variable: display different correlation or other like lm
      
      meta_bacteria_selected %>%
        select(c(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice))) %>%
        drop_na() %>%
        cor_test(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice), method = "pearson") %>%
        as.data.frame() -> bacteria_pearson
      
      meta_bacteria_selected %>%
        select(c(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice))) %>%
        drop_na() %>%
        cor_test(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice), method = "spearman") %>%
        as.data.frame() -> bacteria_spearman
      
      correl_cols <- c("cor", "statistic", "p", "method")
      rbind(bacteria_pearson[, correl_cols], bacteria_spearman[, correl_cols]) -> df_correlation_kruskal_bacteria
      
      df_correlation_kruskal_bacteria
    } else if (is.character(meta_bacteria_selected[, input$bacteria_var_choice]) == T) {
      # Categorical variable: display results of Kruskal wallis
      
      bacteria_var_choice <- input$bacteria_var_choice
      bacteria_index_choice <- input$bacteria_index_choice
      
      meta_bacteria_selected %>%
        select(c(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice))) %>%
        drop_na() %>%
        kruskal_test(as.formula(paste(bacteria_index_choice, paste("~", bacteria_var_choice)))) %>%
        as.data.frame() -> df_correlation_kruskal_bacteria
      
      df_correlation_kruskal_bacteria[, -c(1, 2)]
    }
  })
  
  output$tile_plot_bacteria <- renderPlot({
   
    sites_selected <- sites_selected_reactive()
    
    bacteria_data %>%
      #select(1:12) %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> bacteria_data_index_selected
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% bacteria_data_index_selected$id_sampling_point) -> meta_bacteria_selected
    full_join(meta_bacteria_selected, bacteria_data_index_selected, by = "id_sampling_point") -> meta_bacteria_selected
    
    
    # clean data
    meta_bacteria_selected[meta_bacteria_selected == 9999] <- NA
    meta_bacteria_selected %>%
      filter(!!as.symbol(input$bacteria_var_choice) != "") -> meta_bacteria_selected
    
    validate(
      need(nrow(meta_bacteria_selected) != 0, "")
    )
    
    if (is.character(meta_bacteria_selected[, input$bacteria_var_choice]) == T) {
      # Categorical variable
      
      bacteria_var_choice <- input$bacteria_var_choice
      bacteria_index_choice <- input$bacteria_index_choice
      
      meta_bacteria_selected %>%
        select(c(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice))) %>%
        drop_na() %>%
        wilcox_test(as.formula(paste(bacteria_index_choice, paste("~", bacteria_var_choice))), p.adjust.method = "bonferroni") -> bacteria_wilcox

      meta_bacteria_selected %>%
        select(c(!!as.symbol(input$bacteria_index_choice), !!as.symbol(input$bacteria_var_choice))) %>%
        drop_na() %>%
        wilcox_effsize(as.formula(paste(bacteria_index_choice, paste("~", bacteria_var_choice)))) -> bacteria_effsize

      bacteria_wilcox %>%
        as.data.frame() %>%
        left_join(as.data.frame(bacteria_effsize)[, c("group1", "group2", "effsize")], by = c("group1", "group2")) %>%
        mutate_at(vars(p), funs(ifelse(. > 0.05, NA, .))) %>%
        mutate(effsize = ifelse(is.na(p), NA, effsize)) %>%
        ggplot(aes(
          x = group1,
          y = group2
        )) +
        geom_tile(aes(fill = effsize),
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
        scale_fill_gradientn(
          colors = c("#EDF8B1", "#7FCDBB", "#2C7FB8"),
          na.value = "white",
          limits = c(0, 1),
          name = "Effect size (r)"
        ) +
        theme(
          legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          panel.background = element_blank(),
          axis.text=element_text(size=12)
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt13

      plt13
    }
  })
  
  #### --- Fungi ---####

  output$plot_biodiv_index_fungi <- renderPlot({
  
    sites_selected <- sites_selected_reactive()

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

        plt6
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

        plt6
      }
    } else if (is.character(meta_fungi_selected[, input$fungi_var_choice]) == T) {
      meta_fungi_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$fungi_var_choice) != "") -> meta_fungi_selected

      validate(
        need(nrow(meta_fungi_selected) != 0, "No data for selected variable")
      )

      meta_fungi_selected %>%
        ggplot(aes_string(x = input$fungi_var_choice, y = input$fungi_index_choice, fill = input$fungi_var_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab(input$fungi_index_choice) +
        theme_bw() +
        coord_flip() -> plt6

      plt6
    }
  })

  output$fungi_index_table <- renderDataTable({
   
    sites_selected <- sites_selected_reactive()
    
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
          probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
        "12.5th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "87.5th perc.",
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
            probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
          "12.5th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "87.5th perc.",
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
   
    sites_selected <- sites_selected_reactive()

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

      correl_cols <- c("cor", "statistic", "p", "method")
      rbind(fungi_pearson[, correl_cols], fungi_spearman[, correl_cols]) -> df_correlation_kruskal_fungi

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
  
    sites_selected <- sites_selected_reactive()

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

      meta_fungi_selected %>%
        select(c(!!as.symbol(input$fungi_index_choice), !!as.symbol(input$fungi_var_choice))) %>%
        drop_na() %>%
        wilcox_effsize(as.formula(paste(fungi_index_choice, paste("~", fungi_var_choice)))) -> fungi_effsize

      fungi_wilcox %>%
        as.data.frame() %>%
        left_join(as.data.frame(fungi_effsize)[, c("group1", "group2", "effsize")], by = c("group1", "group2")) %>%
        mutate_at(vars(p), funs(ifelse(. > 0.05, NA, .))) %>%
        mutate(effsize = ifelse(is.na(p), NA, effsize)) %>%
        ggplot(aes(
          x = group1,
          y = group2
        )) +
        geom_tile(aes(fill = effsize),
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
        scale_fill_gradientn(
          colors = c("#EDF8B1", "#7FCDBB", "#2C7FB8"),
          na.value = "white",
          limits = c(0, 1),
          name = "Effect size (r)"
        ) +
        theme(
          legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          panel.background = element_blank(),
          axis.text=element_text(size=12)
          ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt9

      plt9
    }
  })

  #### --- Mesofauna ---####

  output$plot_biodiv_index_meso <- renderPlot({
    
    sites_selected <- sites_selected_reactive()

    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    
    #mesof_data_index_selected <- mesof_data_index # this is useful for testing

    # mutate triplicate data of QBS down to single value
    mesof_data_index_selected %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      group_by(Unique_sample_id) %>% 
      select(c(4,5)) %>% 
      summarise(diversity_index_value = mean(diversity_index_value)) %>% 
      ungroup() -> mesof_data_index_selected_unique
    
    # need to do the same for metadata. Here numerical columns can have average, 
    # while character columns should be subset by unique/duplicate values.
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.numeric)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      select(-id_sampling_point) %>% 
      group_by(Unique_sample_id) %>% 
      summarise_all(mean) %>% 
      ungroup() -> metadata_MINOTAUR_selected_num
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.character)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      arrange(Unique_sample_id) %>%
      group_by(Unique_sample_id) %>% 
      slice(1) %>% 
      ungroup() -> metadata_MINOTAUR_selected_char
 
    # join them all now
    
    full_join(mesof_data_index_selected_unique, full_join(metadata_MINOTAUR_selected_num, metadata_MINOTAUR_selected_char, by = "Unique_sample_id"), by = "Unique_sample_id") -> meta_meso_selected

    diversity_index_value <- "diversity_index_value" # this is needed for use with aes_string() to accomodate for user selection
    
    meta_meso_selected <- as.data.frame(meta_meso_selected)

    # code for plots and analysis
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

        plt5
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

        plt5
      }
    } else if (is.character(meta_meso_selected[, input$meso_var_choice]) == T) {
      meta_meso_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$meso_var_choice) != "") -> meta_meso_selected

      validate(
        need(nrow(meta_meso_selected) != 0, "No data for selected variable")
      )

      meta_meso_selected %>%
        ggplot(aes_string(x = input$meso_var_choice, y = diversity_index_value, fill = input$meso_var_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab("QBS-ar") +
        theme_bw() +
        coord_flip() -> plt5

      plt5
    }
  })

  output$meso_index_table <- renderDataTable({
    
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    
    # select only first record for each sample, as QBS-ar index is the same for each replicates
    mesof_data_index_selected %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      group_by(Unique_sample_id) %>% 
      select(c(4,5)) %>% 
      summarise(diversity_index_value = mean(diversity_index_value)) -> mesof_data_index_selected_unique
    
    # need to do the same for metadata. Here numerical columns can have average, 
    # while character columns should be subset by unique/duplicate values.
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.numeric)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      select(-id_sampling_point) %>% 
      group_by(Unique_sample_id) %>% 
      summarise_all(mean) -> metadata_MINOTAUR_selected_num
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.character)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      arrange(Unique_sample_id) %>%
      group_by(Unique_sample_id) %>% 
      slice(1) -> metadata_MINOTAUR_selected_char
    
    # join them all now
    
    full_join(mesof_data_index_selected_unique, full_join(metadata_MINOTAUR_selected_num, metadata_MINOTAUR_selected_char, by = "Unique_sample_id"), by = "Unique_sample_id") -> meta_meso_selected
    
    diversity_index_value <- "diversity_index_value"
    
    meta_meso_selected <- as.data.frame(meta_meso_selected)

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
          probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
        "12.5th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "87.5th perc.",
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
            probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
          "12.5th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "87.5th perc.",
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
    
    sites_selected <- sites_selected_reactive()

    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    
    # select only first record for each sample, as QBS-ar index is the same for each replicates
    mesof_data_index_selected %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      group_by(Unique_sample_id) %>% 
      select(c(4,5)) %>% 
      summarise(diversity_index_value = mean(diversity_index_value)) -> mesof_data_index_selected_unique
    
    # need to do the same for metadata. Here numerical columns can have average, 
    # while character columns should be subset by unique/duplicate values.
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.numeric)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      select(-id_sampling_point) %>% 
      group_by(Unique_sample_id) %>% 
      summarise_all(mean) -> metadata_MINOTAUR_selected_num
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.character)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      arrange(Unique_sample_id) %>%
      group_by(Unique_sample_id) %>% 
      slice(1) -> metadata_MINOTAUR_selected_char
    
    # join them all now
    
    full_join(mesof_data_index_selected_unique, full_join(metadata_MINOTAUR_selected_num, metadata_MINOTAUR_selected_char, by = "Unique_sample_id"), by = "Unique_sample_id") -> meta_meso_selected
    
    diversity_index_value <- "diversity_index_value"
    
    meta_meso_selected <- as.data.frame(meta_meso_selected)
    
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

      correl_cols <- c("cor", "statistic", "p", "method")
      rbind(meso_pearson[, correl_cols], meso_spearman[, correl_cols]) -> df_correlation_kruskal_meso

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
  
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    mesof_data_index %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) -> mesof_data_index_selected
    
    # select only first record for each sample, as QBS-ar index is the same for each replicates
    mesof_data_index_selected %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      group_by(Unique_sample_id) %>% 
      select(c(4,5)) %>% 
      summarise(diversity_index_value = mean(diversity_index_value)) -> mesof_data_index_selected_unique
    
    # need to do the same for metadata. Here numerical columns can have average, 
    # while character columns should be subset by unique/duplicate values.
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.numeric)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      select(-id_sampling_point) %>% 
      group_by(Unique_sample_id) %>% 
      summarise_all(mean) -> metadata_MINOTAUR_selected_num
    
    metadata_MINOTAUR_selected %>% 
      subset(id_sampling_point %in% mesof_data_index_selected$id_sampling_point) %>% 
      select_if(names(.)=="id_sampling_point" | sapply(., is.character)) %>% 
      mutate(Unique_sample_id = str_sub(id_sampling_point, end = -2)) %>% 
      arrange(Unique_sample_id) %>%
      group_by(Unique_sample_id) %>% 
      slice(1) -> metadata_MINOTAUR_selected_char
    
    # join them all now
    
    full_join(mesof_data_index_selected_unique, full_join(metadata_MINOTAUR_selected_num, metadata_MINOTAUR_selected_char, by = "Unique_sample_id"), by = "Unique_sample_id") -> meta_meso_selected
    
    diversity_index_value <- "diversity_index_value"
    
    meta_meso_selected <- as.data.frame(meta_meso_selected)

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

      meta_meso_selected %>%
        select(c(diversity_index_value, !!as.symbol(input$meso_var_choice))) %>%
        drop_na() %>%
        wilcox_effsize(as.formula(paste("diversity_index_value", paste("~", meso_var_choice)))) -> meso_effsize

      meso_wilcox %>%
        as.data.frame() %>%
        left_join(as.data.frame(meso_effsize)[, c("group1", "group2", "effsize")], by = c("group1", "group2")) %>%
        mutate_at(vars(p), funs(ifelse(. > 0.05, NA, .))) %>%
        mutate(effsize = ifelse(is.na(p), NA, effsize)) %>%
        ggplot(aes(
          x = group1,
          y = group2
        )) +
        geom_tile(aes(fill = effsize),
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
        scale_fill_gradientn(
          colors = c("#EDF8B1", "#7FCDBB", "#2C7FB8"),
          na.value = "white",
          limits = c(0, 1),
          name = "Effect size (r)"
        ) +
        theme(
          legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          panel.background = element_blank(),
          axis.text=element_text(size=12)
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt10

      plt10
    }
  })

  #### --- Microfauna ####

  output$plot_biodiv_index_micro <- renderPlot({
   
    sites_selected <- sites_selected_reactive()

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

        plt7
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

        plt7
      }
    } else if (is.character(meta_micro_selected[, input$micro_var_choice]) == T) {
      meta_micro_selected %>%
        as_tibble() %>%
        filter(!!as.symbol(input$micro_var_choice) != "") %>%
        ggplot(aes_string(x = input$micro_var_choice, y = input$micro_index_choice, fill = input$micro_index_choice)) +
        geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
        geom_boxplot(alpha = 0.5) +
        xlab("") +
        ylab(input$micro_index_choice) +
        theme_bw() +
        coord_flip() -> plt7

      plt7
    }
  })

  output$micro_index_table <- renderDataTable({
  
    sites_selected <- sites_selected_reactive()

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
          probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
        "12.5th perc.",
        "10th perc.",
        "25th perc.",
        "50th perc.",
        "75th perc.",
        "87.5th perc.",
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
            probs = c(0.10, 0.125, 0.25, 0.50, 0.75, 0.875, 0.9),
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
          "12.5th perc.",
          "25th perc.",
          "50th perc.",
          "75th perc.",
          "87.5th perc.",
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
 
    sites_selected <- sites_selected_reactive()

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

      correl_cols <- c("cor", "statistic", "p", "method")
      rbind(micro_pearson[, correl_cols], micro_spearman[, correl_cols]) -> df_correlation_kruskal_micro

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
 
    sites_selected <- sites_selected_reactive()

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

      meta_micro_selected %>%
        select(c(!!as.symbol(input$micro_index_choice), !!as.symbol(input$micro_var_choice))) %>%
        drop_na() %>%
        wilcox_effsize(as.formula(paste(micro_index_choice, paste("~", micro_var_choice)))) -> micro_effsize

      micro_wilcox %>%
        as.data.frame() %>%
        left_join(as.data.frame(micro_effsize)[, c("group1", "group2", "effsize")], by = c("group1", "group2")) %>%
        mutate_at(vars(p), funs(ifelse(. > 0.05, NA, .))) %>%
        mutate(effsize = ifelse(is.na(p), NA, effsize)) %>%
        ggplot(aes(
          x = group1,
          y = group2
          )) +
        geom_tile(aes(fill = effsize),
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
        scale_fill_gradientn(
          colors = c("#EDF8B1", "#7FCDBB", "#2C7FB8"),
          na.value = "white",
          limits = c(0, 1),
          name = "Effect size (r)"
        ) +
        theme(
          legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "black", linetype = "dotted"),
          panel.background = element_blank(),
          axis.text=element_text(size=12)
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) -> plt11

      plt11
    }
  })

  #### --- Macrofauna ####  
  
  output$plot_biodiv_index_macro <- renderPlot({
    
  
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    macrof_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(-2) -> macro_data_selected
    
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% macro_data_selected$id_sampling_point) -> meta_macro_abb_selected
    full_join(meta_macro_abb_selected, macro_data_selected, by = "id_sampling_point") -> meta_macro_abb_selected
    
    
    summary(as.factor(meta_macro_abb_selected$ecological_group))
    
    abundance <- "abundance"

    
    meta_macro_abb_selected$ecological_group <- factor(meta_macro_abb_selected$ecological_group, levels = c("anecic",
                                                                                                            "strict anecic",
                                                                                                            "epi-anecic",
                                                                                                            "endogeic",
                                                                                                            "epigeic"
                                                                                                            ))
    
    meta_macro_abb_selected$soil_humidity <- as.numeric(meta_macro_abb_selected$soil_humidity)
    
    if (is.character(meta_macro_abb_selected[, input$macro_var_choice]) == F) {
      if (input$treshold_line_macro == 0 | is.na(input$treshold_line_macro) == T) { 
        
        meta_macro_abb_selected %>% 
          as_tibble() %>%
          filter(!!as.symbol(input$macro_var_choice) != "") %>%
          filter(ecological_group != "") %>% 
          group_by(ecological_group, id_sampling_point, !!as.symbol(input$macro_var_choice)) %>% 
          summarize(Mean_abb = mean(abundance)) %>% 
          ggplot(aes(x = !!as.symbol(input$macro_var_choice), y = Mean_abb, color = ecological_group)) + 
          geom_point() +
          geom_smooth(method = "lm") +
          ylab("Mean Abundance") +
          xlab(input$macro_var_choice) +
          scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")) +
          theme_bw() -> plt14

        plt14

      }  else if (input$treshold_line_macro != 0) {
        
        meta_macro_abb_selected %>% 
          as_tibble() %>%
          filter(!!as.symbol(input$macro_var_choice) != "") %>%
          filter(ecological_group != "") %>% 
          group_by(ecological_group, id_sampling_point, !!as.symbol(input$macro_var_choice)) %>% 
          summarize(Mean_abb = mean(abundance)) %>% 
          ggplot(aes(x = !!as.symbol(input$macro_var_choice), y = Mean_abb, color = ecological_group)) + 
          geom_vline(xintercept = input$treshold_line_macro, colour = "red", linetype = "dotdash") +
          geom_point() +
          geom_smooth(method = "lm") +
          ylab("Mean Abundance") +
          xlab(input$macro_var_choice) +
          scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")) +
          theme_bw() -> plt14

        plt14

      }
      } else if (is.character(meta_macro_abb_selected[, input$macro_var_choice]) == T) {
      
        meta_macro_abb_selected %>% 
          as_tibble() %>%
          filter(!!as.symbol(input$macro_var_choice) != "") %>%
          filter(ecological_group != "") %>% 
          group_by(ecological_group, id_sampling_point, !!as.symbol(input$macro_var_choice)) %>% 
          summarize(Mean_abb = mean(abundance)) %>% 
          ggplot(aes(x = !!as.symbol(input$macro_var_choice), y = Mean_abb, color = ecological_group)) + 
          geom_jitter(width = 0.4, height = 0, alpha = 0.2) +
          geom_boxplot(alpha = 0.5) +
          facet_wrap(.~ecological_group, nrow = 3, ncol = 2) +
          xlab("") +
          ylab("Mean Abundance") +
          theme_bw() +
          coord_flip() -> plt14

        plt14
    }
  })

  output$barplot_biodiv_index_macro <- renderPlot({
  
    sites_selected <- sites_selected_reactive()
    
    # prepare data
    macrof_data %>%
      subset(id_sampling_point %in% sites_selected$id_sampling_point) %>%
      select(-2) -> macro_data_selected
    
    metadata_MINOTAUR_selected %>%
      subset(id_sampling_point %in% macro_data_selected$id_sampling_point) -> meta_macro_abb_selected
    full_join(meta_macro_abb_selected, macro_data_selected, by = "id_sampling_point") -> meta_macro_abb_selected
    
    
    summary(as.factor(meta_macro_abb_selected$ecological_group))
    
    abundance <- "abundance"

    
    meta_macro_abb_selected$ecological_group <- factor(meta_macro_abb_selected$ecological_group, levels = c("anecic",
                                                                                                            "strict anecic",
                                                                                                            "epi-anecic",
                                                                                                            "endogeic",
                                                                                                            "epigeic"
    ))
    
    
    meta_macro_abb_selected$soil_humidity <- as.numeric(meta_macro_abb_selected$soil_humidity)
    
    if (is.character(meta_macro_abb_selected[, input$macro_var_choice]) == F) {
      
      # basically do nothing
      
    } else if (is.character(meta_macro_abb_selected[, input$macro_var_choice]) == T) {
    
      # plot a barplot
      meta_macro_abb_selected %>% 
        as_tibble() %>%
        filter(!!as.symbol(input$macro_var_choice) != "") %>%
        filter(ecological_group != "") %>% 
        filter(abundance != 0) %>% 
        group_by(ecological_group, id_sampling_point, !!as.symbol(input$macro_var_choice)) %>% 
        summarize(Abundance = sum(abundance)) %>% 
        ungroup() %>% 
        group_by(ecological_group,!!as.symbol(input$macro_var_choice) ) %>% 
        summarize_all(mean) %>% 
        group_by(!!as.symbol(input$macro_var_choice) ) %>%
        mutate(Tot_abb = sum(Abundance)) %>% 
        ungroup() %>% 
        mutate(Rel_abb = Abundance / Tot_abb) %>% 
        ggplot(aes(y = Rel_abb, x = !!as.symbol(input$macro_var_choice), fill = ecological_group)) + 
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")) -> plt15

      plt15

    }


  })

  #### --- Scenario Testing tab ---####

  rv_scenarios <- reactiveValues(scenarios = list(), next_id = 1)

  observeEvent(input$add_scenario, {
    if (length(rv_scenarios$scenarios) >= length(okabe_ito_palette)) return()
    used_colors <- if (length(rv_scenarios$scenarios) > 0) {
      sapply(rv_scenarios$scenarios, function(s) s$color_idx)
    } else {
      integer(0)
    }
    next_color_idx <- setdiff(seq_along(okabe_ito_palette), used_colors)[1]
    new_id <- rv_scenarios$next_id
    rv_scenarios$next_id <- rv_scenarios$next_id + 1
    rv_scenarios$scenarios <- c(rv_scenarios$scenarios, list(list(
      id = new_id,
      name = paste("Scenario", new_id),
      color_idx = next_color_idx,
      color = okabe_ito_palette[next_color_idx]
    )))
  })

  observeEvent(input$remove_scenario_id, {
    rv_scenarios$scenarios <- Filter(function(s) s$id != input$remove_scenario_id, rv_scenarios$scenarios)
  })

  output$scenario_boxes <- renderUI({
    scenarios <- rv_scenarios$scenarios
    if (length(scenarios) == 0) {
      return(p(em("No scenarios yet - click \"+ Add scenario\" to define your first set of national thresholds.")))
    }

    partB <- sml_scenario_variables[sml_scenario_variables$part == "B", ]

    boxes <- lapply(scenarios, function(s) {
      name_id <- paste0("scn_name_", s$id)
      current_name <- isolate(input[[name_id]])
      if (is.null(current_name)) current_name <- s$name

      var_inputs <- lapply(seq_len(nrow(partB)), function(i) {
        v <- partB[i, ]
        input_id <- paste0("thr_", s$id, "_", v$id)
        current_val <- isolate(input[[input_id]])
        dir_symbol <- if (v$direction == "below") "healthy ≤" else "healthy ≥"
        column(width = 4,
          numericInput(input_id,
            label = sprintf("%s (%s) - %s", v$label, v$unit, dir_symbol),
            value = if (is.null(current_val)) NA else current_val,
            min = 0)
        )
      })

      div(style = sprintf("border-left: 6px solid %s; padding: 10px 15px; margin-bottom: 15px; background-color: #fafafa;", s$color),
        fluidRow(
          column(9, textInput(name_id, label = NULL, value = current_name)),
          column(3, tags$button(class = "btn btn-danger btn-sm", type = "button",
                                 onclick = sprintf("Shiny.setInputValue('remove_scenario_id', %d, {priority: 'event'})", s$id),
                                 "Remove"))
        ),
        fluidRow(var_inputs)
      )
    })

    tagList(boxes)
  })

  output$scenario_radar_plot <- renderPlot({
    scenarios <- rv_scenarios$scenarios
    validate(need(length(scenarios) > 0, "Add at least one scenario to see the plot."))

    partB <- sml_scenario_variables[sml_scenario_variables$part == "B", ]
    all_vars <- sml_scenario_variables
    n_vars <- nrow(all_vars)

    scenario_data <- lapply(scenarios, function(s) {
      vals <- vapply(seq_len(nrow(partB)), function(i) {
        v <- input[[paste0("thr_", s$id, "_", partB$id[i])]]
        if (is.null(v)) NA_real_ else as.numeric(v)
      }, numeric(1))
      name_val <- input[[paste0("scn_name_", s$id)]]
      list(
        id = s$id,
        name = if (is.null(name_val) || name_val == "") paste("Scenario", s$id) else name_val,
        color = s$color,
        partB_values = vals,
        complete = !any(is.na(vals))
      )
    })

    complete_scenarios <- Filter(function(s) s$complete, scenario_data)
    validate(need(length(complete_scenarios) > 0,
                  "Fill in all six Part B thresholds for at least one scenario to see the plot."))

    axis_range <- lapply(seq_len(n_vars), function(i) {
      v <- all_vars[i, ]
      vals <- if (v$part == "A") {
        v$fixed_value
      } else {
        sapply(complete_scenarios, function(s) s$partB_values[which(partB$id == v$id)])
      }
      rng <- range(vals, na.rm = TRUE)
      if (diff(rng) == 0) rng <- rng + c(-1, 1) * (abs(rng[1]) * 0.1 + 0.01)
      pad <- diff(rng) * 0.15
      c(rng[1] - pad, rng[2] + pad)
    })
    names(axis_range) <- all_vars$id

    normalize <- function(value, var_id) {
      v <- all_vars[all_vars$id == var_id, ]
      rng <- axis_range[[var_id]]
      pos <- (value - rng[1]) / (rng[2] - rng[1])
      if (v$direction == "above") pos <- 1 - pos
      pmin(pmax(pos, 0), 1)
    }

    plot_df <- do.call(rbind, lapply(complete_scenarios, function(s) {
      do.call(rbind, lapply(seq_len(n_vars), function(i) {
        v <- all_vars[i, ]
        raw_val <- if (v$part == "A") v$fixed_value else s$partB_values[which(partB$id == v$id)]
        data.frame(scenario = s$name, color = s$color,
                   variable = v$label, position = normalize(raw_val, v$id),
                   stringsAsFactors = FALSE)
      }))
    }))
    plot_df$variable <- factor(plot_df$variable, levels = all_vars$label)
    scenario_order <- sapply(complete_scenarios, function(s) s$name)
    plot_df$scenario <- factor(plot_df$scenario, levels = scenario_order)

    color_map <- setNames(sapply(complete_scenarios, function(s) s$color), scenario_order)

    p <- ggplot(plot_df, aes(x = variable, y = position, group = scenario, color = scenario, fill = scenario)) +
      geom_polygon(alpha = 0.18, linewidth = 0) +
      geom_path(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = color_map) +
      scale_fill_manual(values = color_map) +
      ylim(0, 1) +
      coord_radar() +
      theme_bw() +
      theme(axis.title = element_blank(), axis.text.y = element_blank(),
            legend.title = element_blank())

    if (identical(input$scenario_view_mode, "facet")) {
      p <- p + facet_wrap(~scenario) + theme(legend.position = "none")
    }

    p
  })
}
