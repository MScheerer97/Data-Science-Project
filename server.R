# in this file the server is set up #


function(input, output, session) {
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  
  #### INTRODUCTION ####
  #%%%%%%%%%%%%%%%%%%%%#
  
  #### Data Sources ####
  output$data_sources_output <- renderUI({
    if (input$data_sources == "Price Prediction") {
      tags$img(src = 'data_price_graph.PNG', width = "70%")  
    } else if (input$data_sources == "Food Basket") {
      tags$img(src = 'data_basket_graph.PNG', width = "70%") 
  }
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  #### Food items ####
  #%%%%%%%%%%%%%%%%%%#
  
  output$food_items_overview <- renderUI({
    tags$img(src = 'food_items_overview.PNG', width = "70%")  
  })
  
  
  
  #### FOOD-TYPE-SIZE-COUNTRY INFORMATION ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # use df_descr data set but change variable names to capital letters
  df_descr_food_type_size_country <- df_descr 
  names(df_descr_food_type_size_country) <- 
    str_to_title(names(df_descr_food_type_size_country))
  
  
  # create data frame: display food-type-size-country info
  # however, this is only done if the actionButton is clicked
  df_info_table <- eventReactive(input$info_show, {
    
    df_info_table_food_type_size_country <- 
      df_descr_food_type_size_country %>% 
      # select only food items selected by user
      filter(Food %in% input$info_food) %>%
      # keep only variables selected by user
      dplyr::select(Food, all_of(input$info_type_country_size)) %>%
      # drop duplicates
      distinct() 
    
    # identify columns used for sorting the data frame
    cols_food_type_size_country <- colnames(df_info_table_food_type_size_country)
    ## to sort size within arrange() a function is applied
    if ("size" %in% cols_food_type_size_country) {
      cols_food_type_size_country <-
        replace(cols_food_type_size_country, 
                cols_food_type_size_country == "size",
                "func_order_mixed_types(size)")
    }
    
    df_info_table_food_type_size_country %>%
      # sort data frame (size is mixed type and needs special ordering)
      arrange(!!! rlang::syms(cols_food_type_size_country))
  })
  
  # table output
  output$info_table <- DT::renderDataTable({
    # ensure that food item is selected
    shiny::validate(need(!is.null(input$info_food),
                         "Please select at least one fruit or vegetable."))
    
    # create datatable and style
    DT::datatable(
      data = df_info_table() %>% dplyr::rename(`Fruit/Vegetable` = Food), 
      rownames = FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}"),
        dom = 'ltip', # display table, pagination, info
        pageLength = 5, lengthMenu = c(5, 10, 15, 20, 30)
      )) %>%
      formatStyle(columns = names(df_info_table() %>% dplyr::rename(`Fruit/Vegetable` = Food)), 
                  color = "white", backgroundColor = "black", background = "black")
    
  })
  
  
  # descriptives output
  df_info_descr_html <- eventReactive(input$info_show, {
    df_info <-
      df_descr %>% 
      filter(food %in% input$info_food) %>%
      #select(food, all_of(input$info_type_country_size)) %>%
      distinct() 
    
    # extract number of types, sizes, and countries across food items
    df_types <- df_info %>% dplyr::select(food, type) %>% distinct() %>% 
      group_by(food) %>% count() %>% dplyr::rename(type = n)
    df_sizes <- df_info %>% dplyr::select(food, size) %>% distinct() %>% 
      group_by(food) %>% count() %>% dplyr::rename(size = n)
    df_countries <- df_info %>% dplyr::select(food, country) %>% distinct() %>% 
      group_by(food) %>% count() %>% dplyr::rename(country = n)
    
    # create data frame containing everything
    df_info_num <- inner_join(df_types, df_sizes, by = "food")
    df_info_num <- inner_join(df_info_num, df_countries, by = "food")
    
    # subset columns
    df_info_num <-
      df_info_num %>%
      dplyr::select(food, type, size, country) %>%
      ungroup()
    
    # create HTML output
    html_output <- c()
    for (i in unique(df_info_num$food)) {
      # extract numbers
      number_type <- 
        df_info_num %>% filter(food == i) %>% dplyr::select(type) %>% pull()
      number_country <- 
        df_info_num %>% filter(food == i) %>% dplyr::select(country) %>% pull()
      number_size <- 
        df_info_num %>% filter(food == i) %>% dplyr::select(size) %>% pull()
      
      # html output depends on selection
      ## length can be 1, 2 and three
      if (length(input$info_type_country_size) == 3) {
        html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                               "Number of Types:", number_type, "<br/>",
                               "Number of Sizes:", number_size, "<br/>",
                               "Number of Countries:", number_country, "<br/>", 
                               "<br/>"))
        html_output <- c(html_output, html_sub)
      } else if (length(input$info_type_country_size) == 2) {
        if (!"type" %in% input$info_type_country_size) {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Sizes:", number_size, "<br/>",
                                 "Number of Countries:", number_country, "<br/>", 
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        } else if (!"country" %in% input$info_type_country_size) {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Types:", number_type, "<br/>",
                                 "Number of Sizes:", number_size, "<br/>",
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        } else if (!"size" %in% input$info_type_country_size) {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Countries:", number_country, "<br/>", 
                                 "Number of Types:", number_type, "<br/>",
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        }
      } else if (length(input$info_type_country_size) == 1) {
        if (input$info_type_country_size == "Size") {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Sizes:", number_size, "<br/>",
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        } else if (input$info_type_country_size == "Type") {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Types:", number_type, "<br/>",
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        } else if (input$info_type_country_size == "Country") {
          html_sub <- HTML(paste("<b>", "Descriptives for", i, "</b>", "<br/>",
                                 "Number of Countries:", number_country, "<br/>", 
                                 "<br/>"))
          html_output <- c(html_output, html_sub)
        }
      }
    }
    
    # return html_output from reactive expression
    html_output
  })
  

  # returnd escriptives output
  output$info_types_num <- renderText({
    # combine full output in one
    if (length(df_info_descr_html()) > 1) {
      HTML(c(paste(df_info_descr_html()[1], sep = "<br/>"), 
             paste(df_info_descr_html()[2:length(df_info_descr_html())])))
    } else {
      HTML(c(paste(df_info_descr_html()[1])))
    }
  })
  
  
  # return text for explanation box
  output$info_explanation <- renderText({
    HTML(
      paste(
        "In the table above, each fruit and vegetable is distinguished by its type, size, and the country the fruit or rather vegetable
        is imported from.",
        "However, not for every fruit and vegetable all information exists. For instance, for kiwis, the type is not given. In addition,",
        "for bananas the country is unknown. The user of this app should be informed that these are no data pre-processing mistakes.",
        "Instead, the German federal office of agriculture and food simply does not provide this information.",
        "<br/>", "<br/>",
        "In the app, the prices are shown for 1kg/food, except for iceberg lettuce, 
         endive and cauliflower there are given for one piece.", 
        "On the wholesale markets the fruits and vegetables are sorted into boxes. Usually non-private customers, such as hotels, buy their products there.
         Thus, they may have specific preferences regarding the size of a fruit or vegetable. As a result a fruit or vegetable may exist 
         in several sizes which influence the price.", 
        "For instance, apples are cheaper if one buys them in the size category 'Mixed'. 'Mixed' means
         that the apples have different sizes, i.e., the purchaser buys big and small apples. 
         If s(he) instead wants to buy apples of a specific size, the price is higher.", 
        "Some fruits and vegetables are only offered in one size. In this case, the size is 'One Size'.",
        "<br/>", "<br/>",
        "To sum up, the prices in our analysis depend not only on the fruit and vegetable kind, but also
        on their type, size, and country. We refer to this as 'food-type-size-country combination'.
        Furthermore, the prices differ across wholesale markets.
        More can be explored in the 'Variable Development' tab, Section 'Price Development'."
      )
    )
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  #### Nutrition and Footprint ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  ## Nutrition ##
  #%%%%%%%%%%%%%#
  
  
  ## food ##
  
  # include an updateseletInput
    ## only nutrients which have values for combination should be displayed
  var_nutrient_food_sel <- reactive({
    df_nutrient_info %>%
      filter(food_2 == input$nutrition_food_sel) %>%
      dplyr::select(nutrient) %>%
      pull()
  })
  
  observeEvent(input$nutrition_food_sel, {
    updateSelectInput(session, "nutrition_food_sel_var",
                      choices = sort(c("All", var_nutrient_food_sel())),
                      selected = "All"
    )
  })
  
  
  # extract selected food for table
  df_nutrient_table_sub <- eventReactive(input$nutrition_food_sel_show, {
    if (!is.null(input$nutrition_food_sel_var)) {
      # subset data
      if ("All" %in% input$nutrition_food_sel_var ) {
        df_nutrient_table_sub <- df_nutrient_info %>%
          filter(food_2 == input$nutrition_food_sel) 
      } else {
        df_nutrient_table_sub <- df_nutrient_info %>%
          filter(food_2 == input$nutrition_food_sel) %>%
          filter(nutrient %in% all_of(input$nutrition_food_sel_var)) 
      }
      
      # rename variables
      df_nutrient_table_sub <- df_nutrient_table_sub %>%
        dplyr::rename(
          Food = food_2,
          Nutrient = nutrient,
          Unit = nutrient_unit,
          Value = nutrient_value
        )
      
      # make nice table
      DT::datatable(
        data = df_nutrient_table_sub %>% dplyr::rename(`Fruit/Vegetable` = Food), 
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          dom = "t", # show only table
          pageLength = 20)) %>%
        formatStyle(columns = c(1:4), color = "white", 
                    backgroundColor = "black", background = "black")
    } else {
      shiny::validate(need(!is.null(input$nutrition_food_sel_var),
                           "Please select at least one nutrition item"))
    }
    
  })
  
  
  # output_ generate table
  observeEvent(input$nutrition_food_sel_show, {
    output$nutrition_food_table <- DT::renderDataTable({
      df_nutrient_table_sub()
    })
  })
  
  
  #%%%%%%%%%%%%%
  
  ## Food Comparison ##
  
  # include an updateseletInput
    ## only nutrients which have values for combination should be displayed
  var_nutrient_food_comp_sel <- reactive({
    df_nutrient_info %>%
      filter(food_2 %in% input$nutrition_food_food) %>%
      dplyr::select(nutrient) %>%
      pull()
  })
  
  observeEvent(input$nutrition_food_food, {
    updateSelectInput(session, "nutrition_food_var",
                      choices = sort(var_nutrient_food_comp_sel()),
                      selected = input$nutrition_food_var
    )
  })
  
  # extract selected food and nutrition for comparison in plot
  df_nutrient_comp_sub <- eventReactive(input$nutrition_food_show, {
    # only perform operation is food and nutrion is selected
    # otherwise empty plot is returned; this ensures that app does not break :D
    if (!is.null(input$nutrition_food_food) & !is.null(input$nutrition_food_var)) {
      df_nutrient_info %>% 
        filter(food_2 %in% input$nutrition_food_food, 
               nutrient %in% input$nutrition_food_var) %>%
        # combine nutrient with unit
        mutate(nutrient = stringr::str_to_sentence(
          paste0(nutrient, " (in ", nutrient_unit, ")"))
        ) %>%
        dplyr::select(-nutrient_unit)
    } else {
      data.frame()
    }
  })
  
  
  
  # output: generate plot
  
  # if action button is not clicked empty plot (avoids spinner from
  # spinning without selection)
  output$nutrition_food_plot <- renderPlot({
    ggplot() + 
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "black"),
        # no grid
        panel.grid = element_blank()
      )
  })
  
  # otherwise click plot
  observeEvent(input$nutrition_food_show, {
    # only perform operation is food and nutrion is selected
    # otherwise empty plot is returned; this ensures that app does not break :D
    if (!is.null(input$nutrition_food_food) & !is.null(input$nutrition_food_var)) {
      # number of food items and nutrient values
      num_nutrient_food <- length(unique(df_nutrient_comp_sub()$food_2))
      num_nutrient_type <- length(unique(df_nutrient_comp_sub()$nutrient))
      
      # colors
      color_bars <- c("#FFFF00", "#F2EA02", "#00FF00", "#00FF66", 
                      "#00FFFF", "#099FFF", "#0033FF", "#FF00FF", "#FF00CC", 
                      "#FF0099", "#CC00FF", "#9D00FF", "#CC00FF", "#6E0DD0",
                      "#9900FF", "#FFFF33", "#FD1C03", "#00FF33", "#E6FB04", 
                      "#FF3300", "#33FF00", "#099FFF", "#0062FF")
      color_bars <- color_bars[1:num_nutrient_food]
      
      # add colors to data frame
      df_color_nutrition <- 
        data.frame(
          food_2 = unique(df_nutrient_comp_sub()$food_2),
          color = color_bars
        )
      
      df_nutrient_comp_sub_final <- 
        left_join(df_nutrient_comp_sub(), df_color_nutrition, by = "food_2")
      
      # if less than 6 food items are selected x-axis labels are vertical;
      # otherwise vertical for better visibility
      if (num_nutrient_food <= 4 & num_nutrient_type <= 4) {
        # create plot
        output$nutrition_food_plot <- renderPlot({
          shiny::validate(need(!is.null(input$nutrition_food_food), 
                               "Please select at least one fruit or vegetable."))
          shiny::validate(need(!is.null(input$nutrition_food_var), 
                               "Please select at least one nutrition."))
          
          ggplot(data = df_nutrient_comp_sub_final, 
                 aes(x = food_2, y = nutrient_value, fill = color)) +
            geom_bar(stat = "identity") + #, fill = color_bars) +
            facet_wrap(~ nutrient, scales = "free") +
            xlab("") + 
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"), 
              # no grid
              panel.grid = element_blank(),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"), 
              strip.text = element_text(colour = 'white', size = 14, face = "bold"),
              # axis labels
              axis.text = element_text(size = 12, colour = "white"),
              axis.text.y = element_text(size = 12, colour = "white"),
              axis.text.x = element_text(size = 12, colour = "white"), 
              # no legend
              legend.position = "none"
            )
        }, height = 600, width = 900)
      } else if (num_nutrient_food <= 12 & num_nutrient_type <= 12) {
        output$nutrition_food_plot <- renderPlot({
          shiny::validate(need(!is.null(input$nutrition_food_food), 
                               "Please select at least one fruit or vegetable."))
          shiny::validate(need(!is.null(input$nutrition_food_var), 
                               "Please select at least one nutrition."))
          
          ggplot(data = df_nutrient_comp_sub_final, 
                 aes(x = food_2, y = nutrient_value, fill = color)) +
            geom_bar(stat = "identity") + #, fill = color_bars) +
            facet_wrap(~ nutrient, scales = "free") +
            xlab("") + 
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"), 
              # no grid
              panel.grid = element_blank(),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"), 
              strip.text = element_text(colour = 'white', size = 14, face = "bold"),
              # axis labels
              axis.text = element_text(size = 12, colour = "white"),
              axis.text.y = element_text(size = 12, colour = "white"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              # no legend
              legend.position = "none"
            )
        }, height = 700, width = 1000)
      } else {
        output$nutrition_food_plot <- renderPlot({
          shiny::validate(need(!is.null(input$nutrition_food_food), 
                               "Please select at least one fruit or vegetable."))
          shiny::validate(need(!is.null(input$nutrition_food_var), 
                               "Please select at least one nutrition."))
          
          ggplot(data = df_nutrient_comp_sub_final, 
                 aes(x = food_2, y = nutrient_value, fill = color)) +
            geom_bar(stat = "identity") + #, fill = color_bars) +
            facet_wrap(~ nutrient, scales = "free") +
            xlab("") + 
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"), 
              # no grid
              panel.grid = element_blank(),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"), 
              strip.text = element_text(colour = 'white', size = 8, face = "bold"),
              # axis labels
              axis.text = element_text(size = 8, colour = "white"),
              axis.text.y = element_text(size = 8, colour = "white"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              # no legend
              legend.position = "none"
            )
        }, height = 700, width = 1000)
      }
    } else {
      output$nutrition_food_plot <- renderPlot({
      shiny::validate(need(!is.null(input$nutrition_food_food), 
                           "Please select at least one fruit or vegetable."))
      shiny::validate(need(!is.null(input$nutrition_food_var), 
                           "Please select at least one nutrition."))
      
      ggplot() + 
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "black"),
          # no grid
          panel.grid = element_blank()
        )
      })
    }
    
    
  })
  
  
  
  ## Footprint ##
  #%%%
  
  
  # info button
  onclick("footprint_theory", runjs("window.open('Theory_Footprint.html')"))
  
  # create data set
  df_footprint <-
    df_basket_info %>%
    dplyr::select(food_2, Carbon_Footprint_open_field, Water_Footprint_open_field) %>%
    distinct() %>%
    dplyr::rename(
      food = food_2,
      `Carbon Footprint` = Carbon_Footprint_open_field,
      `Water Footprint` = Water_Footprint_open_field
    ) %>%
    mutate(
      `Carbon Footprint` = sprintf(`Carbon Footprint`, fmt = '%#.4f')
    )
  
  # reactive data sets
  ## for table
  df_footprint_table <- eventReactive(input$footprint_show, {
    if (!is.null(input$footprint_type) & !is.null(input$footprint_food)) {
      df_footprint %>%
        filter(food %in% input$footprint_food) %>%
        dplyr::select(food, all_of(input$footprint_type)) %>%
        dplyr::arrange(food) 
    } else {
      shiny::validate(need(!is.null(input$footprint_type), 
                           " "))
      shiny::validate(need(!is.null(input$footprint_food), 
                           " "))
      return()
    }
  })
  
  ## for plot
  df_footprint_plot <- eventReactive(input$footprint_show, {
    if (!is.null(input$footprint_type) & !is.null(input$footprint_food)) {
      df_footprint %>%
        filter(food %in% input$footprint_food) %>%
        dplyr::rename(
          `Carbon Footprint in kg C02 per kg for food item` = `Carbon Footprint`,
          `Water Footprint in liters per kg for food item` = `Water Footprint`
        ) %>%
        dplyr::select(food, starts_with(all_of(input$footprint_type))) %>%
        reshape2::melt(id.vars = "food")
    } else {
      shiny::validate(need(!is.null(input$footprint_type), 
                           " "))
      shiny::validate(need(!is.null(input$footprint_food), 
                           " "))
    }
  })
  
  
  # table output
  observeEvent(input$footprint_show, {
    output$footprint_table <- DT::renderDataTable({
      # remind user to select at least one food and footprint item
      shiny::validate(need(!is.null(input$footprint_type), 
                           "Please select at least one footprint type"))
      shiny::validate(need(!is.null(input$footprint_food), 
                           "Please select at least one food item"))
      
      # display table
      DT::datatable(
        data = df_footprint_table() %>% dplyr::rename(`Fruit/Vegetable` = food), 
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          dom = "ltip", # show only table
          pageLength = 5, 
          lengthMenu = c(5, 10, 15, 25))) %>%
        formatStyle(columns = names(df_footprint_table() %>% dplyr::rename(`Fruit/Vegetable` = food)), 
                    color = "white", 
                    backgroundColor = "black", background = "black")
    })
  })
  
  
  # plot output
  observeEvent(input$footprint_show, {
    
    # select number of food items and footprint variables
    # calculation needed for color selection
    num_footprint_food <- length(unique(df_footprint_plot()$food))
    num_footprint_type <- length(unique(df_footprint_plot()$variable))
    num_footprint_colors <- num_footprint_food * num_footprint_type
    
    # define colors
    color_bars <- c("#FFFF00", "#F2EA02", "#00FF00", "#00FF66", 
                    "#00FFFF", "#099FFF", "#0033FF", "#FF00FF", "#FF00CC", 
                    "#FF0099", "#CC00FF", "#9D00FF", "#CC00FF", "#6E0DD0",
                    "#9900FF", "#FFFF33", "#FD1C03", "#00FF33", "#E6FB04", 
                    "#FF3300", "#33FF00", "#099FFF", "#0062FF")
    
    # select optimal number of colors: same color for each food item
    # across footprint variables (hence rep())
    color_bars <- color_bars[1:length(unique(df_footprint_plot()$food))]
    color_bars <- rep(color_bars, num_footprint_colors / num_footprint_food)
    
    # plot: x-axis rotation depends on number of items selected
    # for 4 or less no rotation; otherwise they are vertical 
    if (num_footprint_food <= 4) {
      output$footprint_plot <- renderPlot({
        
        # ensure that plot is only displayed if everything is selected
        shiny::validate(need(!is.null(input$footprint_type), 
                             " "))
        shiny::validate(need(!is.null(input$footprint_food), 
                             " "))
        
        # for plot values should be numeric:
        ggplot(df_footprint_plot() %>%
                 mutate(value = as.numeric(value)), aes(y = value, x = food)) + 
          geom_bar(stat = "identity", fill = color_bars) +
          facet_wrap(~ variable, scales = "free") +
          xlab("") + ylab("footprint value per kg of food item") +
          # specify maximum number of breaks (otherwise sometimes too many
          # which are overlapping)
          scale_y_continuous(n.breaks = 5) + 
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 12, face = "bold"),
            # axis labels
            axis.text = element_text(size = 12, colour = "white"),
            axis.text.y = element_text(size = 12, colour = "white")
          )
      }, width = 800, height = 300) #490
    } else {
      output$footprint_plot <- renderPlot({
        shiny::validate(need(!is.null(input$footprint_type), 
                             " "))
        shiny::validate(need(!is.null(input$footprint_food), 
                             " "))
        
        ggplot(df_footprint_plot() %>%
                 mutate(value = as.numeric(value)), aes(y = value, x = food)) + 
          geom_bar(stat = "identity", fill = color_bars) +
          facet_wrap(~ variable, scales = "free") +
          xlab("") + ylab("footprint value per kg of food item") +
          # specify maximum number of breaks (otherwise sometimes too many
          # which are overlapping)
          scale_y_continuous(n.breaks = 5) + 
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 12, face = "bold"),
            # axis labels
            axis.text = element_text(size = 12, colour = "white"),
            axis.text.y = element_text(size = 12, colour = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
      }, width = 1000, height = 400) #490
      
    }
    
  }) # close observeevent()
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### CLUSTER ANALYSIS ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # info buttons #
  ## info button on k-means clustering algorithm
  #onclick("cluster_theory", runjs("window.open('228_Clustering_kmeans.html')"))
  
  # update select input button
  ## generate reactive variables
  df_cluster_column_selection <- reactive({
    # generate vectors of columns which should be kept
    keep_cols_2 <- c()
    if ("Mean Price" %in% input$cluster_var) {
      keep_cols_price_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(starts_with("Mean Price")) %>% 
        dplyr::select(-c('Mean Price Total')) %>%
        colnames() 
      
      keep_cols_2 <- c(keep_cols_2, keep_cols_price_2)
    }
    if ("Nutrition" %in% input$cluster_var) {
      keep_cols_nutrition_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(!c(starts_with("Mean Price"), matches("Footprint$"))) %>%
        colnames()
      keep_cols_2 <- c(keep_cols_2, keep_cols_nutrition_2)
    }
    if ("Footprint" %in% input$cluster_var) {
      keep_cols_footprint_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(matches("Footprint$")) %>% colnames()
      keep_cols_2 <- c(keep_cols_2, keep_cols_footprint_2)
    }
    c("All", keep_cols_2)
  })
  
  ## update
  observeEvent(input$cluster_var, {
    updateSelectInput(session, "cluster_var_2",
                      choices = sort(df_cluster_column_selection()))
  })
  
  
  ## k-means clustering output ##
  
  
  # perform k-means clustering using inputs
    ## manually
  kmeans_output_manually <-
    eventReactive(input$cluster_show_manually,{
      if (!is.null(input$cluster_var) & 
          (length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All")) {
        func_kmeans(input$cluster_food, input$cluster_num,
                    input$cluster_var, input$cluster_var_2, 
                    input$cluster_num_sel)
      }

    })
    ## using silhouette method
  kmeans_output_auto <-
    eventReactive(input$cluster_show_auto,{
      if (!is.null(input$cluster_var) & 
          (length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All")) {
      func_kmeans(input$cluster_food, input$cluster_num,
                  input$cluster_var, input$cluster_var_2, 
                  input$cluster_num_sel)
      }
    })
  
  
  ## Manually ##
  
  # show output only if action button is clicked
  observeEvent(input$cluster_show_manually, {
    # cluster table
    output$cluster_output_table <- renderTable({
      # instead of error message, print message if wrong selection is made
      
      ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             "Please select fruits and/or vegetables.")
      )
      
      
      ## at least one cluster category needs to be chosen
      shiny::validate(
        need(!is.null(input$cluster_var),
             "Please select at least one cluster category")
      )
      ## at least two cluster variables need to be chosen
      shiny::validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             "Please select at least two variables")
      )
      
      ## if fruits are selected the number of clusters must not exceed 8
      if (!"Vegetables" %in% input$cluster_food) {
        shiny::validate(
          need(input$cluster_num <= 8,
               "Please also include vegetables or reduce the number of clusters to 8 or lower.")
        )
      }
      
      # adjust data frame columns
      data_kmeans_table <- kmeans_output_manually()[[1]]
        
      for (i in 1:ncol(data_kmeans_table)) {
        colnames(data_kmeans_table)[i] <- paste0("Cluster ", i)
      }
      
      # return table
      print(data_kmeans_table)
    })
  })
  
  
  # create empty plot: avoids that spinner moves while nothing is selected
  output$cluster_output_plot <- renderPlot({
    ggplot() +
      theme(
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black")
      )
  })
  
  
  # create cluster plot when action button is clicked
  observeEvent(input$cluster_show_manually, {
    # cluster plot
    output$cluster_output_plot <- renderPlot({
      # instead of error message, print message if wrong selection is made
      
      ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             " ")
      )
      
      ## at least one cluster category needs to be chosen
      validate(
        need(!is.null(input$cluster_var),
             " ")
      )
      ## at least two cluster variables need to be chosen
      validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             " 
             ")
      )
      
      ## if fruits are selected the number of clusters must not exceed 8
      if (!"Vegetables" %in% input$cluster_food) {
        shiny::validate(
          need(input$cluster_num <= 8, " ")
        )
      }
      
      # return plot
      kmeans_output_manually()[[2]]
    })
  })
  
  
  ## automatic: silhouette method ##
  
  # text which shows optimal number of clusters
  observeEvent(input$cluster_show_auto, {
    # chosen number of clusters
    output$cluster_chosen_k <- renderPrint({
      # instead of text from previous, print message if wrong selection is made
      ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             " ")
      )
      
      ## at least one cluster category needs to be chosen
      shiny::validate(
        need(!is.null(input$cluster_var),
             " ")
      )
      ## at least two cluster variables need to be chosen
      shiny::validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             " ")
      )
      
      cat(paste("Number of chosen clusters using the silhouette method: ",
                kmeans_output_auto()[[3]], "\n"))
    })
    
    # cluster table
    output$cluster_output_table <- renderTable({
      # instead of error message, print message if wrong selection is made
      
      ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             "Please select fruits and/or vegetables.")
      )
      
      ## at least one cluster category needs to be chosen
      shiny::validate(
        need(!is.null(input$cluster_var),
             "Please select at least one cluster category")
      )
      ## at least two cluster variables need to be chosen
      shiny::validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             "Please select at least two variables")
      )
      
      ## if fruits are selected the number of clusters must not exceed 8
      if (!"Vegetables" %in% input$cluster_food) {
        shiny::validate(
          need(input$cluster_num <= 8,
               "Please also include vegetables or reduce the number of clusters to 8 or lower.")
        )
      }
      
      # return table
      print(kmeans_output_auto()[[1]])
    })
    
    
    # cluster plot
    output$cluster_output_plot <- renderPlot({
      # instead of error message, print message if wrong selection is made
      
      ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             " ")
      )
      
      ## at least one cluster category needs to be chosen
      shiny::validate(
        need(!is.null(input$cluster_var),
             " ")
      )
      ## at least two cluster variables need to be chosen
      shiny::validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             " ")
      )
      
      ## if fruits are selected the number of clusters must not exceed 8
      if (!"Vegetables" %in% input$cluster_food) {
        shiny::validate(
          need(input$cluster_num <= 8,
               " ")
        )
      }
      
      # return plot
      kmeans_output_auto()[[2]]
    })
  })
  
  
  # plot for determining optimal number of clusters using silhouette method
  # displayed only if Yes is selected
  output$silhouette_plot <- renderPlot({
    if (input$silhouette_method_plot == "Yes") {
      
      # instead of error message, print message if wrong selection is made
      
        ## at least one food type
      shiny::validate(
        need(!is.null(input$cluster_food),
             " ")
      )
      
        ## at least one cluster category needs to be chosen
      shiny::validate(
        need(!is.null(input$cluster_var),
             " ")
      )
        ## at least two cluster variables need to be chosen
      shiny::validate(
        need(length(input$cluster_var_2) > 1 | input$cluster_var_2 == "All",
             " ")
      )
      
      kmeans_output_auto()[[4]]
    } else {
      ggplot() + 
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "black"),
          # no grid
          panel.grid = element_blank()
        )
    }
  })
  
  
  
  # silhouette plot and info
  onclick("silhouette_method", 
          runjs("window.open('Theory_Clustering_Silhoutte_Method.html')"))
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  
  
  #### Price Development ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## Aggregation information for unfamiliar users
  onclick("Agg_button", runjs("window.open('Theory_Descriptives_Aggregation.html')"))
  
  ## Data Viz Buttons
  onclick("box_button", runjs("window.open('Theory_Boxplot_Info.html')"))
  onclick("hist_button", runjs("window.open('Theory_Histogram_Info.html')"))
  onclick("heat_button", runjs("window.open('Theory_Heatmap_Info.html')"))
  
  # reactive data sets based on selection
  value_pred_food_type_reactive_ind <- reactive({
    df_price %>%
      filter(food %in% input$item_ind) %>%
      dplyr::select(type) %>% unique() %>% pull()
  })
  
  value_pred_food_size_reactive_ind <- reactive({
    df_price %>%
      filter(food %in% input$item_ind,
             type %in% input$type_ind) %>%
      dplyr::select(size) %>% unique() %>% pull()
  })
  
  value_pred_food_country_reactive_ind <- reactive({
    df_price %>%
      filter(food %in% input$item_ind,
             type %in% input$type_ind,
             size %in% input$size_ind) %>%
      dplyr::select(country) %>% unique() %>% pull()
  })
  
  
  observeEvent(input$item_ind, {
    updateSelectInput(session, "type_ind",
                      choices = sort(value_pred_food_type_reactive_ind()))
  })
  
  observeEvent(c(input$item_ind, input$type_ind), {
    updateSelectInput(session, "size_ind",
                      choices = mixedsort(value_pred_food_size_reactive_ind()))
  })
  
  observeEvent(c(input$item_ind, input$type_ind,input$size_ind), {
    updateSelectInput(session, "country_ind",
                      choices = sort(value_pred_food_country_reactive_ind()))
  })
  
  
  df_price_ind <- eventReactive(input$disselect_ind, {
    df_price_ind_sub <- data.frame()
    
    df_price_ind_sub <- df_price %>%
      dplyr::filter(food == input$item_ind, type == input$type_ind,
                    size == input$size_ind, country == input$country_ind) %>%
      dplyr::mutate(year = year(date), month = month(date))
    
  })
  
  observeEvent(input$disselect_ind, {
    output$desc_header <- renderText({
      
      paste0("Descriptive Statistics: ", input$item_ind, " - Type ",
             input$type_ind)
    })
  })
  
  observeEvent(input$disselect_ind, {
    output$price_descr_ind_out <- renderText({
      # validate that input is given, i.e. market is selected
      # show table if selected
      if(input$price_descr_ind == TRUE){
        
        shiny::validate(
          need(!is.null(input$price_ind), "Please select at least one market.")
        )
        
        time_group <- input$Time_Group
        
        # add header
        food_item_choice <- input$item_ind 
        
        tab_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                             "Price per Piece in €",
                             "Price per kg in €")
        
        # aggregate by year
        if(time_group == "Year"){
          
          df_year <- df_price_ind() %>%
            dplyr::filter(market %in% input$price_ind)  %>%
            group_by(market, year) %>%
            dplyr::summarise(`Mean Price` = paste(round(mean(price), 2), "€"),
                             `Median Price` = paste(round(median(price), 2), "€"),
                             `Standard Deviation` = paste(round(sd(price), 2), "€"),
                             `Minimum Price` = paste(min(price), "€"),
                             `Maximum Price` = paste(max(price), "€")) %>%
            dplyr::arrange(market, year) %>%
            dplyr::rename(Market = market, Year = year)
          
          number_year <- nrow(df_year)
          header_cols <- ncol(df_year)
          
          df_year %>%
            knitr::kable("html", caption = "<center><strong>Aggregation Level: Year <br></strong></center>") %>%
            kable_styling("striped", position = "left",
                          html_font = "arial") %>%
            kable_classic() %>%
            add_header_above(header = c(setNames(header_cols, tab_header))) %>%
            row_spec(seq(1, number_year, 2), color = "white", background = "#51565c") %>%
            ## add scroll box with space between table and scroller
            scroll_box(height = "600px", fixed_thead =  list(enabled = TRUE, background = "black"),
                       extra_css = "border: 0px; padding: 12px; border-width: 0px;")
        } # aggregate by month
        else if(time_group == "Month"){
          
          df_month <- df_price_ind() %>%
            dplyr::filter(market %in% input$price_ind)  %>%
            group_by(market, month) %>%
            dplyr::summarise(`Mean Price` = paste(round(mean(price), 2), "€"),
                             `Median Price` = paste(round(median(price), 2), "€"),
                             `Standard Deviation` = paste(round(sd(price), 2), "€"),
                             `Minimum Price` = paste(min(price), "€"),
                             `Maximum Price` = paste(max(price), "€")) %>%
            dplyr::arrange(market, month) %>%
            dplyr::mutate(month = dplyr::recode(month,
                                                `1` = "January", `2` = "February",
                                                `3` = "March", `4` = "April",
                                                `5` = "May", `6` = "June",
                                                `7` = "July", `8` = "August",
                                                `9` = "September", `10` = "October",
                                                `11` = "November", `12` = "December")) %>%
            dplyr::rename(Market = market, Month = month)
          
          number_month <- nrow(df_month)
          header_cols_month <- ncol(df_month)
          
          df_month %>%
            knitr::kable("html", caption = "<center><strong>Aggregation Level: Month <br></strong></center>",
                         escape = FALSE) %>%
            kable_styling("striped", position = "left",
                          html_font = "arial") %>%
            kable_classic() %>%
            add_header_above(header = c(setNames(header_cols_month, tab_header))) %>%
            row_spec(seq(1, number_month, 2), color = "white", background = "#51565c") %>%
            ## add scroll box with space between table and scroller
            scroll_box(height = "600px", fixed_thead =  list(enabled = TRUE, background = "black"),
                       extra_css = "border: 0px; padding: 12px; border-width: 0px;")
          
          
        } # no aggregation
        else if(time_group == "None"){
          
          df_none <- df_price_ind() %>%
            dplyr::filter(market %in% input$price_ind)  %>%
            group_by(market) %>%
            dplyr::summarise(`Mean Price` = paste(round(mean(price), 2), "€"),
                             `Median Price` = paste(round(median(price), 2), "€"),
                             `Standard Deviation` = paste(round(sd(price), 2), "€"),
                             `Minimum Price` = paste(min(price), "€"),
                             `Maximum Price` = paste(max(price), "€")) %>%
            dplyr::arrange(market) %>%
            dplyr::rename(Market = market)
          
          number_none <- nrow(df_none)
          header_cols_none <- ncol(df_none)
          
          df_none %>%
            knitr::kable("html", caption = "<center><strong>Aggregation Level: None <br></strong></center>") %>%
            kable_styling("striped", position = "left",
                          html_font = "arial") %>%
            add_header_above(header = c(setNames(header_cols_none, tab_header))) %>%
            kable_classic() %>%
            row_spec(seq(1, number_none, 2), color = "white", background = "#51565c")
        }
      }
    })
  })
  ## Barplot for Descriptives
  observeEvent(input$disselect_ind, {
    output$bar_desc <- renderPlotly({
      
      df_bar <- df_price_ind() %>%
        dplyr::filter(market %in% input$price_ind)
      
      shiny::validate(
        need(!is.null(input$price_ind), "Please select at least one market.")
      )
      
      time_group <- input$Time_Group
      
      # add y_lab title
      food_item_choice <- input$item_ind 
      
      y_lab_desc <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                           "Price per Piece in €",
                           "Price per kg in €")
      
      # Color palette
      col_pal <- c(Total = "#FFFF00", Munich = "#E69F00", Hamburg = "#56B4E9",
                   Frankfurt =  "#009E73", Berlin = "#00FFFF", Cologne = "#0072B2")
      
      
      # Barplots based on aggregation method
      
      # aggregate by year
      if(time_group == "Year"){
        
        df_bar <- df_bar %>%
          dplyr::group_by(market, year) %>%
          dplyr::arrange(market, year) %>%
          dplyr::summarise(mean = mean(price), sd = sd(price))
        
        ggbar <- ggplot(df_bar, aes(x = year, y = mean, 
                                    text = paste0("Year: ", year, '<br>',
                                                  "Mean Price in ", market, ": ", round(mean, 2), 
                                                  " €"))) +
          geom_bar(alpha = 0.75, stat = "identity", aes(fill = market)) +
          scale_x_continuous(breaks = seq(min(df_bar$year), max(df_bar$year), 1)) +
          geom_errorbar(color = "white", aes(ymin = mean - sd, ymax = mean + sd), width = 0.5,
                        position = position_dodge(0.75)) +
          
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          facet_wrap( ~ market, ncol = 2) +
          labs(x ="Year", y = paste("Mean", y_lab_desc)) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", angle = 90),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(r = 8)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 10)),
            # title colour
            plot.title = element_text(color = "white"),
            panel.spacing = unit(1, "lines"))
        
        ggplotly(ggbar, tooltip = "text") 
        
      } # aggregate by month
      else if(time_group == "Month"){
        
        df_bar <- df_bar %>%
          dplyr::group_by(market, month) %>%
          dplyr::arrange(market, month) %>%
          dplyr::summarise(mean = mean(price), sd = sd(price)) %>%
          dplyr::mutate(month = dplyr::recode(month,
                                              `1` = "January", `2` = "February",
                                              `3` = "March", `4` = "April",
                                              `5` = "May", `6` = "June",
                                              `7` = "July", `8` = "August",
                                              `9` = "September", `10` = "October",
                                              `11` = "November", `12` = "December"))
        
        # factor to order by month
        df_bar$month <- factor(df_bar$month, levels = c("January", "February", "March", "April", "May",
                                                        "June", "July", "August","September", "October",
                                                        "November", "December"))
        
        ggbar <- ggplot(df_bar, aes(x = month, y = mean, 
                                    text = paste0("Month: ", month, '<br>',
                                                  "Mean Price in ", market, ": ", round(mean, 2), 
                                                  " €"))) +
          geom_bar(alpha = 0.75, stat = "identity", aes(fill = market)) +
          geom_errorbar(color = "white", aes(ymin = mean - sd, ymax = mean + sd), width = 0.5,
                        position = position_dodge(0.75)) +
          
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          facet_wrap( ~ market, ncol = 2) +
          labs(x = "Month", y = paste("Mean", y_lab_desc)) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", angle = 90),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 8, b = 0, l = 0)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
            # title colour
            plot.title = element_text(color = "white"),
            panel.spacing = unit(1, "lines"))
        
        ggplotly(ggbar, tooltip = "text")
        
        
      } # no aggregation
      else if(time_group == "None"){
        
        df_bar <- df_bar %>%
          dplyr::group_by(market) %>%
          dplyr::summarise(mean = mean(price), sd = sd(price))
        
        ggbar <- ggplot(df_bar, aes(x = market, y = mean, 
                                    text = paste0("Mean Price in ", market, ": ", round(mean, 2), 
                                                  " €"))) +
          geom_bar(width = 0.55, alpha = 0.75, stat = "identity", aes(fill = market)) +
          geom_errorbar(color = "white", aes(ymin = mean - sd, ymax = mean + sd), width = 0.25,
                        position = position_dodge(0.75)) +
          
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          labs(x = "Market", y = paste("Mean", y_lab_desc)) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", angle = 90),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0)),
            # title colour
            plot.title = element_text(color = "white"),
            panel.spacing = unit(2, "lines"))
        
        ggplotly(ggbar, tooltip = "text")
      }
      
    })
  })
  
  ## reload after refresh: also for EDA
  
  df_price_eda <- eventReactive(input$disselect_ind, {
    df_price_eda_sub <- data.frame()
    
    df_price_eda_sub <- df_price %>%
      dplyr::filter(food == input$item_ind, size == input$size_ind,
                    country == input$country_ind, type == input$type_ind) %>%
      dplyr::mutate(year = year(date), month = month(date))
    
  })
  
  output$eda_header <- renderText({
    
    paste0(input$desc_viz, ": ", input$item_ind, " - Type ", input$type_ind)
    
  })
  
  ## Boxplot
  
  output$box_desc <- renderPlotly({
    shiny::validate(
      need(!is.null(input$price_ind), "Please select at least one market")
    )
    
    time_group <- input$Time_Group_eda
    
    # add y label 
    food_item_choice <- input$item_ind 
    
    tab_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                         "Price per Piece in €",
                         "Price per kg in €")
    
    df_box <- df_price_eda() %>%
      dplyr::filter(market %in% input$price_ind)
    
    # Color palette
    col_pal <- c(Total = "#FFFF00", Munich = "#E69F00", Hamburg = "#56B4E9",
                 Frankfurt =  "#009E73", Berlin = "#00FFFF", Cologne = "#0072B2")
    
    if(input$desc_viz == "Box Plot"){
      
      # aggregate by year
      if(time_group == "Year"){
        
        df_box <- df_box %>%
          dplyr::group_by(market, year)%>%
          dplyr::arrange(market, year)
        
        
        ggbox <- ggplot(df_box, aes(x = market, y = price, fill = market)) +
          geom_boxplot(alpha = 0.8, color = "white") +
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          facet_wrap(~ year, ncol = 4) +
          labs(x ="Market", y = tab_header) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", angle = 90),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 8, b = 0, l = 0)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 6, r = 0, b = 0, l = 0)),
            # title colour
            plot.title = element_text(color = "white"),
            panel.spacing = unit(1, "lines"))
        
        ggplotly(ggbox)
        
        
      } # aggregate by month
      else if(time_group == "Month"){
        
        df_box <- df_box %>%
          dplyr::group_by(market, month) %>%
          dplyr::arrange(market, month)
        
        
        ggbox <- ggplot(df_box, aes(x = market, y = price, fill = market)) +
          geom_boxplot(alpha = 0.8, color = "white") +
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          facet_wrap(~ month, ncol = 4, labeller = labeller(month = c(`1` = "January", `2` = "February",
                                                                      `3` = "March", `4` = "April",
                                                                      `5` = "May", `6` = "June",
                                                                      `7` = "July", `8` = "August",
                                                                      `9` = "September", `10` = "October",
                                                                      `11` = "November", `12` = "December"))) +
          labs(x ="Market", y = tab_header, fill = "Market") +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white", angle = 90),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 4, b = 0, l = 0)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
            # title colour
            plot.title = element_text(color = "white"),
            panel.spacing = unit(1, "lines")) 
        
        
        
        ggplotly(ggbox)
        
      } # no aggregation
      else if(time_group == "None"){
        
        df_box <- df_box %>%
          dplyr::group_by(market)
        
        ggbox <- ggplot(df_box, aes(x = market, y = price, fill = market)) +
          geom_boxplot(alpha = 0.8, color = "white") +
          # change color of boxes
          scale_fill_manual("Market", values = col_pal) +
          labs(x ="Market", y = tab_header) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            # change color of facet
            strip.background = element_rect(fill = "darkgray"),
            strip.text = element_text(colour = 'white'),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            # axis color
            axis.text.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)),
            # title colour
            plot.title = element_text(color = "white"))
        
        
        ggplotly(ggbox)
        
        
      }
      
    }
  })
  
  
  ## Histogram
  
  output$hist_desc <- renderPlotly({
    
    shiny::validate(
      need(!is.null(input$price_ind), "No selection. Please select exactly one market.")
    )
    
    shiny::validate(
      need(length(input$price_ind) == 1, "More than one market selected. Please select exactly one market.")
    )
    
    # colour mapping
    col_pal <- c(Total = "#FFFF00", Munich = "#E69F00", Hamburg = "#56B4E9",
                 Frankfurt =  "#009E73", Berlin = "#00FFFF", Cologne = "#0072B2")
    
    time_group <- input$Time_Group_eda
    
    # add y lab
    food_item_choice <- input$item_ind 
    
    tab_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                         "Price per Piece in €",
                         "Price per kg in €")
    
    df_hist <- df_price_eda() %>%
      dplyr::filter(market %in% input$price_ind)
    
    
    
    if(input$desc_viz == "Histogram"){
      
      # aggregate by year
      if(time_group == "Year"){
        if(input$hist_dens == "Hide Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market, year)%>%
            dplyr::arrange(market, year)
          
          
          gghist <- ggplot(df_hist, aes(x = price, color = market, fill = market,
                                        text = paste0("Price in Market ", market, ": ", 
                                                      paste(round(price, 2), "€")))) +
            geom_histogram(binwidth = input$hist_bin) +
            facet_wrap( ~ year, ncol = 4) +
            
            # change color of boxes
            scale_color_manual("Market", values = col_pal) +
            scale_fill_manual("", values = col_pal) +
            labs(x = tab_header, y = "Count") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white", angle = 90),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 3, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white"),
              panel.spacing = unit(1, "lines"))
          
          
          ggplotly(gghist, tooltip = "text")
          
        } # density false
        
        else if(input$hist_dens == "Show Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market, year)%>%
            dplyr::arrange(market, year)
          
          
          gghist <- ggplot(df_hist, aes(x = price)) +
            geom_histogram(binwidth = input$hist_bin, aes(y = ..density.., fill = market)) +
            geom_density(alpha = 0.7, aes(y=..density..), fill = "skyblue") + 
            facet_wrap( ~ year, ncol = 4) +
            
            # change color of boxes
            scale_fill_manual("Market", values = col_pal) +
            labs(x = tab_header, y = "Density") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white", angle = 90),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 3, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white"),
              panel.spacing = unit(1, "lines"))
          
          
          ggplotly(gghist)
          
        } # density true
        
      } # aggregate by month
      else if(time_group == "Month"){
        if(input$hist_dens == "Hide Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market, month) %>%
            dplyr::arrange(market, month)
          
          
          gghist <- ggplot(df_hist, aes(x = price, color = market, fill = market,
                                        text = paste0("Price in Market ", market, ": ", 
                                                      paste(round(price, 2), "€")))) +
            geom_histogram(binwidth = input$hist_bin) +
            facet_wrap(~ month, ncol = 4, labeller = labeller(month = c(`1` = "January", `2` = "February",
                                                                        `3` = "March", `4` = "April",
                                                                        `5` = "May", `6` = "June",
                                                                        `7` = "July", `8` = "August",
                                                                        `9` = "September", `10` = "October",
                                                                        `11` = "November", `12` = "December"))) +
            labs(x = tab_header, y = "Count") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white", angle = 90),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              panel.spacing = unit(1, "lines")) +
            scale_fill_manual("Market", values = col_pal) +
            scale_color_manual("", values = col_pal)
          
          
          
          ggplotly(gghist, tooltip = "text")
        }
        else if(input$hist_dens == "Show Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market, month) %>%
            dplyr::arrange(market, month)
          
          
          gghist <- ggplot(df_hist, aes(x = price)) +
            geom_histogram(binwidth = input$hist_bin, aes(y = ..density.., fill = market)) +
            geom_density(alpha = 0.70, aes(y=..density..), fill = "skyblue") + 
            facet_wrap(~ month, ncol = 4, labeller = labeller(month = c(`1` = "January", `2` = "February",
                                                                        `3` = "March", `4` = "April",
                                                                        `5` = "May", `6` = "June",
                                                                        `7` = "July", `8` = "August",
                                                                        `9` = "September", `10` = "October",
                                                                        `11` = "November", `12` = "December"))) +
            # change color of boxes
            scale_fill_manual("Market", values = col_pal) +
            labs(x = tab_header, y = "Density") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white", angle = 90),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 3, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              panel.spacing = unit(1, "lines"))
          
          
          
          ggplotly(gghist)
          
        }
        
      } # no aggregation
      else if(time_group == "None"){
        if(input$hist_dens == "Hide Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market)
          
          gghist <- ggplot(df_hist, aes(x = price, color = market, fill = market, 
                                        text = paste0("Price in Market ", market, ": ", 
                                                      paste(round(price, 2), "€")))) +
            geom_histogram(binwidth = input$hist_bin) +
            labs(x = tab_header, y = "Count") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white"),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white")) +
            # change color of boxes
            scale_fill_manual("Market", values = col_pal) +
            scale_color_manual("", values = col_pal)
          
          
          ggplotly(gghist, tooltip = "text")
        } 
        # with density
        else if(input$hist_dens == "Show Density"){
          
          df_hist <- df_hist %>%
            dplyr::group_by(market)
          
          gghist <- ggplot(df_hist, aes(x = price)) +
            geom_histogram(binwidth = input$hist_bin, aes(y = ..density.., fill = market)) +
            geom_density(alpha = 0.70, aes(y=..density..), fill = "skyblue") + 
            # change color of boxes
            scale_fill_manual("Market", values = col_pal) +
            labs(x = tab_header, y = "Density") +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "white"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"),
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"),
              legend.title = element_text(size = 12, colour = "white"),
              # change color of facet
              strip.background = element_rect(fill = "darkgray"),
              strip.text = element_text(colour = 'white'),
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              # axis color
              axis.text.y = element_text(colour = "white"),
              axis.text.x = element_text(colour = "white"),
              axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 3, b = 0, l = 0)),
              axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
              # title colour
              plot.title = element_text(color = "white"))
          
          
          ggplotly(gghist)
          
        }
        
        
      }
      
    }
  })
  
  
  ## Heatmap Plot
  
  output$heat_desc <- renderPlotly({
    
    shiny::validate(
      need(length(input$price_ind) == 1, "Please select exactly one market.")
    )
    
    # add y label 
    food_item_choice <- input$item_ind 
    
    tab_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                         "Price per Piece in €",
                         "Price per kg in €")
    
    
    heat_col <- input$colour_heat
    
    # filter incomplete years, as they cause NA values
    current_year <- lubridate::year(Sys.Date())
    
    df_heat <- df_price_eda() %>%
      dplyr::filter(market %in% input$price_ind) %>%
      dplyr::filter(!(year %in% current_year)) %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise(`Mean Price` = round(mean(price), 2)) %>%
      dplyr::mutate(month = dplyr::recode(month,
                                          `1` = "January", `2` = "February",
                                          `3` = "March", `4` = "April",
                                          `5` = "May", `6` = "June",
                                          `7` = "July", `8` = "August",
                                          `9` = "September", `10` = "October",
                                          `11` = "November", `12` = "December"))
    
    # month to factor as in barplot
    df_heat$month <- factor(df_heat$month, levels = c("January", "February", "March", "April", "May",
                                                      "June", "July", "August","September", "October",
                                                      "November", "December"))
    
    # plot
    if(heat_col == "Viridis"){
      
      ggtile <- ggplot(df_heat, aes(x = year, y = month, 
                                    text = paste0("Year: ", year, '<br>', 
                                                  "Month: ", month, '<br>',
                                                  "Mean Price: ", `Mean Price`, " €"))) + 
        geom_tile(aes(fill= `Mean Price`)) +
        scale_fill_viridis(name = paste("Mean", tab_header), option = "D") +
        labs(x = "Year", y = "Month") + 
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "white"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"),
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"),
          legend.title = element_text(colour = "white"), 
          # change color of facet
          strip.background = element_rect(fill = "darkgray"),
          strip.text = element_text(colour = 'white'),
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          # axis color
          axis.text.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white", angle = 90),
          axis.title.y = element_text(colour = "white", margin = ggplot2::margin(r = 20)),
          axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 10)),
          # title colour 
          plot.title = element_text(color = "white")) + 
        scale_x_continuous(breaks = seq(min(df_heat$year), max(df_heat$year), 1))
      
      
      ggplotly(ggtile, tooltip = "text")
      
    }
    
    else if(heat_col == "Cividis"){
      
      ggtile <- ggplot(df_heat, aes(x = year, y = month, 
                                    text = paste0("Year: ", year, '<br>', 
                                                  "Month: ", month, '<br>',
                                                  "Mean Price: ", `Mean Price`, " €"))) +
        
        geom_tile(aes(fill= `Mean Price`)) +
        scale_fill_viridis(option = "E") +
        labs(x = "Year", y = "Month", fill = paste("Mean", tab_header)) +
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "white"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"),
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"),
          legend.title = element_text(colour = "white"), 
          # change color of facet
          strip.background = element_rect(fill = "darkgray"),
          strip.text = element_text(colour = 'white'),
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          # axis color
          axis.text.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white", margin =ggplot2::margin(r = 60)),
          axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 30)),
          # title colour
          plot.title = element_text(color = "white")) + 
        scale_x_continuous(breaks = seq(min(df_heat$year), max(df_heat$year), 1))
      
      ggplotly(ggtile, tooltip = "text")
      
    }
    else if(heat_col == "Plasma"){
      
      ggtile <- ggplot(df_heat, aes(x = year, y = month, 
                                    text = paste0("Year: ", year, '<br>', 
                                                  "Month: ", month, '<br>',
                                                  "Mean Price: ", `Mean Price`, " €"))) +
        geom_tile(aes(fill = `Mean Price`)) +
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "white"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"),
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"),
          legend.title = element_text(colour = "white"), 
          # change color of facet
          strip.background = element_rect(fill = "darkgray"),
          strip.text = element_text(colour = 'white'),
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          # axis color
          axis.text.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white", margin =ggplot2::margin(r = 60)),
          axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 30)),
          # title colour
          plot.title = element_text(color = "white")) + 
        scale_x_continuous(breaks = seq(min(df_heat$year), max(df_heat$year), 1)) +
        scale_fill_viridis(option = "C") +
        labs(x = "Year", y = "Month", fill = paste("Mean", tab_header)) 
      
      
      ggplotly(ggtile, tooltip = "text")
      
    }
    else if(heat_col == "Rocket"){
      
      ggtile <- ggplot(df_heat, aes(x = year, y = month, 
                                    text = paste0("Year: ", year, '<br>', 
                                                  "Month: ", month, '<br>',
                                                  "Mean Price: ", `Mean Price`, " €"))) +
        geom_tile(aes(fill= `Mean Price`)) +
        scale_fill_viridis(option = "F") +
        labs(x = "Year", y = "Month", fill = paste("Mean", tab_header)) +
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "white"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"),
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"),
          legend.title = element_text(colour = "white"), 
          # change color of facet
          strip.background = element_rect(fill = "darkgray"),
          strip.text = element_text(colour = 'white'),
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          # axis color
          axis.text.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white", margin = ggplot2::margin(r = 2)),
          axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 3)),
          # title colour
          plot.title = element_text(color = "white")) + 
        scale_x_continuous(breaks = seq(min(df_heat$year), max(df_heat$year), 1))
      
      ggplotly(ggtile, tooltip = "text")
      
    }
    
  })
  
  
  # reactive data sets based on selection: this time for second item
  ## only for time series plot
  
  value_pred_food_type_reactive_ind_ts <- reactive({
    df_price %>%
      filter(food %in% input$item_ind2) %>%
      dplyr::select(type) %>% unique() %>% pull()
  })
  
  value_pred_food_country_reactive_ind_ts <- reactive({
    df_price %>%
      filter(food %in% input$item_ind2,
             type %in% input$type_ind2) %>%
      dplyr::select(country) %>% unique() %>% pull()
  })
  
  value_pred_food_size_reactive_ind_ts <- reactive({
    df_price %>%
      filter(food %in% input$item_ind2,
             type %in% input$type_ind2,
             country %in% input$country_ind2) %>%
      dplyr::select(size) %>% unique() %>% pull()
  })
  
  
  # update select input based on selection
  observeEvent(input$item_ind2, {
    updateSelectInput(session, "type_ind2",
                      choices = sort(value_pred_food_type_reactive_ind_ts()))
  })
  
  observeEvent(c(input$item_ind2, input$type_ind2), {
    updateSelectInput(session, "country_ind2",
                      choices = sort(value_pred_food_country_reactive_ind_ts()))
  })
  
  observeEvent(c(input$item_ind2, input$type_ind2, input$country_ind2), {
    updateSelectInput(session, "size_ind2",
                      choices = mixedsort(value_pred_food_size_reactive_ind_ts()))
  })
  
  df_price_ind2 <- eventReactive(input$disselect_ind, {
    df_price_ind_sub2 <- data.frame()
    
    df_price_ind_sub2 <- df_price %>%
      dplyr::filter((food == input$item_ind & type == input$type_ind & size == input$size_ind & country == input$country_ind)
                    |(food == input$item_ind2 & type == input$type_ind2 & size == input$size_ind2 & country == input$country_ind2)) %>%
      dplyr::mutate(year = year(date), month = month(date))
    
  })
  
  ## Time Series header
  
  output$ts_header <- renderText({
    
    if(input$ts_items == "Single Item"){
      
      paste("Price Development: ", input$item_ind, "- Type ", input$type_ind,
            " \n from", input$country_ind, "Size", input$size_ind)
      
    }
    
    else if(input$ts_items == "Two Items"){
      
      paste("Price Development Comparison: \n", input$item_ind, "- Type", input$type_ind,
            "and", input$item_ind2, "- Type", input$type_ind2)
      
    }
    
  })
  
  
  ## time series plots
  
  output$plot_ts_price_ind <- renderPlotly({
    
    shiny::validate(
      need(!is.null(input$price_ind), "No selection. Please select exactly one market.")
    )
    
    # Time Series plot for one item: All market possible in one plot
    if(input$ts_items == "Single Item"){
      
      # add y lab
      food_item_choice <- input$item_ind 
      
      tab_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                           "Price per Piece in €",
                           "Price per kg in €")
      
      # colour mapping
      col_pal <- c(Total = "#FFFF00", Munich = "#E69F00", Hamburg = "#56B4E9",
                   Frankfurt =  "#009E73", Berlin = "#00FFFF", Cologne = "#0072B2")
      
      df_ts <- df_price_ind() %>%
        dplyr::filter(market %in% input$price_ind)
      
      shiny::validate(
        need(!is.null(input$date_range), "Please select a date range")
      )
      
      shiny::validate(
        need((input$date_range[1] >= min(df_ts$date)) & (input$date_range[2] <= max(df_ts$date)),
             "Please select a date range")
      )
      
      df_ts <- df_ts %>%
        dplyr::filter(date >= input$date_range[1], date <= input$date_range[2])
      
      ggplotly(
        
        ggplot(data = df_ts) +
          geom_line(aes(x = date, y = price, color = market, 
                        text = paste0("Date: ", date %>% format("%B %d, %Y"), '<br>',
                                      "Price in ", market, ": ", 
                                      sprintf(price, fmt = '%#.2f'), "\u20ac"), group = 1)) +
          xlab("Date") +
          ylab(tab_header) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.title = element_text(size = 12, colour = "white"),
            legend.key = element_rect(fill = "black"),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white"),
            plot.title = element_text(colour = "white")
          ) +
          # change color of bars
          scale_color_manual("Market", values = col_pal) +
          scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y"), 
        tooltip = "text"
      ) %>%
        
        # add slider to make selections
        plotly::layout(hovermode = "x unified", xaxis = list(title = "year", rangeslider = list(), type = "date"))
      
    } ## Now comparing two items: faceting markets to maintain good structure
    
    else if(input$ts_items == "Two Items"){
      
      shiny::validate(
        need(length(input$price_ind) == 1, "More than one market selected. Please select exactly one market.")
      )
      
      # add y lab
      food_item_choice <- input$item_ind 
      
      ts_header <- ifelse(food_item_choice %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                          "Price per Piecein €",
                          "Price per kg in €")
      
      # add potential second label 
      food_item_choice_2 <- input$item_ind2
      
      ts_header2 <- ifelse(food_item_choice_2 %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                           "Price per Piece in €",
                           "Price per kg in €")
      
      ## set y label 
      y_lab_two_items <- ifelse(ts_header == ts_header2, ts_header, 
                                paste0(ts_header, " for ", food_item_choice, '<br>', 
                                       ts_header2, " for ", food_item_choice_2))
      
      # colour mapping
      col_food_pal <- c("#FFFF00", "#00FFFF")
      
      df_ts <- df_price_ind2() %>%
        dplyr::filter(market %in% input$price_ind)
      
      shiny::validate(
        need(!is.null(input$date_range), "Please select a date range")
      )
      
      shiny::validate(
        need((input$date_range[1] >= min(df_ts$date)) & (input$date_range[2] <= max(df_ts$date)),
             "Please select a date range")
      )
      
      df_ts <- df_ts %>%
        dplyr::filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
        dplyr::mutate(food_type = paste0(food, " - ", type, ": \n", size, " - ", country))
      
      ggplotly(
        
        ggplot(data = df_ts) +
          geom_line(aes(x = date, y = price, color = food_type,  
                        text = paste0("Date: ", date %>% format("%B %d, %Y"), '<br>',
                                      "Price of ", food, " - ", type, '<br>',
                                      " in ", market, ": ", 
                                      sprintf(price, fmt = '%#.2f'), "\u20ac"), group = 1)) +
          xlab("Date") +
          ylab(y_lab_two_items) +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "white"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"),
            # no grid
            panel.grid.major = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 10, colour = "white"),
            legend.key = element_rect(fill = "black"),
            # axis labels
            axis.text = element_text(size = 10, colour = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white"),
            legend.position = "top"
          ) +
          # change color of bars
          scale_color_manual("legend", values = col_food_pal) +
          scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y"), 
        tooltip = "text"
      ) %>%
        
        # add slider to make selections
        plotly::layout(hovermode = "x unified", xaxis = list(title = "year", rangeslider = list(), type = "date"))
      
    }
    
    
  })
  
  #### Predictor Development ####
  #%%%%%%%%%%%%%%%%%%%%%#
  
  observeEvent(input$refresh_weather_price, {
    updateSelectInput(session, "Food_Type_p", choices = var_foodtype_p())
    updateSelectInput(session, "Food__p", choices = var_food_p())
    updateSelectInput(session, "Country_p", choices = var_country_p())
    updateSelectInput(session, "Type_p", choices = var_type_p())
    updateSelectInput(session, "predictorzoom", choices = c("Overall", "Yearly"), selected = c("Overall"))
  })
  
  ################################# General Information ########################
  output$general_info_predictor_anaylsis <- renderUI({
    tags$img(src = 'literature.PNG', width = "70%")
  })
  
  ################################# On Click Buttons for information ###########
  
  onclick("show_single_predictor_info", runjs("window.open('Theory_Single_Predictor_Analysis.html')"))
  
  onclick("show_predictor_comparison_info", runjs("window.open('Theory_Predictor_Comparison.html')"))
  
  onclick("show_weather_analysis_info", runjs("window.open('Theory_Weather_Analysis.html')"))
  
  onclick("show_weather_on_price_info", runjs("window.open('Theory_Weather_on_Price.html')"))
  
  onclick("show_weather_general_info", runjs("window.open('Theory_Weather_General.html')"))
  
  onclick("show_multivariate_comparison", runjs("window.open('Theory_Multivariate_Comparison.html')"))
  
  ########################### Not dataset variable names but real labels #######
  ######## For correct variables on x-axis and y-axis ##########################
  
  #choice_price_vec <- c("Price in Hamburg" = "price_H",
  #"Price in Munich" = "price_M",
  #"Price in Berlin" = "price_B",
  #"Price in Cologne" = "price_K",
  #"Price in Frankfurt" = "price_F")
  
  #choice_predictor_vec <- c("World Container Index" = "wci",
  #"Crude Oil Price" = "Crude_Oil_Price",
  #"Kerosene Price" = "Kerosene_Price",
  #"Diesel Fuel Price in Germany" = "fuel_price_diesel_Germany",
  #"Triple superphosphate" = "TSP",
  #"Di-ammonium Phosphate" = "DAP",
  #"Urea Fertilizer" = "Urea",
  #"Phosphorite" = "Phosphate_Rock",
  #"Potassium Chloride" = "Potassium_chloride",
  #"Exchange Rate South Africa" = "exchange_rate_South_Africa",
  #"Exchange Rate Turkey" = "exchange_rate_Turkey",
  #"Exchange Rate New Zealand" = "exchange_rate_New_Zealand",
  #"Inlfation in Germany in %" = "inflation_perc_Germany",
  #"Price Index" = "Price_Index")
  
  #choice_weather_easy_vec <- c("Temperature" = "lag_2_weeks_temp",
  #"Humidity" = "lag_2_weeks_humidity",
  #"Cloud Cover" = "lag_2_weeks_cloudcover",
  #"Precipitation" = "lag_2_weeks_precip",
  #"Wind Speed" = "lag_2_weeks_windspeed",
  #"Deviation from normal Temperature" = "lag_2_weeks_dev_temp",
  #"Deviation from normal Humidity" = "lag_2_weeks_dev_humidity",
  #"Deviation from normal Cloud Cover" = "lag_2_weeks_dev_cloudcover",
  #"Deviation from normal Precipitation" = "lag_2_weeks_dev_precip",
  #"Deviation from normal Wind Speeds" = "lag_2_weeks_dev_windspeed")
  
  
  ################################# On first tab ###############################
  
  output$text_univariate <- renderText({
    "Summary Statistics for your chosen predictor variable:"
  })
  
  output$summary_univariate <- renderPrint({
    summary(f[, c(input$singlevar)])
  })
  
  output$singleplot <- renderPlot({
    df <- f
    df$singleplot_y <- df[[input$singlevar]]
    ggplot(df, aes(date, singleplot_y)) +
      geom_line(size=2, color = "white") +
      xlab("Date") +
      #ylab(input$singlevar) +
      ylab(names(choice_predictor_vec)[choice_predictor_vec == input$singlevar]) +
      scale_x_date(date_labels = "%Y (%b)") +
      ggtitle("The Development of your chosen variable over time") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, color = "white"),
        axis.line = element_line(color = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
    
    #axis.text = element_text(size=15),
    #axis.title.x = element_blank(),
    #axis.ticks.length=unit(.25, "cm"),
    #axis.ticks = element_line(colour = "white", size = 2)) +
    #ggtitle("The Development of your chosen variable over time")
  }, height = 360, width = 600)
  
  output$singleplot_price <- renderPlot({
    df <- f
    df$singleplot_x <- df[[input$singlevar]]
    df$singleplot_xy <- df[[input$singlemarket]]
    ggplot(df, aes(singleplot_x, singleplot_xy)) +
      geom_point(alpha=0.4, color = "white") +
      geom_smooth(method="lm", se=F, color="blue") +
      xlab(names(choice_predictor_vec)[choice_predictor_vec == input$singlevar]) +
      #xlab(input$singlevar) +
      #ylab(input$singlemarket) +
      ggtitle("The overall relationship between your chosen predictor variable and market prices") +
      ylab(paste0(names(choice_price_vec)[choice_price_vec == input$singlemarket], " in EURO/ KG/Item")) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.text = element_text(size = 12, colour = "white"),
        axis.title = element_text(colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
    #theme(axis.line = element_line(colour = "white", 
    #size = 1.5, linetype = "solid"),
    #axis.text = element_text(size=15),
    #axis.title.x = element_blank(),
    #axis.ticks.length=unit(.25, "cm"),
    #axis.ticks = element_line(colour = "white", size = 2)) +
    #ggtitle("The overall relationship between your chosen predictor variable and market prices")
    
  }, height = 300, width = 600)
  
  ################################# On second tab ##############################
  
  output$text1 <- renderText({
    "Summary Statistics for both variables before blue line:"
  })
  
  output$text2 <- renderText({
    "Summary Statistics for both variables after blue line:"
  })
  
  output$summary <- renderPrint({
    data_before <- dplyr::filter(g, date<input$onedate)
    summary(data_before[, c(input$y, input$y_2)])
  })
  
  output$summarycorona <- renderPrint({
    data <- dplyr::filter(g, date>input$onedate)
    summary(data[, c(input$y, input$y_2)])
  })
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = g, aes(x = date)) +
      geom_line(aes_string(y= input$y), size = 2, color = "white") +
      geom_vline(xintercept=as.numeric(as.Date(input$onedate)), color = "blue") +
      scale_x_date(date_labels = "%Y (%b)") +
      xlab("") +
      ylab(names(choice_predictor_vec)[choice_predictor_vec == input$y]) +
      coord_cartesian(xlim = c(input$date[1], input$date[2])) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
  }, height = 370, width = 600)
  
  output$scatterplot2 <- renderPlot({
    ggplot(data = g, aes(x = date)) +
      geom_line(aes_string(y= input$y_2), size = 2, color="white") +
      geom_vline(xintercept=as.numeric(as.Date(input$onedate)), color = "blue") +
      scale_x_date(date_labels = "%Y (%b)") +
      ylab(names(choice_predictor_vec)[choice_predictor_vec == input$y_2]) +
      coord_cartesian(xlim = c(input$date[1], input$date[2])) +
      xlab("Date") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
  }, height = 370, width = 600)
  
  
  ################################# On third tab ###############################
  
  data <- f
  
  output$foodtype <- renderUI({
    selectInput(inputId = "Food_Type", "Select Food Type:", choices = var_foodtype(), selected = "Fruits", multiple = F)
    #tags$head(tags$style(HTML(".selectize-input {height: 10px; width: 200px; font-size: 15px;}")))
  })
  output$country <- renderUI({
    selectInput(inputId = "Country", "Select Country:", choices = var_country(), selected="Spain", multiple = F)
  })
  
  value_filtered <- reactive({
    dplyr::filter(f, food_type %in% foodtype(), country %in% country())
  })
  
  foodtype <- reactive({
    if (is.null(input$Food_Type)) unique(f$food_type) else input$Food_Type
  })
  
  country <- reactive({
    if (is.null(input$Country)) unique(f$country) else input$Country
  })
  
  var_foodtype <- reactive({
    sort(unique(f$food_type))
  })
  
  var_country <- reactive({
    dplyr::filter(data, food_type %in% foodtype()) %>% 
      dplyr::pull(country) %>% 
      unique() 
  })
  
  output$univariatePlot <- renderPlotly({
    
    df <- value_filtered()
    df$xx <- df[[input$histvar]]
    
    df%>%
      dplyr::group_by(year) %>%
      plot_ly(x = ~ xx,
              frame = ~year) %>%
      add_histogram(nbinsx= 30) %>%
      layout(showlegend= FALSE,
             #xaxis = list(title=input$histvar),
             title="Histogram for your chosen weather variable",
             xaxis = list(title = names(choice_weather_easy_vec)[choice_weather_easy_vec == input$histvar]),
             font = list(color = "white"),
             plot_bgcolor = "black",
             paper_bgcolor = "black")
  })
  
  output$boxplotweatherfactor <- renderPlot({
    
    df <- f
    df$xx <- df[[input$factortemp]]
    df$xy <- df[[input$histvar]]
    
    
    fac <- with(df, reorder(xx, xy, median, order = TRUE))
    df$xx <- factor(df$xx, levels = levels(fac))
    
    
    ggplot(df, aes(x=xx)) +
      geom_boxplot(aes(y=xy)) +
      coord_flip() +
      xlab("") +
      ylab(names(choice_weather_easy_vec)[choice_weather_easy_vec == input$histvar]) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white")
      )
    
    
  })
  
  ################################### On fourth tab ##############################
  data <- f
  
  output$foodtype_s <- renderUI({
    selectInput(inputId = "Food_Type_s", "Select Food Type:", choices = var_foodtype_s(), multiple = F, selected = "Vegetables")
    #tags$head(tags$style(HTML(".selectize-input {height: 10px; width: 200px; font-size: 15px;}")))
  })
  output$food_s <- renderUI({
    selectInput(inputId = "Food_s", "Select Fruit/Vegetable:", choices = var_food_s(), multiple = F, selected = "Carrot")
  })
  output$country_s <- renderUI({
    selectInput(inputId = "Country_s", "Select Country:", choices = var_country_s(), multiple = F, selected = "Italy")
  })
  output$type_s <- renderUI({
    selectInput(inputId = "Type_s", "Select Type:", choices = var_type_s(), multiple = F, selected = "Loose")
  })
  output$size_s <- renderUI({
    selectInput(inputId = "Size_s", "Select Size:", choices = var_size_s(), multiple = F, selected = "One Size")
  })
  
  value_filtered_s <- reactive({
    dplyr::filter(f, food_type %in% foodtype_s(), food %in% food_s(), country %in% country_s(), type %in% type_s(), size %in% size_s())
  })
  
  date_filtered_s <- reactive({
    dplyr::filter(value_filtered_s(), between(date, input$date_s[1], input$date_s[2]))
  })
  
  foodtype_s <- reactive({
    if (is.null(input$Food_Type_s)) unique(f$food_type) else input$Food_Type_s
  })
  
  food_s <- reactive({
    if (is.null(input$Food_s)) unique(f$food) else input$Food_s
  })
  
  country_s <- reactive({
    if (is.null(input$Country_s)) unique(f$country) else input$Country_s
  })
  
  type_s <- reactive({
    if (is.null(input$Type_s)) unique(f$type) else input$Type_s
  })
  
  size_s <- reactive({
    if (is.null(input$Size_s)) unique(f$size) else input$Size_s
  })
  
  var_foodtype_s <- reactive({
    unique(f$food_type)
  })
  
  var_food_s <- reactive({
    dplyr::filter(data, food_type %in% foodtype_s()) %>% 
      dplyr::pull(food) %>% 
      unique()
  })
  
  var_country_s <- reactive({
    dplyr::filter(data, food_type %in% foodtype_s(), food %in% food_s()) %>% 
      dplyr::pull(country) %>% 
      unique()
  })
  
  var_type_s <- reactive({
    dplyr::filter(data, food_type %in% foodtype_s(), food %in% food_s(), country %in% country_s()) %>% 
      dplyr::pull(type) %>% 
      unique()
  })
  
  var_size_s <- reactive({
    dplyr::filter(data, food_type %in% foodtype_s(), food %in% food_s(), country %in% country_s(), type %in% type_s()) %>% 
      dplyr::pull(size) %>% 
      unique()
  })
  
  #output$single_item_ts_plot_1 <- renderPlot({
  #g <- date_filtered_s()
  #ggplot(data = g, aes(x = date)) +
  #geom_line(aes_string(y= input$predictor_s_1), size = 1.3, color = "white") + 
  #coord_cartesian(xlim = c(input$date[1], input$date[2])) +
  #theme(
  # black background of panel and plot
  #panel.background = element_rect(fill = "black", color = "black"),
  #plot.background = element_rect(fill = "black", color = "black"),
  # add white border around facets
  #panel.border = element_rect(fill = NA, color = "white"), 
  # no grid
  #panel.grid = element_blank(),
  # change color of facet
  #strip.background = element_rect(fill = "darkgray"), 
  #strip.text = element_text(colour = 'white', size = 14, face = "bold"),
  # axis labels
  #axis.title = element_text(colour = "white"),
  #axis.text = element_text(size = 12, colour = "white"),
  #axis.text.y = element_text(size = 12, colour = "white"),
  #axis.line = element_line(colour = "white", 
  #size = 1.5, linetype = "solid"),
  #axis.ticks.length=unit(.25, "cm"),
  #axis.ticks = element_line(colour = "white", size = 2)
  #)
  
  #theme( axis.line = element_line(colour = "white", 
  #size = 1.5, linetype = "solid"),
  #axis.text = element_text(size=15),
  #axis.title.x = element_blank()) +
  
  #})
  
  output$single_item_ts_plot_1 <- renderPlot({
    g <- date_filtered_s()
    ggplot(data = g, aes(x = date)) +
      geom_line(aes_string(y= input$predictor_s_1), size = 1.3, color = "white") + 
      geom_vline(xintercept=as.numeric(as.Date(input$onedate_s)), color="blue") +
      coord_cartesian(xlim = c(input$date_s[1], input$date_s[2])) +
      ylab(paste0(names(choice_price_vec)[choice_price_vec == input$predictor_s_1], " in EURO/ KG/Item")) +
      xlab("Date") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
  })
  
  
  
  output$single_item_ts_plot_2 <- renderPlot({
    g <- date_filtered_s()
    ggplot(data = g, aes(x = date)) +
      geom_line(aes_string(y= input$predictor_s_2), size = 1.3, color="white") + 
      geom_vline(xintercept=as.numeric(as.Date(input$onedate_s)), color="blue") +
      coord_cartesian(xlim = c(input$date_s[1], input$date_s[2])) +
      ylab(names(choice_weather_easy_vec)[choice_weather_easy_vec == input$predictor_s_2]) +
      xlab("") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
    #theme(axis.line = element_line(colour = "white", 
    #size = 1.5, linetype = "solid"),
    #axis.text = element_text(size=15),
    #axis.title.x = element_blank(),
    #axis.ticks.length=unit(.25, "cm"),
    #axis.ticks = element_line(colour = "white", size = 2)) +
    
  })
  
  output$single_item_ts_plot_3 <- renderPlot({
    g <- date_filtered_s()
    ggplot(data = g, aes(x = date)) +
      geom_line(aes_string(y= input$predictor_s_3), size = 1.3, color = "white") + 
      geom_vline(xintercept=as.numeric(as.Date(input$onedate_s)), color="blue") +
      coord_cartesian(xlim = c(input$date_s[1], input$date_s[2])) +
      ylab(names(choice_predictor_vec)[choice_predictor_vec == input$predictor_s_3]) +
      xlab("") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid = element_blank(),
        # change color of facet
        strip.background = element_rect(fill = "darkgray"), 
        strip.text = element_text(colour = 'white', size = 14, face = "bold"),
        # axis labels
        axis.title = element_text(colour = "white"),
        axis.text = element_text(size = 12, colour = "white"),
        axis.text.y = element_text(size = 12, colour = "white"),
        axis.line = element_line(colour = "white", 
                                 size = 1.5, linetype = "solid"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "white", size = 2)
      )
    #theme(axis.line = element_line(colour = "white", 
    #size = 1.5, linetype = "solid"),
    #axis.text = element_text(size=15),
    #axis.title.x = element_blank(),
    #axis.ticks.length=unit(.25, "cm"),
    #axis.ticks = element_line(colour = "white", size = 2)) +
    
  })
  
  
  #### Econometric Analysis ####
  #%%%%%%%%%%%%%%%%%%%%%#
  
  data_l <- l
  
  observeEvent(input$refresh_econometrics, {
    #refresh()
    updateSelectInput(session, "independentvar", choices=colnames(predictor_reg), 
                      selected =c("lag_3_months_temp", "lag_3_months_humidity",
                                  "lag_3_months_precip", "lag_3_months_cloudcover",
                                  "lag_3_months_windspeed"))
    updateSelectInput(session, "Food_Type_r", choices = var_foodtype_r())
    updateSelectInput(session, "Food_r", choices = var_food_r())
    updateSelectInput(session, "Country_r", choices = var_country_r())
    updateSelectInput(session, "Type_r",  choices = var_type_r())
    updateSelectInput(session, "Size_r", choices = var_size_r())
  })
  
  onclick("show_regression_info", runjs("window.open('Theory_Regression_Info.html')"))
  
  onclick("show_correlation_info", runjs("window.open('Theory_Correlation_Analysis.html')"))
  
  output$general_info_econometric_anaylsis <- renderUI({
    tags$img(src = 'literature.PNG', width = "70%")
  })
  
  output$foodtype_r <- renderUI({
    selectInput(inputId = "Food_Type_r", "Select Food Type:", choices = var_foodtype_r(), multiple = T)
    #tags$head(tags$style(HTML(".selectize-input {height: 10px; width: 200px; font-size: 15px;}")))
  })
  output$food_r <- renderUI({
    selectInput(inputId = "Food_r", "Select Fruit/Vegetable:", choices = var_food_r(), multiple = T)
  })
  output$country_r <- renderUI({
    selectInput(inputId = "Country_r", "Select Country:", choices = var_country_r(), multiple = T)
  })
  output$type_r <- renderUI({
    selectInput(inputId = "Type_r", "Select Type:", choices = var_type_r(), multiple = T)
  })
  output$size_r <- renderUI({
    selectInput(inputId = "Size_r", "Select Size:", choices = var_size_r(), multiple = T)
  })
  
  value_filtered_r <- reactive({
    dplyr::filter(l, food_type %in% foodtype_r(), food %in% food_r(), country %in% country_r(), type %in% type_r(), size %in% size_r())
  })
  
  date_filtered_r <- reactive({
    dplyr::filter(value_filtered_r(), dplyr::between(date, input$date_r[1], input$date_r[2]))
  })
  
  correlation_update <- reactive({
    d <- date_filtered_r()
    numeric_data <- dplyr::select(d, -c(date, food_type, food, country, type, size))
    
    dummies <- c("season")
    
    predictors <- input$independentvar
    num_vars <- predictors[predictors!=dummies]
    
    correlation_data <- numeric_data %>%
      dplyr::select(num_vars)
    plot_data <- cor(correlation_data)
  })
  
  
  
  regression <- reactive({  # To be extended
    d <- date_filtered_r()
    
    outcome <- input$dependentvar
    predictors <- input$independentvar
    
    dummies <- c("season")
    
    if (input$regform=="Log") {
      
      if(dummies %in% predictors){
        dummy_vars <- predictors[predictors==dummies]
        num_vars <- predictors[predictors!=dummies]
        num_vars <- paste(paste0("log(", num_vars, ")"), collapse = " + ")
        all_vars <- paste(num_vars, dummy_vars, sep = " + ")
      } else{
        num_vars <- predictors[predictors!=dummies]
        all_vars <- paste(paste0("log(", num_vars, ")"), collapse = " + ")
      }
      
      formula <- as.formula(
        paste(paste0("log(", outcome, ")"),
              paste(all_vars, collapse = " + "),
              sep = " ~ ")
      )} else {
        if(dummies %in% predictors){
          dummy_vars <- predictors[predictors==dummies]
          num_vars <- predictors[predictors!=dummies]
          num_vars <- paste(paste0(num_vars), collapse = " + ")
          all_vars <- paste(num_vars, dummy_vars, sep = " + ")
        } else{
          num_vars <- predictors[predictors!=dummies]
          all_vars <- paste(paste0(num_vars), collapse = " + ")
        }
        
        formula <- as.formula(
          paste(paste0(outcome),
                paste(all_vars, collapse = " + "),
                sep = " ~ "))
        
      }
    
    lm1 <- eval(bquote(   lm(.(formula), data = d)   ))
  })
  
  
  # Get filters from inputs
  foodtype_r <- reactive({
    if (is.null(input$Food_Type_r)) sort(unique(l$food_type)) else input$Food_Type_r
  })
  
  food_r <- reactive({
    if (is.null(input$Food_r)) sort(unique(l$food)) else input$Food_r
  })
  
  country_r <- reactive({
    if (is.null(input$Country_r)) sort(unique(l$country)) else input$Country_r
  })
  
  type_r <- reactive({
    if (is.null(input$Type_r)) sort(unique(l$type)) else input$Type_r
  })
  
  size_r <- reactive({
    if (is.null(input$Size_r)) sort(unique(l$size)) else input$Size_r
  })
  
  var_foodtype_r <- reactive({
    unique(l$food_type)
  })
  
  var_food_r <- reactive({
    dplyr::filter(data_l, food_type %in% foodtype_r()) %>% 
      pull(food) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_country_r <- reactive({
    dplyr::filter(data_l, food_type %in% foodtype_r(), food %in% food_r()) %>% 
      pull(country) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_type_r <- reactive({
    dplyr::filter(data_l, food_type %in% foodtype_r(), food %in% food_r(), country %in% country_r()) %>% 
      pull(type) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_size_r <- reactive({
    dplyr::filter(data_l, food_type %in% foodtype_r(), food %in% food_r(), country %in% country_r(), type %in% type_r()) %>% 
      pull(size) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  output$lm1 <- renderText(stargazer(regression(), type="html"))
  
  output$plot <- renderPlot({
    corrplot(correlation_update(), tl.cex=0.85, type="upper", order="hclust", tl.col="black", 
             addCoef.col = "black", diag=FALSE, tl.srt=45, col=brewer.pal(n=8, name="PuOr"))
  })
  
  reg_predictor_list <- c()
  
  regression_results_percent <- reactive({
    
    if(input$regform=="Log") {
      
      reg_predictor_list <- c(reg_predictor_list, input$independentvar)
      lm1 <- regression()
      coefficients_reg <- unname(lm1[["coefficients"]][-1])
      
      coef_interpretation <- as.data.frame(cbind(reg_predictor_list, coefficients_reg))
      
      coef_interpretation$coefficients_reg <- as.numeric(coef_interpretation$coefficients_reg)
      
      coef_interpretation$coefficients_reg <- round(coef_interpretation$coefficients_reg, 2)
      
      coef_interpretation$coefficients_reg <- paste(coef_interpretation$coefficients_reg, "%", sep="")
      
      coef_interpretation$reg_predictor_list <- paste(coef_interpretation$reg_predictor_list, " 1 %", sep="")
      
      colnames(coef_interpretation)[1] <- "When Regressor increases by 1%"
      
      colnames(coef_interpretation)[2] <- "% increase/ decrease in price"
      
      coef_interpretation
      
    } else {
      #reg_predictor_list <- c(reg_predictor_list, input$independentvar)
      #lm1 <- regression()
      #coefficients_reg <- unname(lm1[["coefficients"]][-1])
      
      #coef_interpretation_s <- as.data.frame(cbind(reg_predictor_list, coefficients_reg))
      
      #coef_interpretation_s <- dplyr::rename(coef_interpretation_s, 
      #Regressors = reg_predictor_list,
      #Effect = coefficients_reg) 
      
      #coef_interpretation_s <- dplyr::mutate(coef_interpretation_s,
      #Regressors = Regressors,
      #Effect = round(as.numeric(Effect, 15)))
    }
  })
  
  
  
  output$regtable <- DT::renderDataTable({
    regression_results_percent()
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### MACHINE LEARNING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### ML Hyperparameters ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## define text output ##
  
  # text
  hyperparams_text_lasso_ridge <- HTML(paste(
    "<br>", "The <b>Lambda</b> parameter controls the strength of shrinkage.",
    "<br>", "It ranges from 0 to 1."))
  hyperparams_text_enet <- HTML(paste(
    "<br>", "The <b>Lambda</b> parameter controls the strength of shrinkage. It ranges from 0 to 1.", "<br>", "<br>",
    "The <b>Alpha</b> parameter determines the importance on the l2 vs. l1 norm.", "<br>",
    "For a value of 0 the elastic net regression equals LASSO while for a value of 1 it equals ridge regression."))
  hyperparams_text_xgboost <-
    HTML(paste("<br>", "The <b>Tree Depth</b> parameter controls the depth of a tree.", "<br>",
               "The <b>Trees</b> parameter determines the number of trees.", "<br>",
               "The <b>Learning Rate</b> parameter determines the step size shrinkage", "<br>",
               "The <b>Mtry</b> parameter determines the number of randomly selected features at each split.", "<br>",
               "The <b>Min_n</b> parameter determines the minimal number of observations in a terminal node.", "<br>"
    ))
  
  hyperparams_text_rf <-
    HTML(paste("<br>", "The <b>Trees</b> parameter determines the number of trees in the forest.", "<br>",
               "The <b>Mtry</b> parameter determines the number of randomly selected features at each split.", "<br>",
               "The <b>Min_n</b> parameter determines the minimal number of observations in a terminal node.", "<br>"
    ))
  
  hyperparams_text_svm <- 
    HTML(paste("<br>", "The <b>Sigma</b> reflects the error tolerance.", "<br>",
               "The <b>C</b> indicates the weight to minimize the error.", "<br>"
               
    ))
  
  hyperparams_text_nn <- 
    HTML(paste("<br>", "The <b>First Layer</b> determines the number of neurons at the first layer.", "<br>",
               "The <b>Second Layer</b> determines the number of neurons at the second layer.", "<br>",
               "The <b>Third Layer</b> determines the number of neurons at the third layer.", "<br>"
    ))
  
  
  
  ## Full Model ##
  #%%%%%%%%%%%%%%#
  
  # create text output dependent on model selection 
  df_params_full <- eventReactive(input$params_show_full, {
    if (input$params_full_info == "TRUE") {
      if (input$params_full_model %in% c("LASSO", "RIDGE")) {
        html_params_text_full <- hyperparams_text_lasso_ridge
      } else if (input$params_full_model %in% c("ENET")) {
        html_params_text_full <- hyperparams_text_xgboost
      } else if (input$params_full_model %in% c("XGBoost")) {
        html_params_text_full <- hyperparams_text_rf
      } else if (input$params_full_model %in% c("Random_Forest")) {
        html_params_text_full <- hyperparams_text_rf
      } else if (input$params_full_model %in% c("SVM")) {
        html_params_text_full <- hyperparams_text_svm
      } else if (input$params_full_model %in% c("NN")) {
        html_params_text_full <- hyperparams_text_nn
      }
    } else {
      html_params_text_full <- " "
    }
    
    # read in hyperparameters
    params_path <- paste0("models/", input$params_full_model, 
                          "/hyperparameters/hyperparameters_full_model.rds")
    df_params <- readRDS(params_path) 
    
    # replace market names
    for (i in 1:length(market_replacement)) {
      df_params$market <- 
        unlist(replace(df_params$market, 
                       df_params$market == names(market_replacement[i]), 
                       market_replacement[i]))
    }
    
    # filter on selected market
    df_params <- df_params %>%
      filter(market %in% input$params_full_market) %>%
      dplyr::arrange(market) %>%
      dplyr::rename(
        Market = market
      )
    
    # create data table
    df_params_datatable <- 
      DT::datatable(
        data = df_params, 
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          dom = c("tip")
        )) %>%
      formatStyle(columns = names(df_params), 
                  color = "white", background = "black"
      )
    
    list(html_params_text_full, df_params_datatable)
  })
  
  # create both outputs: table and information about hyperparameters
  output$hyperparameters_full_table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$params_full_market),
                         "Please select at least one market."))
    df_params_full()[[2]]
  })
  
  output$hyperparameters_full_info <- renderText({
    shiny::validate(need(!is.null(input$params_full_market),
                         " "))
    df_params_full()[[1]]
  })
  
  
  
  ## Food Model ##
  #%%%%%%%%%%%%%%#
  
  
  # create text output dependent on model selection 
  df_params_text_food <- eventReactive(input$params_show_food, {
    if (input$params_food_info == "TRUE") {
      if (input$params_food_model %in% c("LASSO", "RIDGE")) {
        html_params_text_food <- hyperparams_text_lasso_ridge
      } else if (input$params_food_model %in% c("ENET")) {
        html_params_text_food <- hyperparams_text_enet
      } else if (input$params_food_model %in% c("XGBoost")) {
        html_params_text_food <- hyperparams_text_xgboost
      } else if (input$params_food_model %in% c("Random_Forest")) {
        html_params_text_food <- hyperparams_text_rf
      } else if (input$params_food_model %in% c("SVM")) {
        html_params_text_food <- hyperparams_text_svm
      } else if (input$params_food_model %in% c("NN")) {
        html_params_text_food <- hyperparams_text_nn
      }
    } else {
      html_params_text_food <- " "
    }
    html_params_text_food
  })
  
  
  # create data set for food model
  df_params_food <- eventReactive(input$params_show_food, {
    # define path
    params_path <- paste0("models/", input$params_food_model, 
                          "/hyperparameters/hyperparameters_food_model.rds")
    
    # read in hyparparameters and improve output
    df_params <- 
      readRDS(params_path) %>%
      mutate(
        market = if_else(market == "price_F", "Frankfurt", market),
        market = if_else(market == "price_B", "Berlin", market), 
        market = if_else(market == "price_M", "Munich", market),
        market = if_else(market == "price_K", "Cologne", market),
        market = if_else(market == "price_H", "Hamburg", market)
      ) %>%
      filter(market %in% input$params_food_market,
             food %in% input$params_food_food_model) %>%
      dplyr::arrange(food, market) %>%
      dplyr::rename(
        `Fruit/Vegetable` = food,
        Market = market
      )
    
    # generate data table
    DT::datatable(
      data = df_params, 
      rownames = FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}"),
        dom = c("tip")
      )) %>%
      formatStyle(columns = names(df_params), 
                  color = "white", background = "black"
      )
  })
  
  
  
  # output: table and text for hyperparameters
  # only returned if action button is clicked
  observeEvent(input$params_show_food, {
    output$hyperparameters_food_table <- DT::renderDataTable({
      
      shiny::validate(need(!is.null(input$params_food_food_model),
                           "Please select at least one fruit or vegetable."))
      
      shiny::validate(need(!is.null(input$params_food_market),
                           "Please select at least one market."))
      
      df_params_food()
    })#, df_params_food_digits())
    
    output$hyperparameters_food_info <- renderText({
      
      shiny::validate(need(!is.null(input$params_food_food_model),
                           " "))
      
      shiny::validate(need(!is.null(input$params_food_market),
                           " "))
      
      df_params_text_food()
    })
  })
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%#
  #### ML Predictions ####
  #%%%%%%%%%%%%%%%%%%%%%%#
  
  #### find best model ####
  #%%%%%%%%%%%%%%%%%%%%%%%#
  
  # info 
  onclick("find_best_info", runjs("window.open('Theory_Best_Model_Info.html')"))
  
  # update slider input based on selection
  # slider is adjusted so that it includes total number of models
  # we have 7 models from ML and deep learning
  # two from naive predictions and two from traditional statistics (both is
  # only displayed if selected)
  ml_best_model_slider <- reactive({
    add_models_1 <- 0
    add_models_2 <- 0
    
    if (input$find_best_models == "Yes") {
      add_models_1 <- 2
    }
    
    if (input$find_best_models_bsts == "Yes") {
      add_models_2 <- 2
    }
    
    7 + add_models_1 + add_models_2
  })
  
  # if any value of naive or time series "yes", "no" is changed, the slider is updated
  observeEvent({
    input$find_best_models 
    input$find_best_models_bsts}, {
      updateSliderInput(session, "find_best_overall", "Select number of models displayed:",
                        min = 1, max = ml_best_model_slider(), 
                        value = input$find_best_overall, # as value the already chosen value is kept
                        step = 1)
    })
  
  
  observeEvent({
    input$find_best_models 
    input$find_best_models_bsts}, {
      updateSliderInput(session, "find_best_food_num", "Select number of models displayed:",
                        min = 1, max = ml_best_model_slider(), 
                        value = input$find_best_food_num, # as value the already chosen value is kept
                        step = 1)
    })
  
  
  observeEvent({
    input$find_best_models 
    input$find_best_models_bsts}, {
      updateSliderInput(session, "find_best_food_detail_num", "Select number of models displayed:",
                        min = 1, max = ml_best_model_slider(), 
                        value = input$find_best_food_detail_num, # as value the already chosen value is kept
                        step = 1)
    })
  
  
  # create reactive data set depending on 
    ## selection regarding naive prediction
    ## selection regarding overall, food, food-type-size-country
  df_perf_best_naive <- reactive({
    
    df_perf_best_naive_create <- df_perf
    
    # selection regarding naive prediction
    if (input$find_best_models == "No") {
      df_perf_best_naive_create <- 
        df_perf_best_naive_create %>%
        filter(!model %in% c("Naive Mean Prediction", "Naive Median Prediction"))
    } else {
      df_perf_best_naive_create <- df_perf_best_naive_create
    }
    
    # selection regarding time series models
    if (input$find_best_models_bsts == "No") {
      df_perf_best_naive_create <- 
        df_perf_best_naive_create %>%
        filter(!model %in% c("BSTS", "BSTS Baseline"))
    } else {
      df_perf_best_naive_create <- df_perf_best_naive_create
    }
    
    # Is aggregation across markets selected?
    # grouping depends on further selection
    # overall: error metrics only aggregated by prediction model
    # only fruit/vegetable: aggregated by prediction model and fruit/vegetable
    # otherwise aggregated by food-type-sitze-country combination
    if ("Aggregate" %in% input$find_best_market) {
      if (input$find_best_sel == "Overall") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          group_by(model) %>%
          # aggregate error measures
          dplyr::summarise(
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE)
          ) %>%
          dplyr::select(model, RMSE, MAE, MAPE)
      } else if (input$find_best_sel == "Only Fruit/Vegetable") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          group_by(model, food) %>%
          dplyr::summarise(
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE)
          ) %>%
          dplyr::select(model, food, RMSE, MAE, MAPE)
      } else if (input$find_best_sel == "Food-Type-Size-Country") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          group_by(model, food, type, size, country) %>%
          dplyr::summarise(
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE)
          ) %>%
          dplyr::select(model, food, type, size, country, RMSE, MAE, MAPE)
      }
    } else {
      # further selection; grouping again depends on further selection
      if (input$find_best_sel == "Overall") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          # keep only selected market
          filter(market %in% input$find_best_market) %>%
          group_by(market, model) %>%
          # aggregate error measures
          dplyr::summarise(
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE)
          ) %>%
          dplyr::select(model, market, RMSE, MAE, MAPE)
      } else if (input$find_best_sel == "Only Fruit/Vegetable") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          filter(market %in% input$find_best_market) %>%
          group_by(market, model, food) %>%
          dplyr::summarise(
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE)
          ) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE)
      } else if (input$find_best_sel == "Food-Type-Size-Country") {
        df_perf_best_naive_create <- 
          df_perf_best_naive_create %>%
          filter(market %in% input$find_best_market) %>%
          dplyr::select(model, market, food, type, size, country, RMSE, MAE, MAPE)
      }
    }
    
    df_perf_best_naive_create
  })
  
  
  
  ## overall ##
  
  # create specific output
  df_perf_best_overall <- eventReactive(input$find_best_show, {
    if (input$find_best_worst_overall == "Best") {
      df_perf_best_naive() %>%
        arrange(!!! rlang::syms(input$find_best_overall_error)) %>%
        head(input$find_best_overall) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae
        )
    } else {
      df_perf_best_naive() %>%
        arrange(desc(!!! rlang::syms(input$find_best_overall_error))) %>%
        head(input$find_best_overall) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae
        )
    }
    
  })
  
  # display table
  output$table_best_overall <- renderTable({
    shiny::validate(need(!is.null(df_perf_best_overall()), " "))
    df_perf_best_overall()
  }, digits = 4)
  
  
  ## food ##
  
  # create specific output
  df_perf_best_food <- eventReactive(input$find_best_show_food, {
    if (input$find_best_worst == "Best") {
      df_perf_best_naive() %>%
        filter(food %in% input$find_best_food) %>%
        arrange(!!! rlang::syms(input$find_best_food_error)) %>%
        head(input$find_best_food_num) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae, `Fruit/Vegetable` = Food
        )
    } else {
      df_perf_best_naive() %>%
        filter(food %in% input$find_best_food) %>%
        arrange(desc(!!! rlang::syms(input$find_best_food_error))) %>%
        head(input$find_best_food_num) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae, `Fruit/Vegetable` = Food
        )
    }
  })
  
  # display table
  output$table_best_food <- renderTable({
    shiny::validate(need(!is.null(df_perf_best_food()), 
                         " "))
    
    if (input$find_best_sel == "Only Fruit/Vegetable") {
      df_perf_best_food()
    } else {
      return()
    }
    
  })
  
  
  ## food-type-size-country ##
  
  # update select input
  ## reactive data sets based on selection
  best_model_food_type_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$find_best_food_detail) %>%
      dplyr::select(type) %>% unique() %>% pull() %>% sort()
  })
  
  
  best_model_food_size_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$find_best_food_detail, 
             type %in% input$find_best_food_type) %>%
      dplyr::select(size) %>% unique() %>% pull() %>% mixedsort()
  })
  
  best_model_food_country_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$find_best_food_detail, 
             type %in% input$find_best_food_type,
             size %in% input$find_best_food_size) %>%
      dplyr::select(country) %>% unique() %>% pull() %>% sort()
  })
  
  
  ## update select input based on selection
  observeEvent(input$find_best_food_detail, {
    updateSelectInput(session, "find_best_food_type",
                      choices = best_model_food_type_reactive())
  })
  
  observeEvent(c(input$find_best_food_detail, input$find_best_food_type), {
    updateSelectInput(session, "find_best_food_size",
                      choices = best_model_food_size_reactive())
  })
  
  observeEvent(c(input$find_best_food_detail, input$find_best_food_type, 
                 input$find_best_food_size), {
                   updateSelectInput(session, "find_best_food_country",
                                     choices = best_model_food_country_reactive())
                 })
  
  
  # create specific output
  df_perf_best_food_detail <- eventReactive(input$find_best_show_food_detail, {
    if (input$find_best_worst_detail == "Best") {
      df_perf_best_naive() %>%
        dplyr::filter(food %in% input$find_best_food_detail, 
                      type %in% input$find_best_food_type, 
                      size %in% input$find_best_food_size,
                      country %in% input$find_best_food_country) %>%
        arrange(!!! rlang::syms(input$find_best_food_detail_error)) %>%
        head(input$find_best_food_detail_num) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae, `Fruit/Vegetable` = Food
        )
    } else {
      df_perf_best_naive() %>%
        dplyr::filter(food %in% input$find_best_food_detail, 
                      type %in% input$find_best_food_type, 
                      size %in% input$find_best_food_size,
                      country %in% input$find_best_food_country) %>%
        arrange(desc(!!! rlang::syms(input$find_best_food_detail_error))) %>%
        head(input$find_best_food_detail_num) %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'),  " \u20ac"),
          MAPE = paste0(sprintf(MAPE*100, fmt = '%#.2f') , " %")
        ) %>% 
        dplyr::rename_with(str_to_title) %>%
        dplyr::rename(
          MAPE = Mape, RMSE = Rmse, MAE = Mae, `Fruit/Vegetable` = Food
        )
    }
  })
  
  # display table
  output$table_best_food_detail <- renderTable({
    shiny::validate(need(!is.null(df_perf_best_food_detail()), " "))
    
    if (input$find_best_sel == "Food-Type-Size-Country") {
      df_perf_best_food_detail()
    } else {
      return()
    }
    
  })
  
  
  #### single comparison ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  ## reactive select input ##
  
  # reactive data sets based on selection
  year_value_pred_food_type_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_year) %>%
      dplyr::select(type) %>% unique() %>% pull() %>% sort()
  })
  
  
  year_value_pred_food_size_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_year,
             type %in% input$pred_food_type_year) %>%
      dplyr::select(size) %>% unique() %>% pull() %>% mixedsort()
  })
  
  year_value_pred_food_country_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_year,
             type %in% input$pred_food_type_year,
             size %in% input$pred_food_size_year) %>%
      dplyr::select(country) %>% unique() %>% pull() %>% sort()
  })
  
  
  # update select input based on selection
  observeEvent(input$pred_food_sel_year, {
    updateSelectInput(session, "pred_food_type_year",
                      choices = year_value_pred_food_type_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_year, input$pred_food_type_year), {
    updateSelectInput(session, "pred_food_size_year",
                      choices = year_value_pred_food_size_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_year, input$pred_food_type_year, input$pred_food_size_year), {
    updateSelectInput(session, "pred_food_country_year",
                      choices = year_value_pred_food_country_reactive())
  })
  
  
  ## MAKE PREDICTIONS ###
  
  # generate plot
  df_pred_2016 <- eventReactive(input$pred_show_year,{
    # to do so a function was created 
    func_plotly_single_comparison(
      input$pred_food_sel_year, input$pred_food_type_year,
      input$pred_food_size_year, input$pred_food_country_year,
      input$pred_model_year, input$pred_price_year,
      input$pred_horizon_year)
  })
  
  
  # generate table
  year_table_price_pred <- eventReactive(input$pred_show_year, {
    
    # make predictions for complete horizon 
    # create data frames to store predictions
    df_price_pred_bsts_all <- data.frame()
    df_price_pred_all <- data.frame()
    
    # make predictions: depends on selected model
    # time series models have different function than other models
    if (any(c("BSTS Baseline", "BSTS") %in% input$pred_model_year)) {
      df_price_pred <-
        func_price_predictions_bsts(input$pred_food_sel_year, input$pred_food_type_year,
                                    input$pred_food_size_year, input$pred_food_country_year,
                                    input$pred_model_year, input$pred_price_year,
                                    input$pred_horizon_year) %>%
        # create column for model
        mutate(model = input$pred_model_year) %>%
        # ensure that date column is properly displayed
        mutate(date = lubridate::ymd(date)) %>%
        # arrange by date 
        dplyr::arrange(date) %>%
        # ensure correct ordering
        dplyr::select(date, model, market, food, type, size, country, 
                      `true price`, `predicted price`, `price deviation`)
      
      
      # append
      df_price_pred_bsts_all <- rbind(df_price_pred_all, df_price_pred)
    }
    
    
    # predictions for other models
    if (any(c("LASSO", "RIDGE", "ENET", "XGBoost", 
              "SVM", "NN", "Random_Forest") %in% input$pred_model_year)) {
      df_price_pred <-
        func_price_predictions_ml(input$pred_food_sel_year, input$pred_food_type_year,
                                  input$pred_food_size_year, input$pred_food_country_year,
                                  input$pred_model_year, input$pred_price_year,
                                  input$pred_horizon_year) %>%
        # create column for model
        mutate(model = input$pred_model_year) %>%
        # ensure that date column is properly displayed
        mutate(date = lubridate::ymd(date)) %>%
        # arrange by date 
        dplyr::arrange(date) %>%
        # ensure correct ordering
        dplyr::select(date, model, market, food, type, size, country, 
                      `true price`, `predicted price`, `price deviation`)
      
      
      df_price_pred_all <- rbind(df_price_pred_all, df_price_pred)
    } 
    
    
    # append data frame; includes the predictions
    df_year_pred <- rbind(df_price_pred_bsts_all, df_price_pred_all)
    
    # data preparation
    df_year_pred <- df_year_pred %>%
      # ensure that date column is properly displayed
      mutate(date = lubridate::ymd(date)) %>%
      # arrange by date
      dplyr::arrange(date) %>%
      # keep only variables needed for table
      dplyr::select(date, `true price`, `predicted price`)
    
    
    # ERROR METRIC FOR COMPLETE HORIZON #
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    year_pred <- data.frame(
      RMSE = paste0(sprintf(yardstick::rmse(data = df_year_pred, truth = `true price`,
                                            estimate = `predicted price`) %>%
                              dplyr::select(.estimate) %>% pull(), fmt = '%#.2f'), " \u20ac"),
      MAE = paste0(sprintf(yardstick::mae(data = df_year_pred, truth = `true price`,
                                          estimate = `predicted price`) %>%
                             dplyr::select(.estimate) %>% pull(),fmt = '%#.2f'), " \u20ac"),
      MAPE = paste0(sprintf(yardstick::mape(data = df_year_pred, truth = `true price`,
                                            estimate = `predicted price`) %>%
                              dplyr::select(.estimate) %>% pull(),fmt = '%#.2f'), " %")
    ) 
    
    
    # ERROR METRIC FOR PREDICTION HORIZON #
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    year_pred_sub <- df_year_pred %>%
      dplyr::arrange(date) %>%
      tail(input$pred_horizon_year)
    
    year_pred_sub <- data.frame(
      RMSE = paste0(sprintf(yardstick::rmse(data = year_pred_sub, truth = `true price`,
                                            estimate = `predicted price`) %>%
                              dplyr::select(.estimate) %>% pull(), fmt = '%#.2f'), " \u20ac"),
      MAE = paste0(sprintf(yardstick::mae(data = year_pred_sub, truth = `true price`,
                                          estimate = `predicted price`) %>%
                             dplyr::select(.estimate) %>% pull(), fmt = '%#.2f'), " \u20ac"),
      MAPE = paste0(sprintf(yardstick::mape(data = year_pred_sub, truth = `true price`,
                                            estimate = `predicted price`) %>%
                              dplyr::select(.estimate) %>% pull(), fmt = '%#.2f'), " %")
    ) 
    
    
    list(df_year_pred, year_pred, year_pred_sub)
  })
  
  
  
  # create text output
  html_single_comparison_table_text <- eventReactive(input$pred_show_year, {
    
    # extract latest date
    pred_table_2016_text_date <- 
      year_table_price_pred()[[1]] %>% arrange(date) %>% tail(1) %>%
      dplyr::select(date) %>% pull() %>% as.Date() %>% format("%B %d, %Y")
    
    # replace models
    selected_model_year <- 
      unname(unlist(ifelse(!is.na(model_replacement[input$pred_model_year]), 
                           model_replacement[input$pred_model_year], 
                           input$pred_model_year)))
    
    # replace markets 
    selected_market_year <- 
      unname(unlist(ifelse(!is.na(market_replacement[input$pred_price_year]), 
                           market_replacement[input$pred_price_year], 
                           input$pred_price_year)))
    
    
    list(pred_table_2016_text_date, selected_market_year, selected_model_year)
    
    
  })
  
  
  observeEvent(input$pred_show_year, {
    output$pred_table_2016_text <- renderUI({
      # validate -> year_table_price_pred() not empty
      shiny::validate(need(!is.null(year_table_price_pred()),
                           "Please press the button"))
      
      
      # create HTML dependend on selection if all or only some
      # predictions should be displayed
      if (input$pred_single_comp_model_table_all == "Yes") {
        HTML(paste0("Predicted vs. true prices from 2016 until ",
                    html_single_comparison_table_text()[[1]], 
                    " for the wholesale market in ",
                    html_single_comparison_table_text()[[2]], " for ",
                    input$pred_food_sel_year, ", ", input$pred_food_type_year, ", ",
                    input$pred_food_size_year, " from ",
                    input$pred_food_country_year, ". <br>", 
                    "The model used to make the predictions is ", 
                    html_single_comparison_table_text()[[3]], "."))
      } else {
        HTML(paste0("Predicted vs. true prices for the next ",
                    input$pred_horizon_year, 
                    " weeks for the wholesale market in ",
                    html_single_comparison_table_text()[[2]], " for ",
                    input$pred_food_sel_year, ", ", input$pred_food_type_year, ", ",
                    input$pred_food_size_year, " from ",
                    input$pred_food_country_year, ". <br>", 
                    "The model used to make the predictions is ", 
                    html_single_comparison_table_text()[[3]], "."))
      }
      
      
    })
  })
  
  
  
  # table output
  single_comparison_table <- eventReactive(input$pred_show_year, {
    df_single_comparison_all <- 
      # show all predictions
      year_table_price_pred()[[1]] %>% 
      as.data.frame() %>%
      mutate(`price deviation` = round(`true price` - `predicted price`, 2)) %>%
      mutate(
        `true price` = paste(sprintf(`true price`, fmt = '%#.2f'), " \u20ac"),
        `predicted price` = paste(sprintf(`predicted price`, fmt = '%#.2f'), " \u20ac"), 
        `price deviation` = paste(sprintf(`price deviation`, fmt = '%#.2f'), " \u20ac")
      ) %>%
      arrange(desc(date)) %>%
      mutate(date = as.character(date))
    
    df_single_comparison_all
  })
  
  # render data table with appropriate style 
  # depends on selection if only predictions from "today" should be selected
  # or all predictions from 2016 onward
  output$pred_table_2016 <- DT::renderDataTable({
    ## all predictions from 2016
    if (input$pred_single_comp_model_table_all == "Yes") {
        DT::datatable(
          data = single_comparison_table(),
          rownames = FALSE,
          options = list(
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'color': '#fff'});",
              "}"),
            pageLength = 15,
            lengthMenu = c(5, 10, 15, 20, 30),
            processing = FALSE
            #dom = c("pli")
          )) %>%
        formatStyle(columns = 1:4, color = "white",
                    background = "black"
        )
    # show only predictions in "future" -> head()
    } else {
      DT::datatable(
        data = single_comparison_table() %>% head(input$pred_horizon_year), 
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          pageLength = 15,
          lengthMenu = c(5, 10, 15, 20, 30),
          processing = FALSE
        )) %>%
        formatStyle(columns = 1:4, color = "white",
                    background = "black"
        )
    }
  })
  
  
  
  # plot output
  output$pred_plot_2016 <- renderPlotly({
    df_pred_2016()
  })
  
  # plot info
  pred_plot_2016_info_html <- eventReactive(input$pred_show_year, {
    HTML("<b>User Instruction:</b> <br> You can zoom into the predictions by using the slider below the plot.
          Moreover, you can zoom into the predictions for the next week(s) <br>  
          by clicking on the button 'Zoom into predictions'. 
          To come back to the complete plot click on the 'Zoom out' button.")
  })
  
  output$pred_plot_2016_info <- renderUI({
    pred_plot_2016_info_html()
  })
  
  
  # error metric output: table and text is displayed if user
  # select it; hence if() conditions
  # moreover, action button needs to be clicked to return result
  pred_table_2016_text_html <- eventReactive(input$pred_show_year, {
    list(
      HTML("Error metrics from 2016 onward:"),
      HTML(paste("Error metrics for predictions of the next",
                 input$pred_horizon_year, "weeks:"))
    )
  })
  
  # error metrics for 2016 onwards
  output$pred_error_2016_text <- renderText({
    if (input$pred_single_comp_model_error == "Yes") {
      pred_table_2016_text_html()[[1]]
    } else {
      " "
    }
  })
  
  
  output$pred_error_2016 <- renderTable({
    if (input$pred_single_comp_model_error == "Yes") {
      year_table_price_pred()[[2]]
    } else {
      data.frame()
    }
  })
  
  
  # error metrics only for next weeks predictions
  output$pred_error_text <- renderText({
    if (input$pred_single_comp_model_error == "Yes") {
      pred_table_2016_text_html()[[2]]
    } else {
      " "
    }
    
  })
  
  output$pred_error <- renderTable({
    if (input$pred_single_comp_model_error == "Yes") {
      year_table_price_pred()[[3]]
    } else {
      data.frame()
    }
  })
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### market comparison ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## reactive select input ##
  
  # reactive data sets based on selection
  market_value_pred_food_type_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_market) %>%
      dplyr::select(type) %>% unique() %>% pull() %>% sort()
  })
  
  
  market_value_pred_food_size_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_market, 
             type %in% input$pred_food_type_market) %>%
      dplyr::select(size) %>% unique() %>% pull() %>% mixedsort()
  })
  
  market_value_pred_food_country_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_market, 
             type %in% input$pred_food_type_market,
             size %in% input$pred_food_size_market) %>%
      dplyr::select(country) %>% unique() %>% pull() %>% sort()
  })
  
  
  # update select input based on selection
  observeEvent(input$pred_food_sel_market, {
    updateSelectInput(session, "pred_food_type_market",
                      choices = market_value_pred_food_type_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_market, input$pred_food_type_market), {
    updateSelectInput(session, "pred_food_size_market",
                      choices = market_value_pred_food_size_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_market, input$pred_food_type_market, input$pred_food_size_market), {
    updateSelectInput(session, "pred_food_country_market",
                      choices = market_value_pred_food_country_reactive())
  })
  
  
  ## data ##
  
  # the header is the table header which explains what is displayed in the table.
  html_pred_header_market <- eventReactive(input$pred_show_market, {
    # replace models
    pred_model_market_sel <- 
      unname(unlist(ifelse(!is.na(model_replacement[input$pred_model_market]), 
                           model_replacement[input$pred_model_market], 
                           input$pred_model_market)))
    
    # replace markets 
    pred_model_price_sel <- 
      unname(unlist(ifelse(!is.na(market_replacement[input$pred_price_market]), 
                           market_replacement[input$pred_price_market], 
                           input$pred_price_market)))
    
    # generate text output
    HTML(
      paste0("<br> <b> Price predictions resulting from ", pred_model_market_sel, 
             " model for the next ", input$pred_horizon_market,
             " weeks for ", input$pred_food_sel_market, ", ", 
             input$pred_food_type_market, ", ",
             input$pred_food_size_market, ", ",
             " from ", input$pred_food_country_market, ".", "</b>", "<br>",
             "The predictions are made for the following wholesale markets: ", 
             sub(",([^,]*)$", ", and\\1", paste(pred_model_price_sel, collapse = ", ")), 
             "."))
  })
  
  
  # make predictions
  df_pred_market_comp <- eventReactive(input$pred_show_market, {
    # create data frames to store predictions
    df_price_pred_bsts_all <- data.frame()
    df_price_pred_all <- data.frame()
    
    # make predictions: depends on selected model
    # time series models have different function than other models
    if (any(c("BSTS Baseline", "BSTS") %in% input$pred_model_market)) {
      # extract only BSTS Baseline and BSTS; drop all other models
      bsts_pred_model_market <- 
        input$pred_model_market[input$pred_model_market %in% c("BSTS", "BSTS Baseline")]
      # iterate over selected markets and models
      for (price_pred_model in bsts_pred_model_market) {
        for (price_pred_market in input$pred_price_market) {
          df_price_pred <-
            func_price_predictions_bsts(input$pred_food_sel_market, 
                                        input$pred_food_type_market,
                                        input$pred_food_size_market, 
                                        input$pred_food_country_market,
                                        price_pred_model, price_pred_market, 
                                        input$pred_horizon_market) %>%
            # create column for model
            mutate(model = price_pred_model) %>%
            # ensure that date column is properly displayed
            mutate(date = lubridate::ymd(date)) %>%
            # arrange by date 
            dplyr::arrange(date) %>%
            # ensure correct ordering
            dplyr::select(date, model, market, food, type, size, country, 
                          `true price`, `predicted price`, `price deviation`)
          
          # replace market names
          for (i in 1:length(market_replacement)) {
            df_price_pred$market <- 
              unlist(replace(df_price_pred$market, 
                             df_price_pred$market == names(market_replacement[i]), 
                             market_replacement[i]))
          }
          
          # append
          df_price_pred_bsts_all <- rbind(df_price_pred_bsts_all, df_price_pred)
        }
      }
    } 
    
    # predictions for other models
    if (any(c("LASSO", "RIDGE", "ENET", "XGBoost", 
              "SVM", "NN", "Random_Forest") %in% input$pred_model_market)) {
      # extract the models
      ml_pred_model_market <- 
        input$pred_model_market[!input$pred_model_market %in% c("BSTS", "BSTS Baseline")]
      # iterate over markets and models to make the predictions
      for (price_pred_model in ml_pred_model_market) {
        for (price_pred_market in input$pred_price_market) {
          df_price_pred <-
            func_price_predictions_ml(input$pred_food_sel_market, input$pred_food_type_market,
                                      input$pred_food_size_market, input$pred_food_country_market,
                                      price_pred_model, price_pred_market, 
                                      input$pred_horizon_market) %>%
            # create column for model
            mutate(model = price_pred_model) %>%
            # ensure that date column is properly displayed
            mutate(date = lubridate::ymd(date)) %>%
            # arrange by date 
            dplyr::arrange(date) %>%
            # ensure correct ordering
            dplyr::select(date, model, market, food, type, size, country, 
                          `true price`, `predicted price`, `price deviation`)
          
          # replace market names
          for (i in 1:length(market_replacement)) {
            df_price_pred$market <- 
              unlist(replace(df_price_pred$market, 
                             df_price_pred$market == names(market_replacement[i]), 
                             market_replacement[i]))
          }
          
          
          df_price_pred_all <- rbind(df_price_pred_all, df_price_pred)
        }
      }
    } 
    
    # append data frame; this is the output
    rbind(df_price_pred_bsts_all, df_price_pred_all)
    
  })
  
  
  # create data table
  market_table_price_pred <- eventReactive(input$pred_show_market, {
    
    # for table output select only selected weeks
    # decide what columns are selected based on type of display
    
    # rows to keep
    market_rows_keep <- input$pred_horizon_market * length(input$pred_price_market)
    
    # extract predictions
    df_price_pred_all <- df_pred_market_comp() %>%
      dplyr::arrange(date) %>%
      tail(market_rows_keep) %>%
      dplyr::select(date, market, `true price`, 
                    `predicted price`, `price deviation`) %>%
      dplyr::arrange(market, date) %>%
      # nice date format
      mutate(date = as.Date(date) %>% format("%B %d, %Y")) %>%
      # add euro 
      mutate(
        `true price` = paste(sprintf(`true price`, fmt = '%#.2f'), " \u20ac"),
        `predicted price` = paste(sprintf(`predicted price`, fmt = '%#.2f'), " \u20ac"),
        `price deviation` = paste(sprintf(`price deviation`, fmt = '%#.2f'), " \u20ac")
      )
    
    
    # create data table from data frame
    DT::datatable(
      data = df_price_pred_all, rownames = FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}"),
        pageLength = 4,
        lengthMenu = c(4, 8, 12, 16, 20),
        dom = "ltip" # no search box 
      )) %>%
      formatStyle(columns = c(1:10), color = "white", 
                  backgroundColor = "black", background = "black")
  })
  
  
  # prepare data for plot
  market_plot_price_pred <- eventReactive(input$pred_show_market,{
    
    
    if (!is.null(input$pred_price_market)) {
      # select only predictions for plot
      market_rows_keep <- input$pred_horizon_market * length(input$pred_price_market)
      df_price_pred_all <- df_pred_market_comp() %>%
        dplyr::arrange(date) %>%
        tail(market_rows_keep) 
      
      
      # change format of data set for plot
      df_price_pred_all_barplot <- reshape2::melt(df_price_pred_all, 
                                                  id.vars = c("market", "date"))
      
      # keep only true and predicted prices
      df_price_pred_all_barplot %>%
        filter(variable %in% c("true price", "predicted price")) %>%
        mutate(value = as.numeric(value)) %>%
        # change date format
        mutate(date = as.Date(date) %>% format("%B %d, %Y")) 
    } else {
      data.frame()
    }
    
  })
  
  
  
  ## output ##
  
  # text output
  output$pred_header_market <- renderText({
    
    shiny::validate(
      need(!is.null(input$pred_price_market), "Please select at least one market")
    )
    
    shiny::validate(need(!is.null(html_pred_header_market()), " "))
    print(html_pred_header_market())
  })
  
  
  # table output
  output$pred_table_market <- DT::renderDataTable({
    # validate that input is given, i.e. market is selected
    # message is printed if market is null, since !is.null() is neeed to
    # show plot
    shiny::validate(
      need(!is.null(input$pred_price_market), " ")
    )
    
    market_table_price_pred() 
    
  })
  
  
  # plot output
  ## create reactive value for height of plot
  ## height should be different depending on the number of predicted weeks
  pred_market_plot_height <- eventReactive(input$pred_show_market, {
    pred_market_horizon <- input$pred_horizon_market
    if (pred_market_horizon <= 2 & length(input$pred_price_market) <= 2) {
      pred_market_plot_height_sel <- 400
    } else {
      pred_market_plot_height_sel <- 600
    }
    pred_market_plot_height_sel
  })
  ## create reactive value for width of plot
  ## height should be different depending on the number of predicted weeks
  pred_market_plot_width <- eventReactive(input$pred_show_market, {
    pred_market_horizon <- input$pred_horizon_market
    if (pred_market_horizon <= 2 & length(input$pred_price_market) <= 2) {
      pred_market_plot_width_sel <- 900
    } else {
      pred_market_plot_width_sel <- 1000
    }
    pred_market_plot_width_sel
  })
  
  
  # if action button is not clicked return empty plot
  output$pred_bar_plot_market <- renderPlot({
    ggplot() +
      theme(
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black")
      )
  })
  
  # if action button is clicked return "true" plot
  observeEvent(input$pred_show_market, {
    pred_market_plot_sel <- input$pred_price_market
    ## create plot
    output$pred_bar_plot_market <- renderPlot({
      # validate that input is given, i.e. market is selected
      # message is printed if market is null, since !is.null() is neeed to
      # message with text is already printed for table
      shiny::validate(
        need(!is.null(input$pred_price_market), " ")
      )
      
      # flexible tickmarks
      if (max(market_plot_price_pred()$value) <= 2) {
        market_comp_breaks <-  0.5
      } else {
        market_comp_breaks <- 1
      }
      
      # layout of plot output depends on the number of selected markets
      if (length(pred_market_plot_sel) <= 2) {
        ggplot(data = market_plot_price_pred()) +
          # stat = "identity" allows to select x and y column
          # position = "dodge" puts columns side by side
          geom_bar(aes(x = market, y = value, fill = variable), 
                   stat = "identity", position = "dodge"#, width = 0.5
          ) +
          # add label with price above bars
          geom_text(aes(x = market, y = value, group = variable, 
                        label = paste0(sprintf(value, fmt = '%#.2f'), "\u20ac")), 
                    position = position_dodge(width = 1), vjust = -.5, 
                    color = "white", size = 6) +
          # one plot for each date
          # x- and y-axis labels
          facet_wrap(~ date, ncol = 2) +
          xlab(" ") +
          ylab(pred_line_plot_market_model_axis_labels()[[2]]) +
          # y-axis limits
          #ylim(0, max(market_plot_price_pred()$value) + 0.1) +
          # scale y-axis
          scale_y_continuous(
            limits = c(0, max(market_plot_price_pred()$value) + 0.15), 
            breaks = seq(0, max(market_plot_price_pred()$value), 
                         market_comp_breaks)
          ) +
          # change layout of p
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 20, colour = "white"), 
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(size = 16, colour = 'white'),
            # axis labels
            axis.title = element_text(size = 16, colour = "white"),
            axis.text = element_text(size = 16, colour = "white"),
            axis.text.x = element_text(size = 16, colour = "white"),
            axis.text.y = element_text(size = 16, colour = "white")
          ) +
          # change color of bars
          scale_fill_manual("legend", values = c("#FFFF00", "#00FFFF"))
      } else if (length(pred_market_plot_sel) <= 3) {
        ggplot(data = market_plot_price_pred()) +
          # stat = "identity" allows to select x and y column
          # position = "dodge" puts columns side by side
          geom_bar(aes(x = market, y = value, fill = variable), 
                   stat = "identity", position = "dodge"#, width = 0.5
          ) +
          # add label with price above bars
          geom_text(aes(x = market, y = value, group = variable, 
                        label = paste0(sprintf(value, fmt = '%#.2f'), "\u20ac")), 
                    position = position_dodge(width = 1), vjust = -.5, 
                    color = "white", size = 4) +
          # one plot for each date
          # x- and y-axis labels
          facet_wrap(~ date, ncol = 2) +
          xlab(" ") +
          ylab(pred_line_plot_market_model_axis_labels()[[2]]) +
          # y-axis limits
          #ylim(0, max(market_plot_price_pred()$value) + 0.1) +
          # scale y-axis
          scale_y_continuous(
            limits = c(0, max(market_plot_price_pred()$value) + 0.15), 
            breaks = seq(0, max(market_plot_price_pred()$value), 
                         market_comp_breaks)
          ) +
          # change layout of p
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 20, colour = "white"), 
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(size = 16, colour = 'white'),
            # axis labels
            axis.title = element_text(size = 16, colour = "white"),
            axis.text = element_text(size = 16, colour = "white"),
            axis.text.x = element_text(size = 16, colour = "white"),
            axis.text.y = element_text(size = 16, colour = "white")
          ) +
          # change color of bars
          scale_fill_manual("legend", values = c("#FFFF00", "#00FFFF"))
      } else {
        ggplot(data = market_plot_price_pred()) +
          # stat = "identity" allows to select x and y column
          # position = "dodge" puts columns side by side
          geom_bar(aes(x = market, y = value, fill = variable), 
                   stat = "identity", position = "dodge"#, width = 0.5
          ) +
          # add label with price above bars
          geom_text(aes(x = market, y = value, group = variable, 
                        label = paste0(sprintf(value, fmt = '%#.2f'), "\u20ac")), 
                    position = position_dodge(width = 1), vjust = -.5, 
                    color = "white", size = 3) +
          # one plot for each date
          # x- and y-axis labels
          facet_wrap(~ date, ncol = 2) +
          xlab(" ") +
          ylab(pred_line_plot_market_model_axis_labels()[[2]]) +
          # y-axis limits
          #ylim(0, max(market_plot_price_pred()$value) + 0.1) +
          # scale y-axis
          scale_y_continuous(
            limits = c(0, max(market_plot_price_pred()$value) + 0.15), 
            breaks = seq(0, max(market_plot_price_pred()$value), 
                         market_comp_breaks)
          ) +
          # change layout of p
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 20, colour = "white"), 
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(size = 16, colour = 'white'),
            # axis labels
            axis.title = element_text(size = 16, colour = "white"), 
            axis.text = element_text(size = 16, colour = "white"),
            axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5, 
                                       hjust = 1, colour = "white"),
            axis.text.y = element_text(size = 16, colour = "white")
          ) +
          # change color of bars
          scale_fill_manual("legend", values = c("#FFFF00", "#00FFFF"))
      }
    }, height = pred_market_plot_height(), width = pred_market_plot_width())
    
  })
  
  
  
  # plot output all predictions
  
  # define plot title and y-axis label
  pred_line_plot_market_model_axis_labels <- eventReactive(input$pred_show_market, {
    
    # replace models
    pred_line_plot_market_model_sel <- 
      unname(unlist(ifelse(!is.na(model_replacement[input$pred_model_market]), 
                           model_replacement[input$pred_model_market], 
                           input$pred_model_market)))
    
    # replace markets 
    pred_line_plot_market_market_sel <- 
      unname(unlist(ifelse(!is.na(market_replacement[input$pred_price_market]), 
                           market_replacement[input$pred_price_market], 
                           input$pred_price_market)))
    pred_line_plot_market_market_sel <- 
      paste0(sub(",([^,]*)$", ", and\\1", 
                 paste(pred_line_plot_market_market_sel, collapse = ", ")), ".")
    
    ## create title ##
    pred_line_plot_market_title <-
      paste0("True vs. predicted prices for ", input$pred_food_sel_market, ", ", 
             input$pred_food_type_market, 
             ", ", input$pred_food_size_market, " from ", input$pred_food_country_market, "<br>",  
             "<sup>",
             "Predictions origin from ", pred_line_plot_market_model_sel, " algorithm for markets ",
             pred_line_plot_market_market_sel, "<br>", "<br>", "<br>", "<br>")
    
    
    ## define y-axis labels (depended on food item) ##
    if (input$pred_food_sel_market %in% c("Iceberg Lettuce", "Cauliflower", "Endive")) {
      plot_market_y_axis <- "price for one piece in \u20ac "
    } else {
      plot_market_y_axis <- "price for 1kg in \u20ac "
    }
    
    
    # first return title, then y-axis label
    list(pred_line_plot_market_title, plot_market_y_axis)
    
  })
  
  
  # show plot
  observeEvent(input$pred_show_market, {
    output$pred_line_plot_market <- renderPlotly({
      
      shiny::validate(
        need(!is.null(input$pred_price_market), "Please select at least one market")
      )
      
      if (!is.null(input$pred_price_market)) {
        #if ("Line Plot" %in% input$display_sel_model) {
        # define colors
        color_true_price <- "#00FFFF"
        color_other_models <- c("#FFFF00", "#0033FF", "#00FF00", "#FF0000", "#FF6600", 
                                "#9900FF", "#FF00CC", "#099FFF", "#FF00FF", "#FF0099")
        color_other_models <- 
          color_other_models[1:length(unique(df_pred_market_comp()$market))]
        color_all <- c(color_true_price, color_other_models)
        
        
        if (input$pred_line_plot_market_sel_button == "Yes") {
          gg_plot_market_comparison <-
            ggplot(data = df_pred_market_comp()) +
            geom_line(aes(x = date, y = `true price`, color = market, color = market, 
                          text = paste0("date: ", date %>% format("%B %d, %Y"), '<br>',
                                        "true price in ", market, ": ",  
                                        sprintf(`true price`, fmt = '%#.2f'), " \u20ac"
                          ),
                          group = 1),
                      linetype = "dotted", size = 0.1) +
            geom_line(aes(x = date, y = `predicted price`, color = market, 
                          text = paste0("date: ", date %>% format("%B %d, %Y"), '<br>',
                            "predicted price in ", market, ": ", 
                            sprintf(`predicted price`, fmt = '%#.2f'), " \u20ac"),
                          #'<br>', "date: ", date %>% format("%B %d, %Y"),
                          #'<br>', "model: ", model),
                          group = 1), size = 0.2) +
            facet_wrap(~ market, ncol = 2) + 
            xlab(" ") +
            ylab(pred_line_plot_market_model_axis_labels()[[2]]) +
            labs(title = pred_line_plot_market_model_axis_labels()[[1]]) +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white", size = 1), 
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"), 
              legend.key = element_rect(fill = "black"),
              # title color
              plot.title = element_text(colour = "white"), 
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title.x = element_text(colour = "white"),
              axis.title.y = element_text(colour = "white"),
              # color of facet
              strip.background = element_rect(fill = "black", colour = "white"), 
              strip.text = element_text(colour = 'white', size = 10, face = "bold")
            ) +
            # change color of bars
            scale_color_manual("legend", values = color_all) +
            scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y")
          ggplotly( 
            gg_plot_market_comparison, width = 1000, height = 700,
            tooltip = "text"
          ) %>%
            plotly::layout(margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4))#%>%
            #plotly::layout(hovermode = "x unified")
        } else {
          gg_plot_market_comparison <-
            ggplot(data = df_pred_market_comp()) +
            geom_line(aes(x = date, y = `predicted price`, color = market, 
                          text = paste0(
                            "date: ", date %>% format("%B %d, %Y"), '<br>',
                            "predicted price in ", market, ": ", 
                            sprintf(`predicted price`, fmt = '%#.2f'), " \u20ac"),
                          #'<br>', "date: ", date %>% format("%B %d, %Y"),
                          #'<br>', "model: ", model),
                          group = 1), size = 0.1) +
            xlab(" ") +
            ylab(pred_line_plot_market_model_axis_labels()[[2]]) +
            labs(title = pred_line_plot_market_model_axis_labels()[[1]]) +
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "white"), 
              # no grid
              panel.grid.major = element_blank(),
              # legend
              legend.background = element_rect(fill = "black", colour = "black"),
              legend.text = element_text(size = 10, colour = "white"), 
              legend.key = element_rect(fill = "black"),
              # title color
              plot.title = element_text(colour = "white"), 
              # axis labels
              axis.text = element_text(size = 10, colour = "white"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title.x = element_text(colour = "white"),
              axis.title.y = element_text(colour = "white")
            ) +
            # change color of bars
            scale_color_manual("legend", values = color_all) +
            scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y")
          ggplotly( 
            gg_plot_market_comparison, width = 1000, height = 650,
            tooltip = "text"
          ) %>%
            plotly::layout(hovermode = "x unified")
        }
      } else {
        ggplotly(
          ggplot() + 
            theme(
              # black background of panel and plot
              panel.background = element_rect(fill = "black", color = "black"),
              plot.background = element_rect(fill = "black", color = "black"),
              # add white border around facets
              panel.border = element_rect(fill = NA, color = "black"),
              # no grid
              panel.grid = element_blank()
            )
        )
      }
      
      
    })
  })
  
  
  observeEvent(input$pred_show_market, {
    output$pred_line_plot_market_info <- renderUI({
      if (!is.null(input$pred_price_market)) {
        HTML("User instruction: You can hover over the lines to get the exact 
        date and price information. Moreover, you can zoom into the plot 
        by drawing a window with your mouse. <br/>
        The solid line presents the predicted value while the dotted line
        presents the true value.")
      } else {
        " "
      }
    })
  })
  
  
  
  ## error metrics for market comparison ##
  
  # generate error metrics
  df_pred_market_error <- eventReactive(input$pred_show_market, {
    if (!is.null(input$pred_price_market)) {
      # error metrics for complete period
      df_error_pred_market_complete <- df_pred_market_comp() %>%
        group_by(market) %>%
        dplyr::summarise(
          RMSE = paste0(sprintf(MLmetrics::RMSE(`predicted price`, `true price`), 
                                fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MLmetrics::MAE(`predicted price`, `true price`), 
                               fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MLmetrics::MAPE(`predicted price`, `true price`)*100,
                                fmt = '%#.2f'), " %")
        )
      
      # error metrics for predictions
      market_rows_keep <- input$pred_horizon_market * length(input$pred_price_market)
      df_error_pred_market_pred <-     
        df_price_pred_all <- df_pred_market_comp() %>%
        dplyr::arrange(date) %>%
        tail(market_rows_keep) %>%
        group_by(market) %>%
        dplyr::summarise(
          RMSE = paste0(sprintf(MLmetrics::RMSE(`predicted price`, `true price`), 
                                fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MLmetrics::MAE(`predicted price`, `true price`), 
                               fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MLmetrics::MAPE(`predicted price`, `true price`)*100,
                                fmt = '%#.2f'), " %")
        )
      
      
      # return both outputs in list
      list(df_error_pred_market_complete, df_error_pred_market_pred)
    } else {
      list(data.frame(), data.frame())
    }
    
  })
  
  # show output
  observeEvent(input$pred_show_market, {
    # text for error metrics
    output$pred_line_plot_market_error_text_1 <- renderUI({
      shiny::validate(
        need(!is.null(input$pred_price_market), " ")
      )
      
      if (input$pred_line_plot_market_sel_error == "Yes") {
        HTML("Error metrics from 2016 onward: <br>")
      } else {
        " "
      }
      
    })
    
    output$pred_line_plot_market_error_text_2 <- renderUI({
      shiny::validate(
        need(!is.null(input$pred_price_market), " ")
      )
      
      if (input$pred_line_plot_market_sel_error == "Yes") {
        HTML(paste("Error metrics for the next", 
                   input$pred_horizon_market, "weeks: <br>"))
      } else {
        " "
      }
    })
    
    
    # error metric tables
    output$pred_line_plot_market_error_1 <- renderTable({
      shiny::validate(
        need(!is.null(input$pred_price_market), " ")
      )
      
      if (input$pred_line_plot_market_sel_error == "Yes") {
        df_pred_market_error()[[1]]
      } else {
        data.frame()
      }
      
    })
    
    output$pred_line_plot_market_error_2 <- renderTable({
      shiny::validate(
        need(!is.null(input$pred_price_market), " ")
      )
      
      if (input$pred_line_plot_market_sel_error == "Yes") {
        df_pred_market_error()[[2]]
      } else {
        data.frame()
      }
    })
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### model comparison ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## reactive select input ##
  
  # reactive data sets based on selection
  model_value_pred_food_type_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_model) %>%
      dplyr::select(type) %>% unique() %>% pull() %>% sort()
  })
  
  
  model_value_pred_food_size_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_model, 
             type %in% input$pred_food_type_model) %>%
      dplyr::select(size) %>% unique() %>% pull() %>% mixedsort()
  })
  
  model_value_pred_food_country_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$pred_food_sel_model, 
             type %in% input$pred_food_type_model,
             size %in% input$pred_food_size_model) %>%
      dplyr::select(country) %>% unique() %>% pull() %>% sort()
  })
  
  
  # update select input based on selection
  observeEvent(input$pred_food_sel_model, {
    updateSelectInput(session, "pred_food_type_model",
                      choices = model_value_pred_food_type_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_model, input$pred_food_type_model), {
    updateSelectInput(session, "pred_food_size_model",
                      choices = model_value_pred_food_size_reactive())
  })
  
  observeEvent(c(input$pred_food_sel_model, input$pred_food_type_model, input$pred_food_size_model), {
    updateSelectInput(session, "pred_food_country_model",
                      choices = model_value_pred_food_country_reactive())
  })
  
  
  ## Header ##
  
  # the header is the table header which explains what is displayed in the table.
  html_pred_header_model <- eventReactive(input$pred_show_model, {
    # replace models
    selected_model_model <- 
      unname(unlist(ifelse(!is.na(model_replacement[input$pred_model_model]), 
                           model_replacement[input$pred_model_model], 
                           input$pred_model_model)))
    
    # replace markets 
    selected_market_model <- 
      unname(unlist(ifelse(!is.na(market_replacement[input$pred_price_model]), 
                           market_replacement[input$pred_price_model], 
                           input$pred_price_model)))
    
    
    HTML(
      paste0("<b>", "Price predictions resulting from wholesale market in ", 
             selected_market_model, 
             " for the next ", input$pred_horizon_model,
             " weeks for ", input$pred_food_sel_model, ", ", 
             input$pred_food_size_model, ", ",
             " from ", input$pred_food_country_model, ".", "</b>", "<br>", 
             "The models used to make the predictions are: ", 
             sub(",([^,]*)$", ", and\\1", paste(selected_model_model, 
                                                collapse = ", ")), 
             ".")
    )
  })
  
  
  output$pred_header_model <- renderText({
    # message is already printed below
    # ensure that caption is only displayed if model is selected
    shiny::validate(
      need(!is.null(input$pred_model_model), "Please select at least one model")
    )
    
    shiny::validate(need(!is.null(html_pred_header_model()), " "))
    print(html_pred_header_model())
  })
  
  ## Make predictions ##
  df_model_price_pred <- eventReactive(input$pred_show_model, {
    # create data frames to store predictions
    df_price_pred_bsts_all <- data.frame()
    df_price_pred_all <- data.frame()
    
    # make predictions: depends on selected model
    # time series models have different function than other models
    if (any(c("BSTS Baseline", "BSTS") %in% input$pred_model_model)) {
      # extract only BSTS Baseline and BSTS; drop all other models
      bsts_pred_model_model <- 
        input$pred_model_model[input$pred_model_model %in% c("BSTS", "BSTS Baseline")]
      # iterate over selected markets and models
      for (price_pred_model in bsts_pred_model_model) {
        for (price_pred_market in input$pred_price_model) {
          df_price_pred <-
            func_price_predictions_bsts(input$pred_food_sel_model, 
                                        input$pred_food_type_model,
                                        input$pred_food_size_model, 
                                        input$pred_food_country_model,
                                        price_pred_model, price_pred_market, 
                                        input$pred_horizon_model) %>%
            # create column for model
            mutate(model = price_pred_model) %>%
            # ensure that date column is properly displayed
            mutate(date = lubridate::ymd(date)) %>%
            # arrange by date 
            dplyr::arrange(date) %>%
            # ensure correct ordering
            dplyr::select(date, model, market, food, type, size, country, 
                          `true price`, `predicted price`, `price deviation`)
          
          # replace market names
          for (i in 1:length(market_replacement)) {
            df_price_pred$market <- 
              unlist(replace(df_price_pred$market, 
                             df_price_pred$market == names(market_replacement[i]), 
                             market_replacement[i]))
          }
          
          # append
          df_price_pred_bsts_all <- 
            rbind(df_price_pred_bsts_all, df_price_pred)
          
          # ensure that no duplicates are included
          df_price_pred_bsts_all <- df_price_pred_bsts_all %>% distinct()
        }
      }
    } 
    
    # predictions for other models
    if (any(c("LASSO", "RIDGE", "ENET", "XGBoost", 
              "SVM", "NN", "Random_Forest") %in% input$pred_model_model)) {
      # extract the models
      ml_pred_model_model <- 
        input$pred_model_model[!input$pred_model_model %in% c("BSTS", "BSTS Baseline")]
      # iterate over markets and models to make the predictions
      for (price_pred_model in ml_pred_model_model) {
        for (price_pred_market in input$pred_price_model) {
          df_price_pred <-
            func_price_predictions_ml(input$pred_food_sel_model, 
                                      input$pred_food_type_model,
                                      input$pred_food_size_model, 
                                      input$pred_food_country_model,
                                      price_pred_model, price_pred_market, 
                                      input$pred_horizon_model) %>%
            # create column for model
            mutate(model = price_pred_model) %>%
            # ensure that date column is properly displayed
            mutate(date = lubridate::ymd(date)) %>%
            # arrange by date 
            dplyr::arrange(date) %>%
            # ensure correct ordering
            dplyr::select(date, model, market, food, type, size, country, 
                          `true price`, `predicted price`, `price deviation`)
          
          # replace market names
          for (i in 1:length(market_replacement)) {
            df_price_pred$market <- 
              unlist(replace(df_price_pred$market, 
                             df_price_pred$market == names(market_replacement[i]), 
                             market_replacement[i]))
          }
          
          
          df_price_pred_all <- rbind(df_price_pred_all, df_price_pred)
        }
      }
    } 
    
    # append data frames
    df_price_pred_model_table_all <- 
      rbind(df_price_pred_bsts_all, df_price_pred_all)
    
    # replace model names
    for (i in 1:length(model_replacement)) {
      df_price_pred_model_table_all$model <- 
        unlist(replace(df_price_pred_model_table_all$model, 
                       df_price_pred_model_table_all$model == names(model_replacement[i]), 
                       model_replacement[i]))
    }
    
    # return data frame with predictions
    df_price_pred_model_table_all
  })
  
  
  ## TABLE OUTPUT if action button is clicked ##
  
  # create datatable only if action button is clicked
  # make predictions
  model_table_price_pred <- eventReactive(input$pred_show_model, {
    
    if (!is.null(input$pred_model_model)) {
      # final preparation
      df_price_pred_model_table_all <- 
        df_model_price_pred() %>%
        # keep only variables of interest
        dplyr::select(date, model, `true price`,
                      `predicted price`, `price deviation`) %>%
        # arrange by date and keep only columns for predictions
        arrange(date) %>%
        tail(input$pred_horizon_model * length(input$pred_model_model)) %>% 
        # nice date format
        mutate(date = as.Date(date) %>% format("%B %d, %Y")) %>%
        # add euro
        mutate(
          `true price` = paste(sprintf(`true price`, fmt = '%#.2f'), " \u20ac"),
          `predicted price` = paste(sprintf(`predicted price`, fmt = '%#.2f'), " \u20ac"),
          `price deviation` = paste(sprintf(`price deviation`, fmt = '%#.2f'), " \u20ac")
        ) %>%
        as.data.frame()
      
      # create data table from data frame
      DT::datatable(
        data = df_price_pred_model_table_all, rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"))) %>%
        formatStyle(columns = c(1:10), color = "white",
                    backgroundColor = "black", background = "black")
    } else {
      data.frame()
    }
    
    
  })
  
  
  # table output
  output$pred_table_model <- DT::renderDataTable({
    #if ("Table" %in% input$display_sel_model) {
    # validate that input is given, i.e. market is selected
    # message is printed if market is null, since !is.null() is neeed to
    # show plot
    shiny::validate(
      need(!is.null(input$pred_model_model), " ")
    )
    
    model_table_price_pred()
    #}
  })
  
  
  ## BAR PLOT OUTPUT when action button is clicked ##
  
  model_bar_plot_price_pred <- eventReactive(input$pred_show_model,{
    
    if (!is.null(input$pred_model_model)) {
      # final preparation for data bar plot
      df_price_pred_model_plot_all <- 
        df_model_price_pred() %>%
        # arrange by date and keep only columns for predictions
        arrange(date) %>%
        tail(input$pred_horizon_model * length(input$pred_model_model))
      
      # change structure of data frame
      df_price_pred_model_plot_all <- reshape2::melt(df_price_pred_model_plot_all, 
                                                     id.vars = c("model", "date"))
      
      # keep only necessary variables
      df_price_pred_model_plot_all %>%
        filter(variable %in% c("true price", "predicted price")) %>%
        mutate(value = as.numeric(value)) %>%
        # nice date format
        mutate(date = as.Date(date) %>% format("%B %d, %Y")) 
    } else {
      data.frame()
    }
    
  })
  
  
  ## create reactive value for height of plot
  # height should be different depending on the number of predicted weeks
  pred_model_plot_height <- eventReactive(input$pred_show_model, {
    pred_model_horizon <- input$pred_horizon_model
    if (pred_model_horizon <= 2 & length(input$pred_model_model) <= 2) {
      pred_model_plot_height_sel <- 400
    } else {
      pred_model_plot_height_sel <- 650
    }
    pred_model_plot_height_sel
  })
  ## create reactive value for width of plot
  ## height should be different depending on the number of predicted weeks
  pred_model_plot_width <- eventReactive(input$pred_show_model, {
    pred_model_horizon <- input$pred_horizon_model
    if (pred_model_horizon <= 2 & length(input$pred_model_model) <= 2) {
      pred_model_plot_width_sel <- 900
    } else {
      pred_model_plot_width_sel <- 1000
    }
    pred_model_plot_width_sel
  })
  
  
  
  # if action button is not clicked return empty plot
  output$pred_bar_plot_model <- renderPlot({
    # only display if at least one model is selected
    shiny::validate(
      need(!is.null(input$pred_model_model), " ")
    )
    
    # plot
    ggplot() +
      theme(
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black")
      )
  })
  
  observeEvent(input$pred_show_model, {
    output$pred_bar_plot_model <- renderPlot({
      #if ("Bar Plot" %in% input$display_sel_model) {
      # only display this plot if market comparison is clicked
      # validate that input is given, i.e. market is selected
      # message is printed if market is null, since !is.null() is needed to
      # show plot.
      # message already printed for table above
      shiny::validate(
        need(!is.null(input$pred_model_model), " ")
      )
      
      # flexible tickmarks
      if (max(model_bar_plot_price_pred()$value) <= 2) {
        model_comp_breaks <-  0.5
      } else {
        model_comp_breaks <- 1
      }
     
      
      # plot depends on number of models selected
      if (length(input$pred_model_model) <= 2) {
        gg_model_barplot <- 
          ggplot(data = model_bar_plot_price_pred()) +
          # stat = "identity" allows to select x and y column
          # position = "dodge" puts columns side by side
          geom_bar(aes(x = model, y = value, fill = variable), 
                   stat = "identity", position = "dodge") +
          # add label with price above bars
          geom_text(aes(x = model, y = value, group = variable, 
                        label = paste0(sprintf(value, fmt = '%#.2f'), "\u20ac")), 
                    position = position_dodge(width = 1), vjust = -.5, 
                    color = "white", size = 6)  +
          # one plot for each date
          facet_wrap(~ date, ncol = 2) +
          # x- and y-axis labels
          xlab(" ") +
          ylab(pred_line_plot_model_axis_labels()[[2]]) +
          # y-axis limits
          scale_y_continuous(
            limits = c(0, max(model_bar_plot_price_pred()$value) + 0.15), 
            breaks = seq(0, max(model_bar_plot_price_pred()$value), 
                         model_comp_breaks)
          ) +
          # change layout of p
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 20, colour = "white"), 
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(size = 16, colour = 'white'),
            # axis labels
            axis.title = element_text(size = 16, colour = "white"),
            axis.text = element_text(size = 16, colour = "white"),
            axis.text.x = element_text(size = 16, colour = "white"),
            axis.text.y = element_text(size = 16, colour = "white")
          ) +
          # change color of bars
          scale_fill_manual("legend", values = c("#FFFF00", "#00FFFF"))
      } else {
        gg_model_barplot <- 
          ggplot(data = model_bar_plot_price_pred()) +
          # stat = "identity" allows to select x and y column
          # position = "dodge" puts columns side by side
          geom_bar(aes(x = model, y = value, fill = variable), 
                   stat = "identity", position = "dodge") +
          # add label with price above bars
          geom_text(aes(x = model, y = value, group = variable, 
                        label = paste0(sprintf(value, fmt = '%#.2f'), "\u20ac")), 
                    position = position_dodge(width = 1), vjust = -.5, 
                    color = "white", size = 4)  +
          # one plot for each date
          facet_wrap(~ date, ncol = 2) +
          # x- and y-axis labels
          xlab(" ") +
          ylab(pred_line_plot_model_axis_labels()[[2]]) +
          # y-axis limits
          scale_y_continuous(
            limits = c(0, max(model_bar_plot_price_pred()$value) + 0.15), 
            breaks = seq(0, max(model_bar_plot_price_pred()$value), 
                         model_comp_breaks)
          ) +
          # change layout of p
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 20, colour = "white"), 
            # change color of facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(size = 16, colour = 'white'),
            # axis labels
            axis.title = element_text(size = 16, colour = "white"),
            axis.text = element_text(size = 16, colour = "white"),
            axis.text.x = element_text(size = 16, colour = "white",
                                       angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 16, colour = "white")
          ) +
          # change color of bars
          scale_fill_manual("legend", values = c("#FFFF00", "#00FFFF"))
      }
      
      gg_model_barplot
    }, height = pred_model_plot_height(), width = pred_model_plot_width())
  })
  
  
  ## LINE PLOT OUTPUT when action button is clicked ##
  
  pred_line_plot_model_axis_labels <- eventReactive(input$pred_show_model, {
    # define plot title
    ## replace models
    pred_line_plot_model_model_sel <- 
      unname(unlist(ifelse(!is.na(model_replacement[input$pred_model_model]), 
                           model_replacement[input$pred_model_model], 
                           input$pred_model_model)))
    pred_line_plot_model_model_sel <- 
      paste0(sub(",([^,]*)$", ", and\\1", 
                 paste(pred_line_plot_model_model_sel, collapse = ", ")), ".")
    ## replace markets 
    pred_line_plot_model_market_sel <- 
      unname(unlist(ifelse(!is.na(market_replacement[input$pred_price_model]), 
                           market_replacement[input$pred_price_model], 
                           input$pred_price_model)))
    ## title
    pred_line_plot_model_title <-
      paste0("True vs. predicted prices for ", input$pred_food_sel_model, ", ", 
             input$pred_food_type_model, ", ", input$pred_food_size_model, " from ", 
             input$pred_food_country_model, "<br>",  
             "<sup>",
             "Predictions are for wholesale market ", pred_line_plot_model_market_sel, 
             " made by the following models: ", pred_line_plot_model_model_sel,
             "<br>", "<br>", "<br>", "<br>")
    
    
    # define y-axis labels (depended on food item)
    if (input$pred_food_sel_model %in% c("Iceberg Lettuce", "Cauliflower", "Endive")) {
      plot_model_y_axis <- "price for one piece in  \u20ac "
    } else {
      plot_model_y_axis <- "price for 1kg in \u20ac "
    }
    
    
    # return: first title, then axis
    list(pred_line_plot_model_title, plot_model_y_axis)
  })
  
  
  output$pred_line_plot_model <- renderPlotly({
    
    # message if no selected is made
    shiny::validate(
      need(!is.null(input$pred_model_model), 
           "Please select at least one model.")
    )
    
    # define colors
    color_true_price <- "#00FFFF"
    color_other_models <- c("#FFFF00", "#0033FF", "#00FF00", "#FF0000", "#FF6600", 
                            "#9900FF", "#FF00CC", "#099FFF", "#FF00FF", "#FF0099")
    color_other_models <- 
      color_other_models[1:length(unique(df_model_price_pred()$model))]
    color_all <- c(color_true_price, color_other_models)
    
    
    # create plot
    gg_plot_model_comparison <-
      ggplot(data = df_model_price_pred()) +
      geom_line(aes(x = date, y = `true price`, color = "true", 
                    text = paste0("date: ", date %>% format("%B %d, %Y"), '<br>',
                                  "true price: ", sprintf(`true price`, fmt = '%#.2f'), 
                                  " \u20ac"
                    ),
                    group = 1), size = 0.4) +
      geom_line(aes(x = date, y = `predicted price`, color = model, 
                    text = paste0("predicted price from ", model, ": ", 
                                  sprintf(`predicted price`, fmt = '%#.2f'), 
                                  " \u20ac"),
                    #'<br>', "date: ", date %>% format("%B %d, %Y"),
                    #'<br>', "model: ", model),
                    group = 1), size = 0.4) +
      xlab(" ") +
      ylab(pred_line_plot_model_axis_labels()[[2]]) +
      labs(title = pred_line_plot_model_axis_labels()[[1]]) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"), 
        # no grid
        panel.grid.major = element_blank(),
        # legend
        legend.background = element_rect(fill = "black", colour = "black"),
        legend.text = element_text(size = 10, colour = "white"), 
        legend.key = element_rect(fill = "black"),
        # plot title
        plot.title = element_text(colour = "white"), 
        # axis labels
        axis.text = element_text(size = 10, colour = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white")
      ) +
      # change color of bars
      scale_color_manual("legend", values = color_all) +
      scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y")
    ggplotly( 
      gg_plot_model_comparison, width = 1000, height = 650,
      tooltip = "text"
    ) %>%
      plotly::layout(hovermode = "x unified")
  })
  
  
  observeEvent(input$pred_show_model, {
    output$pred_line_plot_model_info <- renderUI({
      shiny::validate(
        need(!is.null(input$pred_model_model), 
             " ")
      )
      
      HTML("User instruction: 
      You can hover over the lines to get the exact date and price information. <br/>
     Moreover, you can zoom into the plot by drawing a window with your mouse.")
    })
  })
  
  
  
  ## error metrics for model comparison ##
  
  # generate error metrics
  df_pred_model_error <- eventReactive(input$pred_show_model, {
    
    if (!is.null(input$pred_model_model)) {
      # error metrics for complete period
      df_error_pred_model_complete <- 
        df_model_price_pred() %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = paste0(sprintf(MLmetrics::RMSE(`predicted price`, `true price`), 
                                fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MLmetrics::MAE(`predicted price`, `true price`), 
                               fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MLmetrics::MAPE(`predicted price`, `true price`)*100,
                                fmt = '%#.2f'), " %")
        )
      
      # error metrics for predictions
      model_rows_keep <- input$pred_horizon_model * length(input$pred_model_model)
      
      df_error_pred_model_pred <-     
        df_price_pred_all <- df_model_price_pred() %>%
        dplyr::arrange(date) %>%
        tail(model_rows_keep) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = paste0(sprintf(MLmetrics::RMSE(`predicted price`, `true price`), 
                                fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MLmetrics::MAE(`predicted price`, `true price`), 
                               fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MLmetrics::MAPE(`predicted price`, `true price`)*100,
                                fmt = '%#.2f'), " %")
        )
      
      
      # return both outputs in list
      list(df_error_pred_model_complete, df_error_pred_model_pred)
    } else {
      list(data.frame(), data.frame())
    }
    
  })
  
  
  # show output
  observeEvent(input$pred_show_model, {
    # text for error metrics
    output$pred_line_plot_model_error_text_1 <- renderUI({
      
      shiny::validate(
        need(!is.null(input$pred_model_model), 
             "")
      )
      
      if (input$pred_line_plot_model_error == "Yes") {
        HTML("Error metrics from 2016 onward: <br>")
      } else {
        " "
      }
      
    })
    
    output$pred_line_plot_model_error_text_2 <- renderUI({
      
      shiny::validate(
        need(!is.null(input$pred_model_model), 
             " ")
      )
      
      if (input$pred_line_plot_model_error == "Yes") {
        HTML(paste("Error metrics for the next", 
                   input$pred_horizon_model, "weeks: <br>"))
      } else {
        " "
      }
    })
    
    
    # error metric tables
    output$pred_line_plot_model_error_1 <- renderTable({
      
      shiny::validate(
        need(!is.null(input$pred_model_model), 
             " ")
      )
      
      if (input$pred_line_plot_model_error == "Yes") {
        df_pred_model_error()[[1]]
      } else {
        data.frame()
      }
      
    })
    
    output$pred_line_plot_model_error_2 <- renderTable({
      
      shiny::validate(
        need(!is.null(input$pred_model_model), 
             " ")
      )
      
      if (input$pred_line_plot_model_error == "Yes") {
        df_pred_model_error()[[2]]
      } else {
        data.frame()
      }
    })
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### ML Performance Measures ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  # info button
  onclick("error_analysis_type", 
          runjs("window.open('Theory_Performance_Metrics_Analysis_Types.html')"))
  
  # update selectinput based on previous selection
  # type, size, and country have also category "none"
  
  # reactive data sets based on selection
  perf_value_food_type_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$error_food_detail) %>%
      dplyr::select(type) %>% unique() %>% pull() %>% sort()
  })
  
  perf_value_food_size_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$error_food_detail, 
             type %in% input$error_type) %>%
      dplyr::select(size) %>% unique() %>% pull() %>% mixedsort()
  })
  
  perf_value_food_country_reactive <- reactive({
    df_descr %>%
      filter(food %in% input$error_food_detail, 
             type %in% input$error_type,
             size %in% input$error_size) %>%
      dplyr::select(country) %>% unique() %>% pull() %>% sort()
  })
  
  # update select input based on selection
  observeEvent(input$error_food_detail, {
    updateSelectInput(session, "error_type",
                      choices = perf_value_food_type_reactive())
  })
  
  observeEvent(c(input$error_food_detail, input$error_type), {
    updateSelectInput(session, "error_size",
                      choices = perf_value_food_size_reactive())
  })
  
  observeEvent(c(input$error_food_detail, input$error_type, input$error_size), {
    updateSelectInput(session, "error_country",
                      choices = perf_value_food_country_reactive())
  })
  
  
  #### performance output:  select food manually ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## error metrics for overall food ##
  
  # data for table and plot
  df_perf_food <- eventReactive(input$error_food_show, {
    
    df_perf_food_aggregate <- data.frame()
    df_perf_food_market <- data.frame()
    
    # "Aggregate" means that performance measure is across all five markets
    if ("Aggregate" %in% input$error_market) {
      df_perf_food_aggregate <- 
        df_perf %>%
        filter(food %in% input$error_food & model %in% input$error_model) %>%
        group_by(model, food) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4) * 100
        ) %>%
        dplyr::arrange(model, food, RMSE) %>%
        mutate(market = "Aggregate") %>%
        dplyr::select(model, market, food, RMSE, MAE, MAPE) %>%
        ungroup() 
      # Otherwise if market is selected performance measures are shown for this market
    } 
    
    if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
            input$error_market)) {
      df_perf_food_market <- 
        df_perf %>%
        filter(food %in% input$error_food & model %in% input$error_model &
                 market %in% input$error_market) %>%
        group_by(model, market, food) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4) * 100
        ) %>%
        dplyr::arrange(model, food, market, RMSE) %>%
        dplyr::select(model, market, food, RMSE, MAE, MAPE) %>%
        ungroup() 
    }
    
    rbind(df_perf_food_aggregate, df_perf_food_market)
  })
  
  # output
  ## create reactive plot output
  gg_error_metrics_food_plot <- eventReactive(input$error_food_show, {
    # to order mutate(model = fct_reorder(model, RMSE))
    
    color_models <- c("#0033FF", "#00FF00", "#FF0000", "#FF6600", 
                      "#9900FF", "#FF00CC", "#099FFF", "#FF00FF", "#FF0099", 
                      "#00FFFF", "#FFFF00")
    
    color_models <- color_models[1:length(input$error_model)]
    
    # plot output depends on market selection: aggregate vs. markets
    if (length(input$error_market) == 1 & "Aggregate" %in% input$error_market) {
      ggplot(data = df_perf_food()) +
        # generate bar chart
        geom_bar(aes(x = model, y = RMSE, fill = model), 
                 stat = "identity", width = 0.5) +
        # add labels
        geom_text(aes(x = model, y = RMSE, 
                      label = paste0(sprintf(RMSE, fmt = '%#.2f'), "\u20ac")),
                  vjust = -0.25, color = "white", size = 3) +
        # grid of plots: facet only by food
        facet_grid(~ food) +
        # color bars according to model
        scale_fill_manual(values = color_models) +
        # expand y-axis for labels a bit
        ylim(0, max(df_perf_food()$RMSE) + 0.1) +
        ylab("RMSE") + 
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"), 
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"), 
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) 
    } else {
      # differentiate outcome by the number of models and food items
      if (length(input$error_model) <= 4 & length(input$error_food) <= 2 & 
          length(input$error_market) <= 3) {
        ggplot(data = df_perf_food()) +
          # generate bar chart
          geom_bar(aes(x = model, y = RMSE, fill = model), 
                   stat = "identity", width = 0.5) +
          # add labels
          geom_text(aes(x = model, y = RMSE, 
                        label = paste0(sprintf(RMSE, fmt = '%#.2f'), "\u20ac")),
                    vjust = -0.25, color = "white", size = 6) +
          # grid of plots: market on the left, food at the top
          facet_grid(market ~ food) +
          # color bars according to model
          scale_fill_manual(values = color_models) +
          # expand y-axis for labels a bit
          ylim(0, max(df_perf_food()$RMSE) + 0.1) +
          ylab("RMSE") + 
          xlab(" ") +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 14, colour = "white"), 
            # facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 14, face = "bold"),
            # axis labels
            axis.text = element_text(size = 14, colour = "white"),
            axis.text.x = element_text(size = 14),
            axis.title.x = element_text(colour = "white", size = 16),
            axis.title.y = element_text(colour = "white", size = 16)
          )
      } else {
        ggplot(data = df_perf_food() %>% arrange(RMSE)) +
          # generate bar chart
          geom_bar(aes(x = model, y = RMSE, fill = model), 
                   stat = "identity", width = 0.5) +
          # add labels
          geom_text(aes(x = model, y = RMSE, 
                        label = paste0(sprintf(RMSE, fmt = '%#.2f'), "\u20ac")),
                    vjust = -0.25, color = "white", size = 3) +
          # grid of plots: market on the left, food at the top
          facet_grid(market ~ food) +
          # color bars according to model
          scale_fill_manual(values = color_models) +
          # expand y-axis for labels a bit
          ylim(0, max(df_perf_food()$RMSE) + 0.1) +
          ylab("RMSE") + 
          xlab(" ") +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 14, colour = "white"), 
            # facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 14, face = "bold"),
            # axis labels
            axis.text = element_text(size = 14, colour = "white"),
            axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(colour = "white", size = 16),
            axis.title.y = element_text(colour = "white", size = 16)
          )
      }
    }
    
  })
  
  
  observeEvent(input$error_food_show, {
    
    # table
    output$error_metrics_food <- DT::renderDataTable({
      
      shiny::validate(need(!is.null(input$error_model),
                           "Please select at least one prediction model."))
      
      shiny::validate(need(!is.null(input$error_market),
                           "Please select at least one market."))
      
      shiny::validate(need(!is.null(input$error_food),
                           "Please select at least one fruit or vegetable."))
      
      # make output nicer with euro sign and %
      df_perf_food_nice <- df_perf_food() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"),
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          `Fruit/Vegetable` = food,
          Market = market,
          Model = model
        ) %>% 
        as.data.frame()
      
      # create data table
      DT::datatable(
        data = df_perf_food_nice,
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          pageLength = 15,
          lengthMenu = c(10, 15, 20, 30),
          dom = c("ltip")
        )) %>%
        formatStyle(columns = names(df_perf_food_nice), color = "white", 
                    backgroundColor = "black", background = "black")
    })
    
    
    # plot output
    output$error_metrics_food_plot <- renderPlot({
      # ensure that something is selected
      shiny::validate(need(!is.null(input$error_model),
                           "Please select at least one prediction model."))
      
      shiny::validate(need(!is.null(input$error_market),
                           "Please select at least one market."))
      
      shiny::validate(need(!is.null(input$error_food),
                           "Please select at least one fruit or vegetable."))
      
      # ensure that user selects only at the most 4 food items
      # ... at the most 6 models
      shiny::validate(need(length(input$error_food) <= 4,
                           "For the plot you can only select at most 4 fruits/vegetables."))
      shiny::validate(need(length(input$error_model) <= 6,
                           "For the plot you can only select at most 6 models."))
      
      # plot output
      gg_error_metrics_food_plot()
    }, width = 1000, height = 750)
  })
  
  
  ## output: error metrics for food-type-size-country combination ##
  
  # header
  html_pred_header_error_metrics_all <- eventReactive(input$error_all_show, {
    HTML(
      paste0("<b>", "Error metrics for ", input$error_food_detail, 
             ", ",  input$error_type, ", ", input$error_size, ", ",
             " from ", input$error_country, ":", "</b>")
    )
  })
  
  
  
  # data for table and plot
  df_perf_food_combi <- eventReactive(input$error_all_show, {
    
    df_perf_food_aggregate <- data.frame()
    df_perf_food_market <- data.frame()
    
    # distinguish between aggregation and not
    if ("Aggregate" %in% input$error_market) {
      df_perf_food_aggregate <- 
        df_perf %>%
        filter(food == input$error_food_detail & model %in% input$error_model &
                 type == input$error_type &
                 size == input$error_size & country == input$error_country) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4) * 100
        ) %>%
        mutate(market = "Aggregate") %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) %>%
        dplyr::arrange(model)
    } 
    
    if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
            input$error_market)) {
      df_perf_food_market <- 
        df_perf %>%
        filter(food == input$error_food_detail & model %in% input$error_model &
                 market %in% input$error_market & type == input$error_type &
                 size == input$error_size & country == input$error_country) %>%
        group_by(model, market) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4) * 100
        ) %>%
        dplyr::arrange(model, market) %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) 
    }
    
    rbind(df_perf_food_aggregate, df_perf_food_market)
  })
  
  
  # plot output dependent on selection
  gg_error_metrics_all_plot <- eventReactive(input$error_all_show, {
    
    # flexible number of facets depending on selected number of markets
    facets_error_metrics_all_plot <- 
      if (length(input$error_market) %in% c(2, 4)) {
        2
      } else {
        3
      }
    
    # define colors
    color_models <- c("#0033FF", "#00FF00", "#FF0000", "#FF6600", 
                      "#9900FF", "#FF00CC", "#099FFF", "#FF00FF", "#FF0099", 
                      "#00FFFF", "#FFFF00")
    
    color_models <- color_models[1:length(input$error_model)]
    
    # plot output depends on market selection: aggregate vs. markets
    if (length(input$error_market) == 1 & "Aggregate" %in% input$error_market) {
      ggplot(data = df_perf_food_combi()) +
        # generate bar chart
        geom_bar(aes(x = model, y = RMSE, fill = model), 
                 stat = "identity", width = 0.5) +
        # add labels
        geom_text(aes(x = model, y = RMSE, 
                      label = paste0(sprintf(RMSE, fmt = '%#.2f')), "\u20ac"),
                  vjust = -0.25, color = "white", size = 3) +
        # color bars according to model
        scale_fill_manual(values = color_models) +
        # expand y-axis for labels a bit
        ylim(0, max(df_perf_food()$RMSE) + 0.1) +
        ylab("RMSE") +
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "white"), 
          # no grid
          panel.grid.major = element_blank(),
          # legend
          legend.background = element_rect(fill = "black", colour = "black"),
          legend.text = element_text(size = 10, colour = "white"), 
          # axis labels
          axis.text = element_text(size = 10, colour = "white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) 
    } else {
      # differentiate outcome by the number of models and food items
      if (length(input$error_model) <= 4 & length(input$error_market) <= 2) {
        ggplot(data = df_perf_food_combi()) +
          # generate bar chart
          geom_bar(aes(x = model, y = RMSE, fill = model), 
                   stat = "identity", width = 0.5) +
          # add labels
          geom_text(aes(x = model, y = RMSE, 
                        label = paste0(sprintf(RMSE, fmt = '%#.2f'), "\u20ac")),
                    vjust = -0.25, color = "white", size = 6) +
          # grid of plots: market on the left, food at the top
          facet_wrap(~ market, ncol = facets_error_metrics_all_plot) +
          # color bars according to model
          scale_fill_manual(values = color_models) +
          # expand y-axis for labels a bit
          ylim(0, max(df_perf_food_combi()$RMSE) + 0.1) +
          ylab("RMSE") + 
          xlab(" ") +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 14, colour = "white"), 
            # facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 14, face = "bold"),
            # axis labels
            axis.text = element_text(size = 14, colour = "white"),
            axis.text.x = element_text(size = 14),
            axis.title.x = element_text(colour = "white", size = 16),
            axis.title.y = element_text(colour = "white", size = 16)
          )
      } else {
        ggplot(data = df_perf_food_combi() %>% arrange(RMSE)) +
          # generate bar chart
          geom_bar(aes(x = model, y = RMSE, fill = model), 
                   stat = "identity", width = 0.5) +
          # add labels
          geom_text(aes(x = model, y = RMSE, 
                        label = paste0(sprintf(RMSE, fmt = '%#.2f'), "\u20ac")),
                    vjust = -0.25, color = "white", size = 3) +
          # grid of plots: market on the left, food at the top
          facet_wrap(~ market, ncol = facets_error_metrics_all_plot) +
          # color bars according to model
          scale_fill_manual(values = color_models) +
          # expand y-axis for labels a bit
          ylim(0, max(df_perf_food_combi()$RMSE) + 0.1) +
          ylab("RMSE") + 
          xlab(" ") +
          theme(
            # black background of panel and plot
            panel.background = element_rect(fill = "black", color = "black"),
            plot.background = element_rect(fill = "black", color = "black"),
            # add white border around facets
            panel.border = element_rect(fill = NA, color = "white"), 
            # no grid
            panel.grid = element_blank(),
            # legend
            legend.background = element_rect(fill = "black", colour = "black"),
            legend.text = element_text(size = 14, colour = "white"), 
            # facet
            strip.background = element_rect(fill = "darkgray"), 
            strip.text = element_text(colour = 'white', size = 14, face = "bold"),
            # axis labels
            axis.text = element_text(size = 14, colour = "white"),
            axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(colour = "white", size = 16),
            axis.title.y = element_text(colour = "white", size = 16)
          )
      }
    }
    
  })
  
  # output
  #observeEvent(input$error_all_show, {
  # header
  output$error_metrics_all_header <- renderText({
    shiny::validate(need(!is.null(html_pred_header_error_metrics_all()), " "))
    print(html_pred_header_error_metrics_all())
  })
  
  # table
  output$error_metrics_all <- DT::renderDataTable({
    
    shiny::validate(need(!is.null(input$error_model),
                         "Please select at least one prediction model."))
    
    shiny::validate(need(!is.null(input$error_market),
                         "Please select at least one market."))
    
    
    df_perf_food_combi_2 <-
      df_perf_food_combi() %>%
      mutate(
        RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
        MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
        MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
      ) %>% 
      as.data.frame() %>%
      dplyr::rename(
        Market = market,
        Model = model
      ) 
    
    DT::datatable(
      data = df_perf_food_combi_2,
      rownames = FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}"),
        pageLength = 15,
        lengthMenu = c(10, 15, 20, 30),
        dom = c("ltip")
      )) %>%
      formatStyle(columns = names(df_perf_food_combi_2), color = "white", 
                  backgroundColor = "black", background = "black")
  })
  
  # plot
  output$error_metrics_all_plot <- renderPlot({
    
    # ensure that input is given
    shiny::validate(need(!is.null(input$error_model),
                         "Please select at least one prediction model."))
    
    shiny::validate(need(!is.null(input$error_market),
                         "Please select at least one market."))
    
    
    # plot output
    gg_error_metrics_all_plot()
  }, width = 1000, height = 750)
  #})
  
  
  #### performance output: best ####
  
  # update slider dependent on "Only Food" or "Food-Type-Size-Country"
  df_perf_best_slider <- reactive({
    # if only food is selected 23 entries can be made
    if (input$error_best_sel == "Only Fruit/Vegetable") {
      length(unique(df_descr$food)) * length(input$error_model) * length(input$error_market)
    } else {
      nrow(unique(df_descr[, c("food", "type", "size", "country")])) * 
        length(input$error_model) * length(input$error_market)
    } 
  })
  
  ## if any of those inputs changes update the slider
  observeEvent({
    input$error_best_sel
    input$error_market
    input$error_model}, {
      updateSliderInput(session, "error_best", 
                        "Select Number of Fruits/Vegetables Displayed:",
                        min = 1, max = df_perf_best_slider(), value = input$error_best, 
                        step = 1)
    })
  
  
  # create output data set
  df_perf_best <- eventReactive(input$error_best_show, {
    
    df_perf_food_best_aggregate <- data.frame()
    df_perf_food_best_market <- data.frame()
    
    
    # output depends on "Only Food" and "food-type-size-country" selection
    # the individual outputs again depend on "Aggregate" or "market selection"
    if (input$error_best_sel == "Only Fruit/Vegetable") {
      
      if ("Aggregate" %in% input$error_market) {
        df_perf_food_best_aggregate <- 
          df_perf %>%
          filter(model %in% input$error_model) %>%
          dplyr::select(model, food, RMSE, MAE, MAPE) %>%
          group_by(model, food) %>%
          dplyr::summarise(
            RMSE = round(mean(RMSE), 4), 
            MAE = round(mean(MAE), 4), 
            MAPE = round(mean(MAPE * 100), 4) 
          ) %>%
          mutate(market = "Aggregate") %>%
          # dplyr::arrange(!!! rlang::syms(input$error_best_metric)) %>% # syms() turns string into symbol
          # head(input$error_best) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE)
      } 
      
      if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
              input$error_market)) {
        df_perf_food_best_market <- 
          df_perf %>%
          filter(market %in% input$error_market, model %in% input$error_model) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE) %>%
          group_by(model, market, food) %>%
          dplyr::summarise(
            RMSE = round(mean(RMSE), 4), 
            MAE = round(mean(MAE), 4), 
            MAPE = round(mean(MAPE * 100), 4) 
          ) %>%
          # dplyr::arrange(!!! rlang::syms(input$error_best_metric)) %>% # syms() turns string into symbol
          # head(input$error_best) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE)
      }
      
    } else if (input$error_best_sel == "Food-Type-Size-Country") {
      
      if ("Aggregate" %in% input$error_market) {
        df_perf_food_best_aggregate <- 
          df_perf %>%
          filter(model %in% input$error_model) %>%
          # dplyr::arrange(!!! rlang::syms(input$error_best_metric)) %>%
          # head(input$error_best) %>%
          mutate(
            RMSE = round(RMSE, 4),
            MAE = round(MAE, 4),
            MAPE = round(MAPE * 100, 4),
            market = "Aggregate"
          ) %>%
          dplyr::select(model, market, food, type, size, country, RMSE, MAE, MAPE)
      } 
      
      if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
              input$error_market)) {
        df_perf_food_best_market <- 
          df_perf %>%
          filter(market %in% input$error_market, model %in% input$error_model) %>%
          # dplyr::arrange(!!! rlang::syms(input$error_best_metric)) %>%
          # head(input$error_best) %>%
          mutate(
            RMSE = round(RMSE, 4),
            MAE = round(MAE, 4),
            MAPE = round(MAPE * 100, 4) 
          ) %>%
          dplyr::select(model, market, food, type, size, country, RMSE, MAE, MAPE)
      }
    }
    
    rbind(df_perf_food_best_aggregate, df_perf_food_best_market) %>% 
      dplyr::arrange(!!! rlang::syms(input$error_best_metric)) %>%
      head(input$error_best)
  })
  
  # data frame for best
  df_perf_best_table <- eventReactive(input$error_best_show, {
    if (input$error_best_sel == "Food-Type-Size-Country") {
      df_perf_best_2 <-
        df_perf_best() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          `Fruit/Vegetable` = food,
          Market = market,
          Model = model,
          Type = type,
          Size = size,
          Country = country
        ) 
    } else {
      df_perf_best_2 <-
        df_perf_best() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          `Fruit/Vegetable` = food,
          Market = market,
          Model = model
        ) 
    }
    
    df_perf_best_2
    
  })
  
  observeEvent(input$error_best_show,{
    output$error_metrics_best <- DT::renderDataTable({
      
      shiny::validate(need(!is.null(input$error_model),
                           "Please select at least one prediction model."))
      
      shiny::validate(need(!is.null(input$error_market),
                           "Please select at least one market."))
      
      
      DT::datatable(
        data = df_perf_best_table(),
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          pageLength = 20,
          lengthMenu = c(5, 10, 20, 50),
          scrollX = '600px' # add horizontal scroll bar
          #dom = c("pli")
        )) %>%
        formatStyle(columns = names(df_perf_best_table()), color = "white", 
                    backgroundColor = "black", background = "black")
    })
  })
  
  
  
  #### performance output: worst ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # update slider dependent on "Only Food" or "Food-Type-Size-Country"
  df_perf_worst_slider <- reactive({
    # if only food is selected 23 entries can be made
    if (input$error_worst_sel == "Only Fruit/Vegetable") {
      length(unique(df_descr$food)) * length(input$error_model) * 
        length(input$error_market)
    } else {
      nrow(unique(df_descr[, c("food", "type", "size", "country")])) * 
        length(input$error_model) * length(input$error_market)
    } 
  })
  
  ## if any of those inputs changes update the slider
  observeEvent({
    input$error_worst_sel
    input$error_market
    input$error_model}, {
      updateSliderInput(session, "error_worst", 
                        "Select Number of Fruits/Vegetables Displayed:",
                        min = 1, max = df_perf_worst_slider(), value = input$error_worst, 
                        step = 1)
    })
  
  
  # create data set
  df_perf_worst <- eventReactive(input$error_worst_show, {
    
    df_perf_food_worst_aggregate <- data.frame()
    df_perf_food_worst_market <- data.frame()
    
    # output depends on "only food" and "food-type-size-country" selection
    # the individual outputs again depend on "Aggregate" or "market selection"
    if (input$error_worst_sel == "Only Fruit/Vegetable") {
      if ("Aggregate" %in% input$error_market) {
        df_perf_food_worst_aggregate <- 
          df_perf %>%
          filter(model %in% input$error_model) %>%
          dplyr::select(model, food, RMSE, MAE, MAPE) %>%
          group_by(model, food) %>%
          dplyr::summarise(
            RMSE = round(mean(RMSE), 4), 
            MAE = round(mean(MAE), 4), 
            MAPE = round(mean(MAPE), 4) * 100
          ) %>%
          mutate(market = "Aggregate") %>%
          # dplyr::arrange(desc(!!! rlang::syms(input$error_worst_metric))) %>% # syms() turns string into symbol
          # head(input$error_worst) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE)
      } 
      
      if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
              input$error_market)) {
        df_perf_food_worst_market <- 
          df_perf %>%
          filter(market %in% input$error_market, model %in% input$error_model) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE) %>%
          group_by(model, market, food) %>%
          dplyr::summarise(
            RMSE = round(mean(RMSE), 4), 
            MAE = round(mean(MAE), 4), 
            MAPE = round(mean(MAPE), 4) 
          ) %>%
          # dplyr::arrange(desc(!!! rlang::syms(input$error_worst_metric))) %>% # syms() turns string into symbol
          # head(input$error_worst) %>%
          dplyr::select(model, market, food, RMSE, MAE, MAPE)
      }
    } else if (input$error_worst_sel == "Food-Type-Size-Country") {
      if ("Aggregate" %in% input$error_market) {
        df_perf_food_worst_aggregate <- 
          df_perf %>%
          filter(model %in% input$error_model) %>%
          # dplyr::arrange(desc(!!! rlang::syms(input$error_worst_metric))) %>%
          # head(input$error_worst) %>%
          mutate(
            RMSE = round(RMSE, 4),
            MAE = round(MAE, 4),
            MAPE = round(MAPE, 4) * 100,
            market = "Aggregate",
          ) %>%
          dplyr::select(model, market, food, type, size, country, RMSE, MAE, MAPE)
      } 
      
      if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
              input$error_market)) {
        df_perf_food_worst_market <- 
          df_perf %>%
          filter(market %in% input$error_market, model %in% input$error_model) %>%
          # dplyr::arrange(desc(!!! rlang::syms(input$error_worst_metric))) %>%
          # head(input$error_worst) %>%
          mutate(
            RMSE = round(RMSE, 4),
            MAE = round(MAE, 4),
            MAPE = round(MAPE, 4) * 100
          ) %>%
          dplyr::select(model, market, food, type, size, country, RMSE, MAE, MAPE)
      }
      
    }
    
    rbind(df_perf_food_worst_aggregate, df_perf_food_worst_market) %>%
      dplyr::arrange(desc(!!! rlang::syms(input$error_worst_metric))) %>%
      head(input$error_worst)
  })
  
  # create output for table (same procedur as above)
  df_perf_worst_table <- eventReactive(input$error_worst_show, {
    if (input$error_worst_sel == "Food-Type-Size-Country") {
      df_perf_worst_2 <-
        df_perf_worst() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          `Fruit/Vegetable` = food,
          Market = market,
          Model = model,
          Type = type,
          Size = size,
          Country = country
        ) 
    } else {
      df_perf_worst_2 <-
        df_perf_worst() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          `Fruit/Vegetable` = food,
          Market = market,
          Model = model
        )
    }
    
    df_perf_worst_2
  })
  
  # return table if action button is clicked
  observeEvent(input$error_worst_show,{
    output$error_metrics_worst <-  DT::renderDataTable({
      # ensure that model and market is chosen
      shiny::validate(need(!is.null(input$error_model),
                           "Please select at least one prediction model."))
      
      shiny::validate(need(!is.null(input$error_market),
                           "Please select at least one market."))
      
      # format data table as before
      DT::datatable(
        data = df_perf_worst_table(),
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          pageLength = 20,
          lengthMenu = c(5, 10, 20, 50),
          scrollX = '600px' # add horizontal scroll bar
          #dom = c("pli")
        )) %>%
        formatStyle(columns = names(df_perf_worst_table()), color = "white", 
                    backgroundColor = "black", background = "black")
      
      
    })
  })
  
  
  
  
  #### performance output: overall error metrics ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # create data based on selection
  ## food model
  df_perf_overall_table <- eventReactive(input$error_overall_show, {
    
    df_perf_overall_aggregate <- data.frame()
    df_perf_overall_market <- data.frame()
    
    # aggregation
    if ("Aggregate" %in% input$error_market) {
      df_perf_overall_aggregate <- 
        df_perf_overall %>%
        filter(model %in% input$error_model) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4)
        ) %>%
        mutate(market = "Aggregate") %>%
        arrange(RMSE) %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) %>%
        as.data.frame() 
      
    } 
    
    # no aggregation: error metrics for each market
    if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
            input$error_market)) {
      df_perf_overall_market <- 
        df_perf_overall %>%
        filter(market %in% input$error_market, model %in% input$error_model) %>%
        mutate(
          RMSE = round(RMSE, 4),
          MAE = round(MAE, 4),
          MAPE = round(MAPE, 4)
        ) %>%
        arrange(RMSE) %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) %>%
        as.data.frame() 
    }
    
    
    rbind(df_perf_overall_aggregate, df_perf_overall_market)
  })
  
  
  
  ## full model
  df_perf_overall_table_comp <- eventReactive(input$error_overall_show, {
    
    df_perf_overall_full_aggregate <- data.frame()
    df_perf_overall_full_market <- data.frame()
    
    
    # aggregation
    if ("Aggregate" %in% input$error_market) {
      df_perf_overall_full_aggregate <- 
        df_perf_overall_comb %>%
        filter(model %in% input$error_model) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 4),
          MAE = round(mean(MAE), 4),
          MAPE = round(mean(MAPE), 4)
        ) %>%
        mutate(market = "Aggregate") %>%
        arrange(RMSE) %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) %>%
        as.data.frame() 
      
    } 
    
    # no aggregation: error metrics for each market
    if (any(c("Frankfurt", "Hamburg", "Berlin", "Cologne", "Munich") %in% 
            input$error_market)) {
      
      df_perf_overall_full_market <- 
        df_perf_overall_comb %>%
        filter(market %in% input$error_market, model %in% input$error_model) %>%
        mutate(
          RMSE = round(RMSE, 4),
          MAE = round(MAE, 4),
          MAPE = round(MAPE, 4)
        ) %>%
        arrange(RMSE) %>%
        dplyr::select(model, market, RMSE, MAE, MAPE) %>%
        as.data.frame() 
      
    }
    
    rbind(df_perf_overall_full_aggregate, df_perf_overall_full_market)
  })
  
  
  # table output
  observeEvent(input$error_overall_show,{
    output$error_metrics_overall <-  DT::renderDataTable({
      
      shiny::validate(need(!is.null(input$error_model),
                           "Please select at least one prediction model."))
      
      shiny::validate(need(!is.null(input$error_market),
                           "Please select at least one market."))
      
      # create nice data table
      df_perf_overall_table_2 <-
        df_perf_overall_table() %>%
        mutate(
          RMSE = paste0(sprintf(RMSE, fmt = '%#.4f'), " \u20ac"), 
          MAE = paste0(sprintf(MAE, fmt = '%#.4f'), " \u20ac"),
          MAPE = paste0(sprintf(MAPE * 100, fmt = '%#.2f'), " %")
        ) %>%
        dplyr::rename(
          Market = market,
          Model = model
        ) 
      
      #dplyr::select(model, market, RMSE, MAE, MAPE) %>%
      DT::datatable(
        data = df_perf_overall_table_2,
        rownames = FALSE,
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"),
          pageLength = 20,
          lengthMenu = c(5, 10, 20, 50)#,
          #dom = c("pli")
        )) %>%
        formatStyle(columns = names(df_perf_overall_table_2), color = "white", 
                    backgroundColor = "black", background = "black")
    })
  })
  
  
  # plot for comparison to overall model
  observeEvent(input$error_overall_show, {
    
    if (!is.null(input$error_model) & !is.null(input$error_market)) {
      df_perf_overall_table_final <- df_perf_overall_table() %>%
        mutate(
          RMSE = round(RMSE, 4),
          MAE = round(MAE, 4),
          MAPE = round(MAPE, 4)
        )
    } else {
      df_perf_overall_table_final <- df_perf_overall_table() 
    }
    

    if (!is.null(input$error_model) & !is.null(input$error_market) & 
        !c("BSTS", "BSTS Baseline", 
           "Naive Mean Prediction", "Naive Median Prediction") %in% input$error_model) {
      df_perf_overall_table_final_comp <- df_perf_overall_table_comp() %>%
        mutate(
          RMSE = round(RMSE, 4),
          MAE = round(MAE, 4),
          MAPE = round(MAPE, 4)
        )
    } else {
      df_perf_overall_table_final_comp <- df_perf_overall_table_comp() 
    }
    

    
    # if only one market is selected the plot differs from the fact where
    # multiple markets are selected
    if (length(input$error_market) == 1) { # & "Aggregate" %in% input$error_market) {
      
      output$error_metrics_overall_plot_comp_1 <- renderPlotly({
        
        # ensure that model and market is given
        shiny::validate(need(!is.null(input$error_model),
                             "Please select at least one prediction model."))
        shiny::validate(need(!is.null(input$error_market),
                             "Please select at least one market."))
        
        
        df_perf_overall_table_final %>%
          mutate(
            RMSE = round(RMSE, 2),
            MAE = round(MAE, 2),
            MAPE = round(MAPE, 2)
          ) %>%
          pivot_longer(!c(model, market), names_to = "metric", values_to = "value") %>%
          mutate(
            value_2 = if_else(
              metric == "MAPE", paste0(sprintf(value*100, fmt = '%#.2f'), "%"), 
              paste0(sprintf(value, fmt = '%#.2f'),  " \u20ac")
            )
          ) %>%
          plot_ly(x = ~ model) %>%
          add_trace(type = 'bar', y = ~ value, name = ~ metric,
                    text = ~ value_2,  textposition = 'outside',
                    visible = TRUE) %>%
          # add layout
          plotly::layout(
            # title
            title = 'Food Model', 
            # axis labels
            xaxis = list(title = "algorithm"), 
            yaxis = list(title = "error metric value"), 
            # black background
            plot_bgcolor = "black",
            paper_bgcolor = "black",
            # white font color
            font = list(color = 'white')
          ) %>% 
          # no hover information
          style(hoverinfo = 'none')
        
      })
      
      output$error_metrics_overall_plot_comp_2 <- renderPlotly({
        
        # ensure that a model and market is selected
        shiny::validate(need(!is.null(input$error_model),
                             " "))
        shiny::validate(need(!is.null(input$error_market),
                             " "))
        
        # full models only exist for machine learning models
        # if BSTS models or naive predictions are selected remember user 
        # that for those no full model exists
        shiny::validate(need(!"BSTS" %in% input$error_model,
                             "Please remove the BSTS model. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"BSTS Baseline" %in% input$error_model,
                             "Please remove the BSTS Baseline model. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"Naive Mean Prediction" %in% input$error_model,
                             "Please remove the Naive Mean Prediction. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"Naive Median Prediction" %in% input$error_model,
                             "Please remove the Naive Median Prediction. The full model only exists for the machine learning models."))
        
        
        df_perf_overall_table_final_comp %>%
          mutate(
            RMSE = round(RMSE, 2),
            MAE = round(MAE, 2),
            MAPE = round(MAPE, 2)
          ) %>%
          pivot_longer(!c(model, market), names_to = "metric", values_to = "value") %>%
          mutate(
            value_2 = if_else(
              metric == "MAPE", paste0(sprintf(value*100, fmt = '%#.2f'), "%"), 
              paste0(sprintf(value, fmt = '%#.2f'), " \u20ac")
            )
          ) %>%
          plot_ly(x = ~ model) %>%
          add_trace(type = 'bar', y = ~ value, name = ~ metric,
                    text = ~ value_2,  textposition = 'outside',
                    visible = TRUE) %>%
          # add layout
          plotly::layout(
            # title
            title = 'Full Model', 
            # axis labels
            xaxis = list(title = "algorithm"), 
            yaxis = list(title = "error metric value"), 
            # black background
            plot_bgcolor = "black",
            paper_bgcolor = "black",
            # white font color
            font = list(color = 'white')
          ) %>% 
          # no hover information
          style(hoverinfo = 'none')
      })
    } else {
      output$error_metrics_overall_plot_comp_1 <- renderPlotly({
        
        shiny::validate(need(!is.null(input$error_model),
                             "Please select at least one prediction model."))
        shiny::validate(need(!is.null(input$error_market),
                             "Please select at least one market."))
        
        
        num_markets <- length(input$error_market) # number of markets
        
        df_perf_overall_table_final %>%
          mutate(MAPE = MAPE * 100) %>%
          plot_ly(x = ~ model, color = ~ market) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ RMSE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "RMSE :", sprintf(RMSE, fmt = '%#.4f'), '\u20ac'),  
                    visible = TRUE) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ MAE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "MAE :", sprintf(MAE, fmt = '%#.4f'), '\u20ac'),  
                    visible = FALSE) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ MAPE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "MAPE: ", sprintf(MAPE, fmt = '%#.2f'), '%'), 
                    
                    visible = FALSE) %>%
          plotly::layout(
            # title
            title = 'Food Model', 
            # axis labels and limits
            xaxis = list(title = "algorithm"), 
            yaxis = list(title = "error metric value"#, range = c(0, max(df_perf_overall_table()$RMSE) + 0.1)
            ), 
            # black background
            plot_bgcolor = "black",
            paper_bgcolor = "black",
            # white font color
            font = list(color = 'white'),
            # position of button: top and centered
            xanchor = 'center',
            yanchor = "top",
            pad = list('r' = 0, 't' = 10, 'b' = 10),
            x = 0.5,
            y = 1.17,
            # restyle button
            updatemenus = list(
              list(
                type = "list",
                label = 'Category',
                buttons = list(
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(TRUE, num_markets), 
                                     rep(FALSE, num_markets), 
                                     rep(FALSE, num_markets))),
                       label = "RMSE"),
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(FALSE, num_markets), 
                                     rep(TRUE, num_markets), 
                                     rep(FALSE, num_markets))),
                       label = "MAE"),
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(FALSE, num_markets), 
                                     rep(FALSE, num_markets), 
                                     rep(TRUE, num_markets))),
                       label = "MAPE")
                )
              )
            )
          ) %>%
          # no hover information
          style(hoverinfo = 'text')
      })
      
      
      output$error_metrics_overall_plot_comp_2 <- renderPlotly({
        
        
        # ensure that model and market are selected
        shiny::validate(need(!is.null(input$error_model),
                             " "))
        shiny::validate(need(!is.null(input$error_market),
                             " "))
        
        
        # full mdoels only exist for machine learning models
        # if BSTS models or naive predictions are selected remember user 
        # that for those no full model exists
        shiny::validate(need(!"BSTS" %in% input$error_model,
                             "Please remove the BSTS model. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"BSTS Baseline" %in% input$error_model,
                             "Please remove the BSTS Baseline model. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"Naive Mean Prediction" %in% input$error_model,
                             "Please remove the Naive Mean Prediction. The full model only exists for the machine learning models."))
        
        shiny::validate(need(!"Naive Median Prediction" %in% input$error_model,
                             "Please remove the Naive Median Prediction. The full model only exists for the machine learning models."))
        
        
        num_markets <- length(input$error_market) # number of markets
        
        df_perf_overall_table_final_comp %>%
          mutate(MAPE = MAPE * 100) %>%
          plot_ly(x = ~ model, color = ~ market) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ RMSE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "RMSE :", sprintf(RMSE, fmt = '%#.4f'), '\u20ac'),  
                    visible = TRUE) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ MAE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "MAE :", sprintf(MAE, fmt = '%#.4f'),  '\u20ac'),  
                    visible = FALSE) %>%
          add_trace(type = 'bar', name = ~ market, y = ~ MAPE, 
                    text = ~ paste0("Market: ", input$error_market, "<br>", 
                                    "MAPE: ", sprintf(MAPE, fmt = '%#.2f'), '%'), 
                    visible = FALSE) %>%
          plotly::layout(
            # title of plot
            title = 'Full Model', 
            # axis labels and limits
            xaxis = list(title = "algorithm"), 
            yaxis = list(title = "error metric value"#, range = c(0, max(df_perf_overall_table()$RMSE) + 0.1)
            ), 
            # black background
            plot_bgcolor = "black",
            paper_bgcolor = "black",
            # white font color
            font = list(color = 'white'),
            # position of button: top and centered
            xanchor = 'center',
            yanchor = "top",
            pad = list('r' = 0, 't' = 10, 'b' = 10),
            x = 0.5,
            y = 1.17,
            # restyle button
            updatemenus = list(
              list(
                type = "list",
                label = 'Category',
                buttons = list(
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(TRUE, num_markets), 
                                     rep(FALSE, num_markets), 
                                     rep(FALSE, num_markets))),
                       label = "RMSE"),
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(FALSE, num_markets), 
                                     rep(TRUE, num_markets), 
                                     rep(FALSE, num_markets))),
                       label = "MAE"),
                  list(method = "restyle",
                       args = list('visible', 
                                   c(rep(FALSE, num_markets), 
                                     rep(FALSE, num_markets), 
                                     rep(TRUE, num_markets))),
                       label = "MAPE")
                )
              )
            )
          ) %>%
          # no hover information
          style(hoverinfo = 'text')
      })
    }
  })
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Feature Importance ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # info on computation time of permutation feature importance
  onclick("imp_show_time", 
          runjs("window.open('Theory_Feature_Importance_Computation_Time.html')"))
  
  ## LASSO ##
  #%%%%%%%%%#
  
  
  # table output generation 
  feature_importance_lasso <- eventReactive(input$imp_show_lasso, {
    
    if (input$imp_type_lasso == "Zero Coefficients") {
      # use function to get coefficients which are set to 0 in LASSO model
      table_imp_lasso <- 
        func_lasso_importance(input$imp_food_lasso, input$imp_market_lasso, 
                              input$imp_features_lasso, input$imp_type_lasso)
      
      # delete columns with only NA's
      table_imp_lasso[, colSums(is.na(table_imp_lasso)) != nrow(table_imp_lasso)]
    } else {
      func_lasso_importance(input$imp_food_lasso, input$imp_market_lasso, 
                            input$imp_features_lasso, input$imp_type_lasso)
    }
  })
  
  
  
  # table output: zero coefficients
  output$imp_lasso_table <- DT::renderDataTable({
    return()
  })
  
  
  observeEvent(input$imp_show_lasso, {
    output$imp_lasso_table <- DT::renderDataTable({
      if (input$imp_type_lasso == "Zero Coefficients") {
        
        shiny::validate(need("data.frame" %in% class(feature_importance_lasso()), " "))
        
        DT::datatable(
          data = feature_importance_lasso() %>% data.frame(),
          rownames = FALSE,
          options = list(
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'color': '#fff'});",
              "}"),
            dom = 'ltip', 
            pageLength = 10, lengthMenu = c(10, 20, 30),
            scrollX = TRUE
          )) %>%
          formatStyle(columns = names(feature_importance_lasso()), color = "white", 
                      backgroundColor = "black", background = "black")
      } else {
        shiny::validate(need("data.frame" %in% class(feature_importance_lasso()), " "))
      }
    })
  })
  
  
  
  # plot output: feature importance
  output$imp_lasso_plot <- renderPlot({
    # this is just a work around to...
    ## ...remove the warning if output of function is data frame and ...
    shiny::validate(need("gg" %in% class(feature_importance_lasso()), " "))
    ggplot() +  theme(
      ## ...remove the empty white box :D
      panel.background = element_rect(fill = "black", color = "black"),
      plot.background = element_rect(fill = "black", color = "black")
    )
  }, width = 1000, height = 700)
  
  
  
  observeEvent(input$imp_show_lasso, {
    output$imp_lasso_plot <- renderPlot({
      if (input$imp_type_lasso == "Non-Zero Coefficients") {
        shiny::validate(need("gg" %in% class(feature_importance_lasso()), " "))
        
        feature_importance_lasso()
      } else {
        # this is just a work around to...
        ## ...remove the warning if output of function is data frame and ...
        shiny::validate(need("gg" %in% class(feature_importance_lasso()), " "))
        ggplot() +  theme(
          ## ...remove the empty white box :D
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black")
        )
      }
    }, width = 1000, height = 700)
  })
  
  
  
  
  
  ## XGBoost ##
  #%%%%%%%%%%%#
  
  # create reactive output
  feature_importance_plot_xgb <- eventReactive(input$imp_show_xgb, {
    func_xgb_importance(input$imp_food_xgb, input$imp_market_xgb, 
                        input$imp_features_xgb, input$imp_type_xgb)
  })
  
  # render plot
  output$imp_plot_xgb <- renderPlot({
    ggplot() +  theme(
      ## ...remove the empty white box :D
      panel.background = element_rect(fill = "black", color = "black"),
      plot.background = element_rect(fill = "black", color = "black")
    )
  }) 
  
  observeEvent(input$imp_show_xgb, {
    output$imp_plot_xgb <- renderPlot({
      feature_importance_plot_xgb()
    }, width = 1000, height = 700)
  })
  
  
  
  
  #### Food Basket ####
  #%%%%%%%%%%%%%%%%%%%#
  
  
  onclick("basket_infos", runjs("window.open('Theory_Footprint_Nutrition_Basket.html')"))
  
  # update select input based on selection
  
  observeEvent(input$nutrient_1, {
    
    choice_2 <- unique(df_footprint_nutrients$nutrient)[!(unique(df_footprint_nutrients$nutrient) %in% input$nutrient_1)]
    
    updateSelectInput(session, "nutrient_2",
                      choices = sort(choice_2))
    
    
  })
  
  observeEvent(c(input$nutrient_1, input$nutrient_2), {
    
    choice_3 <- unique(df_footprint_nutrients$nutrient)[!(unique(df_footprint_nutrients$nutrient) %in% 
                                                            c(input$nutrient_1, input$nutrient_2))]
    
    updateSelectInput(session, "nutrient_3",
                      choices = sort(choice_3))
    
    
  })
  
  
  ## Create data frame for outputs 
  
  observeEvent(input$nutrient_1, {
    
    updateRadioButtons(session, "nutrition_1", 
                       label = input$nutrient_1)
    
  })
  
  observeEvent(input$nutrient_2, {
    
    updateRadioButtons(session, "nutrition_2", 
                       label = input$nutrient_2)
    
  })
  
  observeEvent(input$nutrient_3, {
    
    updateRadioButtons(session, "nutrition_3", 
                       label = input$nutrient_3)
    
  })
  
  radio_nutrition <- reactive({
    
    # differentiating between good and bad nutrients
    if(input$nutrient_1 %in% c("Total Lipid", "Sugars", "Cholesterol")){
      
      if(input$nutrition_1 == "Low"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level == "Low") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_1 == "Medium"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level %in% c("Low", "Medium")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_1 == "High"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    } else {
      
      if(input$nutrition_1 == "Low"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_1 == "Medium"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level %in% c("Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_1 == "High"){
        
        nut_1 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_1 & nutrient_level == "High") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    }
    
    # same for nutrient 2 
    
    if(input$nutrient_2 %in% c("Total Lipid", "Sugars", "Cholesterol")){
      
      if(input$nutrition_2 == "Low"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level == "Low") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_2 == "Medium"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level %in% c("Low", "Medium")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_2 == "High"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    } else {
      
      if(input$nutrition_2 == "Low"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_2 == "Medium"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level %in% c("Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_2 == "High"){
        
        nut_2 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_2 & nutrient_level == "High") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    }
    
    # same for nutrient 3 
    
    if(input$nutrient_3 %in% c("Total Lipid", "Sugars", "Cholesterol")){
      
      if(input$nutrition_3 == "Low"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level == "Low") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_3 == "Medium"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level %in% c("Low", "Medium")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_3 == "High"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    } else {
      
      if(input$nutrition_3 == "Low"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level %in% c("Low", "Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_3 == "Medium"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level %in% c("Medium", "High")) %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      } else if(input$nutrition_3 == "High"){
        
        nut_3 <- df_footprint_nutrients  %>%
          filter(nutrient == input$nutrient_3 & nutrient_level == "High") %>% 
          dplyr::select(food) %>% unique() %>% pull()
        
      }
      
    }
    
    nut_1_2 <- nut_1[nut_1 %in% nut_2]
    
    nut_1_2[nut_1_2 %in% nut_3]
    
  })
  
  radio_footprint <- reactive({
    
    if(input$carbon_slide == "High"){
      
      carb <- df_footprint_nutrients %>%
        filter(`Carbon Footprint Level` %in% c("Low", "Medium", "High")) %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    } else if(input$carbon_slide == "Medium"){
      
      carb <- df_footprint_nutrients %>%
        filter(`Carbon Footprint Level` %in% c("Low", "Medium")) %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    } else if(input$carbon_slide == "Low"){
      
      
      carb <- df_footprint_nutrients %>%
        filter(`Carbon Footprint Level` == "Low") %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    }
    
    
    if(input$water_slide == "High"){
      
      water <- df_footprint_nutrients %>%
        filter(`Water Footprint Level` %in% c("Low", "Medium", "High")) %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    } else if(input$water_slide == "Medium"){
      
      water <- df_footprint_nutrients %>%
        filter(`Water Footprint Level` %in% c("Low", "Medium")) %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    } else if(input$water_slide == "Low"){
      
      
      water <- df_footprint_nutrients %>%
        filter(`Water Footprint Level` == "Low") %>%
        dplyr::select(food) %>% unique() %>% pull()
      
    }
    
    carb[carb %in% water]
    
  })
  
  basket_price <- reactive({
    
    
    df_price_basket %>% dplyr::filter(market == input$market_basket, `predicted price` <= input$price_basket) %>% 
      dplyr::select(food) %>% unique() %>%
      pull()
    
  })
  
  # obtain final choice items
  
  #observeEvent(c(input$food_rem, input$price_basket, input$ref_basket), {
  observeEvent(c(input$ref_basket), {
    
    # filter nutrition values
    
    if(input$radio_basket == "Nutrition"){
      
      food_filtered <- radio_nutrition()
      sub_food <- food_filtered[!(food_filtered %in% input$food_rem)]
      
    }
    
    # else 
    if(input$radio_basket == "Footprint"){           
      
      food_filtered <- radio_footprint()
      sub_food <- food_filtered[!(food_filtered %in% input$food_rem)]
      
    }
    
    
    # filter only removed items
    else if(input$radio_basket == "None"){
      
      food_filtered <- unique(df_footprint_nutrients$food)[!(unique(df_footprint_nutrients$food) %in% input$food_rem)]
      sub_food <- food_filtered[!(food_filtered %in% input$food_rem)]
      
    }
    
    
    ## add price filter on all options
    sub_food <- sub_food[sub_food %in% basket_price()]
    
    
    # final choice update based on all options
    
    updateSelectInput(session, "final_choice",
                      choices = sort(sub_food))
    
    output$final_empty <- renderText({
      
      if(length(sub_food) == 0){
        
        "No Item for your Selection. Please adjust the filter system."
        
      }
      
    })
    
    
  })
  
  ## header
  observeEvent(input$shop_button, {
    
    output$nutr_rec_header <- renderText({
      if(!is.null(input$age_basket) & !is.null(input$gender_basket) & !is.null(input$pal_basket)){
        
        paste("Age Group:", input$age_basket, ",", "Gender:", 
              input$gender_basket, ",", "PAL Level:", input$pal_basket)
        
      }
      
    })
    
  })
  
  ## nutrition recommendation
  
  combo_reactive <- eventReactive(input$nutr_button, {
    
    shiny::validate(
      need(!is.null(input$age_basket), "Please choose an age group."),
      need(!is.null(input$gender_basket), "Please choose a gender."),
      need(!is.null(input$pal_basket), "Please choose a PAL level."),
      need(!is.null(input$final_choice), "Please select at least one item.")
    )
    
    
    num_items <- length(input$final_choice)
    items <- input$final_choice
    
    vals <- numeric(0)
    food <- character(0)
    
    for(j in 1:num_items){
      
      vals <- rbind(vals, input[[paste0("num_", items[j])]])
      food <- rbind(food, items[j])
      
    }
    
    amounts <- data.frame(Amount = vals, food = food)
    amounts[amounts$food %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), "Amount"] <- 
      amounts[amounts$food %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), "Amount"]*100
    
    
    
    shopping <- df_price_basket %>%
      dplyr::filter(food %in% items, market == input$market_basket, `predicted price` <= input$price_basket) %>%
      dplyr::group_by(food) %>%
      dplyr::arrange(`predicted price`) %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::select(-c(model, size)) %>%
      dplyr::arrange(food) %>%
      dplyr::mutate(`Price Prediction` = round(`predicted price`/10, 2)) %>%
      dplyr::select(-`predicted price`) %>% 
      dplyr::ungroup()
    
    shopping <- dplyr::inner_join(shopping, amounts, by = "food")
    
    
    sum_basket <- sum(shopping$`Price Prediction`*shopping$Amount/100)
    
    shopping <- shopping %>%
      dplyr::mutate(`Price Prediction` = `Price Prediction`*Amount/100) %>% 
      dplyr::mutate(`Price Prediction` = paste(`Price Prediction`, "€")) %>% 
      dplyr::mutate(Amount = ifelse(food %in% c("Cauliflower", "Endive", "Iceberg Lettuce"), 
                                    ifelse(Amount == 100, "1 Piece", paste(str_sub(Amount, 1, 1), "Pieces")),
                                    paste(Amount, "g")))
    
    
    number_shopping <- nrow(shopping)
    
    colnames(shopping) <- str_to_title(colnames(shopping))
    
    shopping <- shopping %>%
      kbl("html", align = "r", escape = F, caption = "<center><strong>Final Basket - Recommendation</strong></center>") %>%
      kable_styling("striped", position = "right",
                    html_font = "arial") %>%
      kable_classic() %>%
      row_spec(seq(1, number_shopping, 2), color = "white", background = "#51565c") %>%
      footnote(general = paste(sum_basket, "€"),
               general_title = "Total Price:",
               footnote_as_chunk = TRUE, title_format = "italic")
    
    
    #### nutrition
    
    n_nutrients <- length(unique(df_footprint_nutrients$nutrient))
    
    df_footprint_nutrients$nutrient <- cell_spec(df_footprint_nutrients$nutrient,
                                                 color = ifelse(df_footprint_nutrients$nutrient %in%  c("Fat", "Sugars"),
                                                                "red", "white"))
    
    
    # nutrition recommendation based on selection
    if(input$nutr_day_week == "Daily"){
      
      if(input$age_basket == "19 - 24" & input$gender_basket == "Female" & 
         input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
        
        
      } else if(input$age_basket == "19 - 24" & input$gender_basket == "Female" & 
                input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
      } else if(input$age_basket == "25 - 50" & input$gender_basket == "Female" & 
                input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      } else if(input$age_basket == "25 - 50" & input$gender_basket == "Female" & 
                input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Female" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Female" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
        
      }else if(input$age_basket == "19 - 24" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "19 - 24" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "25 - 50" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
        
      }else if(input$age_basket == "25 - 50" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Daily Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Daily Intake` = `Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Daily Intake`/`Recommended Daily Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Daily Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
      }
    } # close if daily 
    
    
    
    ## space to better find Weekly 
    
    else if(input$nutr_day_week == "Weekly"){
      
      
      if(input$age_basket == "19 - 24" & input$gender_basket == "Female" & 
         input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
        
        
      } else if(input$age_basket == "19 - 24" & input$gender_basket == "Female" & 
                input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
      } else if(input$age_basket == "25 - 50" & input$gender_basket == "Female" & 
                input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      } else if(input$age_basket == "25 - 50" & input$gender_basket == "Female" & 
                input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Female" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.6_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.6_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Female" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_w_1.8_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_w_1.8_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
        
      }else if(input$age_basket == "19 - 24" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "19 - 24" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_19) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_19) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
        
      }else if(input$age_basket == "25 - 50" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic")  
        
        
        
      }else if(input$age_basket == "25 - 50" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_25) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_25) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Male" & 
               input$pal_basket == "Sedentary or Light Activity Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.6_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.6_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
        
      }else if(input$age_basket == "> 51" & input$gender_basket == "Male" & 
               input$pal_basket == "Active or Moderately Active Lifestyle"){
        
        recommended_nutrition <- df_footprint_nutrients %>% 
          dplyr::select(food, nutrient, nutrient_unit, nutrient_value, 
                        recommendation_m_1.8_51) %>%
          dplyr::rename(Nutrient = nutrient, Unit = nutrient_unit, 
                        `Recommended Daily Intake` = recommendation_m_1.8_51) %>%
          dplyr::filter(food %in% input$final_choice) 
        
        recommended_nutrition <- dplyr::inner_join(recommended_nutrition, amounts, by = "food") %>% 
          dplyr::select(-food) %>% 
          dplyr::group_by(Nutrient, Unit) %>% 
          dplyr::summarise(`Actual Weekly Intake` = sum(nutrient_value*Amount/100), 
                           `Recommended Weekly Intake` = 7*`Recommended Daily Intake`, 
                           `Percentage of Recommendation` = paste0(100*round(`Actual Weekly Intake`/`Recommended Weekly Intake`, 2),
                                                                   "%")) %>%
          unique() %>% 
          kbl(align = "r", "html", escape = F, 
              caption = "<center><strong>Weekly Nutrition - Actual vs. Recommendation</strong></center>") %>%
          kable_styling("striped", position = "right",
                        html_font = "arial") %>%
          kable_classic() %>%
          row_spec(seq(1, n_nutrients, 2), color = "white", background = "#51565c") %>%
          
          
          ## add scroll box with space between table and scroller
          scroll_box(height = "350px", fixed_thead =  list(enabled = TRUE, background = "black"),
                     extra_css = "border: 0px; padding: 12px; border-width: 0px;") %>%
          ## add note 
          footnote(general = "The recommended intake of Energy, Carbohydrate, Total Lipid and Protein 
refers to the proportion solely coming from fruits and vegetables.", 
                   general_title = "Note:", 
                   footnote_as_chunk = TRUE, title_format = "italic") 
      }
    } # close else if
    
    
    # combine reactive outputs!
    combo <- list(shopping = shopping, recommended_nutrition = recommended_nutrition)
    combo
    
  })
  
  observeEvent(input$nutr_button, {
    
    output$shopping_list <- renderText({
      
      nutr_out <- combo_reactive()
      
      nutr_out$shopping
      
    })
    
    output$nutrition_rec <- renderText({
      
      nutr_out <- combo_reactive()
      
      nutr_out$recommended_nutrition
      
    })
    
  })
    ## Empty Output after refresh
  
  
  observeEvent(input$ref_basket, {

    
    output$shopping_list <- renderText({
      
      
    })
    
    output$nutrition_rec <- renderText({
      
      
    })
    
  })


  ## Final Barplots for nutrition and footprint
  
  output$foot_bar_basket <- renderPlotly({
    
    shiny::validate(
      need(!is.null(input$final_choice), "Please select at least one final item in the previous tab.")
    )
    
    # colours 
    col_pal <- c("#FFFF00", "#56B4E9")
    
    data_foot_bar <- df_footprint_nutrients %>%
      dplyr::filter(food %in% input$final_choice) %>%
      dplyr::select(food, `Carbon Footprint`, `Water Footprint`) %>%
      pivot_longer(cols = ends_with("Footprint"), names_to = "Footprint", values_to = "Value") %>% unique()
    
    data_foot_bar$footprint_unit <- ifelse(data_foot_bar$Footprint == "Carbon Footprint", "kg CO2 equivalent per kg", 
                                           "litres of water per kg")
    
    
    foot_bar <-  ggplot(data_foot_bar, aes(x = food, y = Value, 
                                         text = paste0("Footprint: ", Footprint, '<br>', 
                                                       "Amount: ", round(Value, 2)))) +
      geom_bar(width = 0.5, stat = "identity", aes(fill = Footprint)) +
      
      # change color of boxes
      scale_fill_manual(values = col_pal) +
      labs(x = "Items", fill = "Footprints") +
      facet_wrap(Footprint ~ footprint_unit, ncol = 2, scales = "free_y",
                 strip.position = "top") +
      ylab(NULL) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "white"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"),
        # no grid
        panel.grid.major = element_blank(),
        # legend
        legend.background = element_rect(fill = "black", colour = "black"),
        legend.text = element_text(size = 10, colour = "white"),
        legend.title = element_text(size = 10, colour = "white"),
        # change color of facet
        strip.background = element_blank(),
        strip.text = element_text(colour = 'white'),
        # axis labels
        axis.text = element_text(size = 10, colour = "white"),
        # axis color
        axis.text.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white", angle = 90),
        axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 8, r = 0, b = 0, l = 0)),
        # title colour
        plot.title = element_text(color = "white"),
        panel.spacing = unit(3, "lines"))
    
    ggplotly(foot_bar, tooltip = "text") %>% layout(height = 400, width = 750)
    
    
    
  }) 
  
  output$nutr_bar_basket <- renderPlotly({
    
    shiny::validate(
      need(!is.null(input$final_choice), "Please select at least one final item in the previous tab.")
    )
    
    # colours 
    col_pal <- c("#FFFF00", "#E69F00", "#56B4E9")
    
    # data prep
    data_nut_bar <- df_footprint_nutrients %>% 
      dplyr::filter(food %in% input$final_choice, nutrient %in% c(input$nutrient_1, 
                                                                  input$nutrient_2, input$nutrient_3)) %>% unique()
    
    nut_bar <-  ggplot(data_nut_bar, aes(x = food, y = nutrient_value, text = paste0("Item: ", food, '<br>', 
                                                     "Unit: ", nutrient_unit, '<br>', 
                                                     "Concentration: ", round(nutrient_value, 2)))) +
      geom_bar(width = 0.5, stat = "identity", aes(fill = nutrient)) +
      
      # change color of boxes
      scale_fill_manual(name = "Nutrients", values = col_pal) +
      labs(x = "Items", y = "Concentration per 100g") +
      facet_wrap(nutrient ~ nutrient_unit, ncol = 3, scales = "free_y",
                 strip.position = "top") +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "white"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "white"),
        # no grid
        panel.grid.major = element_blank(),
        # legend
        legend.background = element_rect(fill = "black", colour = "black"),
        legend.text = element_text(size = 10, colour = "white"),
        legend.title = element_text(size = 10, colour = "white"),
        # change color of facet
        strip.background = element_blank(),
        strip.text = element_text(colour = 'white'),
        # axis labels
        axis.text = element_text(size = 10, colour = "white"),
        # axis color
        axis.text.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white", angle = 90),
        axis.title.y = element_text(colour = "white", margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(colour = "white", margin = ggplot2::margin(t = 8, r = 0, b = 0, l = 0)),
        # title colour
        plot.title = element_text(color = "white"),
        panel.spacing = unit(1, "lines"))
    
    ggplotly(nut_bar, tooltip = "text") %>% layout(height = 400, width = 800)
    
  })  
  
  #### Price Recommendation ####
  #%%%%%%%%%%%%%%%%%%%%%#
  
  observeEvent(input$refresh_price_recommendation, {
    updateSelectInput(session, "market_m", choices= c("Price in Hamburg" = "price_H",
                                                      "Price in Frankfurt" = "price_F",
                                                      "Price in Cologne" = "price_K",
                                                      "Price in Berlin" = "price_B",
                                                      "Price in Munich" = "price_M"), 
                      selected=c("price_H"))
  })
  
  observeEvent(input$refresh_pr_food_combi, {
    updateSelectInput(session, "Food_m", selected = c(""))
    updateSelectInput(session, "Type_m", selected = c(""))
    updateSelectInput(session, "Size_m", selected = c(""))
    updateSelectInput(session, "Country_m", selected = c(""))
  })
  
  #output$market_m <- renderUI({
  #selectInput(inputId = "Market_m", "Select Market(s):",choices=var_market_m(), multiple = T)
  #tags$head(tags$style(HTML(".selectize-input {height: 10px; width: 200px; font-size: 15px;}")))
  #})
  output$food_m <- renderUI({
    selectInput(inputId = "Food_m", "Select Fruit/Vegetable:", choices = var_food_m(), multiple = T)
  })
  output$type_m <- renderUI({
    selectInput(inputId = "Type_m", "Select Type:", choices = var_type_m(), multiple = T)
  })
  output$size_m <- renderUI({
    selectInput(inputId = "Size_m", "Select Size:", choices = var_size_m(), multiple = T)
  })
  output$country_m <- renderUI({
    selectInput(inputId = "Country_m", "Select Country:", choices = var_country_m(), multiple = T)
  })
  
  value_filtered_m <- reactive({
    markets <- input$market_m
    
    dplyr::filter(future_price_recommendation, food %in% food_m(), country %in% country_m(), 
                  type %in% type_m(), size %in% size_m(), market %in% markets)
  })
  
  #market_m <- reactive({
  #if (is.null(input$Market_m)) unique(future_price_recommendation$market) else input$Market_m
  #})
  
  food_m <- reactive({
    if (is.null(input$Food_m)) unique(future_price_recommendation$food) else input$Food_m
  })
  
  country_m <- reactive({
    if (is.null(input$Country_m)) unique(future_price_recommendation$country) else input$Country_m
  })
  
  type_m <- reactive({
    if (is.null(input$Type_m)) unique(future_price_recommendation$type) else input$Type_m
  })
  
  size_m <- reactive({
    if (is.null(input$Size_m)) unique(future_price_recommendation$size) else input$Size_m
  })
  
  #var_market_m <- reactive({
  #unique(price_recommendation_df$market)
  #})
  
  var_food_m <- reactive({
    #filter(price_recommendation_df, market %in% market_m()) %>% 
    #pull(food) %>% 
    #unique()
    sort(unique(price_recommendation_df$food))
  })
  
  var_type_m <- reactive({
    dplyr::filter(price_recommendation_df, food %in% food_m()) %>% 
      dplyr::pull(type) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_size_m <- reactive({
    dplyr::filter(price_recommendation_df, food %in% food_m(), type %in% type_m()) %>% 
      dplyr::pull(size) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_country_m <- reactive({
    dplyr::filter(price_recommendation_df, food %in% food_m(), type %in% type_m(), size %in% size_m()) %>% 
      dplyr::pull(country) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  
  
  output$future_price_recommendation_table <- DT::renderDataTable({
    
    df <- value_filtered_m()
    
    
    df$market[df$market == "price_H"] <- "Hamburg"
    df$market[df$market == "price_F"] <- "Frankfurt"
    df$market[df$market == "price_B"] <- "Berlin"
    df$market[df$market == "price_K"] <- "Cologne"
    df$market[df$market == "price_M"] <- "Munich"
    
    df$model[df$model == "Random_Forest"] <- "Random Forest"
    
    df <- dplyr::rename(df, Market = market, `Best Time to buy` = Best_time_to_buy, `Predicted Price` = `predicted price`,
                        `Machine Learning Model` = model)
    
    df %>%
      #ungroup() %>%
      dplyr::select(Market, `Best Time to buy`, `Predicted Price`, `Machine Learning Model`) %>%
      dplyr::arrange(`Predicted Price`)
    
    
  })
  
  #### Past Prices ####
  #%%%%%%%%%%%%%%%%%%%%%#
  
  onclick("future_price_prediction_info", runjs("window.open('Theory_Future_Price_Recommendation_info.html')"))
  
  observeEvent(input$refresh_price_recommendation_o, {
    updateSelectInput(session, "market_o", choices= c("Price in Hamburg" = "price_H",
                                                      "Price in Frankfurt" = "price_F",
                                                      "Price in Cologne" = "price_K",
                                                      "Price in Berlin" = "price_B",
                                                      "Price in Munich" = "price_M"), 
                      selected=c("price_H"))
  })
  
  observeEvent(input$refresh_pr_food_combi_o, {
    updateSelectInput(session, "Food_o", selected = c(""))
    updateSelectInput(session, "Type_o", selected = c(""))
    updateSelectInput(session, "Size_o", selected = c(""))
    updateSelectInput(session, "Country_o", selected = c(""))
  })
  
  output$food_o <- renderUI({
    selectInput(inputId = "Food_o", "Select Fruit/Vegetable:", choices = var_food_o(), multiple = T)
  })
  output$type_o <- renderUI({
    selectInput(inputId = "Type_o", "Select Type:", choices = var_type_o(), multiple = T)
  })
  output$size_o <- renderUI({
    selectInput(inputId = "Size_o", "Select Size:", choices = var_size_o(), multiple = T)
  })
  output$country_o <- renderUI({
    selectInput(inputId = "Country_o", "Select Country:", choices = var_country_o(), multiple = T)
  })
  
  value_filtered_o <- reactive({
    markets_o <- input$market_o
    
    dplyr::filter(past_prices_rates, food %in% food_o(), country %in% country_o(), 
                  type %in% type_o(), size %in% size_o(), market %in% markets_o)
  })
  
  
  food_o <- reactive({
    if (is.null(input$Food_o)) unique(past_prices_rates$food) else input$Food_o
  })
  
  country_o <- reactive({
    if (is.null(input$Country_o)) unique(past_prices_rates$country) else input$Country_o
  })
  
  type_o <- reactive({
    if (is.null(input$Type_o)) unique(past_prices_rates$type) else input$Type_o
  })
  
  size_o <- reactive({
    if (is.null(input$Size_o)) unique(past_prices_rates$size) else input$Size_o
  })
  
  #var_market_m <- reactive({
  #unique(price_recommendation_df$market)
  #})
  
  var_food_o <- reactive({
    #filter(price_recommendation_df, market %in% market_m()) %>% 
    #pull(food) %>% 
    #unique()
    sort(unique(past_prices_rates$food))
  })
  
  var_type_o <- reactive({
    dplyr::filter(past_prices_rates, food %in% food_o()) %>% 
      dplyr::pull(type) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_size_o <- reactive({
    dplyr::filter(past_prices_rates, food %in% food_o(), type %in% type_o()) %>% 
      dplyr::pull(size) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  var_country_o <- reactive({
    dplyr::filter(past_prices_rates, food %in% food_o(), type %in% type_o(), size %in% size_o()) %>% 
      dplyr::pull(country) %>% 
      unique() %>%
      forcats::fct_relevel()
  })
  
  output$future_price_recommendation_table_o <- DT::renderDataTable({
    
    df_o <- value_filtered_o()
    
    df_o <- df_o %>%
      dplyr::select(food, type, size, country, market, contains(input$week_o) & contains(input$average_rec)) %>%
      dplyr::arrange(across(ncol(.), desc))
    
    df_o <- df_o[,-6]
    
    #df_o[, ncol(df_o)] <- round(df_o[, ncol(df_o)], 2)
    
    colnames(df_o)[6] <- "Price increase/decrease in %"
    
    df_o
    
  })
  
  #### Include HTMLS ####
  #%%%%%%%%%%%%%%%%%%%%%#
  
  # html files need to be included at the end of the server function
  # otherwise, problems with action button arise
  
  # Use Case 
  output$use_case <- renderUI({
    tags$iframe(src = "00_a_Introduction_Markdown.html", 
                height = 700, width = 1000)
  })
  
  
  # Sitemap
  output$sitemap <- renderUI({
    tags$iframe(src = "00_b_Sitemap_Markdown.html", 
                height = 700, width = 1000)
    
  })
  
  # explanation price development
  output$price_info <- renderUI({
    #withMathJax(includeHTML("231_Price_Development_Info.html"))
    tags$iframe(src = "Theory_Price_Development_Info.html", 
                height = 700, width = 1000)
  })
  
  
  # literature
  output$literature <- renderUI({
    #withMathJax(includeHTML("228_Clustering_kmeans.html")) # makes problems with actionButton
    tags$iframe(src = "Literature_Overview.html", 
                height = 700, width = 1000)
  })
  
  
  # cluster theory
  output$cluster_theory <- renderUI({
    #withMathJax(includeHTML("228_Clustering_kmeans.html")) # makes problems with actionButton
    tags$iframe(src = "Theory_Clustering_kmeans.html", 
                height = 700, width = 1000)
  })
  
  # price prediction introduction
  output$ml_intro <- renderUI({
    #includeHTML("220_Price_Predictions.html") # makes problems with actionButton
    tags$iframe(src = "Theory_Price_Predictions.html", 
                height = 700, width = 1000)
  })
  
  # price prediction theory
  output$price_prediction_info <- renderUI({
    #includeHTML("237_Price_Predictions_Theory.html") # now working; but still double click; makes problems with actionButton
    tags$iframe(src = "Theory_Price_Predictions_Timeline.html", 
                height = 700, width = 1000)
  })
  
  
  # performance measures theory and literature review
  output$info_error_metrics <- renderUI({
    #withMathJax(includeHTML("222_Performance_Metrics.html")) # requires double click but still working
    tags$iframe(src = "Theory_Performance_Metrics.html", 
                height = 700, width = 1000)
  })
  
  output$info_error_metrics_literature <- renderUI({
    #includeHTML("240_Literature_Review_Error_Metrics.html") # requires double click but still working
    tags$iframe(src = "Theory_Literature_Review_Error_Metrics.html", 
                height = 700, width = 1000)
  })
  
  
  ## ML Theory ##
  
  # time series model theory
  output$theory_time_series <- renderUI({
    tags$iframe(src = "Theory_BSTS.html", 
                height = 700, width = 1000)
  })
  
  # regularized regression theory
  output$theory_regularized_regression <- renderUI({
    tags$iframe(src = "Theory_RegularizedRegression.html", 
                height = 700, width = 1000)
  })
  
  # svm theory
  output$theory_svm <- renderUI({
    tags$iframe(src = "Theory_SVM.html", 
                height = 700, width = 1000)
  })
  
  # random forests theory
  output$theory_rf <- renderUI({
    tags$iframe(src = "Theory_Random_Forest.html", 
                height = 700, width = 1000)
  })
  
  # xgboost theory
  output$theory_xgboost <- renderUI({
    tags$iframe(src = "Theory_XGBoost.html", 
                height = 700, width = 1000)
  })
  
  # neural network theory
  output$theory_nn <- renderUI({
    tags$iframe(src = "Theory_NN.html", 
                height = 700, width = 1000)
  })
  
  ## Feature Importance ##
  output$imp_info <- renderUI({
    #withMathJax(includeHTML("234_Feature_Importance.html")) # problem!
    tags$iframe(src = "Theory_Feature_Importance.html", 
                height = 700, width = 1000)
    #width = '100%', style = "height: 500vh;")
  })
  
  
  output$food_basket_info <- renderUI({
    tags$iframe(src = "Theory_Food_Basket_Info.html", 
                height = 500, width = 1000)
    #withMathJax(includeHTML("233_Food_Basket_Info.html"))
  })

output$price_rec_guide <- renderUI({
  tags$iframe(src = "Theory_Future_Price_Recommendation_info.html", 
              height = 500, width = 1000)
  #withMathJax(includeHTML("233_Food_Basket_Info.html"))
})
  
  
}