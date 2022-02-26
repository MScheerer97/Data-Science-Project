# In this file the ui is set up #

#### Define User Input ####

dashboardPage(
  
  ## Header Content
  dashboardHeader(
    title = "Prediction of Fruit and Vegetable Prices",
    titleWidth = 450
  ),
  ## Sidebar contentpas
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("users")),
      menuItem("Information", tabName = "Information", icon = icon("info-circle")),
      menuItem("Variable Development", tabName = "features_outcome", icon = icon("chart-line")),
      menuItem("Econometric Analysis", tabName = "econometric_analysis", icon = icon("calculator")),
      menuItem("Price Prediction", tabName = "ml", icon = icon("robot")),
      menuItem("Food Basket", tabName = "food_basket", icon = icon("utensils"))
    )
  ),
  ## Body content
  dashboardBody(
    # Include the custom styling
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_sheet_2.css")
    ),
    # color of DT::datatable information summary and page next
    # (could not do it in CSS file)
    tags$head(tags$style(HTML(
      "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
    background-color: white !important;}"))),
    tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, 
    .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
    .dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper 
    .dataTables_paginate .paginate_button.disabled {
    color: white !important;}")),
    # hide scroll bar (not needed)
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
    # size and style of selectInput
    tags$head(tags$style(HTML('
    .selectize-input {word-wrap: break-word;}
    #pred_model_model+ div>.selectize-dropdown{word-wrap: break-word !important; font-size:10px}
    #pred_model_model+ div>.selectize-input{word-wrap: break-word !important;font-size:10px}
                            '))),
    #input2+ div>.selectize-dropdown{width: 300px !important; color: red;font-style: italic; font-weight: bold; }
    #input2+ div>.selectize-input{width: 300px !important; color: red; margin-bottom: 0px;}
    
    # include line break in selectize input
    #tags$style(type = 'text/css', ".selectize-input { word-wrap: break-word;}"),
    #nutrition_food_food+ div>.selectize-dropdown{width: 400px !important; color: darkgrey;font-size: 8px;}
    #nutrition_food_food+ div>.selectize-input{width: 400px !important; color: black; margin-bottom: -10px;font-size: 8px;}
    
    #nutrition_food_var+ div>.selectize-dropdown{width: 400px !important; color: darkgrey;font-size: 12px;white-space:wrap !important;}
    #nutrition_food_var+ div>.selectize-input{width: 400px !important; color: black; margin-bottom: -10px;font-size: 12px; white-space:wrap !important;}
    
    
    #tags$style(js),
    tabItems(
      
      #### Introduction ####
      #%%%%%%%%%%%%%%%%%%%%#
      tabItem(tabName = "Introduction",
              fluidRow(
                tabBox(width = 12,
                       # Explanation of Use Case
                       tabPanel("Use Case", icon = icon("info"),
                                uiOutput("use_case")
                       ),
                       # Sitemap
                       tabPanel("Sitemap", icon = icon("sitemap"),
                                uiOutput("sitemap")
                       ), 
                       # Literature
                       tabPanel("Literature", icon = icon("book"),
                                uiOutput("literature")
                       ), 
                       # Data Sources
                       tabPanel("Data Sources", icon = icon("server"),
                                selectInput(
                                  "data_sources", "Select Use Case:", 
                                  choices = c("Price Prediction", "Food Basket"), 
                                  multiple = FALSE
                                ),
                                uiOutput("data_sources_output")
                       )
                       
                )
              )),
      
      #### Information ####
      #%%%%%%%%%%%%%%%%%%%#
      tabItem(tabName = "Information",
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Fruits/Vegetables", icon = icon("utensils"), 
                                uiOutput("food_items_overview")),
                       #### food-type-size-country combinations ####
                       tabPanel("Type, Size, and Country", icon = icon("binoculars"),
                                fluidRow(
                                  # create info box
                                  infoBox(title = "Number of Fruits and Vegetables:", 
                                          value = length(unique(df_descr$food)),
                                          icon = icon("utensils"), 
                                          color = "teal", fill = TRUE, width = 4),
                                  infoBox("Number of Fruits:", length(fruits),
                                          icon = icon("apple-alt"), 
                                          color = "teal", fill = TRUE, width = 4),
                                  infoBox("Number of Vegetables:", 
                                          length(unique(df_descr$food)) - length(fruits),
                                          icon = icon("carrot"), 
                                          color = "teal", fill = TRUE, width = 4)
                                ),
                                fluidRow(
                                  infoBox("Number of Fruit and Vegetable Types:", 
                                          length(unique(df_descr$type)),
                                          icon = icon("seedling"), 
                                          color = "teal", fill = TRUE, width = 4),
                                  infoBox("Number of Fruit and Vegetable Sizes:", length(unique(df_descr$size)),
                                          icon = icon("search"), 
                                          color = "teal", fill = TRUE, width = 4), 
                                  infoBox("Number of Fruit and Vegetable Countries:", length(unique(df_descr$country)),
                                          icon = icon("globe-europe"), 
                                          color = "teal", fill = TRUE, width = 4)
                                ),
                                fluidPage(
                                  fluidRow(
                                    column(width = 5, offset = 0, style = 'padding-left:0px; padding-right:1px',
                                           selectInput(
                                             "info_food", "Select Fruit/Vegetable:",
                                             choices = sort(unique(df_descr$food)),
                                             selected = NULL,
                                             multiple = TRUE
                                           )),
                                    column(width = 3, offset = 0,  style = 'padding-left:0px; padding-right:1px',
                                           checkboxGroupInput(
                                             "info_type_country_size", "Select Information to be Displayed:",
                                             choices = c("Type", "Size", "Country"),
                                             selected = c("Type", "Size", "Country"),
                                             inline = TRUE
                                           )),
                                    column(width = 2,
                                           actionButton("info_show", "Display Table and Descriptives", icon("play-circle"),
                                                        style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           box(title = HTML("Table"),
                                               status = "primary", solidHeader = TRUE,
                                               style = 'font-size:15px;color:white;background-color:black;',
                                               collapsible = TRUE,
                                               DT::dataTableOutput("info_table"),
                                               width = 14)
                                    ),
                                    column(width = 6, 
                                           box(title = HTML("Descriptives"), # HTML() makes background black
                                               status = "primary", solidHeader = TRUE,
                                               style = 'font-size:15px;color:white;background-color:black;',
                                               collapsible = TRUE,
                                               htmlOutput("info_types_num"),
                                               width = 8)
                                    )
                                  ), # close fluidRow()
                                  fluidRow(
                                    column(width = 6,
                                           box(title = HTML("Explanation"),
                                               status = "primary", solidHeader = TRUE,
                                               style = 'font-size:15px;color:white;background-color:black;',
                                               collapsible = TRUE,
                                               htmlOutput("info_explanation"),
                                               width = 14)
                                    )
                                  ) # close fluidRow()
                                ) # close fluidPage()
                       ), # close tabPanel()
                       
                       #### nutrition and footprint ####
                       #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                       tabPanel("Nutrition and Footprint", icon = icon("seedling"),
                                tabsetPanel(
                                  tabPanel("Nutrition", icon = icon("fa-solid fa-book-medical", verify_fa = FALSE),
                                           sidebarLayout(
                                             sidebarPanel(
                                               style = "color: white; background-color: black; border-color: black",
                                               radioButtons("nutrition_method",
                                                            "Select Analysis Type:",
                                                            choices = c("Food", "Food Comparison"),
                                                            selected = "Food"),
                                               conditionalPanel(
                                                 "input.nutrition_method == 'Food'",
                                                 selectInput(
                                                   "nutrition_food_sel", "Select Fruit/Vegetable:",
                                                   choices = sort(unique(df_descr$food)),
                                                   selected = "Apple",
                                                   multiple = FALSE, width = "400px"
                                                 ),
                                                 selectInput(
                                                   "nutrition_food_sel_var", "Select Nutrition:",
                                                   choices = sort(c("All", unique(df_basket_info$nutrient))),
                                                   selected = NULL, 
                                                   multiple = TRUE, width = "400px"
                                                 ),
                                                 actionButton("nutrition_food_sel_show",
                                                              "Show Result", icon("play-circle"),
                                                              width = "400px", 
                                                              style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                               ),
                                               conditionalPanel(
                                                 "input.nutrition_method == 'Food Comparison'",
                                                 selectizeInput(
                                                   "nutrition_food_food", "Select Fruit/Vegetable:",
                                                   choices = sort(unique(df_descr$food)),
                                                   selected = NULL,
                                                   multiple = TRUE, width = "400px"
                                                   # options = list(maxItems = 10),
                                                   # size = 10
                                                 ),
                                                 selectizeInput(
                                                   "nutrition_food_var", "Select Nutrition:",
                                                   choices = sort(unique(df_basket_info$nutrient)),
                                                   selected = NULL, 
                                                   multiple = TRUE, width = "400px"
                                                   # options = list(maxItems = 9),
                                                   # size = 10
                                                 ),
                                                 actionButton("nutrition_food_show",
                                                              "Show Result", icon("play-circle"),
                                                              width = "400px", 
                                                              style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                               ),
                                               # add note
                                               tags$h6(
                                                 HTML("<br>Note: Nutrition information is displayed for 100g/food. 
                                                      Moreover, only the <br> nutrition information for which the
                                                      nutrient values are not zero are displayed <br> or 
                                                      rather can be selected.")), 
                                             ), # close sidebarPanel()
                                             mainPanel(
                                               conditionalPanel(
                                                 "input.nutrition_method == 'Food'",
                                                 DT::dataTableOutput("nutrition_food_table", width = "800px")
                                               ),
                                               conditionalPanel(
                                                 "input.nutrition_method == 'Food Comparison'",
                                                 plotOutput("nutrition_food_plot", width = "100%") %>%
                                                   withSpinner(color = "#0dc5c1", type = 6)
                                               )
                                             ) # close mainPanel()
                                           ) # close sidebarLayout()
                                  ), # close tabPanel("Nutrition")
                                  tabPanel("Footprint", icon = icon("shoe-prints"), 
                                           sidebarLayout(
                                             sidebarPanel(
                                               style = "color: white; background-color: black; border-color: black",
                                               width = 3, 
                                               div(useShinyjs(),actionButton("footprint_theory", 
                                                                             "Info Footprint Types", 
                                                                             width = "280px", icon("info"),
                                                                             style = "color: black; background-color: #8bbeea; border-color: #8bbeea"),
                                                   style = 'padding-bottom:10px'),
                                               selectizeInput(
                                                 "footprint_food", "Select Fruit/Vegetable:",
                                                 choices = sort(unique(df_descr$food)),
                                                 selected = NULL,
                                                 multiple = TRUE, width = "280px", 
                                               ),
                                               checkboxGroupButtons(
                                                 "footprint_type", "Select Footprint Type:",
                                                 choices = c("Carbon Footprint", "Water Footprint"),
                                                 selected = c("Carbon Footprint", "Water Footprint"),
                                                 individual = TRUE, width = "280px", 
                                                 direction = "vertical",
                                                 checkIcon = list(
                                                   yes = icon("check-square"),
                                                   no = icon("square")
                                                 )
                                               ),
                                               # checkboxGroupButtons(
                                               #   "footprint_display_type", "Select display option:",
                                               #   choices = c("Table", "Plot"),
                                               #   selected = c("Table", "Plot"),
                                               #   individual = TRUE,
                                               #   checkIcon = list(
                                               #     yes = icon("check-square"),
                                               #     no = icon("square")
                                               #   )
                                               # ),
                                               actionButton("footprint_show", 
                                                            "Show Result", width = "280px", icon("play-circle"),
                                                            style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                             ), # close sidebarPanel()
                                             mainPanel(width = 6, 
                                                       DT::dataTableOutput("footprint_table", width = "500px"),
                                                       br(), br(), 
                                                       plotOutput("footprint_plot")
                                             )
                                           ) # close sidebarLayout()
                                  ) # close tabPanel("Footprint")
                                ) # close tabsetPanel()
                       ), # close tabPanel("Nutrition and Footprint")
                       
                       #### Cluster Analysis ####
                       #%%%%%%%%%%%%%%%%%%%%%%%%#
                       
                       tabPanel("Cluster Analysis", icon = icon("object-group"),
                                tabsetPanel(
                                  tabPanel("K-Means Clustering Theory", icon = icon("bookmark"),
                                           column(6, uiOutput("cluster_theory"))),
                                  tabPanel("K-Means Clustering Calculation", icon = icon("calculator"),
                                           splitLayout(cellWidths = c("30%", "70%"), 
                                                       fluidPage(
                                                         fluidRow(
                                                           column(width = 10, style = 'padding-top:20px',
                                                                  selectInput(
                                                                    "cluster_food", "Select Food Type:",
                                                                    choices = c("Fruits", "Vegetables"),
                                                                    selected = NULL,
                                                                    multiple = TRUE,
                                                                    width = "345px"
                                                                  )
                                                           )),
                                                         fluidRow(
                                                           column(width = 10,
                                                                  selectInput(
                                                                    "cluster_var", "Select Cluster Category:",
                                                                    choices = sort(c("Nutrition", "Footprint", "Mean Price")),
                                                                    selected = NULL,
                                                                    multiple = TRUE, 
                                                                    width = "345px"
                                                                  )
                                                           )),
                                                         fluidRow(column(width = 10,
                                                                         selectizeInput(
                                                                           "cluster_var_2", "Select Variables used for Clustering:",
                                                                           choices = NULL,
                                                                           selected = NULL,
                                                                           multiple = TRUE,
                                                                           width = "345px"
                                                                         )
                                                         )),
                                                         fluidRow(
                                                           column(width = 10,
                                                                  radioButtons(
                                                                    "cluster_num_sel", "How Do You Want to Determine the Number of Clusters?",
                                                                    choices = c("Manually", "Automatically using the Silhouette Method"),
                                                                    selected = c("Manually"),
                                                                    width = "460px"
                                                                  ))),
                                                         fluidRow(
                                                           column(width = 10,
                                                                  conditionalPanel(
                                                                    "input.cluster_num_sel == 'Manually'",
                                                                    sliderInput(
                                                                      "cluster_num", "Select Number of Clusters:",
                                                                      min = 2,
                                                                      max = 10, 
                                                                      value = 4,
                                                                      width = "345px"
                                                                    ),
                                                                    actionButton(
                                                                      "cluster_show_manually", "Show Cluster Solution:",
                                                                      icon("play-circle"), width = "345px", 
                                                                      style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                                  )
                                                           ),
                                                           conditionalPanel(
                                                             "input.cluster_num_sel == 'Automatically using the Silhouette Method'",
                                                             useShinyjs(),
                                                             fluidRow(
                                                               column(width = 10, style = 'padding-left:-50px; padding-right:0px; padding-top:5px; padding-bottom:5px',
                                                                      actionButton("silhouette_method", "Info on Silhouette Method",
                                                                                   icon("info"), width = "345px",
                                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(width = 10, 
                                                                      radioGroupButtons(
                                                                        "silhouette_method_plot", "Show Plot for Silhouette Method?:",
                                                                        choices = c("Yes", "No"),
                                                                        selected = "No", width = "345px", 
                                                                        individual = TRUE,
                                                                        checkIcon = list(
                                                                          yes = icon("check-square"),
                                                                          no = icon("square")
                                                                        )
                                                                      )
                                                               )),
                                                             # fluidRow(
                                                             #   column(width = 8, style = 'padding-left:-35px; padding-right:0px; padding-top:5px; padding-bottom:5px',
                                                             #          actionButton("silhouette_method_plot", "Plot for Silhouette Method", 
                                                             #                       icon("image"), width = "350px", 
                                                             #                       style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                             #   )
                                                             # ),
                                                             fluidRow(
                                                               column(width = 10, 
                                                                      style = 'padding-left:-1px; padding-right:0px; padding-top:5px; padding-bottom:5px',
                                                                      actionButton(
                                                                        "cluster_show_auto", "Show cluster solution:",
                                                                        icon("play-circle"), width = "345px",
                                                                        style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(width = 10,
                                                                      br(), 
                                                                      plotOutput("silhouette_plot", width = "110%") 
                                                               )
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       fluidPage(
                                                         # show text with clusters only when number is determined automatically
                                                         conditionalPanel(
                                                           "input.cluster_num_sel == 'Automatically using the Silhouette Method'",
                                                           fluidRow(
                                                             column(width = 12,
                                                                    textOutput("cluster_chosen_k"), br())
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width = 12, 
                                                                  tableOutput("cluster_output_table")
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width = 8,
                                                                  br(), 
                                                                  plotOutput("cluster_output_plot", width = "100%") %>%
                                                                    withSpinner(color = "#0dc5c1", type = 6)
                                                           )
                                                         )
                                                       )
                                           ) # close splitLayout()
                                  ) # close tabPanel(K-Means Clustering Calculatoon)
                                ) # close tabsetPanel()
                       ) # close tabPanel()
                ))),
      
      #### Features & Outcome ####
      #%%%%%%%%%%%%%%%%%%%%%%%%%%#
      tabItem(tabName = "features_outcome",
              fluidRow(
                tabBox(width = 12,
                       #### Price Development ####
                       tabPanel("Price Development", icon = icon("euro-sign"),
                                tabsetPanel(
                                  tabPanel("Information", icon = icon("book"),
                                           column(6, uiOutput("price_info"))
                                  ),
                                  tabPanel("Descriptive Statistics", icon = icon("calculator"),
                                           sidebarLayout(
                                             sidebarPanel(style = "color: white; background-color: #000000; border-color: #000000;
                                                          position:fixed;",
                                                          
                                                          #tags$style(make_css(list('.well', 'border-width', '10px'))),
                                                          
                                                          #fluidRow(
                                                          #div(#style = "margin-top:1em",
                                                          #column(width = 6,
                                                          # select price, 
                                                          # ind is short for: individual
                                                          # as only one item can be selected here
                                                          selectInput(
                                                            "price_ind", "Select Market:", 
                                                            choices = sort(unique(df_price$market)[unique(df_price$market) %in% c("Berlin", 
                                                                                                                                  "Munich", 
                                                                                                                                  "Cologne", 
                                                                                                                                  "Hamburg", 
                                                                                                                                  "Frankfurt")]),
                                                            selected = character(0),
                                                            multiple = TRUE, width = "200px"), #)
                                                          # ))),
                                                          # fluidRow(
                                                          #  div(#style = "margin-top:1em",
                                                          #column(width = 6,
                                                          # select item
                                                          selectInput(
                                                            "item_ind", "Select Fruit/Vegetable:", 
                                                            choices = sort(unique(df_price$food)),
                                                            selected = sort(unique(df_price$food))[1],
                                                            multiple = FALSE, width = "200px"), #),
                                                          #  ))),
                                                          # select type
                                                          # fluidRow(
                                                          #  div(#style = "margin-top:1em",
                                                          #column(width = 6,
                                                          selectInput(
                                                            "type_ind", "Select Type:", 
                                                            choices = sort(unique(df_price$type)), 
                                                            selected = sort(unique(df_price$type))[1],
                                                            multiple = FALSE, width = "200px"), #),
                                                          #))),
                                                          
                                                          # select size
                                                          #  fluidRow(
                                                          #   div(#style = "margin-top:1em",
                                                          #column(width = 6,
                                                          selectInput(
                                                            "size_ind", "Select Size:", 
                                                            choices = mixedsort(unique(df_price$size)), 
                                                            selected = mixedsort(unique(df_price$size))[1],
                                                            multiple = FALSE, width = "200px"), #),
                                                          #  ))),
                                                          
                                                          # select country
                                                          #fluidRow(
                                                          # div(#style = "margin-top:1em",
                                                          #column(width = 6,
                                                          selectInput(
                                                            "country_ind", "Select Country:", 
                                                            choices = unique(df_price$country), 
                                                            selected = unique(df_price$country)[1],
                                                            multiple = FALSE, width = "200px"), #),
                                                          # ))),
                                                          
                                                          tags$h6(
                                                            HTML("<br>Please make sure to refresh your selection.")),
                                                          
                                                          # action button
                                                          actionButton(
                                                            "disselect_ind", "Refresh", icon = icon("sync"), 
                                                            style = "padding: 3px; color: black; background-color: #8bbeea; border-color: #8bbeea")#)  
                                                          #)))
                                             ), # sidebar
                                             
                                             mainPanel(style = "margin-left: 350px;",
                                                       tabsetPanel(
                                                         ## Outputs
                                                         tabPanel("Descriptive Statistics - Overview",
                                                                  
                                                                  #fluidRow(div(style = "margin-top:1em",
                                                                  column(width = 10, style = "background-color: #000000; height: 50px;",
                                                                         useShinyjs(),
                                                                         actionButton("Agg_button", 
                                                                                      "Info on Aggregation", 
                                                                                      width = 320, icon("info"),
                                                                                      style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                                  ),
                                                                  #) ),
                                                                  #fluidRow(div(style = "margin-top:1em",
                                                                  column(width = 3, style = "background-color: #000000; height: 100px;",
                                                                         # choose aggregation
                                                                         radioButtons(
                                                                           "Time_Group", "Aggregation Level:", 
                                                                           choices = c("None", "Month", "Year"), 
                                                                           selected = "None"
                                                                         )), 
                                                                  
                                                                  # show descriptives
                                                                  column(width = 7, style = "background-color: #000000; height: 100px;",
                                                                         checkboxInput(
                                                                           "price_descr_ind", "Show Descriptive Statistics",
                                                                           value = TRUE
                                                                         )
                                                                  ),
                                                                  #) ),
                                                                  
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 10,
                                                                                      textOutput("desc_header")), 
                                                                               tags$head(tags$style("#desc_header{font-size: 18px;}"))
                                                                  )),
                                                                  
                                                                  
                                                                  ## Descriptive Statistics
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 12,
                                                                                      htmlOutput("price_descr_ind_out", width = "500px",height = "500px") %>%
                                                                                        withSpinner(color = "#0dc5c1", type = 6))
                                                                               
                                                                  )),
                                                                  
                                                                  # Barplot with standard deviation
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 12,
                                                                                      plotlyOutput("bar_desc", width = "700px", height = "600px") %>%
                                                                                        withSpinner(color = "#0dc5c1", type = 6)
                                                                               )
                                                                  )
                                                                  )
                                                         ), # tabpanel descriptive statistics
                                                         
                                                         
                                                         # tabpanel explorative data analysis
                                                         tabPanel("Explorative Data Analysis" ,
                                                                  
                                                                  #fluidRow(div(style = "margin-top:1em",
                                                                  
                                                                  # Info button conditional
                                                                  column(width = 12, style = "background-color: #000000; height: 50px;",
                                                                         # Box Plot
                                                                         conditionalPanel(
                                                                           "input.desc_viz == 'Box Plot'",
                                                                           useShinyjs(),
                                                                           actionButton("box_button", 
                                                                                        "Info on Box Plots", 
                                                                                        width = 320, icon("info"),
                                                                                        style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                                           
                                                                         ),
                                                                         
                                                                         # Box Plot
                                                                         conditionalPanel(
                                                                           "input.desc_viz == 'Histogram'",
                                                                           useShinyjs(),
                                                                           actionButton("hist_button", 
                                                                                        "Info on Histograms", 
                                                                                        width = 320, icon("info"),
                                                                                        style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                                           
                                                                         ),
                                                                         
                                                                         # Box Plot
                                                                         conditionalPanel(
                                                                           "input.desc_viz == 'Heatmap'",
                                                                           useShinyjs(),
                                                                           actionButton("heat_button", 
                                                                                        "Info on Heatmaps", 
                                                                                        width = 320, icon("info"),
                                                                                        style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                                                                           
                                                                         ),
                                                                  ),
                                                                  #   )),
                                                                  
                                                                  #  fluidRow(div(style = "margin-top:1em",
                                                                  
                                                                  # choose method of data visualization
                                                                  column(width = 4, style = "background-color: #000000; height: 125px;",
                                                                         radioButtons(
                                                                           "desc_viz", "Data Visualization Method:", 
                                                                           choices = c("Box Plot", "Histogram", "Heatmap"), 
                                                                           selected = "Box Plot"
                                                                         )
                                                                  ),
                                                                  
                                                                  column(width = 3, style = "background-color: #000000; height: 125px;",
                                                                         conditionalPanel("input.desc_viz != 'Heatmap'", 
                                                                                          # choose aggregation
                                                                                          radioButtons(
                                                                                            "Time_Group_eda", "Aggregation Level:", 
                                                                                            choices = c("None", "Month", "Year"), 
                                                                                            selected = "None"
                                                                                          )
                                                                         ),
                                                                         conditionalPanel("input.desc_viz == 'Heatmap'", 
                                                                                          # choose aggregation
                                                                                          radioButtons(
                                                                                            "colour_heat", "Colour Palette:", 
                                                                                            choices = c("Viridis", "Cividis", "Plasma", 
                                                                                                        "Rocket"), 
                                                                                            selected = "Viridis"
                                                                                          )
                                                                         )), 
                                                                  
                                                                  # Binwidth conditional panel
                                                                  column(width = 3, style = "background-color: #000000; height: 125px;",
                                                                         conditionalPanel(
                                                                           "input.desc_viz == 'Histogram'", 
                                                                           numericInput("hist_bin", "Bin Width Histogram:", 0.05, min = 0.01, max = 0.8, step = 0.01)
                                                                         )
                                                                  ),
                                                                  # Density conditional
                                                                  column(width = 2, style = "background-color: #000000; height: 125px;",
                                                                         conditionalPanel(
                                                                           "input.desc_viz == 'Histogram'",
                                                                           radioButtons("hist_dens", "Density:", 
                                                                                        choices = c("Hide Density", "Show Density"), 
                                                                                        selected = "Hide Density")
                                                                         )
                                                                  ), 
                                                                  ## heatmap colour choice
                                                                  # column(width = 3, style = "background-color: #000000; height: 100px;",
                                                                  #        conditionalPanel("input.desc_viz == 'Heatmap'", 
                                                                  #                         # choose aggregation
                                                                  #                         radioButtons(
                                                                  #                           "colour_heat", "Colour Palette:", 
                                                                  #                           choices = c("Viridis", "Cividis", "Plasma", 
                                                                  #                                       "Rocket"), 
                                                                  #                           selected = "Viridis"
                                                                  #                         )
                                                                  #        ))
                                                                  
                                                                  #) ),
                                                                  
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 10,
                                                                                      textOutput("eda_header")), 
                                                                               tags$head(tags$style("#eda_header{font-size: 18px;}"))
                                                                  )),
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 12,
                                                                                      
                                                                                      # Box Plot
                                                                                      conditionalPanel(
                                                                                        "input.desc_viz == 'Box Plot'",
                                                                                        plotlyOutput("box_desc", width = "700px", height = "600px") %>%
                                                                                          withSpinner(color = "#0dc5c1", type = 6)
                                                                                        
                                                                                      ),
                                                                                      
                                                                                      
                                                                                      # Histogram
                                                                                      conditionalPanel(
                                                                                        "input.desc_viz == 'Histogram'", 
                                                                                        plotlyOutput("hist_desc", width = "700px", height = "500px") %>%
                                                                                          withSpinner(color = "#0dc5c1", type = 6)
                                                                                        
                                                                                      ), 
                                                                                      # Histogram
                                                                                      conditionalPanel(
                                                                                        "input.desc_viz == 'Heatmap'", 
                                                                                        plotlyOutput("heat_desc", width = "700px", height = "500px") %>%
                                                                                          withSpinner(color = "#0dc5c1", type = 6)
                                                                                        
                                                                                      )
                                                                               )
                                                                  )
                                                                  )
                                                                  
                                                         ), # tabpanel EDA
                                                         
                                                         
                                                         tabPanel("Time Series Visualization",
                                                                  
                                                                  #fluidRow(div(style = "margin-top:1em",
                                                                  
                                                                  column(width = 3, style = "background-color: #000000; height: 100px;",
                                                                         # choose comparison mode
                                                                         radioButtons(
                                                                           "ts_items", "Exploration Mode:", 
                                                                           choices = c("Single Item", "Two Items"), 
                                                                           selected = "Single Item"
                                                                         )
                                                                  ),
                                                                  
                                                                  column(width = 9, style = "background-color: #000000; height: 100px;",
                                                                         airDatepickerInput("date_range", 
                                                                                            value = c(min(df_price$date), max(df_price$date)),
                                                                                            startView = min(df_price$date),
                                                                                            view = "years", 
                                                                                            separator = " to ",
                                                                                            minDate = min(df_price$date), 
                                                                                            maxDate = max(df_price$date), 
                                                                                            label = "Select Date Range:",
                                                                                            range = TRUE
                                                                                            
                                                                         )
                                                                  ),
                                                                  #)),
                                                                  
                                                                  # provide second item:
                                                                  #fluidRow(div(style = "margin-top:1em",
                                                                  
                                                                  column(width = 6, style = "background-color: #000000; height: 150px;",
                                                                         conditionalPanel(
                                                                           "input.ts_items == 'Two Items'",
                                                                           
                                                                           # food item 
                                                                           selectInput(
                                                                             "item_ind2", "Select Fruit/Vegetable:", 
                                                                             choices = sort(unique(df_price$food)),
                                                                             selected = unique(df_price$food)[2],
                                                                             multiple = FALSE, width = "200px"
                                                                           )), 
                                                                         
                                                                         conditionalPanel(
                                                                           "input.ts_items == 'Two Items'",
                                                                           
                                                                           # food type 
                                                                           selectInput(
                                                                             "type_ind2", "Select Type:",
                                                                             choices = sort(unique(df_price$type)),
                                                                             selected = unique(df_price$type)[1],
                                                                             multiple = FALSE, width = "200px"
                                                                           ))
                                                                  ), 
                                                                  column(width = 6, style = "background-color: #000000; height: 150px;",
                                                                         conditionalPanel(
                                                                           "input.ts_items == 'Two Items'",
                                                                           
                                                                           # food country 
                                                                           selectInput(
                                                                             "country_ind2", "Select Country:",
                                                                             choices = sort(unique(df_price$country)),
                                                                             selected = unique(df_price$country)[1],
                                                                             multiple = FALSE, width = "200px"
                                                                           )), 
                                                                         
                                                                         conditionalPanel(
                                                                           "input.ts_items == 'Two Items'",
                                                                           
                                                                           # food size 
                                                                           selectInput(
                                                                             "size_ind2", "Select Size:",
                                                                             choices = sort(unique(df_price$size)),
                                                                             selected = unique(df_price$size)[1],
                                                                             multiple = FALSE, width = "200px"
                                                                           ))
                                                                  ),
                                                                  # )), ## fluidRow for all conditionals
                                                                  
                                                                  
                                                                  # Header 
                                                                  fluidRow(div(style = "margin-top:20em",
                                                                               column(width = 10,
                                                                                      textOutput("ts_header")), 
                                                                               tags$head(tags$style("#ts_header{font-size: 18px;}"))
                                                                  )),
                                                                  
                                                                  
                                                                  
                                                                  fluidRow(div(style = "margin-top:1em",
                                                                               column(width = 12,
                                                                                      plotlyOutput("plot_ts_price_ind", width = "800px", height = "550px") %>%
                                                                                        withSpinner(color = "#0dc5c1", type = 6))
                                                                  )))
                                                       )
                                                       
                                             )
                                           )
                                  ) # bracket of tabpanel for descriptives
                                )
                       ), #style = 'width: 950px; height: 800px'),
                       
                       
                       
                       
                       
                       
                       
                       
                       #### Predictor Development ####
                       #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                       
                       tabPanel("Predictor Development", icon = icon("sort-amount-up"),
                                tabsetPanel(tabPanel("Non-Weather Predictor Analysis",  icon = icon("chart-bar"),
                                                     sidebarLayout(sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                                                   selectInput("singlevar", "Select Predictor Variable:",
                                                                               choices=c("World Container Index" = "wci",
                                                                                         "Crude Oil Price" = "Crude_Oil_Price",
                                                                                         "Kerosene Price" = "Kerosene_Price",
                                                                                         "Diesel Fuel Price in Germany" = "fuel_price_diesel_Germany",
                                                                                         "Triple superphosphate" = "TSP",
                                                                                         "Di-ammonium Phosphate" = "DAP",
                                                                                         "Urea Fertilizer" = "Urea",
                                                                                         "Phosphorite" = "Phosphate_Rock",
                                                                                         "Potassium Chloride" = "Potassium_chloride",
                                                                                         "Exchange Rate South Africa" = "exchange_rate_South_Africa",
                                                                                         "Exchange Rate Turkey" = "exchange_rate_Turkey",
                                                                                         "Exchange Rate New Zealand" = "exchange_rate_New_Zealand",
                                                                                         "Inlfation in Germany" = "inflation_perc_Germany")),
                                                                   selectInput("singlemarket", "Select Market:",
                                                                               choices=c("Price in Hamburg" = "price_H",
                                                                                         "Price in Munich" = "price_M",
                                                                                         "Price in Berlin" = "price_B",
                                                                                         "Price in Cologne" = "price_K",
                                                                                         "Price in Frankfurt" = "price_F")),
                                                                   textOutput("text_univariate"),
                                                                   verbatimTextOutput("summary_univariate"),
                                                                   actionButton("show_single_predictor_info", 
                                                                                "More Info")), 
                                                      mainPanel(plotOutput("singleplot"),
                                                                plotOutput("singleplot_price")))),
                                            
                                            tabPanel("Non-Weather Predictors Comparison", icon = icon("greater-than"), 
                                                     
                                                     # Sidebar layout with a input and output definitions
                                                     sidebarLayout(
                                                       
                                                       # Inputs
                                                       sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                                                    # Select variable for y-axis
                                                                    selectInput(inputId = "y", 
                                                                                label = "Select Y-Axis Variable for the first Plot:",
                                                                                choices=c("World Container Index" = "wci",
                                                                                          "Crude Oil Price" = "Crude_Oil_Price",
                                                                                          "Kerosene Price" = "Kerosene_Price",
                                                                                          "Diesel Fuel Price in Germany" = "fuel_price_diesel_Germany",
                                                                                          "Triple superphosphate" = "TSP",
                                                                                          "Di-ammonium Phosphate" = "DAP",
                                                                                          "Urea Fertilizer" = "Urea",
                                                                                          "Phosphorite" = "Phosphate_Rock",
                                                                                          "Potassium Chloride" = "Potassium_chloride",
                                                                                          "Exchange Rate South Africa" = "exchange_rate_South_Africa",
                                                                                          "Exchange Rate Turkey" = "exchange_rate_Turkey",
                                                                                          "Exchange Rate New Zealand" = "exchange_rate_New_Zealand",
                                                                                          "Inflation in Germany" = "inflation_perc_Germany"), 
                                                                                selected = "wci"),
                                                                    selectInput(inputId = "y_2", 
                                                                                label = "Select Y-Axis Variable for the second Plot:",
                                                                                choices = c("World Container Index" = "wci",
                                                                                            "Crude Oil Price" = "Crude_Oil_Price",
                                                                                            "Kerosene Price" = "Kerosene_Price",
                                                                                            "Diesel Fuel Price in Germany" = "fuel_price_diesel_Germany",
                                                                                            "Triple superphosphate" = "TSP",
                                                                                            "Di-ammonium Phosphate" = "DAP",
                                                                                            "Urea Fertilizer" = "Urea",
                                                                                            "Phosphorite" = "Phosphate_Rock",
                                                                                            "Potassium Chloride" = "Potassium_chloride",
                                                                                            "Exchange Rate South Africa" = "exchange_rate_South_Africa",
                                                                                            "Exchange Rate Turkey" = "exchange_rate_Turkey",
                                                                                            "Exchange Rate New Zealand" = "exchange_rate_New_Zealand",
                                                                                            "Inflation in Germany" = "inflation_perc_Germany"),
                                                                                selected = "inflation_perc_Germany"),
                                                                    shinyWidgets::airDatepickerInput("onedate", "Select Date for Blue Separating Line:",
                                                                                                     value = c("2020-01-01"),
                                                                                                     minDate = c("2016-01-05"),
                                                                                                     maxDate = max(g$date)),
                                                                    shinyWidgets::airDatepickerInput("date", "Select starting/end Date to zoom in Plot:",
                                                                                                     range = TRUE,
                                                                                                     value = c(min(g$date), max(g$date)),
                                                                                                     minDate = c("2016-01-05"),
                                                                                                     maxDate = max(g$date),
                                                                                                     update_on = c("change", "close")),
                                                                    textOutput("text1"),
                                                                    verbatimTextOutput("summary"),
                                                                    textOutput("text2"),
                                                                    verbatimTextOutput("summarycorona"),
                                                                    actionButton("show_predictor_comparison_info", 
                                                                                 "More Info")
                                                       ), 
                                                       
                                                       # Outputs
                                                       mainPanel(
                                                         plotOutput(outputId = "scatterplot"),
                                                         plotOutput(outputId = "scatterplot2")
                                                         
                                                       )
                                                     )
                                            ),
                                            tabPanel("Weather Analysis", icon = icon("cloud-sun-rain"), 
                                                     sidebarLayout(
                                                       sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                                                    uiOutput("foodtype"),
                                                                    uiOutput("country"),
                                                                    selectInput("histvar",
                                                                                "Select Weather Variable for Histogram and Boxplot:",
                                                                                choices= c("Temperature" = "lag_2_weeks_temp",
                                                                                           "Humidity" = "lag_2_weeks_humidity",
                                                                                           "Cloud Cover" = "lag_2_weeks_cloudcover",
                                                                                           "Precipitation" = "lag_2_weeks_precip",
                                                                                           "Wind Speed" = "lag_2_weeks_windspeed",
                                                                                           "Deviation from normal Temperature" = "lag_2_weeks_dev_temp",
                                                                                           "Deviation from normal Humidity" = "lag_2_weeks_dev_humidity",
                                                                                           "Deviation from normal Cloud Cover" = "lag_2_weeks_dev_cloudcover",
                                                                                           "Deviation from normal Precipitation" = "lag_2_weeks_dev_precip",
                                                                                           "Deviation from normal Wind Speeds" = "lag_2_weeks_dev_windspeed"
                                                                                ),
                                                                                selected = "lag_2_weeks_temp"),
                                                                    selectInput("factortemp",
                                                                                "Select Grouping Variable for Boxplot:",
                                                                                choices=c("By Vegetable/ Fruit" = "food", "By Origin Country" = "country",
                                                                                          "All Vegetables/ Fruits" = "food_type")),
                                                                    actionButton("show_weather_analysis_info", 
                                                                                 "More Info")),
                                                       mainPanel(plotlyOutput("univariatePlot"),
                                                                 plotOutput("boxplotweatherfactor"))
                                                     )), 
                                            tabPanel("Time Series Development of Prices and all Predictors", icon = icon("chart-line"), 
                                                     sidebarLayout(sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                                                                uiOutput("foodtype_s"),
                                                                                uiOutput("food_s"),
                                                                                uiOutput("country_s"),
                                                                                uiOutput("type_s"),
                                                                                uiOutput("size_s"),
                                                                                shinyWidgets::airDatepickerInput("date_s", "Select Date to Zoom in Plots:",
                                                                                                                 range = TRUE,
                                                                                                                 value = c(min(f$date), max(f$date)),
                                                                                                                 minDate = min(f$date),
                                                                                                                 maxDate = max(f$date)),
                                                                                selectInput("predictor_s_1", "Select Market:",
                                                                                            choices = c("Price in Hamburg" = "price_H",
                                                                                                        "Price in Munich" = "price_M",
                                                                                                        "Price in Berlin" = "price_B",
                                                                                                        "Price in Cologne" = "price_K",
                                                                                                        "Price in Frankfurt" = "price_F")),
                                                                                selectInput("predictor_s_2", "Select Weather Predictor:",
                                                                                            choices = c("Temperature" = "lag_2_weeks_temp",
                                                                                                        "Humidity" = "lag_2_weeks_humidity",
                                                                                                        "Cloud Cover" = "lag_2_weeks_cloudcover",
                                                                                                        "Precipitation" = "lag_2_weeks_precip",
                                                                                                        "Wind Speed" = "lag_2_weeks_windspeed",
                                                                                                        "Deviation from normal Temperature" = "lag_2_weeks_dev_temp",
                                                                                                        "Deviation from normal Humidity" = "lag_2_weeks_dev_humidity",
                                                                                                        "Deviation from normal Cloud Cover" = "lag_2_weeks_dev_cloudcover",
                                                                                                        "Deviation from normal Precipitation" = "lag_2_weeks_dev_precip",
                                                                                                        "Deviation from normal Wind Speeds" = "lag_2_weeks_dev_windspeed")),
                                                                                selectInput("predictor_s_3", "Select Non-Weather Predictor:",
                                                                                            choices = c("World Container Index" = "wci",
                                                                                                        "Crude Oil Price" = "Crude_Oil_Price",
                                                                                                        "Kerosene Price" = "Kerosene_Price",
                                                                                                        "Diesel Fuel Price in Germany" = "fuel_price_diesel_Germany",
                                                                                                        "Triple superphosphate" = "TSP",
                                                                                                        "Di-ammonium Phosphate" = "DAP",
                                                                                                        "Urea Fertilizer" = "Urea",
                                                                                                        "Phosphorite" = "Phosphate_Rock",
                                                                                                        "Potassium Chloride" = "Potassium_chloride",
                                                                                                        "Exchange Rate South Africa" = "exchange_rate_South_Africa",
                                                                                                        "Exchange Rate Turkey" = "exchange_rate_Turkey",
                                                                                                        "Exchange Rate New Zealand" = "exchange_rate_New_Zealand",
                                                                                                        "Inflation in Germany" = "inflation_perc_Germany")),
                                                                                shinyWidgets::airDatepickerInput("onedate_s", "Select Date for Blue Separating Line:",
                                                                                                                 value = c("2020-01-01"),
                                                                                                                 minDate = min(g$date),
                                                                                                                 maxDate = max(g$date)),
                                                                                actionButton("show_multivariate_comparison", 
                                                                                             "More Info")
                                                     ),
                                                     mainPanel(plotOutput("single_item_ts_plot_1"),
                                                               plotOutput("single_item_ts_plot_2"),
                                                               plotOutput("single_item_ts_plot_3"))))
                                ), style = 'width: 950px; height: 500px'
                       )
                )
              )
      ),
      
      #### Econometric Analysis ####
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      tabItem(tabName = "econometric_analysis",
              fluidRow(
		tabBox(width = 12,
                	tabPanel("Regression Analysis", icon = icon("chart-line"),
                         #titlePanel("Select food"),
                         sidebarLayout(
                           sidebarPanel(style = "color: white; background-color: black; border-color: black", width = 3,
                                        uiOutput("foodtype_r"),
                                        uiOutput("food_r"),
                                        uiOutput("country_r"),
                                        uiOutput("type_r"),
                                        uiOutput("size_r"),
                                        #dateRangeInput(
                                        #"date_r",
                                        #"Date:",
                                        #min = min(l$date),
                                        #max = max(l$date),
                                        #start = min(l$date),
                                        #end = max(l$date),
                                        #startview = "year"
                                        #),
                                        shinyWidgets::airDatepickerInput("date_r", "Select Date:",
                                                                         range = TRUE,
                                                                         value = c(min(l$date), max(l$date)),
                                                                         minDate = min(l$date),
                                                                         maxDate = max(l$date)),
                                        selectInput(inputId = "dependentvar", 
                                                    label = "Select Dependent Variable:",
                                                    choices = c("Price in Frankfurt" = "price_F", 
                                                                "Price in Munich" = "price_M",
                                                                "Price in Berlin" = "price_B",
                                                                "Price in Hamburg" = "price_H",
                                                                "Price in Cologne" = "price_K"),
                                                    selected = "price_H"),
                                        selectInput(inputId = "independentvar", 
                                                    label = "Select Independent Variables:",
                                                    choices = colnames(predictor_reg),
                                                    selected = c("lag_3_months_temp", "lag_3_months_humidity",
                                                                 "lag_3_months_precip", "lag_3_months_cloudcover",
                                                                 "lag_3_months_windspeed"),
                                                    multiple = T),
                                        selectInput(inputId = "regform", 
                                                    label="Linear or Log-Log-Specification?",
                                                    choices=c("Log", "Linear"),
                                                    selected =c("Log")),
                                        actionButton("refresh_econometrics", "Refresh", icon = icon("sync"),
                                                     style="color: #fff; background-color: #e60b25; border-color: #2e6da4"),
                                        actionButton("show_regression_info", icon = icon("info"),
                                                     "Regression Info", style="color: #fff; background-color: #e60b25; border-color: #2e6da4")
                           ),
                           mainPanel(#style = color: white; background-color: black; border-color: black",
                                     dashboardBody(style = "color: white; background-color: black; border-color: black",
                                                   fluidRow(style = "color: white; background-color: black; border-color: black",
                                                            box(style = "color: white; background-color: black; border-color: black", title = "Regression table", uiOutput("lm1")),
                                                            DT::dataTableOutput("regtable")
                                                   )
                                     )
                           )
                         )
                ), tabPanel("Correlation Analysis", icon = icon("equals"),
                            plotOutput("plot"),
                            actionButton("show_correlation_info", 
                                         "Correlation Info"))
              ))),
      
		#### Machine Learning ####
		#%%%%%%%%%%%%%%%%%%%%%%%%#
		
		tabItem(tabName = "ml",
		        fluidRow(
		          tabBox(width = 12,
		                 tabPanel("Introduction", icon = icon("play"),
		                          column(6,(div(style = 'background:black;',
		                                        uiOutput("ml_intro"))))
		                 ),
		                 tabPanel("Theory", icon = icon("lightbulb"),
		                          tabsetPanel(
		                            tabPanel("Time Series Model", icon = icon("bookmark"),
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_time_series"))))
		                            ),
		                            tabPanel("Regularized Regression", icon = icon("bookmark"),
		                                     # box(style = 'width:2000px;overflow-x: scroll;height:1000px;overflow-y: scroll;',
		                                     #     uiOutput("theory_regularized_regression")
		                                     # )
		                                     # overflow-x: scroll
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_regularized_regression"))))
		                                     
		                            ), 
		                            tabPanel("Support Vector Machines", icon = icon("bookmark"),
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_svm"))))
		                            ),
		                            tabPanel("Random Forests", icon = icon("bookmark"),
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_rf"))))
		                            ),
		                            tabPanel("XGBoost", icon = icon("bookmark"),
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_xgboost"))))
		                            ),
		                            tabPanel("Neural Network", icon = icon("bookmark"),
		                                     column(6,(div(style = 'width:1000px;height:700px;overflow-y: scroll;',
		                                                   uiOutput("theory_nn"))))
		                            ),
		                          ), style = 'width: 800px; height: 500px'
		                 ),
		                 #### HYPERPARAMETERS ####
		                 tabPanel("Hyperparameters", icon = icon("subscript"),
		                          sidebarLayout(
		                            sidebarPanel(style = "color: white; background-color: black; border-color: black",
		                                         radioGroupButtons(
		                                           "params_model", "Select Model Type:",
		                                           choices = c("Full model", "Food model"),
		                                           selected = "Food model",
		                                           individual = TRUE,
		                                           checkIcon = list(
		                                             yes = icon("check-square"),
		                                             no = icon("square")
		                                           ),
		                                           width = "300px",
		                                         ),
		                                         conditionalPanel(
		                                           "input.params_model == 'Full model'",
		                                           selectInput(
		                                             "params_full_model", "Select Prediction Model:",
		                                             choices = sort(c(
		                                               "LASSO Regression" = "LASSO",
		                                               "Ridge Regression" = "RIDGE",
		                                               "Elastic Net Regression" = "ENET",
		                                               "SVM",
		                                               "Random Forests" = "Random_Forest",
		                                               "XGBoost",
		                                               "Neural Network" = "NN")),
		                                             selected = "LASSO", width = "300px"
		                                           ),
		                                           selectInput(
		                                             "params_full_market", "Select Market:",
		                                             choices = sort(c("Berlin", "Cologne", "Frankfurt",
		                                                              "Hamburg", "Munich")),
		                                             selected = NULL,
		                                             multiple = TRUE, width = "300px"
		                                           ),
		                                           switchInput(
		                                             "params_full_info", 
		                                             label = "Show Info on Hyperparameters",
		                                             value = FALSE,
		                                             onLabel = "ON", offLabel = "OFF",
		                                             onStatus = "danger", width = "150%"
		                                           ),
		                                           # tags$h6(style = "color:white;", 
		                                           #         HTML("<br> Please submit your selection by pressing the following <br> button. 
		                                           #              If you change the predictive model, you may click <br> the button twice 
		                                           #              in order to show the result. <br> <br>")), 
		                                           actionButton(
		                                             "params_show_full", 
		                                             "Show hyperparameters", 
		                                             icon("play-circle"), width = "300px",
		                                             style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                           )
		                                         ), # close conditionalPanel()
		                                         conditionalPanel(
		                                           "input.params_model == 'Food model'",
		                                           selectInput(
		                                             "params_food_model", "Select Prediction Model:",
		                                             choices = sort(c(
		                                               "LASSO Regression" = "LASSO",
		                                               "Ridge Regression" = "RIDGE",
		                                               "Elastic Net Regression" = "ENET",
		                                               "SVM",
		                                               "Random Forests" = "Random_Forest",
		                                               "XGBoost",
		                                               "Neural Network" = "NN")),
		                                             selected = "LASSO",
		                                             width = "300px"
		                                           ),
		                                           selectInput(
		                                             "params_food_food_model", "Select Fruit/Vegetable:",
		                                             choices = sort(unique(df_descr$food)),
		                                             selected = NULL,
		                                             multiple = TRUE, width = "300px"
		                                           ),
		                                           selectInput(
		                                             "params_food_market", "Select Market:",
		                                             choices = sort(c("Berlin", "Cologne", "Frankfurt",
		                                                              "Hamburg", "Munich")),
		                                             selected = NULL,
		                                             multiple = TRUE, width = "300px"
		                                           ),
		                                           switchInput(
		                                             "params_food_info", 
		                                             label = "Show Info on Hyperparameters",
		                                             value = FALSE,
		                                             onLabel = "ON", offLabel = "OFF",
		                                             onStatus = "danger", width = "100%"
		                                           ),
		                                           # tags$h6(style = "color:white;", 
		                                           #         HTML("<br> Please submit your selection by pressing the following <br> button. 
		                                           #              If you change the predictive model, you may click <br> the button twice 
		                                           #              in order to show the result. <br> <br>")), 
		                                           actionButton(
		                                             "params_show_food", "Show Hyperparameter Values", icon("play-circle"),
		                                             style = "color: black; background-color: #8bbeea; border-color: #8bbeea", 
		                                             width = "300px"
		                                           )
		                                         ) # close conditionalPanel
		                            ), # close sidebarPanel
		                            mainPanel(
		                              conditionalPanel(
		                                "input.params_model == 'Full model'",
		                                br(), 
		                                DT::dataTableOutput("hyperparameters_full_table", width = "600px"),
		                                htmlOutput("hyperparameters_full_info")
		                              ),
		                              conditionalPanel(
		                                "input.params_model == 'Food model'",
		                                br(), 
		                                DT::dataTableOutput("hyperparameters_food_table", width = "700px"),
		                                htmlOutput("hyperparameters_food_info")
		                              )
		                            ) # close mainPanel
		                          ) # close sidebarLayout
		                 ), # close tabPanel("Hyperparameters")
		                 
		                 #### PERFORMANCE MEASURES ####
		                 tabPanel("Predictive Performance", icon = icon("balance-scale-right"),
		                          tabsetPanel(
		                            tabPanel("Error Metrics Theory", icon = icon("bookmark"),
		                                     column(6, 
		                                            uiOutput("info_error_metrics"))),
		                            tabPanel("Error Metrics Calculation", icon = icon("calculator"),
		                                     sidebarLayout(
		                                       sidebarPanel(style = "color: white; background-color: black; border-color: black", ##BDB1B1", ##066556
		                                                    # tags$h6(style = "color:white;", 
		                                                    #         HTML("Note: The error metrics are displayed for the food model.
		                                                    #        You can compare them to the full model by selecting 'Show overall mean' under method. 
		                                                    #        Then, the comparison is provided within the 'Comparison Full Model' tab. <br> <br>")), 
		                                                    div(style = "margin-top:1em; margin-bottom:1em;",
		                                                        actionButton(
		                                                          "error_analysis_type", "Info Analysis Type",
		                                                          icon("info"), width = "300px", 
		                                                          style = "color: black; background-color: #8bbeea; border-color: #8bbeea")),
		                                                    selectInput("error_model", "Select Prediction Model:",
		                                                                choices = sort(
		                                                                  c("BSTS Baseline", "BSTS", 
		                                                                    "Naive Mean Prediction",
		                                                                    "Naive Median Prediction",
		                                                                    "LASSO Regression",
		                                                                    "Ridge Regression", 
		                                                                    "Elastic Net Regression",
		                                                                    "Support Vector Machines", 
		                                                                    "Random Forests",  
		                                                                    "XGBoost",
		                                                                    "Neural Network")),
		                                                                selected = NULL,
		                                                                width = "300px", 
		                                                                multiple = TRUE), 
		                                                    selectInput("error_market", "Select Market:",
		                                                                choices = c("Aggregate", 
		                                                                            "Berlin", "Cologne", 
		                                                                            "Frankfurt", "Hamburg",
		                                                                            "Munich"),
		                                                                selected = NULL, width = "300px", 
		                                                                multiple = TRUE),
		                                                    radioButtons("error_sel", "Select Analysis Type:",
		                                                                 choices = c("Select Fruit/Vegetable Manually", "Show Best", 
		                                                                             "Show Worst", "Show Overall Mean"),
		                                                                 width = "300px", 
		                                                                 selected = "Select food manually"),
		                                                    conditionalPanel(
		                                                      "input.error_sel == 'Select Fruit/Vegetable Manually'",
		                                                      radioButtons("error_food_manually",
		                                                                   "Select Aggregation Type:",
		                                                                   choices = c("Only Fruit/Vegetable", "Food-Type-Size-Country"),
		                                                                   selected = "Only Fruit/Vegetable"),
		                                                      conditionalPanel(
		                                                        "input.error_food_manually == 'Only Fruit/Vegetable'",
		                                                        selectInput(
		                                                          "error_food", "Select Fruit/Vegetable:",
		                                                          choices = sort(unique(df_descr$food)),
		                                                          selected = unique(df_descr$food)[1],
		                                                          multiple = TRUE, width = "300px"),
		                                                        actionButton("error_food_show",
		                                                                     "Show Error Metrics", icon("play-circle"), width = "300px", 
		                                                                     style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                      ),
		                                                      conditionalPanel(
		                                                        "input.error_food_manually == 'Food-Type-Size-Country'",
		                                                        selectInput(
		                                                          "error_food_detail", "Select Fruit/Vegetable:",
		                                                          choices = sort(unique(df_descr$food)),
		                                                          selected = NULL, width = "300px"),
		                                                        selectInput(
		                                                          "error_type", "Select Type:",
		                                                          choices = sort(unique(df_descr$type)),
		                                                          selected = NULL, width = "300px"),
		                                                        selectInput(
		                                                          "error_size", "Select Size:",
		                                                          choices = mixedsort(unique(df_descr$size)),
		                                                          selected = NULL, width = "300px"),
		                                                        selectInput(
		                                                          "error_country", "Select Country:",
		                                                          choices = sort(unique(df_descr$country)),
		                                                          selected = NULL, width = "300px"),
		                                                        actionButton("error_all_show",
		                                                                     "Show Error Metrics", icon("play-circle"), width = "300px", 
		                                                                     style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                      )
		                                                    ),
		                                                    conditionalPanel(
		                                                      "input.error_sel == 'Show Best'",
		                                                      chooseSliderSkin("Flat"),#, color = "#849192"),
		                                                      #setSliderColor(color = "#849192", sliderId = 0),
		                                                      radioButtons("error_best_sel", "Select Aggregation Type",
		                                                                   choices = c("Only Fruit/Vegetable", "Food-Type-Size-Country")),
		                                                      
		                                                      radioButtons("error_best_metric", "Select Error Metric:",
		                                                                   choices = c("RMSE", "MAE", "MAPE")),
		                                                      sliderInput("error_best", "Select Number of Fruits/Vegetables Displayed:", 
		                                                                  min = 1, max = df_descr[, c("food", "type", "size", "country")] %>% distinct() %>% nrow(), 
		                                                                  #max = length(unique(df_descr$food)), 
		                                                                  value = 5, step = 1, width = "300px"),
		                                                      actionButton("error_best_show", width = "300px", 
		                                                                   "Show Error Metrics", icon("play-circle"),
		                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                    ),
		                                                    conditionalPanel(
		                                                      "input.error_sel == 'Show Worst'",
		                                                      radioButtons("error_worst_sel", "Select Aggregation Type",
		                                                                   choices = c("Only Fruit/Vegetable", "Food-Type-Size-Country")),
		                                                      radioButtons("error_worst_metric", "Select Error Metric:",
		                                                                   choices = c("RMSE", "MAE", "MAPE")),
		                                                      sliderInput("error_worst", "Select Number of Fruits/Vegetables Displayed:",
		                                                                  min = 1, max = df_descr[, c("food", "type", "size", "country")] %>% distinct() %>% nrow(), 
		                                                                  #max = length(unique(df_descr$food)), 
		                                                                  value = 5, step = 1, width = "300px"),
		                                                      actionButton("error_worst_show",
		                                                                   "Show Error Metrics", icon("play-circle"), width = "300px", 
		                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                    ),
		                                                    conditionalPanel(
		                                                      "input.error_sel == 'Show Overall Mean'",
		                                                      actionButton("error_overall_show",
		                                                                   "Show error metrics", icon("play-circle"), width = "300px", 
		                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                    )
		                                       ),
		                                       ## mainPanel ##
		                                       mainPanel(
		                                         conditionalPanel(
		                                           "input.error_sel == 'Show Best'",
		                                           br(), 
		                                           DT::dataTableOutput("error_metrics_best", width = "1000px")
		                                         ),
		                                         conditionalPanel(
		                                           "input.error_sel == 'Show Worst'",
		                                           br(), 
		                                           DT::dataTableOutput("error_metrics_worst", width = "1000px")
		                                         ),
		                                         conditionalPanel(
		                                           "input.error_sel  == 'Select Fruit/Vegetable Manually'", 
		                                           tabsetPanel(
		                                             tabPanel("Table",
		                                                      # output depends on previous selection
		                                                      conditionalPanel(
		                                                        "input.error_sel == 'Select Fruit/Vegetable Manually'",
		                                                        conditionalPanel(
		                                                          "input.error_food_manually == 'Only Fruit/Vegetable'",
		                                                          br(), 
		                                                          fluidRow(column(width = 8,
		                                                                          DT::dataTableOutput(
		                                                                            "error_metrics_food",
		                                                                            width = "600px")))
		                                                        ),
		                                                        conditionalPanel(
		                                                          "input.error_food_manually == 'Food-Type-Size-Country'",
		                                                          br(), 
		                                                          htmlOutput("error_metrics_all_header"), br(), 
		                                                          DT::dataTableOutput("error_metrics_all", width = "600px")
		                                                        )
		                                                      )
		                                             ), # close tabPanel("Table")
		                                             tabPanel("Plot", 
		                                                      # output depends on previous selection
		                                                      conditionalPanel(
		                                                        "input.error_sel == 'Select Fruit/Vegetable Manually'",
		                                                        conditionalPanel(
		                                                          "input.error_food_manually == 'Only Fruit/Vegetable'",
		                                                          br(), 
		                                                          fluidRow(column(width = 12,
		                                                                          plotOutput(
		                                                                            "error_metrics_food_plot",
		                                                                            width = '100%'
		                                                                            #width = "600px", height = "400px"
		                                                                          )))
		                                                        ),
		                                                        conditionalPanel(
		                                                          "input.error_food_manually == 'Food-Type-Size-Country'",
		                                                          br(), 
		                                                          plotOutput("error_metrics_all_plot", width = '100%')
		                                                        )
		                                                      )
		                                             )#,
		                                             #tabPanel("Comparison Full Model")
		                                           ) # close tabSetPanel()
		                                         ), # close conditionalPanel for select food manually
		                                         conditionalPanel(
		                                           "input.error_sel  == 'Show Overall Mean'", 
		                                           tabsetPanel(
		                                             tabPanel("Table",
		                                                      br(), 
		                                                      fluidRow(column(width = 8,
		                                                                      DT::dataTableOutput("error_metrics_overall")
		                                                      ))
		                                             ), # close tabPanel("Table")
		                                             tabPanel("Comparison Full Model",
		                                                      br(), 
		                                                      fluidRow(column(width = 8,
		                                                                      plotlyOutput("error_metrics_overall_plot_comp_1") %>%
		                                                                        withSpinner(color = "#0dc5c1", type = 6))),
		                                                      fluidRow(column(width = 8, br(), 
		                                                                      plotlyOutput("error_metrics_overall_plot_comp_2") %>%
		                                                                        withSpinner(color = "#0dc5c1", type = 6)))
		                                             )
		                                           ) # close tabSetPanel()
		                                         ) # close conditionalPanel for overall error metrocs
		                                       ) # close mainPanel()
		                                     ) # close sidebarLayout()
		                            ) # close tabPanel("Calculation Error Metrics")
		                          ) # close tabSetPanel()
		                 ), # close tabPanel() for performance measures
		                 
		                 #### Feature Importance ####
		                 tabPanel("Feature Importance", icon = icon("star"),
		                          tabsetPanel(
		                            tabPanel("Feature Importance Theory", icon = icon("bookmark"),
		                                     column(6,
		                                            uiOutput("imp_info"))),
		                            tabPanel("Feature Importance Plot", icon = icon("image"),
		                                     sidebarLayout(
		                                       sidebarPanel(style = "color: white; background-color: black; border-color: black", #BDB1B1",
		                                                    radioGroupButtons("imp_model", "Select algorithm:",
		                                                                      choices = c("XGBoost", "LASSO"),
		                                                                      selected = "XGBoost",
		                                                                      individual = TRUE,
		                                                                      checkIcon = list(
		                                                                        yes = icon("check-square"),
		                                                                        no = icon("square")
		                                                                      )),
		                                                    conditionalPanel(
		                                                      "input.imp_model == 'XGBoost'",
		                                                      selectInput("imp_food_xgb", "Select Fruit/Vegetable:",
		                                                                  choices = sort(unique(df_descr$food)),
		                                                                  selected = unique(df_descr$food)[1],
		                                                                  multiple = FALSE, width = "300px"),
		                                                      selectInput("imp_market_xgb", "Select Market:",
		                                                                  choices = c("Berlin" = "price_B",
		                                                                              "Cologne" = "price_K",
		                                                                              "Frankfurt" = "price_F", 
		                                                                              "Hamburg" = "price_H",
		                                                                              "Munich" = "price_M"),
		                                                                  width = "300px"),
		                                                      sliderInput("imp_features_xgb", "Select Number of Features:",
		                                                                  min = 5, max = 30, value = 10, step = 1, width = "300px"),
		                                                      radioButtons("imp_type_xgb", "Select Importance Type:",
		                                                                   choices = c("Gain", "Permutation"),
		                                                                   selected = "Gain",
		                                                                   inline = TRUE),
		                                                      actionButton("imp_show_xgb", "Show Feature Importance Plot",
		                                                                   icon("play-circle"),  width = "300px",
		                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea"),
		                                                      conditionalPanel(
		                                                        "input.imp_type_xgb == 'Permutation'",
		                                                        tags$h6(HTML("<br> Note: The permutation based feature importance has a 
                                                                         <br> long computation time. Depending on the selected fruit 
                                                                         <br> or rather vegetable it takes between 1 and 20 minutes. <br>
                                                                           Please click on the following button to get an idea about <br>
                                                                           the computation time of each fruit and vegetable.")),
		                                                        div(style = "margin-top:2em; margin-bottom:1em;",
		                                                            actionButton("imp_show_time", "Info Computation Time",
		                                                                         icon("info"),  width = "300px",
		                                                                         style = "color: black; background-color: #8bbeea; border-color: #8bbeea")),
		                                                      )
		                                                    ), # close conditionalPanel()
		                                                    conditionalPanel(
		                                                      "input.imp_model == 'LASSO'",
		                                                      selectInput("imp_food_lasso", "Select Fruit/Vegetable:",
		                                                                  choices = sort(unique(df_descr$food)),
		                                                                  selected = unique(df_descr$food)[1],
		                                                                  multiple = FALSE, width = "300px",),
		                                                      selectInput("imp_market_lasso", "Select Market:",
		                                                                  choices = c("Berlin" = "price_B",
		                                                                              "Cologne" = "price_K",
		                                                                              "Frankfurt" = "price_F", 
		                                                                              "Hamburg" = "price_H",
		                                                                              "Munich" = "price_M"),
		                                                                  width = "300px",),
		                                                      sliderInput("imp_features_lasso", "Select Number of Features:",
		                                                                  min = 5, max = 30, value = 10, step = 1, 
		                                                                  width = "300px",),
		                                                      radioButtons("imp_type_lasso", "Select Importance Type:",
		                                                                   choices = c("Zero Coefficients", "Non-Zero Coefficients"),
		                                                                   selected = "Non-Zero Coefficients",
		                                                                   inline = TRUE),
		                                                      actionButton("imp_show_lasso", "Show Feature Importance Plot",
		                                                                   icon("play-circle"), width = "300px", 
		                                                                   style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                                                      #uiOutput("imp_show_lasso")
		                                                      # conditionalPanel(
		                                                      #   "input.imp_type_lasso = 'zero coefficients'",
		                                                      #   actionButton("imp_show_lasso_table", "Show coefficients set to zero",
		                                                      #                icon("signal"), width = "300px", 
		                                                      #                style = "color: white; background-color: black; border-color: white")
		                                                      # ),
		                                                      # conditionalPanel(
		                                                      #   "input.imp_type_lasso = 'non-zero coefficients'",
		                                                      #   actionButton("imp_show_lasso_plot", "Show Feature Importance Plot",
		                                                      #                icon("signal"), width = "300px",
		                                                      #                style = "color: white; background-color: black; border-color: white")
		                                                      # )
		                                                    ) # close conditionalPanel() for LASSO
		                                       ), # close sidebarPanel
		                                       mainPanel(
		                                         column(width = 12,
		                                                conditionalPanel(
		                                                  "input.imp_model == 'XGBoost'",
		                                                  plotOutput("imp_plot_xgb", width = "100%") %>%
		                                                    withSpinner(color = "#0dc5c1", type = 6)
		                                                ),
		                                                conditionalPanel(
		                                                  "input.imp_model == 'LASSO'",
		                                                  conditionalPanel(
		                                                    "input.imp_type_lasso = 'zero coefficients'",
		                                                    DT::dataTableOutput("imp_lasso_table") %>%
		                                                      withSpinner(color = "#0dc5c1", type = 6)
		                                                  ),
		                                                  conditionalPanel(
		                                                    "input.imp_type_lasso = 'non-zero coefficients'",
		                                                    plotOutput("imp_lasso_plot") %>%
		                                                      withSpinner(color = "#0dc5c1", type = 6)
		                                                  )
		                                                )
		                                         )
		                                         
		                                         
		                                       ) # close mainPanel()
		                                     ) # close sidebarLayout()
		                            ) # close tabPanel("Feature Importance plot")
		                          )), # close tabsetPanel and tabPanel()
		                 #### PRICE PREDICTION ####
		                 tabPanel("Price Prediction", icon = icon("euro-sign"),
		                          # fluidRow(div(style = "margin-top:1em;margin-bottom:1em", # more space between user inputs # font-size: 10px
		                          #              column(width = 6,
		                          #                     useShinyjs(),
		                          #                     actionButton("pred_type_explanation", "Info Prediction Type",
		                          #                                  icon("info"),
		                          #                                  style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
		                          #              ))),
		                          tabsetPanel(
		                            tabPanel("Price Prediction Info", icon = icon("info"),
		                                     column(6, uiOutput("price_prediction_info"))
		                            ),
		                            #### best model ####
		                            tabPanel("Find Best Model", icon = icon("trophy"),
		                                     splitLayout(cellWidths = c("35%", "65%"),
		                                                 fluidPage(
		                                                   div(style = "margin-top:1em; margin-bottom:1em;",
		                                                       actionButton("find_best_info", 
		                                                                    "Info", 
		                                                                    icon("info"), width = "430px",
		                                                                    style = "color: black; background-color: #8bbeea; border-color: #8bbeea")),
		                                                   radioGroupButtons(
		                                                     "find_best_sel", "Select Method:",
		                                                     choices = c("Overall", "Only Fruit/Vegetable", "Food-Type-Size-Country"),
		                                                     selected = "Only Fruit/Vegetable",
		                                                     individual = TRUE,
		                                                     checkIcon = list(
		                                                       yes = icon("check-square"),
		                                                       no = icon("square")
		                                                     )
		                                                   ),
		                                                   radioGroupButtons(
		                                                     "find_best_models", "Include Naive Predictions?",
		                                                     choices = c("Yes", "No"),
		                                                     selected = "No",
		                                                     individual = TRUE,
		                                                     checkIcon = list(
		                                                       yes = icon("check-square"),
		                                                       no = icon("square")
		                                                     )
		                                                   ),
		                                                   radioGroupButtons(
		                                                     "find_best_models_bsts", "Include Time Series Models?",
		                                                     choices = c("Yes", "No"),
		                                                     selected = "No",
		                                                     individual = TRUE,
		                                                     checkIcon = list(
		                                                       yes = icon("check-square"),
		                                                       no = icon("square")
		                                                     )
		                                                   ),
		                                                   selectInput(
		                                                     "find_best_market", "Select Market:",
		                                                     choices = c("Aggregate", "Berlin", "Cologne", "Frankfurt", "Hamburg", "Munich"),
		                                                     selected = "Aggregate", width = "430px"
		                                                   ), 
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Overall'",
		                                                     radioButtons("find_best_worst_overall", "Show best or worst:",
		                                                                  choices = c("Best", "Worst"), inline = TRUE,
		                                                                  selected = "Best"),
		                                                     radioButtons("find_best_overall_error", "Select error metric for ranking:",
		                                                                  choices = c("RMSE", "MAE", "MAPE"), inline = TRUE, 
		                                                                  selected = "RMSE"
		                                                     ),
		                                                     #conditionalPanel(
		                                                     #"input.find_best_models == 'No'",
		                                                     sliderInput("find_best_overall", "Select Number of Models to be Displayed:",
		                                                                 min = 1, max = 7, value = 1, step = 1, width = "430px"),
		                                                     actionButton("find_best_show", "Display Models",
		                                                                  icon("play-circle"), width = "430px",
		                                                                  style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                                     )
		                                                     #),
		                                                     # conditionalPanel(
		                                                     #   "input.find_best_models == 'Yes'",
		                                                     #   sliderInput("find_best_overall", "Select number of models displayed:",
		                                                     #               min = 1, max = 11, value = 1, step = 1)
		                                                     # )
		                                                   ),
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Only Fruit/Vegetable'",
		                                                     selectInput(
		                                                       "find_best_food", "Select Fruit/Vegetable:", 
		                                                       choices = sort(unique(df_descr$food)),
		                                                       selected = sort(unique(df_descr$food))[1],
		                                                       width = "430px"
		                                                     ),
		                                                     radioButtons("find_best_worst", "Show Best or Worst:",
		                                                                  choices = c("Best", "Worst"),
		                                                                  selected = "Best"),
		                                                     radioButtons("find_best_food_error", "Select Error Metric for Ranking:",
		                                                                  choices = c("RMSE", "MAE", "MAPE"), inline = TRUE, 
		                                                                  selected = "RMSE"
		                                                     ),
		                                                     #conditionalPanel(
		                                                     #"input.find_best_models == 'No'",
		                                                     sliderInput("find_best_food_num", "Select Number of Models to be Displayed:",
		                                                                 min = 1, max = 9, value = 1, step = 1, width = "430px"),
		                                                     actionButton("find_best_show_food", "Display Models",
		                                                                  icon("play-circle"), width = "430px",
		                                                                  style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                                     )
		                                                     #),
		                                                     # conditionalPanel(
		                                                     #   "input.find_best_models == 'Yes'",
		                                                     #   sliderInput("find_best_food_num", "Select number of models displayed:",
		                                                     #               min = 1, max = 11, value = 1, step = 1)
		                                                     # )
		                                                   ),
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Food-Type-Size-Country'",
		                                                     selectInput(
		                                                       "find_best_food_detail", "Select Fruit/Vegetable:", 
		                                                       choices = sort(unique(df_descr$food)),
		                                                       selected = sort(unique(df_descr$food))[1],
		                                                       width = "430px",
		                                                     ),
		                                                     selectInput(
		                                                       "find_best_food_type", "Select Type:", 
		                                                       choices = sort(unique(df_descr$type)),
		                                                       selected = sort(unique(df_descr$type))[1],
		                                                       width = "430px",
		                                                     ),
		                                                     selectInput(
		                                                       "find_best_food_size", "Select Size:", 
		                                                       choices = mixedsort(unique(df_descr$size)),
		                                                       selected = mixedsort(unique(df_descr$size))[1],
		                                                       width = "430px",
		                                                     ),
		                                                     selectInput(
		                                                       "find_best_food_country", "Select Country:", 
		                                                       choices = sort(unique(df_descr$country)),
		                                                       selected = sort(unique(df_descr$country))[1],
		                                                       width = "430px",
		                                                     ),
		                                                     radioButtons("find_best_worst_detail", "Show Best or Worst:",
		                                                                  choices = c("Best", "Worst"),
		                                                                  selected = "Best"),
		                                                     radioButtons("find_best_food_detail_error", "Select Error Metric for Ranking:",
		                                                                  choices = c("RMSE", "MAE", "MAPE"), inline = TRUE, 
		                                                                  selected = "RMSE"
		                                                     ),
		                                                     # conditionalPanel(
		                                                     #   "input.find_best_models == 'No'",
		                                                     sliderInput("find_best_food_detail_num", "Select Number of Models to be Displayed:",
		                                                                 min = 1, max = 9, value = 1, step = 1, width = "430px"),
		                                                     actionButton("find_best_show_food_detail", "Display models",
		                                                                  icon("play-circle"), width = "430px",
		                                                                  style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                                     )
		                                                     # ),
		                                                     # conditionalPanel(
		                                                     #   "input.find_best_models == 'Yes'",
		                                                     #   sliderInput("find_best_food_detail_num", "Select number of models displayed:",
		                                                     #               min = 1, max = 11, value = 1, step = 1)
		                                                     # )
		                                                   )
		                                                 ), # close fluidPage()
		                                                 fluidPage(
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Overall'",
		                                                     tableOutput("table_best_overall")
		                                                   ),
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Only Fruit/Vegetable'",
		                                                     tableOutput("table_best_food")
		                                                   ),
		                                                   conditionalPanel(
		                                                     "input.find_best_sel == 'Food-Type-Size-Country'",
		                                                     tableOutput("table_best_food_detail")
		                                                   )
		                                                 )
		                                     ) # close splitLayout
		                            ), # close tabPanel("Find Best Model")
		                            #### single comparison ####
		                            tabPanel("Single Comparison", icon = icon("user"),
		                                     splitLayout(cellWidths = c("35%", "65%"), # define width of cells
		                                                 fluidPage(
		                                                   div(style = "margin-top:1em",
		                                                       selectInput(
		                                                         "pred_model_year", "Select Prediction Model:",
		                                                         choices = c(
		                                                           "BSTS Baseline", "BSTS", 
		                                                           "LASSO Regression" = "LASSO",
		                                                           "Ridge Regression" = "RIDGE", 
		                                                           "Elastic Net Regression" = "ENET",
		                                                           "Support Vector Machines " = "SVM",
		                                                           "Random Forests" = "Random_Forest",
		                                                           "XGBoost",
		                                                           "Neural Network" = "NN"
		                                                         ),
		                                                         selected = "LASSO",
		                                                         multiple = FALSE
		                                                       )),
		                                                   selectInput(
		                                                     "pred_price_year", "Select Market:",
		                                                     choices = c("Berlin" = "price_B",
		                                                                 "Cologne" = "price_K",
		                                                                 "Frankfurt" = "price_F",
		                                                                 "Hamburg" = "price_H",
		                                                                 "Munich" = "price_M"),
		                                                     selected = "price_B",
		                                                     multiple = FALSE
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_sel_year", "Select Fruit/Vegetable:",
		                                                     choices = sort(unique(df_descr$food)),
		                                                     selected = sort(unique(df_descr$food))[1],
		                                                     multiple = FALSE
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_type_year", "Select Type:",
		                                                     choices = sort(unique(df_descr$type)),
		                                                     selected = unique(df_descr$type)[1],
		                                                     multiple = FALSE
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_size_year", "Select Size:",
		                                                     choices = mixedsort(unique(df_descr$size)),
		                                                     selected = unique(df_descr$size)[1],
		                                                     multiple = FALSE
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_country_year", "Select Country:",
		                                                     choices = sort(unique(df_descr$country)),
		                                                     selected = unique(df_descr$country)[1],
		                                                     multiple = FALSE
		                                                   ),
		                                                   sliderInput("pred_horizon_year", "Select Number of Weeks:",
		                                                               min = 1, max = 4, value = 2, step = 1),
		                                                   actionButton(
		                                                     "pred_show_year", "Show prediction", icon("play-circle"), width = "300px",
		                                                     style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                                   ),
		                                                   div(tags$h6(HTML("<br> <b>User information:</b> <br> <br> The prices displayed are for 1kg/food. 
                                                         For iceberg lettuce, <br> endive, and cauliflower there are for one piece.")),
		                                                       style = "color:darkgrey;")
		                                                 ),
		                                                 fluidPage(
		                                                   tabsetPanel(
		                                                     tabPanel("Table", 
		                                                              div(style = "margin-top:1em; margin-left:-0.2em;",
		                                                                  radioGroupButtons(
		                                                                    "pred_single_comp_model_table_all", 
		                                                                    "Display all Predictions from 2016 onward?",
		                                                                    choices = c("Yes", "No"), selected = "No",
		                                                                    individual = TRUE, status = "primary",
		                                                                    checkIcon = list(
		                                                                      yes = icon("check-square"),
		                                                                      no = icon("square")
		                                                                    ))),
		                                                              br(),
		                                                              uiOutput("pred_table_2016_text"), br(), 
		                                                              DT::dataTableOutput(
		                                                                "pred_table_2016"
		                                                              )  %>% 
		                                                                withSpinner(color = "#0dc5c1", type = 6)#, 
		                                                              # br(), 
		                                                              # span(textOutput("pred_table_2016_info"),
		                                                              #      style = "color:darkgrey")
		                                                     ),
		                                                     tabPanel("Plot",
		                                                              fluidRow(column(
		                                                                width = 12, 
		                                                                div(style = "margin-top:1em; margin-left:-0.5em;",
		                                                                    radioGroupButtons(
		                                                                      "pred_single_comp_model_error", "Display Error Metrics?",
		                                                                      choices = c("Yes", "No"), selected = "No",
		                                                                      individual = TRUE, status = "primary",
		                                                                      checkIcon = list(
		                                                                        yes = icon("check-square"),
		                                                                        no = icon("square")
		                                                                      )))
		                                                              )),
		                                                              fluidRow(
		                                                                column(width = 12,
		                                                                       br(), plotlyOutput("pred_plot_2016", 
		                                                                                          width = 1000, height = 600)  %>% 
		                                                                         withSpinner(color = "#0dc5c1", type = 6)
		                                                                )
		                                                              ),
		                                                              fluidRow(
		                                                                column(width = 12, 
		                                                                       br(), 
		                                                                       span(uiOutput("pred_plot_2016_info"),
		                                                                            style =  "color:darkgrey"))
		                                                              ), 
		                                                              fluidRow(
		                                                                column(width = 4,
		                                                                       br(), br(), textOutput("pred_error_2016_text")
		                                                                ),
		                                                                column(width = 4,
		                                                                       br(), br(), textOutput("pred_error_text")
		                                                                )
		                                                              ),
		                                                              fluidRow(
		                                                                column(width = 4,
		                                                                       br(), tableOutput("pred_error_2016")),
		                                                                column(width = 4,
		                                                                       br(), tableOutput("pred_error"))
		                                                                
		                                                              )#,
		                                                              # fluidRow(
		                                                              #   column(width = 8,
		                                                              #          br(), textOutput("pred_error_text")
		                                                              #   )
		                                                              # ),
		                                                              # fluidRow(
		                                                              #   column(width = 8,
		                                                              #          br(), tableOutput("pred_error")
		                                                              #   )
		                                                              # )
		                                                     ) # close tabPanel for plot
		                                                   ) # close tabsetPanel() for Output
		                                                 ) # close fluidPage() from Output
		                                     ) # close splitLayout()
		                            ), # close inside tabPanel()
		                            #### market comparison ####
		                            tabPanel("Market Comparison", icon = icon("map-marked"),
		                                     splitLayout(cellWidths = c("35%", "65%"), # define width of cells
		                                                 fluidPage(
		                                                   div(style = "margin-top:1em",
		                                                       selectInput(
		                                                         "pred_model_market", "Select Prediction Model:",
		                                                         choices = c(
		                                                           "BSTS Baseline", "BSTS", 
		                                                           "LASSO Regression" = "LASSO",
		                                                           "Ridge Regression" = "RIDGE", 
		                                                           "Elastic Net Regression" = "ENET",
		                                                           "Support Vector Machines " = "SVM",
		                                                           "Random Forests" = "Random_Forest",
		                                                           "XGBoost",
		                                                           "Neural Network" = "NN"
		                                                         ),
		                                                         selected = "LASSO",
		                                                         multiple = FALSE, width = "400px" 
		                                                       )),
		                                                   selectInput(
		                                                     "pred_price_market", "Select Market:",
		                                                     choices = c("Berlin" = "price_B", 
		                                                                 "Cologne" = "price_K", 
		                                                                 "Frankfurt" = "price_F", 
		                                                                 "Hamburg" = "price_H",
		                                                                 "Munich" = "price_M"),
		                                                     selected = NULL,
		                                                     multiple = TRUE, width = "400px"
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_sel_market", "Select Fruit/Vegetable:",
		                                                     choices = sort(unique(df_descr$food)),
		                                                     selected = NULL,
		                                                     #selected = sort(unique(df_descr$food))[1],
		                                                     multiple = FALSE, width = "400px" 
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_type_market", "Select Type:",
		                                                     choices = sort(unique(df_descr$type)),
		                                                     selected = unique(df_descr$type)[1],
		                                                     multiple = FALSE, width = "400px" 
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_size_market", "Select Size:",
		                                                     choices = mixedsort(unique(df_descr$size)),
		                                                     selected = unique(df_descr$size)[1],
		                                                     multiple = FALSE, width = "400px" 
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_country_market", "Select Country:",
		                                                     choices = sort(unique(df_descr$country)),
		                                                     selected = unique(df_descr$country)[1],
		                                                     multiple = FALSE, width = "400px" 
		                                                   ),
		                                                   sliderInput("pred_horizon_market", "Select Number of Weeks:",
		                                                               min = 1, max = 4, value = 2, step = 1, width = "400px" ),
		                                                   actionButton(
		                                                     "pred_show_market", "Show prediction", icon("play-circle"),
		                                                     style = "color: black; background-color: #8bbeea; border-color: #8bbeea",
		                                                     width = "400px" 
		                                                   ),
		                                                   div(tags$h6(HTML("<br> <b>User information:</b> <br> <br> The prices displayed are for 1kg/food. <br> For iceberg lettuce,
                                                                 endive, and cauliflower there are for one piece. <br> <br>",
		                                                                    "The date mentioned in the table and plot refers to the Monday of the
                                                                      respective <br> calender week; the prices, however, apply for the total week.")),
		                                                       style = "color:darkgrey;")
		                                                 ), # close fluidPage()
		                                                 fluidPage(
		                                                   tabsetPanel(
		                                                     tabPanel("Table",
		                                                              column(width = 10,
		                                                                     br(), htmlOutput("pred_header_market"), br(), br()),
		                                                              column(width = 10,
		                                                                     DT::dataTableOutput("pred_table_market") %>% 
		                                                                       withSpinner(color = "#0dc5c1", type = 6),
		                                                                     br()),
		                                                              column(width = 12, 
		                                                                     plotOutput("pred_bar_plot_market", 
		                                                                                width = '100%', height = '100%') %>%
		                                                                       withSpinner(color = "#0dc5c1", type = 6))
		                                                     ), 
		                                                     tabPanel("Plot",
		                                                              br(), 
		                                                              splitLayout(cellWidths = c("25%", "25%"),
		                                                                          radioGroupButtons(
		                                                                            "pred_line_plot_market_sel_button", "Display True Values?",
		                                                                            choices = c("Yes", "No"), selected = "Yes",
		                                                                            individual = TRUE, status = "primary",
		                                                                            checkIcon = list(
		                                                                              yes = icon("check-square"),
		                                                                              no = icon("square")
		                                                                            )),
		                                                                          radioGroupButtons(
		                                                                            "pred_line_plot_market_sel_error", "Display Error Metrics?",
		                                                                            choices = c("Yes", "No"), selected = "No",
		                                                                            individual = TRUE, status = "primary",
		                                                                            checkIcon = list(
		                                                                              yes = icon("check-square"),
		                                                                              no = icon("square")
		                                                                            ))
		                                                              ),
		                                                              fluidRow(column(
		                                                                width = 12, 
		                                                                br(), 
		                                                                plotlyOutput("pred_line_plot_market", 
		                                                                             height = '100%', 
		                                                                             width = '100%') %>%
		                                                                  withSpinner(color = "#0dc5c1", type = 6))
		                                                              ),
		                                                              fluidRow(column(
		                                                                width = 12, 
		                                                                br(), span(uiOutput("pred_line_plot_market_info"),
		                                                                           style = "color:darkgrey;"), br(), br()
		                                                              )),
		                                                              fluidRow(
		                                                                column(width = 4, uiOutput("pred_line_plot_market_error_text_1")),
		                                                                column(width = 4, offset = 1, uiOutput("pred_line_plot_market_error_text_2"))
		                                                              ),
		                                                              fluidRow(
		                                                                column(width = 4, br(), 
		                                                                       tableOutput("pred_line_plot_market_error_1")),
		                                                                column(width = 4, offset = 1, br(), 
		                                                                       tableOutput("pred_line_plot_market_error_2"))
		                                                              )
		                                                     )
		                                                   )# close tabsetPanel() of output
		                                                 ) # close fluidPage()
		                                     ) # close splitLayout()
		                            ), # close tabPanel()
		                            #### model comparison ####
		                            tabPanel("Model Comparison", icon = icon("users"),
		                                     splitLayout(cellWidths = c("35%", "65%"), # define width of cells
		                                                 fluidPage(
		                                                   div(style = "margin-top:1em",
		                                                       selectizeInput(
		                                                         "pred_model_model", "Select at most 4 Prediction Models:",
		                                                         choices = c(
		                                                           "BSTS Baseline", "BSTS", 
		                                                           "LASSO Regression" = "LASSO",
		                                                           "Ridge Regression" = "RIDGE", 
		                                                           "Elastic Net Regression" = "ENET",
		                                                           "Support Vector Machines " = "SVM",
		                                                           "Random Forests" = "Random_Forest",
		                                                           "XGBoost",
		                                                           "Neural Network" = "NN"
		                                                         ),
		                                                         selected = NULL,
		                                                         multiple = TRUE,
		                                                         options = list(maxItems = 4),
		                                                         width = "400px"
		                                                       )),
		                                                   selectInput(
		                                                     "pred_price_model", "Select Market:",
		                                                     choices = c("Berlin" = "price_B",
		                                                                 "Cologne" = "price_K",
		                                                                 "Frankfurt" = "price_F",
		                                                                 "Hamburg" = "price_H",
		                                                                 "Munich" = "price_M"),
		                                                     selected = "price_B",
		                                                     multiple = FALSE, width = "400px"
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_sel_model", "Select Fruit/Vegetable:",
		                                                     choices = sort(unique(df_descr$food)),
		                                                     selected = sort(unique(df_descr$food))[1],
		                                                     multiple = FALSE, width = "400px"
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_type_model", "Select Type:",
		                                                     choices = unique(df_descr$type),
		                                                     selected = unique(df_descr$type)[1],
		                                                     multiple = FALSE, width = "400px"
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_size_model", "Select Size:",
		                                                     choices = mixedsort(unique(df_descr$size)),
		                                                     selected = unique(df_descr$size)[1],
		                                                     multiple = FALSE, width = "400px"
		                                                   ),
		                                                   selectInput(
		                                                     "pred_food_country_model", "Select Country:",
		                                                     choices = unique(df_descr$country),
		                                                     selected = unique(df_descr$country)[1],
		                                                     multiple = FALSE, width = "400px"
		                                                   ),
		                                                   sliderInput("pred_horizon_model", "Select Number of Weeks:",
		                                                               min = 1, max = 4, value = 2, step = 1,
		                                                               width = "400px"),
		                                                   # checkboxGroupInput(
		                                                   #   inputId = "display_sel_model",
		                                                   #   label = "Select display options:",
		                                                   #   choices = c("Table", "Bar Plot", "Line Plot"),
		                                                   #   selected = c("Table", "Bar Plot"),
		                                                   #   inline = TRUE
		                                                   # ),
		                                                   actionButton(
		                                                     "pred_show_model", "Show Prediction", icon("play-circle"),
		                                                     width = "400px",
		                                                     style = "color: black; background-color: #8bbeea; border-color: #8bbeea"
		                                                   ),
		                                                   div(tags$h6(HTML("<br> <b>User information:</b> <br> <br> The prices displayed are for 1kg/food. <br> For iceberg lettuce,
                                                                 endive, and cauliflower there are for one piece. <br> <br>",
		                                                                    "The date mentioned in the table and plot refers to the Monday of the
                                                                      respective <br> calender week; the prices, however, apply for the total week.")),
		                                                       style = "color:darkgrey;")
		                                                 ), # close fluidPage()
		                                                 fluidPage(
		                                                   tabsetPanel(
		                                                     tabPanel("Table",
		                                                              column(width = 12, br(), 
		                                                                     htmlOutput("pred_header_model"), br(), br()),
		                                                              column(width = 12,
		                                                                     DT::dataTableOutput("pred_table_model") %>%
		                                                                       withSpinner(color = "#0dc5c1", type = 6),
		                                                                     br()),
		                                                              column(width = 12, plotOutput("pred_bar_plot_model",
		                                                                                            width = '100%',
		                                                                                            height = '100%') %>%
		                                                                       withSpinner(color = "#0dc5c1", type = 6))
		                                                     ),
		                                                     tabPanel("Plot",
		                                                              fluidRow(column(
		                                                                width = 12, 
		                                                                div(style = "margin-top:1em; margin-left:-0.7em;",
		                                                                    radioGroupButtons(
		                                                                      "pred_line_plot_model_error", "Display Error Metrics?",
		                                                                      choices = c("Yes", "No"), selected = "No",
		                                                                      individual = TRUE, status = "primary",
		                                                                      checkIcon = list(
		                                                                        yes = icon("check-square"),
		                                                                        no = icon("square")
		                                                                      )))
		                                                              )),
		                                                              fluidRow(column(
		                                                                width = 12, br(), 
		                                                                plotlyOutput("pred_line_plot_model", 
		                                                                             height = '100%', 
		                                                                             width = '100%') %>%
		                                                                  withSpinner(color = "#0dc5c1", type = 6))
		                                                              ),
		                                                              fluidRow(column(
		                                                                width = 12, 
		                                                                br(), span(uiOutput("pred_line_plot_model_info"),
		                                                                           style = "color:darkgrey;"), br(), br(), 
		                                                              )),
		                                                              fluidRow(
		                                                                column(width = 4, 
		                                                                       uiOutput("pred_line_plot_model_error_text_1")),
		                                                                column(width = 4, offset = 1, 
		                                                                       uiOutput("pred_line_plot_model_error_text_2"))
		                                                              ),
		                                                              fluidRow(
		                                                                column(width = 4, 
		                                                                       br(), tableOutput("pred_line_plot_model_error_1")),
		                                                                column(width = 4, offset = 1, 
		                                                                       br(), tableOutput("pred_line_plot_model_error_2"))
		                                                              )
		                                                              # fluidRow(column(width = 12,
		                                                              #                 uiOutput("pred_line_plot_model_date_range")
		                                                              #   ))
		                                                     ) # close tabPanel
		                                                   ) # close tabsetPanel() from output
		                                                 )# close fluidPage() for right output
		                                     ) # close splitLayout()
		                            ) # close tabPanel()
		                          )), # close tabsetPanel and tabPanel
		          ))),
      
      #### Food Basket ####
      #%%%%%%%%%%%%%%%%%%%#
      
      tabItem(tabName = "food_basket",
              fluidRow(
                tabBox(width = 12,
             # Food Print and Nutrition
             tabPanel("Item Choice", icon = icon("store"),
                      tabsetPanel(
                        
                        tabPanel("Food Basket Guide", icon = icon("info"),
                                 column(6,(div(style = 'width:550px;height:400px;',
                                 uiOutput("food_basket_info"))),
                          
                          useShinyjs(),
                          actionButton("basket_infos", 
                                       "Footprint and Nutrition", 
                                       width = 320, icon("info"),
                                       style = "color: black; background-color: #8bbeea; border-color: #8bbeea")
                   )),
                        
                                  tabPanel("Preferences", style = 'height: 1000px', icon = icon("cash-register"),
                                           sidebarLayout(
                                             sidebarPanel(style = "color: black; background-color: #000000; border-color: #000000",
                                                          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3399FF}")),
                                                          
                    # Deselect items!
                    selectInput(
                      "food_rem", "Exclude Fruit/Vegetable:",
                      choices = sort(unique(df_footprint_nutrients$food)),
                      selected = NULL, multiple = TRUE, width = "200px"),
                    
                    # select market 
                    selectInput(
                      "market_basket", "Select Market:",
                      choices = sort(unique(df_price$market))[1:5],
                      selected = sort(unique(df_price$market))[1],
                      multiple = FALSE, width = "200px"),
                    
                    # select price range 
                    sliderInput(
                      "price_basket", "Set Maximum Price per 1kg/per Piece per Item:", min = 0, value = mean(df_price$price), 
                      max = max(df_price$price), width = "300px"),
                    
                    # show nutrition or footprint
                    radioButtons(
                      "radio_basket", "Decision Making:",
                      choices = c("Footprint", "Nutrition", 
                                  "None"),
                      selected = "None"
                    ),
                    
                    
                    # Select preferred Nutrients
                    conditionalPanel("input.radio_basket == 'Both' || input.radio_basket == 'Nutrition'",
                                     selectInput(
                                       "nutrient_1", "Nutrient 1",
                                       choices = sort(unique(df_footprint_nutrients$nutrient)),
                                       selected = NULL, multiple = FALSE, width = "200px")),
                    
                    # Select preferred Nutrients
                    conditionalPanel("input.radio_basket == 'Both' || input.radio_basket == 'Nutrition'",
                                     selectInput(
                                       "nutrient_2", "Nutrient 2:",
                                       choices = sort(unique(df_footprint_nutrients$nutrient)),
                                       selected = NULL, multiple = FALSE, width = "200px")),
                    
                    # Select preferred Nutrients
                    conditionalPanel("input.radio_basket == 'Both' || input.radio_basket == 'Nutrition'",
                                     selectInput(
                                       "nutrient_3", "Nutrient 3:",
                                       choices = sort(unique(df_footprint_nutrients$nutrient)),
                                       selected = NULL, multiple = FALSE, width = "200px")), 
                    
                    tags$h6(style = "color:white;", 
                      HTML("<br>Please refresh after changing your selection.")), 
                    
                    # refresh 
              actionButton(
              "ref_basket", "Refresh Item Filter", icon = icon("sync"), 
              style = "padding: 3px; color: black; background-color: #8bbeea; border-color: #8bbeea")
              # )
              
              ),   # sidebarPanel
              
              mainPanel(style = "height: 1500px; width = 1500px;",
              
              tabsetPanel(
              tabPanel("Selection", 
                  # tableoutputs
                  fluidRow(div(
                    column(width = 3, style = "background-color: #000000; height: 110px;", 
                           radioButtons("age_basket", "Select Age Group:",
                                        choices = c("19 - 24", "25 - 50", "> 51"),
                                        selected = character(0))),
                    
                    column(width = 3, style = "background-color: #000000;height: 110px;", 
                           radioButtons("gender_basket", "Select Gender:",
                                        choices = c("Female", "Male"),
                                        selected = character(0))),
                    
                    column(width = 6, style = "background-color: #000000;height: 110px;", 
                           radioButtons("pal_basket", "Select PAL Value:",
                                        choices = c("Sedentary or Light Activity Lifestyle", 
                                                    "Active or Moderately Active Lifestyle"),
                                        selected = character(0)))
                    
                  )), 
                  
                  fluidRow(div(
                    
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3399FF}")),
                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #3399FF}")),
                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #3399FF}")),
                    
                    column(width = 3, style = "background-color: #000000; height: 110px;", 
                           conditionalPanel("input.radio_basket == 'Nutrition'",
                                            radioButtons("nutrition_1", "Level Nutrient 1",
                                                         choices = c("Low", "Medium", "High"), selected = "Medium")), 
                           
                           conditionalPanel("input.radio_basket == 'Footprint'",
                                            
                                            radioButtons("carbon_slide", "Level Carbon Footprint",
                                                         choices = c("Low", "Medium", "High")))),
                    
                    column(width = 3, style = "background-color: #000000; height: 110px;", 
                           conditionalPanel("input.radio_basket == 'Nutrition'",
                                            radioButtons("nutrition_2", "Level Nutrient 2",
                                                         choices = c("Low", "Medium", "High"),
                                                         selected = "Medium")), 
                           conditionalPanel("input.radio_basket == 'Footprint'",
                                            radioButtons("water_slide", "Level Water Footprint",
                                                         choices = c("Low", "Medium", "High")
                                            ))),
                    
                    column(width = 3, style = "background-color: #000000; height: 110px;", 
                           conditionalPanel("input.radio_basket == 'Nutrition' ",
                                            radioButtons("nutrition_3", "Level Nutrient 3",
                                                         choices = c("Low", "Medium", "High"),
                                                         selected = "Medium")), 
                    ),
                    
                    
                    # empty column for layout
                    column(width = 3, style = "background-color: #000000; height: 110px;")
                    
                  )),  
                                                                  
            # Food Basket and recommendation
            fluidRow(div(style = "margin-top:1em",
                       # Deselect items!
                       column(width = 3, selectizeInput(
                         "final_choice", "Final Selection:",
                         choices = sort(unique(df_footprint_nutrients$food)),
                         selected = NULL, multiple = TRUE, width = "200px"),
                         radioButtons("nutr_day_week", "Nutrition Recommendation:", 
                                      choices = c("Daily", "Weekly"), 
                                      selected = "Daily"), 
                         textOutput("final_empty")), 
                       
                       
                       column(width = 4,
                              conditionalPanel("input.final_choice.includes('Apple')",
                                               numericInput("num_Apple", label = "Amount in Gramm: Apple",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              
                              conditionalPanel("input.final_choice.includes('Artichoke')",
                                               numericInput("num_Artichoke", label = "Amount in Gramm: Artichoke",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              
                              conditionalPanel("input.final_choice.includes('Banana')",
                                               numericInput("num_Banana", label = "Amount in Gramm: Banana",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Beans')",
                                               numericInput("num_Beans", label = "Amount in Gramm: Beans",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Bell Pepper')",
                                               numericInput("num_Bell Pepper", label = "Amount in Gramm: Bell Pepper",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Carrot')",
                                               numericInput("num_Carrot", label = "Amount in Gramm: Carrot",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Cauliflower')",
                                               numericInput("num_Cauliflower", label = "Number of Pieces: Cauliflower",
                                                            value = 1, min = 1, max = 10, step = 1, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Cucumber')",
                                               numericInput("num_Cucumber", label = "Amount in Gramm: Cucumber",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Eggplant')",
                                               numericInput("num_Eggplant", label = "Amount in Gramm: Eggplant",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Endive')",
                                               numericInput("num_Endive", label = "Number of Pieces: Endive",
                                                            value = 1, min = 1, max = 10, step = 1, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Grapes')",
                                               numericInput("num_Grapes", label = "Amount in Gramm: Grapes",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Iceberg Lettuce')",
                                               numericInput("num_Iceberg Lettuce", label = "Number of Pieces: Iceberg Lettuce",
                                                            value = 1, min = 1, max = 10, step = 1, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Kiwi')",
                                               numericInput("num_Kiwi", label = "Amount in Gramm: Kiwi",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Leek')",
                                               numericInput("num_Leek", label = "Amount in Gramm: Leek",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Lemon')",
                                               numericInput("num_Lemon", label = "Amount in Gramm: Lemon",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Nectarine')",
                                               numericInput("num_Nectarine", label = "Amount in Gramm: Nectarine",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Onion')",
                                               numericInput("num_Onion", label = "Amount in Gramm: Onion",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Orange')",
                                               numericInput("num_Orange", label = "Amount in Gramm: Orange",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Peach')",
                                               numericInput("num_Peach", label = "Amount in Gramm: Peach",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Pear')",
                                               numericInput("num_Pear", label = "Amount in Gramm: Pear",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Strawberry')",
                                               numericInput("num_Strawberry", label = "Amount in Gramm: Strawberry",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Tomato')",
                                               numericInput("num_Tomato", label = "Amount in Gramm: Tomato",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px")),
                              
                              conditionalPanel("input.final_choice.includes('Zucchini')",
                                               numericInput("num_Zucchini", label = "Amount in Gramm: Zucchini",
                                                            value = 100, min = 100, max = 1000, step = 100, width = "250px"))), 
                       
                       column(width = 3, 
                              
                              actionButton("nutr_button", "Reload", width ="150px", icon = icon("play-circle"), 
                                           style = "padding: 3px; color: black; background-color: #8bbeea; border-color: #8bbeea"),
                       )
            )), # fluidRow
                                                                  
                                            # adding output
                                            fluidRow(div(style = "margin-top:1em",
                                                         
                                                         column(width = 10, 
                                                                htmlOutput("shopping_list", width = "500px",height = "500px") %>%
                                                                  withSpinner(color = "#0dc5c1", type = 6)))),
                                            
                                            # Header 
                                            fluidRow(div(style = "margin-top:5em",
                                                         column(width = 10,
                                                                textOutput("nutr_rec_header")), 
                                                         tags$head(tags$style("#nutr_rec_header{font-size: 18px;}"))
                                            )),
                                            
                                            fluidRow(div(style = "margin-top:1em",
                                                         column(width = 10,
                                                                htmlOutput("nutrition_rec", width = "500px",height = "750px") %>%
                                                                  withSpinner(color = "#0dc5c1", type = 6))
                                                         
                                            ))
                                   ),# tabpanel selection
                                   tabPanel("Nutrition and Footprint", 
                                            # nutrition barplot
                                            fluidRow(div(style = "margin-top:1em",
                                                         column(width = 12,
                                                                plotlyOutput("nutr_bar_basket", width = "500px", height = "400px") %>%
                                                                  withSpinner(color = "#0dc5c1", type = 6))
                                                         
                                            )), 
                                            
                                            # footprints barplot
                                            fluidRow(div(style = "margin-top:1em",
                                                         column(width = 12,
                                                                plotlyOutput("foot_bar_basket", width = "500px",height = "750px") %>%
                                                                  withSpinner(color = "#0dc5c1", type = 6))
                                                         
                                            ))
                                   ) # tabpanel
                                 ) # tabset panel
                                 
                       ) # mainPanel
                     ) # sidebarLayout  
            ) 
          ) # tabset panel 
 ),  # tabpanel Item Choice
 
#### Price Recommendation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Price Recommendation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tabPanel("Future Price Recommendation", icon = icon("coins"),
         tabsetPanel(
	   tabPanel("Price Recommendation Guide", icon = icon("info"),
         uiOutput("price_rec_guide")), 
           tabPanel("Past Prices Comparison", icon = icon("euro-sign"),
                    sidebarLayout(sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                               selectInput("market_o", "Select Market:",
                                                           choices = c("Price in Hamburg" = "price_H",
                                                                       "Price in Frankfurt" = "price_F",
                                                                       "Price in Cologne" = "price_K",
                                                                       "Price in Berlin" = "price_B",
                                                                       "Price in Munich" = "price_M"),
                                                           selected="price_H",
                                                           multiple = T),
                                               uiOutput("food_o"),
                                               uiOutput("type_o"),
                                               uiOutput("size_o"),
                                               uiOutput("country_o"),
                                               selectInput("week_o", "Select Price for Predicted Week:", 
                                                           choices = c("In 1 Week" = "Week_1",
                                                                       "In 2 Weeks" = "Week_2",
                                                                       "In 3 Weeks" = "Week_3",
                                                                       "In 4 Weeks" = "Week_4")),
                                               selectInput("average_rec", "Select Price Comparison to Time Point",
                                                           choices = c("1 Month" = "1_month",
                                                                       "1 Year" = "1_year")),
                                               uiOutput("future_price_prediction_info"),
                                               actionButton("refresh_price_recommendation_o", "Refresh Market Selection", 
                                                            icon = icon("sync"), style="color: #fff; background-color: #e60b25; border-color: #2e6da4"),
                                               actionButton("refresh_pr_food_combi_o", "Refresh Food Selection",
                                                            icon = icon("sync"), style="color: #fff; background-color: #e60b25; border-color: #2e6da4")),
                                  mainPanel(DT::dataTableOutput("future_price_recommendation_table_o")))),
           tabPanel("Price Recommendation", icon = icon("barcode"),
                    sidebarLayout(sidebarPanel(style = "color: white; background-color: black; border-color: black",
                                               selectInput("market_m", "Select market:",
                                                           choices = c("Price in Hamburg" = "price_H",
                                                                       "Price in Frankfurt" = "price_F",
                                                                       "Price in Cologne" = "price_K",
                                                                       "Price in Berlin" = "price_B",
                                                                       "Price in Munich" = "price_M"),
                                                           selected="price_H",
                                                           multiple = T),
                                               uiOutput("food_m"),
                                               uiOutput("type_m"),
                                               uiOutput("size_m"),
                                               uiOutput("country_m"),
                                               actionButton("refresh_price_recommendation", "Refresh Market Selection", 
                                                            icon = icon("sync"), style="color: #fff; background-color: #e60b25; border-color: #2e6da4"),
                                               actionButton("refresh_pr_food_combi", "Refresh Food Selection",
                                                            icon = icon("sync"), style="color: #fff; background-color: #e60b25; border-color: #2e6da4")),
                                  mainPanel(DT::dataTableOutput("future_price_recommendation_table"))))
         ))
                )
              )
      )# bracket for tabitem
    ) # close tabItems
  ) # close dashboardbody 
) # close dashboardpage