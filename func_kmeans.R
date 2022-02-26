#%%%%%%%%%%%%%%%%%%%%#
# k-means Clustering #
#%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# This script contains a function which can be used to perform k-means clustering.
# A function is written as it can be flexible used in the R Shiny App.
# The function takes the following input arguments:
  # cluster_num: number of clusters
  # cluster_var: variables used for clustering.
    ## choices are: Footprint, Nutrition, and Mean Price
  # cluster_var_2: detailed cluster variables, for instance for nutrition sugar 
  # and protein

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load packages (comment out as this is done in the load_file.R)
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)
# 
# if (!require("ggplot2")) install.packages("ggplot2")
# library(ggplot2)
# 
# if (!require("cluster")) install.packages("cluster")
# library(cluster)
# 
# if (!require("factoextra")) install.packages("factoextra")
# library(factoextra)



#### Create Function ####

func_kmeans <- function(cluster_food, cluster_num, 
                        cluster_var, cluster_var_2, 
                        cluster_determine) {
  
  # for fruits only a specific number of clusters can be chosen
  # that is number of fruits - 1 (this is 8)
  if (cluster_num > 8 & (!"Vegetables" %in% cluster_food)) {
    return(list(
      data.frame(), 
      ggplot() + 
        theme(
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # add white border around facets
          panel.border = element_rect(fill = NA, color = "black"),
          # no grid
          panel.grid = element_blank()
      ), 
      cluster_num))
  } else {
    
    
    #### Prepare data ####
    
    # load data 
    df_cluster <- readRDS("output/final_cluster.rds")
    
    # select type of food (fruits, vegetables or both)
    if (length(cluster_food) == 2) {
      df_cluster <- df_cluster
    } else if (length(cluster_food) == 1) {
      if ("Fruits" %in% cluster_food) {
        fruits <- c("Apple", "Pear", "Grapes", "Strawberry", "Peach", "Nectarine",
                    "Kiwi", "Orange", "Lemon","Banana")
        df_cluster <- df_cluster %>% 
          filter(food %in% fruits)
      } else {
        fruits <- c("Apple", "Pear", "Grapes", "Strawberry", "Peach", "Nectarine",
                    "Kiwi", "Orange", "Lemon","Banana")
        df_cluster <- df_cluster %>% 
          filter(!food %in% fruits)
      }
    }
    
    # if chosen number of clusters is higher than number of food items,
    # stop computation
    if (cluster_num >= nrow(df_cluster)) {
      stop("Selected number of clusters is too high")
    }
    
    # store food feature in vector for later
    food <- df_cluster$food
    
    # food is not used for clustering
    df_cluster_analysis <- df_cluster %>% dplyr::select(-food) %>% as.data.frame()
    
    ## Selection 
    # select cluster variables based on first selection 
    keep_cols <- c()
    if ("Mean Price" %in% cluster_var) {
      keep_cols_price_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(starts_with("Mean Price")) %>% colnames() 
      keep_cols <- c(keep_cols, keep_cols_price_2)
    }
    if ("Nutrition" %in% cluster_var) {
      keep_cols_nutrition_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(!c(starts_with("Mean Price"), matches("Footprint$"))) %>%
        colnames()
      keep_cols <- c(keep_cols, keep_cols_nutrition_2)
    }
    if ("Footprint" %in% cluster_var) {
      keep_cols_footprint_2 <- df_cluster %>% dplyr::select(-food) %>%
        dplyr::select(matches("Footprint$")) %>% colnames()
      keep_cols <- c(keep_cols, keep_cols_footprint_2)
    }
    
    df_cluster_analysis_sub_1 <- df_cluster_analysis %>% 
      dplyr::select(all_of(keep_cols))
    
    # select cluster variable based on second selection
    if ("All" %in% cluster_var_2) {
      df_cluster_analysis_sub_2 <- df_cluster_analysis_sub_1
    } else {
      df_cluster_analysis_sub_2 <- df_cluster_analysis_sub_1 %>%
        dplyr::select(all_of(cluster_var_2))
    }
    
    
    
    #### k-Means Clustering ####
    
    ## k-means clustering ##
    
    # number of clusters k
    if (cluster_determine == "Manually") {
      k <- cluster_num
      silhouette_plot <- 
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
    } else if (cluster_determine == "Automatically using the Silhouette Method") {
      # maximum k depends on chosen food category
      if ("Vegetables" %in% cluster_food) {
        silhouette_max_k <- 10
      } else {
        silhouette_max_k <- 8
      }
      
      # create plot
      silhouette_plot <-
        fviz_nbclust(df_cluster_analysis_sub_2, kmeans, 
                     method = "silhouette", k.max = silhouette_max_k) +
        ggtitle("Silhouette Method: Optimal Number of Clusters") +
        theme(
          # center title
          plot.title = element_text(hjust = 0.5, size = 10, colour = "white"),
          # black background of panel and plot
          panel.background = element_rect(fill = "black", color = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          # no grid
          panel.grid = element_blank(),
          # color and size of axis
          axis.text = element_text(size = 10, colour = "white"),
          axis.title.x = element_text(colour = "white", size = 10),
          axis.title.y = element_text(colour = "white", size = 10),
          axis.line = element_line(color = "white")
        )
      # save plot (later loaded in Shiny)
      # ggsave("www/silhouette_plot.png", silhouette_plot, width = 4, height = 2.5)
      # calculate score
      func_silhouette_score <- function(df, k_values){
        km <- kmeans(df, centers = k_values, nstart = 25)
        ss <- silhouette(km$cluster, dist(df))
        mean(ss[, 3])
      }
      k_values <- 2:silhouette_max_k
      silhouette_score <- vapply(k_values, func_silhouette_score, 
                                 df = df_cluster_analysis_sub_2, 
                                 FUN.VALUE = numeric(1))
      # create data frame
      df_silhouette_score <- data.frame(
        k = k_values,
        score = silhouette_score
      )
      # find optimal number of clusters
      k <- df_silhouette_score %>% filter(score == max(score)) %>% 
        dplyr::select(k) %>% pull()
    }
    
    # z-score standardization
    df_cluster_analysis <- as.data.frame(lapply(df_cluster_analysis_sub_2, scale))
    row.names(df_cluster_analysis) <- food
    
    # conduct k-means
    set.seed(42)
    cluster_solution <- kmeans(df_cluster_analysis, 
                               centers = k, 
                               nstart = 25)
    
    # append clustering vector to nutrient data set
    df_cluster$cluster <- cluster_solution$cluster
    
    # create title
    cluster_plot_title <- 
      paste("Cluster Plot using", paste(cluster_var , collapse = " and "))
    
    # create figure indicating which fruits/vegetables are similar
    options(ggrepel.max.overlaps = Inf) # ensures that all labels are shown
    cluster_plot <- fviz_cluster(cluster_solution, data = df_cluster_analysis, 
                                 # ellipse type
                                 ellipse.type = "confidence", 
                                 # layout of plot
                                 labelsize = 20, palette = "Set2", 
                                 ggtheme = theme_minimal(),
                                 main = cluster_plot_title,
                                 # avoid cluster overlapping
                                 repel = TRUE) +
      # add legend title
      #labs(fill = "Cluster Number") +
      theme(
        # center title
        plot.title = element_text(hjust = 0.5, colour = "white", size = 16),
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        # add white border around facets
        panel.border = element_rect(fill = NA, color = "black"),
        # no grid
        panel.grid = element_blank(),
        #panel.grid.major = element_line(colour = "transparent"),
        # legend
        legend.background = element_rect(fill = "black", colour = "black"),
        legend.text = element_text(size = 14, colour = "white"), 
        legend.title = element_text(size = 14, colour = "white"),
        # axis labels
        axis.text = element_text(size = 14, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", size = 14)
      ) 
    # add labels to plot (replace Dim1 and Dim2 with Principal Component 1 and PC 2
    y_label <- paste("Principal Component 2", 
                     str_extract_all(cluster_plot$labels$y, "\\([^()]+\\)")[[1]])
    x_label <- paste("Principal Component 1", 
                     str_extract_all(cluster_plot$labels$x, "\\([^()]+\\)")[[1]])
    cluster_plot <- cluster_plot + xlab(x_label) + ylab(y_label)
    
    # create table 
    ## maximum number of foods in one cluster
    foods_max <- table(df_cluster$cluster) %>% max()
    ## number of clusters
    cluster_num <- length(unique(df_cluster$cluster))
    ## create table with NAs                         
    df_cluster_table <- data.frame(
      matrix(NA, nrow = foods_max, ncol = cluster_num)
    )
    ## give appropriate column names
    names(df_cluster_table) <- paste0(rep("cluster", 
                                          each = cluster_num), 
                                      "_",1:cluster_num)
    
    for (i in 1:length(unique(df_cluster$cluster))) {
      # extract food names per cluster 
      df <- data.frame(
        col_cluster = df_cluster %>% filter(cluster == i) %>% 
          dplyr::select(food) %>% pull()
      ) 
      # sort alphabetically
      df <- arrange_all(df)
      # append NAs so that vector has length foods_max
      df <- df[, 1][1:foods_max]
      # add to data frame
      df_cluster_table[, i] <- df
    }
    
    # return output
    return(list(df_cluster_table, cluster_plot, k, silhouette_plot))
    ## first list output: table
    ## second plot
    ## third number of clusters k
    ## fourth output: silhouette plot (empty for manually)
  }
}


## test function
cluster_food <- c("Fruits", "Vegetables")
cluster_num <- 4
cluster_var <- c("Nutrition", "Mean Price")
cluster_var_2 <- "All"
cluster_determine <- "Automatically using the Silhouette Method"


func_kmeans(cluster_food, cluster_num, cluster_var,
            cluster_var_2, cluster_determine)


