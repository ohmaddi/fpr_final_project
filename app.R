## app.R ##
library(shinydashboard)
library(factoextra)
library(here)
library(R.utils)
library(tidyverse)
library(janitor)
library(DT)
library(colorblindr)


# Read in pokemon data ---------------------------------------------------

pokemon_data <- clean_names(read.csv("pokemon.csv"))  %>% 
    select(-percentage_male, -type2) %>%  
    filter(type1 %in% c("ghost", "fairy", "dragon")) %>%  # Let's limit this to a few pokemon
    mutate(type1 = droplevels(type1)) %>% # Few poke have data for these
    drop_na()
first_row <- function(.x, to_chr = FALSE) {
    first <- .x[1, ]  #pull the firt row of the data
    if (to_chr) {
        first[] <- walk(first, as.character) #make all data points a character if to_chr = TRUE
    }
    
    tibble::as_tibble(first)
}

pokemon_data <- pokemon_data %>% 
    group_by(type1) %>%
    nest() %>%
    mutate(., data = walk(data, first_row)) %>%
    unnest()

pokemon_type <- pokemon_data$type1
pokemon_data <- pokemon_data %>% 
    select(starts_with("against"), hp) %>% 
    scale() %>% as.data.frame()


# Load output from k-means clustering -------------------------------------

get_filenames <- function(folder) {
    files <- list.files(here(glue::glue("{folder}/")))
    map_chr(files, ~word(.x, sep = "\\."))
}

load_clustdata <- function(file) {
    temp <- loadObject(here(glue::glue("kmeans/{file}.Rda")))
    assign(file, temp, envir = .GlobalEnv)
    }

map(get_filenames("kmeans"), ~load_clustdata(.x))

# Custom functinos --------------------------------------------------------

# function to create data for silhouette table
get_summary_data <- function(clust){
    
    k <- clust$nbclust  # number of clusters 
    clust_num <- map_chr(seq(1:k), ~paste("Cluster", .x))  # cluster number
    wss <- round(clust$withinss, 2)  # ss-within
    bss <- round(rep(clust$betweenss, k), 2)  # ss-between
    nobs <- clust$size  # n observations per cluster
    neg_sil <- rep(0, k)  # number of observations with negative sil value (misclassified)
    neg_sil_clust <- clust$silinfo$widths[, c("cluster","sil_width")] %>% 
        filter(sil_width < 0) %>% 
        group_by(cluster) %>% 
        summarize(neg_sil = n())
    neg_sil[neg_sil_clust$cluster] <- neg_sil_clust$neg_sil 
    data.frame(clust_num, nobs, wss, bss, neg_sil)  #bind elements to data frame
}

# function to create sil table
make_summary_table <- function(clust) {
    table <- get_summary_data(clust) %>% 
        datatable(rownames = FALSE, 
                  colnames = c("Cluster", "N", "Within SS", "Between SS", "Neg. Silhouette"),
                  caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: left;',
                      htmltools::em('N = number of observations per cluster; SS = sum of squares')))
    
    return(table)
}

# Scatterplot function

scatplot <- function(data){
    pca_data <- prcomp(pokemon_data)
    plot_data <- data.frame(pokemon_type, data$cluster, pca_data$x[, 1:3]) %>% 
        rename(cluster = 2) %>% 
        gather(starts_with("PC"), value = value, key = principal_component)  
    
    # Plot clusters by pokemon type
    facet_labels <- c(dragon = "Dragon", fairy = "Fairy", ghost = "Ghost")
    
    plot_data %>% 
        ggplot(aes(x = principal_component, y = value, color = factor(cluster))) +
        geom_point(position = position_jitter(width = 0.5), alpha = 0.6, size = 5) + 
        coord_flip() +
        scale_color_OkabeIto() +
        labs(x = "Principal Component \n", y = "") + 
        facet_wrap(~pokemon_type, labeller = labeller(pokemon_type = facet_labels)) +
        theme_minimal(base_size = 17) + 
        theme(panel.grid.minor = element_blank())
}



# Dashboard header --------------------------------------------------------

header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data",
                    titleWidth = 450)


# Dashboard sidebar -------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro to clustering", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cluster Plot", tabName = "clustplot", icon = icon("cookie")),
        menuItem("Silhuoette Plot", tabName = "silplot", icon = icon("chart-area")),
        menuItem("Scatter Plot", tabName = "scatplot", icon = icon("braille")),
        selectInput(inputId = "clusters",
                    label = "Number of centroids:",
                    c("2" = "clust2", # turn this into a function!
                      "3" = "clust3", 
                      "4" = "clust4", 
                      "5" = "clust5",
                      "6" = "clust6"),
                    selected = "clust2")
    ))


# Dashboard body ----------------------------------------------------------

body <- dashboardBody(
    tabItems(
        # Intro tab content
        tabItem(tabName = "intro",
                    box("This dashboard is the final project for an R functional programming class. We use the Kaggle Pokemon dataset (available here [placeholder]) to demonstrate how different visualization of k-means clustering can provide help determine how well a clustering solution fits the data.", width = 12)),
        
        # clustplot tab content
        tabItem(tabName = "clustplot",
                fluidRow(
                    box(plotOutput("clustplot"), width = 6),
                    box(DTOutput("summarytable1"), width = 6)),
                fluidRow(
                    box("This plot shows the cluster results on the first two principal components of the data that were used to create them.", width = 12))
                ),

                
        # silplot tab content
        tabItem(tabName = "silplot",
                fluidRow(
                    box(plotOutput("silplot"), width = 6),
                    box(DTOutput("summarytable2"), width = 6)),
                fluidRow(
                    box("A silhouette plot shows cluster distance, a combination of within cluster compactness and of between cluster separation. 
                        A silhouette coefficient closer to 1 means that the data are well classified, whereas a coefficient near 0 
                        means observations are between clusters. A negative silhouette coefficient means observations are likely misclassified 
                        and that the data do not group well with any of the identified clusters. The height of each cluster in this plot 
                        represents the number of observations per cluster. Generally, we want clusters to be of roughly the same size, which we can gain 
                        information about by examining the silhouette plot.", width = 12))
        ),
        
        # scatplot tab content
        tabItem(tabName = "scatplot",
                fluidRow(
                    box(plotOutput("scatplot"), width = 12)),
                fluidRow(
                    box(width = 12, "K-means clustering is a form of unsupervised learning, meaning that it is intended to find grouping structure in unlabeled data.
                        However, we know that the pokemon in this dataset already 'grouped' by type of pokemon. 
                        So, we might want to ask how well the clusters we have identified in the data set map onto this pre-existing grouping variable, pokemon type.
                        Here we show a scatterplot that is faceted by 3 popular types of pokemon: dragon, fairy, and ghost. Rather than showing the raw data from the original variables that were fed into the 
                        k-means clustering algorithm, we used principal components analysis (PCA) to reduce the data for simplicity of visualization. Here we show the 
                        first 3 principal components. You will notice that with a 3-cluster solution, the clusters perfectly map onto the 3 types of pokemon. This is apparent from the fact that 
                        each pokemon type only contains a single color. However, with alternative clustering solutions, we see a mix of colors within each pokemon type, meaning that 
                        cluster membership is not completely corresponding to pokemon type. In general, greater mixing of colors across pokemon types corresponds to a weaker relationship between 
                        the identified clusters in the data and pokemon type.")))
        )
    )



# User interface ----------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    # Silhuoette plot
    output$silplot <-
        renderPlot({
            data <- get(input$clusters)
            data <- data$silinfo$widths

            ggplot(data, aes(x = seq_along(cluster), y = sil_width, fill = cluster, color = cluster)) +
                geom_col() +
                coord_flip() +
                geom_hline(yintercept = mean(data$sil_width, na.rm = TRUE), linetype = 2, size = .7) +
                scale_fill_OkabeIto() +
                scale_color_OkabeIto() +
                theme_minimal() + 
                labs(title = paste0("Average Silhouette Width = ", round(mean(data$sil_width, na.rm = TRUE), 2)),
                     x = NULL,
                     y = "Silhouette width") 
        })
    
    
    # Cluster plot
    output$clustplot <-
        renderPlot({
            data <- get(input$clusters)
            data$clust_plot
        })
    
    # Scatterplot
    output$scatplot <- 
        renderPlot({
            data <- get(input$clusters)
            scatplot(data)
})
    
    output$summarytable1 <- 
        renderDT({
            data <- get(input$clusters)
            make_summary_table(data)
        })
    
    output$summarytable2 <- 
        renderDT({
            data <- get(input$clusters)
            make_summary_table(data)
        })
}

shinyApp(ui, server)