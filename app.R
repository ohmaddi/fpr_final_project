## app.R ##
library(shinydashboard)
library(factoextra)
library(here)
library(R.utils)
library(tidyverse)
library(janitor)
library(DT)


# Read in pokemon data ---------------------------------------------------

pokemon_data <- clean_names(read.csv("pokemon.csv"))  %>% 
    select(-percentage_male, -type2) %>%  
    filter(type1 %in% c("ghost", "fairy", "dragon")) %>%  # Let's limit this to a few pokemon
    mutate(type1 = droplevels(type1)) %>% # Few poke have data for these
    drop_na()
pokemon_type <- pokemon_data$type1
pokemon_data <- pokemon_data %>% 
    select(starts_with("against"), hp) %>% 
    scale() %>% as.data.frame()


# Load output from k-means clustering -------------------------------------
#  turn this into a function!
clust2 <- loadObject(here("kmeans/clust2.Rda"))
clust3 <- loadObject(here("kmeans/clust3.Rda"))
clust4 <- loadObject(here("kmeans/clust4.Rda"))
clust5 <- loadObject(here("kmeans/clust5.Rda"))
clust6 <- loadObject(here("kmeans/clust6.Rda"))


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


# Dashboard header --------------------------------------------------------

header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data")


# Dashboard sidebar -------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro to clustering", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cluster Plot", tabName = "clustplot", icon = icon("cookie")),
        menuItem("Silhuoette Plot", tabName = "silplot", icon = icon("chart-area")),
        menuItem("Scatter Plot", tabName = "scatplot", icon = icon("chart-scatter")),
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
        # clustplot tab content
        tabItem(tabName = "clustplot",
                fluidRow(
                    box(plotOutput("clustplot", height = 250)))),
                
        # silplot tab content
        tabItem(tabName = "silplot",
                fluidRow(
                    box(plotOutput("silplot", height = 250))),
                fluidRow(
                    box(DTOutput("summarytable")))),
        
        # scatplot tab content
        tabItem(tabName = "scatplot",
                fluidRow(
                    box(plotOutput("scatplot", height = 250))))
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
            fviz_silhouette(
                data,
                palette = "jco",
                print.summary = FALSE,
                ggtheme = theme_minimal()
            )
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
            plot_data <- cbind(data$cluster, pokemon_data, pokemon_type) %>%  
                rename(cluster = 1) %>%
                mutate(cluster = as.character(cluster),
                       type = as.character(pokemon_type)) %>%
                gather(value = value, key = variable_name,-type,-cluster)
            
            # Plot clusters by pokemon type
            plot_data %>% 
                ggplot(aes(x = variable_name, y = value, color = cluster)) +
                geom_jitter(alpha = 0.6) + 
                coord_flip() +
                facet_wrap(~type) +
                theme_minimal()
        })
    
    output$summarytable <- 
        renderDT({
            data <- get(input$clusters)
            make_summary_table(data)
        })
    
}

shinyApp(ui, server)