## app.R ##
library(shinydashboard)
library(factoextra)
library(here)
library(R.utils)
library(tidyverse)
library(janitor)
library(DT)

# Read in pokemone data
pokemon_data <- clean_names(read.csv("pokemon.csv"))  %>% 
    select(-percentage_male, -type2) %>%  
    filter(type1 %in% c("ghost", "fairy", "dragon")) %>%  # Let's limit this to a few pokemon
    mutate(type1 = droplevels(type1)) %>% # Few poke have data for these
    drop_na()
pokemon_type <- pokemon_data$type1
pokemon_data <- pokemon_data %>% 
    select(starts_with("against"), hp) %>% 
    scale() %>% as.data.frame()

## Load in output from k-means clustering
#  turn this into a function!
clust2 <- loadObject(here("kmeans/clust2.Rda"))
clust3 <- loadObject(here("kmeans/clust3.Rda"))
clust4 <- loadObject(here("kmeans/clust4.Rda"))
clust5 <- loadObject(here("kmeans/clust5.Rda"))
clust6 <- loadObject(here("kmeans/clust6.Rda"))


## Define functions for creating tables

# function to create data for silhouette table
get_sil_data <- function(clust){
    # number of clusters 
    k <- clust$nbclust
    # cluster number
    clust_num <- map_chr(seq(1:k), ~paste("Cluster", .x))
    # ss-within
    wss <- round(clust$withinss, 2)
    # ss-between
    bss <- round(rep(clust$betweenss, k), 2)
    # n observations per cluster
    nobs <- clust$size
    # number of observations with negative sil value (misclassified)
    neg_sil <- rep(0, k)
    neg_sil_clust <- clust$silinfo$widths[, c("cluster","sil_width")] %>% 
        filter(sil_width < 0) %>% 
        group_by(cluster) %>% 
        summarize(neg_sil = n())
    neg_sil[neg_sil_clust$cluster] <- neg_sil_clust$neg_sil 
    #bind elements to data frame
    data.frame(clust_num, nobs, wss, bss, neg_sil) 
}

# function to create sil table
make_sil_table <- function(clust) {
    table <- get_sil_data(clust) %>% 
    datatable(rownames = FALSE, 
              colnames= c("Cluster", "N", "Within SS", "Between SS", "Neg. Silhouette"),
              caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  htmltools::em('N = number of observations per cluster; SS = sum of squares')))
    
    return(table)
    }

# Dashboard header
header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data")

# Dashboard sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro to clustering", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cluster Plot", tabName = "clustplot", icon = icon("cookie")),
        menuItem("Silhuoette Plot", tabName = "silplot", icon = icon("chart-area")),
        menuItem("Scatter Plot", tabName = "scatplot", icon = icon("chart-scatter"))
    )
)

# Dashboard body
body <- dashboardBody(
    tabItems(
        # clustplot tab content
        tabItem(tabName = "clustplot",
                fluidRow(
                    box(plotOutput("clustplot", height = 250)),
                    
                    box(
                        title = "Controls",
                        selectInput("clusters",
                                    "Number of centroids to try:",
                                    c("2" = "clust2", # turn this into a function!
                                      "3" = "clust3", 
                                      "4" = "clust4", 
                                      "5" = "clust5",
                                      "6" = "clust6"),
                                    selected = "clust2")
                    )
                )),
        
        tabItem(tabName = "silplot",
                fluidRow(
                    box(plotOutput("silplot", height = 250)),
                    
                    box(
                        title = "Controls",
                        selectInput("clusters",
                                    "Number of centroids to try:",
                                    c("2" = "clust2", 
                                      "3" = "clust3", 
                                      "4" = "clust4", 
                                      "5" = "clust5",
                                      "6" = "clust6"),
                                    selected = "clust2")
                    )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                h2("Widgets tab content"))
    )
)

# user interface
ui <- dashboardPage(header, sidebar, body)

# server
server <- function(input, output) {
    
    
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
    
    output$clustplot <-
        renderPlot({
            data <- get(input$clusters)
            data$clust_plot
        })
    
}

shinyApp(ui, server)