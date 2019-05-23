## app.R ##
library(shinydashboard)
library(ggplot2)
library(factoextra)
library(here)
library(R.utils)

clust2 <- loadObject(here("kmeans/clust2.Rda"))
clust3 <- loadObject(here("kmeans/clust3.Rda"))
clust4 <- loadObject(here("kmeans/clust4.Rda"))
clust5 <- loadObject(here("kmeans/clust5.Rda"))
clust6 <- loadObject(here("kmeans/clust6.Rda"))


header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro to clustering", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cluster Plot", tabName = "clustplot", icon = icon("cookie")),
        menuItem("Silhuoette Plot", tabName = "silplot", icon = icon("chart-area")),
        menuItem("Scatter Plot", tabName = "scatplot", icon = icon("chart-scatter"))
    )
)

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
                                    c("2" = "clust2", 
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

ui <- dashboardPage(header, sidebar, body)


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