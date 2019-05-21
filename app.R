## app.R ##
library(shinydashboard)
library(ggplot2)
library(factoextra)
library(here)

clust2 <- loadObject(here("kmeans/clust2.Rda"))
clust3 <- loadObject(here("kmeans/clust3.Rda"))
clust4 <- loadObject(here("kmeans/clust4.Rda"))
clust5 <- loadObject(here("kmeans/clust5.Rda"))
clust6 <- loadObject(here("kmeans/clust6.Rda"))


header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
)

body <- dashboardBody(
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    
                    box(
                        title = "Controls",
                        radioButtons("clusters",
                                     "Number of centroids to try:",
                                    c("2" = clust2, 
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
    data <- input$cluster
    
    output$plot1 <-
        renderPlot({
            fviz_silhouette(
                data,
                palette = "jco",
                print.summary = FALSE,
                ggtheme = theme_minimal()
            )
        })
    
}

shinyApp(ui, server)