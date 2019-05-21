## app.R ##
library(shinydashboard)
library(ggplot2)
library(factoextra)

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
                                     choices = c("2" = "pokemon_against_km2", 
                                                 "3" = "pokemon_against_km3", 
                                                 "4" = "pokemon_against_km4", 
                                                 "5" = "pokemon_against_km5",
                                                 "6" = "pokemon_against_km6"),
                                     selected = "pokemon_against_km3")
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
    output$plot1 <-
        renderPlot({
            fviz_silhouette(
                pokemon_against_km,
                palette = "jco",
                print.summary = FALSE,
                ggtheme = theme_minimal()
            )
        })
    
}

shinyApp(ui, server)