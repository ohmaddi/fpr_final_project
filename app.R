## app.R ##
library(shinydashboard)
header <- dashboardHeader(title = "K-means Clustering of Pokemon Data")

sidebar <- dashboardSidebar()

body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) { }

shinyApp(ui, server)