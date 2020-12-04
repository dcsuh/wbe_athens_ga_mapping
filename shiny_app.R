library(shiny)

library(mapview)
library(tidyverse)
library(dplyr)
library(sf)
library(plyr)
library(RColorBrewer)
library(heat.col)
library(leaflet)
library(here)

#load viral data and then clean. Here, we're summarizing the two targets into a single value, by averaging.
n1_n2 = read.csv("n1_n2_cleaned.csv")
n1_n2 = mutate(n1_n2, log_total_copies= log10(mean_total_copies))
n1_n2 = plyr::ddply(n1_n2, c("wrf", "date"), summarize, total_copies = mean(log_total_copies))
#n1_n2 = n1_n2 %>% select(wrf, date, target, log_total_copies)

#now join the viral data to the catchment regions.
wrf_catchment = st_read("./wrf_catchment/wrf_catchment.shp")
n1_n2_catchment = left_join(wrf_catchment, n1_n2, by = "wrf")
n1_n2_catchment = as(n1_n2_catchment, "sf")

filtered <- n1_n2_catchment %>% filter(date == n1_n2_catchment$date[1])




ui <- pageWithSidebar(
  headerPanel("Title"),
  sidebarPanel(
    numericInput("week", label = "week", value=1), 
  ),
  mainPanel(
    mapviewOutput("map"),
    textOutput("text")
  )
)


server <- function(input, output){
data <- reactive({
  data <- n1_n2_catchment %>% filter(date == n1_n2_catchment$date[input$week])
  data
})

output$text <- renderText({
  data <- data()
  data$week
})

output$map <- renderLeaflet({
  data <- data()
  mapview(data, zcol = "total_copies", at = seq(10, 16, 1))
  })
}


ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)





