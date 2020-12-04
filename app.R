library(shiny)
library(tidyverse)
library(magrittr)
library(here)
library(sf)
library(mapview)

#load viral data and then clean. Here, we're summarizing the two targets into a single value, by averaging.
n1_n2 = read.csv("n1_n2_cleaned.csv")
n1_n2 = mutate(n1_n2, log_total_copies= log10(mean_total_copies))
n1_n2 = plyr::ddply(n1_n2, c("wrf", "date"), summarize, total_copies = mean(log_total_copies))
#n1_n2 = n1_n2 %>% select(wrf, date, target, log_total_copies)

#now join the viral data to the catchment regions.
wrf_catchment = st_read("./wrf_catchment/wrf_catchment.shp")
n1_n2_catchment = left_join(wrf_catchment, n1_n2, by = "wrf")
n1_n2_catchment = as(n1_n2_catchment, "sf")

filtered <- n1_n2_catchment %>% filter(date == n1_n2_catchment$date[4])

filtered <- list()

for (i in 1:43){
  filtered[i] <- n1_n2_catchment %>% filter(date == n1_n2_catchment$date[i])
}

mapview(filtered[1], zcol = "total_copies", at = seq(10, 16, 1))


ui <- fluidPage(
  sliderInput("week", "Week", min=1, max=10, value = 1)
)

server <- function(input, output, session) {
  data <- reactive({
    n1_n2_catchment
  })
  filter <- reactive({
    filter(data(), date == n1_n2_catchment$date[input$week])
  })
  output$plot <- renderMapview(mapview(filter(), zcol = "total_copies", at = seq(10, 16, 1)))
}

shinyApp(ui, server)

data <- as.data.frame(c(1:10))

library(shiny)

ui <- fluidPage(
  numericInput("week", "week", value=1),
  tableOutput("table")
)

server <- function(input, output, session) {
  catchment <- reactive(
    n1_n2_catchment
  )
  weekly_data <- reactive({
    filter(catchment(), date == n1_n2_catchment$date[input$week])
    })
  output$table <- renderTable(weekly_data$date)
}

shinyApp(ui, server)
