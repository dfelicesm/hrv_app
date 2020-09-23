library(shiny)

ui <- fluidPage(
  selectInput(inputId = "basal_hrv_period", 
              label = "Length of basal HRV period:",
              c("30 days", "60 days"))
  
)

server <- function(input, output) {
  
}

shinyApp(ui, server)