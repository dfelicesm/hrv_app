library(shiny)

ui <- fluidPage(
  selectInput(inputId = "basal_hrv_period", 
              label = "Length of basal HRV period:",
              c("30 days", "60 days")),
  plotOutput("hrv_plot")
  
)

server <- function(input, output) {
  output$hrv_plot <- renderPlot(hist(rnorm(100)))
  
}

shinyApp(ui, server)
