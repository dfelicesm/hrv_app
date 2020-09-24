library(shiny)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)

fileCSV <- "data/MyMeasurements_Android.csv"
# Problems with an empty column so I'll read the CSV without that last column
data = read.csv(fileCSV, header = FALSE)[-65] # I know that with the empty column there're 65

write.table(data, "clean.csv", sep = ",", row.names = FALSE, col.names = FALSE)


data <- read.csv("clean.csv")

# Transform data to date format
data <- mutate(data, date = ydm(as.character(date)))



ui <- fluidPage(
  
  # Choosing the length of time to calculate normal HRV values
  selectInput(inputId = "basal_hrv_period", 
              label = "Length of basal HRV period:",
              choices = c("30 days", 
                          "60 days"),
              selected = "30 days"), # default period for HRV values is 30 days
  
  plotOutput("hrv_plot"),
  
  tableOutput("table")
  
)

server <- function(input, output) {
  # Create new columns for different baselines
  
  dataInput <- reactive({
    basal_period <- switch(input$basal_hrv_period, 
           "30 days" = 30,
           "60 days" = 60)
    
    data %>% mutate( 
      recovery_7d = rollapply(HRV4T_Recovery_Points, 7, mean, na.rm = T, fill = NA, 
                              align = "right"),
      recovery_normal_values = rollapply(HRV4T_Recovery_Points, basal_period, mean, 
                                         na.rm = T, fill = NA,  align = "right"),
      recovery_normal_values_sd = rollapply(HRV4T_Recovery_Points, basal_period, sd, 
                                            na.rm = T, fill = NA,  align = "right"))
  })
 
  
  output$table <- renderTable(dataInput())
}

shinyApp(ui, server)
