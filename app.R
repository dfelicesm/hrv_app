library(shiny)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)

data <- read.csv("data/last.csv")

# Transform data to date format
data <- mutate(data, date = ydm(as.character(date)))



ui <- fluidPage(
  
  # Choosing the length of time to calculate normal HRV values
  selectInput(inputId = "basal_hrv_period", 
              label = "Length of basal HRV period:",
              choices = c("30 days", "60 days"),
              selected = "30 days"), # default period for normal HRV values is 30 days
  
  selectInput(inputId = "hrv_metric",
              label = "HRV metric:",
              choices = c("HRV4T Recovery Points", "ln rMSSD", "Resting HR"),
              selected = "HRV4T Recovery Points"),
  
  dateRangeInput(inputId = "dates",
                 label = "Dates:",
                 start = today()-days(90),
                 end = today(),
                 min = "2000-01-01",
                 max = today()+years(1),
                 format = "dd/mm/yyyy",
                 weekstart = 1),
  fileInput(inputId = "input_CSV",
            label = "Load new data:",
            accept = ".csv"),
  
  plotOutput("hrv_plot")

)

server <- function(input, output) {
  
  # Create new_df if new data is uploaded to the app
  new_df <- reactive({
    
    inFile <- input$input_CSV
    
    # Check if data is uploaded
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      temp_df <- req(inFile)
    }
    
    # Read the data uploaded, avoiding issues with extra , in the HRV4T file
    temp_df <- read.csv(inFile$datapath, header = FALSE)[-65]
    
    
    # Update the data stored in the data folder
    write.table(temp_df, "data/last.csv", sep = ",", row.names = FALSE, col.names = FALSE)
    
    # Read the new df
    read.csv("data/last.csv")
    
  })
  

  # Create new columns for different baselines:
  # It changes depending on the time period for normal values and the HRV metric
  dataInput <- reactive({
    basal_period <- switch(input$basal_hrv_period, 
                           "30 days" = 30,
                           "60 days" = 60)
    
    # update data source if new data has been added
    if (is.data.frame(new_df())){
      data <- new_df() %>% mutate(date = ydm(as.character(date)))
    }
    
    # Relate the chosen metric to a vector
    metric <- switch(input$hrv_metric, 
                     "HRV4T Recovery Points" = data$HRV4T_Recovery_Points,
                     "ln rMSSD" = if_else(data$rMSSD != 0, log(data$rMSSD), NULL), # calculate ln except for 0
                     "Resting HR" = if_else(data$X.HR != 0, data$X.HR, NULL)) # transform zeros into NULL

    # Create the variables that we want to plot
    data %>% 
      mutate(
        metric = metric,
        weekly_average = rollapply(metric, 7, mean, na.rm = TRUE, fill = NA, 
                                   align = "right"),
        normal_values = rollapply(metric, basal_period, mean, na.rm = TRUE, 
                                  fill = NA,  align = "right"),
        normal_values_sd = rollapply(metric, basal_period, sd, na.rm = TRUE, 
                                     fill = NA,  align = "right"))      %>% 
      
      select(date, metric, weekly_average, normal_values, normal_values_sd)
  })
 
  output$hrv_plot <- renderPlot({
    
    ggplot(data = dataInput() %>% 
             filter(date >= input$dates[1], date <= input$dates[2]), 
           mapping = aes(date)) +
      geom_col(mapping = aes(y = metric), 
               alpha = 0.3) +
      geom_ribbon(mapping = aes(ymin = normal_values - 0.75*normal_values_sd, 
                                ymax = normal_values + 0.75*normal_values_sd ),
                  fill = "lightblue", 
                  alpha = 0.5,
                  color = "lightblue",
                  linetype = 2)  +
      geom_line(mapping = aes(y = weekly_average), 
                color="steelblue", 
                size=1.5) +
      scale_x_date(name = '', date_breaks = '5 days',
                   date_labels = '%d/%m/%Y') +
      scale_y_continuous(name = "",
                         limits = c(0, 70)) +
      ggtitle(toupper(input$hrv_metric)) + 
      theme(plot.title = element_text( face = "bold", colour = "navyblue", size = 20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 11)) + 
      coord_cartesian(ylim = c(min(dataInput()$metric, na.rm = TRUE), 
                               max(dataInput()$metric, na.rm = TRUE)))
    
  })
}

shinyApp(ui, server)
