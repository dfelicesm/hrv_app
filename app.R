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
  
  new_df <- reactive({
    
    inFile <- input$input_CSV
    
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      temp_df <- req(inFile)
    }
    
    temp_df <- read.csv(inFile$datapath, header = FALSE)[-65]
    
    
    
    write.table(temp_df, "data/last.csv", sep = ",", row.names = FALSE, col.names = FALSE)
    
    read.csv("data/last.csv")
    
  })
  

  # Create new columns for different baselines:
  # It changes depending on the time period for normal values and the HRV metric
  dataInput <- reactive({
    basal_period <- switch(input$basal_hrv_period, 
                           "30 days" = 30,
                           "60 days" = 60)
    
    if (is.data.frame(new_df())){
      data <- new_df() %>% mutate(date = ydm(as.character(date)))
    }
    
    
    if (input$hrv_metric == "HRV4T Recovery Points") {
      data %>% 
        
        mutate(
          metric = HRV4T_Recovery_Points,
          weekly_average = rollapply(metric, 7, mean, na.rm = TRUE, fill = NA, 
                                  align = "right"),
          normal_values = rollapply(metric, basal_period, mean, na.rm = TRUE, 
                                             fill = NA,  align = "right"),
          normal_values_sd = rollapply(metric, basal_period, sd, na.rm = TRUE, 
                                                fill = NA,  align = "right"))      %>% 
        
        select(date, metric, weekly_average, normal_values, normal_values_sd)
      
    } else if (input$hrv_metric == "ln rMSSD") {
        data %>% 
          mutate(metric = if_else(rMSSD != 0, log(rMSSD), NULL),
                 weekly_average = rollapply(metric, 7, mean, na.rm = T, fill = NA, align = "right"),
                 normal_values = rollapply(metric, basal_period, mean, na.rm = T, fill = NA, 
                                           align = "right"),
                 normal_values_sd = rollapply(metric, basal_period, sd, na.rm = TRUE, 
                                              fill = NA,  align = "right")) %>%   
            
    
          select(date, metric, weekly_average, normal_values, normal_values_sd)
      
          
        
    } else {
      data %>% 
        
        mutate(
          metric = na_if(X.HR, 0),
          weekly_average = rollapply(metric, 7, mean, na.rm = TRUE, fill = NA, 
                                     align = "right"),
          normal_values = rollapply(metric, basal_period, mean, na.rm = TRUE, 
                                    fill = NA,  align = "right"),
          normal_values_sd = rollapply(metric, basal_period, sd, na.rm = TRUE, 
                                       fill = NA,  align = "right"))      %>% 
        
        select(date, metric, weekly_average, normal_values, normal_values_sd)
      }
    
  })
 
  output$hrv_plot <- renderPlot({
    
    ggplot(dataInput() %>% filter(date >= input$dates[1], date <= input$dates[2]), aes(date)) +
      geom_col(aes(x= date, y = metric), 
               alpha = 0.3) +
      geom_ribbon(aes(ymin = normal_values - 0.75*normal_values_sd, 
                      ymax = normal_values + 0.75*normal_values_sd ),
                  fill = "lightblue", 
                  alpha = 0.5,
                  color = "lightblue",
                  linetype = 2)  +
      geom_line(aes(y = weekly_average), 
                color="steelblue", 
                size=1.5) +
      scale_x_date(name = '', date_breaks = '5 days',
                   date_labels = '%d/%m/%Y') +
      scale_y_continuous(name = "",
                         limits = c(0, 70)) +
      ggtitle(toupper(input$hrv_metric)) + 
      theme(plot.title = element_text( face = "bold", colour = "navyblue", size = 20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 11)) + 
      if (input$hrv_metric == "HRV4T Recovery Points") {
          
          coord_cartesian(ylim = c(min(data$HRV4T_Recovery_Points, na.rm = TRUE), 
                                   max(data$HRV4T_Recovery_Points, na.rm = TRUE)))
      } else if (input$hrv_metric == "ln rMSSD") {
        
        coord_cartesian(ylim = c(log(min(data[data$rMSSD >0, "rMSSD"], na.rm = TRUE)), 
                                 log(max(data$rMSSD, na.rm = TRUE))))
      } else {
        
        coord_cartesian(ylim = c(min(data[data$X.HR > 0, "X.HR"], na.rm = TRUE), 
                                 max(data$X.HR, na.rm = TRUE)))
      }
    
  })
}

shinyApp(ui, server)
