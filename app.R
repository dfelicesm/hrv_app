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

# Create new columns for different baselines
data <- mutate(data, 
               recovery_7d = rollapply(HRV4T_Recovery_Points, 7, mean, na.rm = T, fill = NA, align = "right"),
               recovery_30d = rollapply(HRV4T_Recovery_Points, 30, mean, na.rm = T, fill = NA,  align = "right"),
               recovery_30d_sd = rollapply(HRV4T_Recovery_Points, 30, sd, na.rm = T, fill = NA,  align = "right"))


ui <- fluidPage(
  # Choosing the length of time to calculate normal HRV values
  selectInput(inputId = "basal_hrv_period", 
              label = "Length of basal HRV period:",
              choices = c("30 days", "60 days"),
              selected = "30 days"), # default period for HRV values is 30 days
  plotOutput("hrv_plot")
  
)

server <- function(input, output) {
  output$hrv_plot <- renderPlot(ggplot(data %>% 
                                         select(date, HRV4T_Recovery_Points, recovery_7d, recovery_30d, recovery_30d_sd) %>%
                                         filter(date >= today() - days(60)),
                                       aes(date)) +
                                  geom_bar(aes(y = HRV4T_Recovery_Points), 
                                           stat = "identity",
                                           alpha = 0.3) +
                                  geom_ribbon(aes(ymin = recovery_30d - 0.75*recovery_30d_sd, 
                                                  ymax = recovery_30d + 0.75*recovery_30d_sd ),
                                              fill = "lightblue", 
                                              alpha = 0.5,
                                              color = "lightblue",
                                              linetype = 2)  +
                                  geom_line(aes(y = recovery_7d), color="steelblue", size=1.5) +
                                  coord_cartesian(ylim = c(min(data$HRV4T_Recovery_Points - 0.15, na.rm = TRUE), 
                                                           max(data$HRV4T_Recovery_Points + 0.15, na.rm = TRUE))) +
                                  scale_x_date(name = '', date_breaks = '5 days',
                                               date_labels = '%d-%b-%y') +
                                  scale_y_continuous(breaks=seq(0, 20, 0.5)) +
                                  ylab("HRV4T Recovery Points\n") + 
                                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
  )
  
}

shinyApp(ui, server)
