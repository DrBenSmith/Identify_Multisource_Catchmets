#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Preamble ----------------------------------------------------------------

# library(ggplot2)
# library(shiny)
library(plotly)

# Load in the data:
inds = read.csv("../IMS Results - Flows and NRFA Station data.csv", check.names = FALSE)
GWI=inds$`Groundwater Index`
SWI=inds$`Surface Water Index`

# Set paths to flow records depending on their paths:
data_to_use <- data.frame("files" = c(list.files("../Data/NRFA River Flow Data/", full.names = TRUE, pattern = ".csv"),
                                      list.files("../Data/EA_Data_Request/",      full.names = TRUE, pattern = ".csv")),
                          "source" = c(rep("NRFA", length(list.files("../Data/NRFA River Flow Data/", full.names = TRUE, pattern = ".csv"))),
                                       rep("EA", length(list.files("../Data/EA_Data_Request/", full.names = TRUE, pattern = ".csv")))),
                          "gauge_id" = c(list.files("../Data/NRFA River Flow Data/", full.names = FALSE, pattern = ".csv"),
                                         list.files("../Data/EA_Data_Request/", full.names = FALSE, pattern = ".csv")))
data_to_use$gauge_id = substr(x = data_to_use$gauge_id, start = 1, stop = nchar(data_to_use$gauge_id)-4)


# Begin the App -----------------------------------------------------------

ui <- fluidPage(
    fluidRow(
        column(width = 8, h4("Event Indexes"),
               fluidRow(
                   column(width = 6, plotlyOutput("index_plot")),
                   column(width = 6, plotlyOutput("event_plot"))
))))


server <- function(input, output){

    output$index_plot <- renderPlotly({
        plot_ly(x=GWI,y=SWI, mode="markers", type="scatter", source = "index_plot")
    })

    observe({
        value <- event_data("plotly_click", source = "index_plot")
        
        print(value)

       output$event_plot <- renderPlotly({
            value_row = which(inds$`Groundwater Index`==value$x & inds$`Surface Water Index`==value$y)
            
            plot_ly(x=GWI[value_row], y=SWI[value_row], mode="markers", type="scatter", source = "event_plot")
        })
    })}

shinyApp(ui, server)


# Test the function for reading in the data -------------------------------

# record = "3002"
# 
# # path = paste0("../Data/NRFA River Flow Data", record, ".csv")
# 
# # Read in the data depending on what was its source:
# record_row = which(data_to_use$gauge_id==record)
# 
# if(data_to_use$source[record_row]=="NRFA"){
#     
#     gauge_data = read.csv(data_to_use$files[record_row],
#                           colClasses = c("character", "NULL", "numeric", "NULL"),
#                           skip = 8, na.strings = "-9999.0000")
#     
#     # Convert characters dates into POSTIXct:
#     gauge_data$Date <- as.POSIXct(gauge_data$Date, format="%Y-%m-%d %H:%M", tz = "UTC")
#     
# } else {
#     
#     gauge_data = read.csv(data_to_use$files[record_row],
#                           colClasses = c("character", "numeric", "NULL", "NULL", "NULL", "NULL"),
#                           skip = 20, header = TRUE, na.strings = "---")
#     
#     colnames(gauge_data) = c("Date", "Flow")
#     
#     # Convert characters dates into POSTIXct:
#     gauge_data$Date <- as.POSIXct(gauge_data$Date, format="%d/%m/%Y %H:%M:%S", tz = "UTC")
#     
#     # Remove the empty cells (these are rows that have comments in them but now values).
#     gauge_data <- gauge_data[which(!is.na(gauge_data$Date)),]
#     
# }


