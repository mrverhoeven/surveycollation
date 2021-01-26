#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram

ui <- fluidPage(
    titlePanel("Mapping Metadata"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("contributor",
                           "Data Contributor:",
                           c("All",
                             unique(as.character(md_sf$contributor))))
        ),
        column(4,
               selectInput("lake",
                           "Lake name:",
                           c("All",
                             unique(as.character(md_sf$lake))))
        ),
        column(4,
               selectInput("year",
                           "Year of Survey:",
                           c("All",
                             unique(as.character(md_sf$year))))
        )
    ),
    
    # add a map
    plotlyOutput("myplot"),
    
    # Create a new row for the table.
    DT::dataTableOutput("table")
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    # Filter and display data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- md_sf
        if (input$contributor != "All") {
            data <- data[data$contributor == input$contributor,]
        }
        if (input$lake != "All") {
            data <- data[data$lake == input$lake,]
        }
        if (input$year != "All") {
            data <- data[data$year == input$year,]
        }
        data
    }))
    
    # Render plot based on filtering
    output$myplot <-  renderPlotly({
        
        mapdata <- md_sf
        
        if (input$contributor != "All") {
            mapdata <- mapdata[mapdata$contributor == input$contributor,]
            ggplot(mapdata)+
                geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
                # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
                # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
                # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
                theme(text = element_text(size=15))+
                ylab("Latitude")+
                xlab("Longitude")
        }
        if (input$lake != "All") {
            mapdata <- mapdata[mapdata$lake == input$lake,]
            ggplot(mapdata)+
                geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
                # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
                # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
                # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
                theme(text = element_text(size=15))+
                ylab("Latitude")+
                xlab("Longitude")
        }
        if (input$year != "All") {
            mapdata <- mapdata[mapdata$year == input$year,]
            ggplot(mapdata)+
                geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
                # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
                # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
                # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
                theme(text = element_text(size=15))+
                ylab("Latitude")+
                xlab("Longitude")
        }
        
        ggplot(mapdata)+
            geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
            # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
            # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
            # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
            theme(text = element_text(size=15))+
            ylab("Latitude")+
            xlab("Longitude")
        
            
            
            # hover_line <- event_data("plotly_click")
            
            # what to do upon click
            # if(!is.null(hover_line)){
            #     
            #     selected <- md_sf[dow %in% hover_line[[3]] & mspifoc %in% hover_line[[4]], .(DOWLKNUM) ]
            #     
            #     print(selected)
            #     #print(hover_line)
            #     
            #     # plot bkgd data          
            #     p <- p+ geom_line(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
            #                       aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25) +
            #         geom_point(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
            #                    aes(x = date, y = mspifoc, group = DOWLKNUM))
            #     
            #     #plot selected data
            #     p <- p + geom_line(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
            #                        aes(x = date, y = mspifoc), size=2)+
            #         geom_point(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
            #                    aes(x = date, y = mspifoc), size=1, color = "white")
            #     
            # } else
            #     p <- p+ geom_line(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25)+
            #     geom_point(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM))
            # 
            # ggplotly(p)
        })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
