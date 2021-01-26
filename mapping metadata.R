
library(sf)
library(maps)
library(rgdal)
library(ggsn)
library(moments)
library(data.table)
library(shiny)

#' mapping lake survey metadata using Rshiny and plotly 
#' 



# a little bitty plotly ---------------------------------------------------



library(dplyr)
library(ggplot2)
library(plotly)
ggplot_object <- mpg %>%
    ggplot(aes(x = displ, y = hwy)) + 
    geom_point(mapping = aes(color = class)) + 
    geom_smooth()

ggplotly(ggplot_object)



# a basic shiny app for a data table --------------------------------------

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    if (input$man != "All") {
      data <- data[data$manufacturer == input$man,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    if (input$trans != "All") {
      data <- data[data$trans == input$trans,]
    }
    data
  }))
  
}


library(ggplot2)

fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("man",
                       "Manufacturer:",
                       c("All",
                         unique(as.character(mpg$manufacturer))))
    ),
    column(4,
           selectInput("trans",
                       "Transmission:",
                       c("All",
                         unique(as.character(mpg$trans))))
    ),
    column(4,
           selectInput("cyl",
                       "Cylinders:",
                       c("All",
                         unique(as.character(mpg$cyl))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

# shiny app for lakes in EWM by time --------------------------------------


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(data.table)
library(googledrive)
#drive_download("https://drive.google.com/file/d/1H09jgXMovn1uvqwKAIPNJNV20cGzlElS/view?usp=sharing", overwrite = T)
plotdata <- fread(file = "plotdatashinyEWMviz.csv")

## Visualize lakes through time in UI plot

# display with interactive highlighting  




#Spin up a shiny app:
shinyApp(ui=fluidPage(plotlyOutput("myplot")), server=function(input, output, session){
  
  output$myplot = renderPlotly({
    p <- ggplot(plotdata )+ 
      theme_classic()  +
      xlab("Year") +
      ylab("Myriophyllum spicatum frequency")+
      labs(title =  "click a datapoint (lake survey) to highlight that lake's path through time")
    
    hover_line <- event_data("plotly_click")
    
    # what to do upon click
    if(!is.null(hover_line)){
      
      selected <- plotdata[date %in% hover_line[[3]] & mspifoc %in% hover_line[[4]], .(DOWLKNUM) ]
      
      print(selected)
      #print(hover_line)
      
      # plot bkgd data          
      p <- p+ geom_line(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
                        aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25) +
        geom_point(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
                   aes(x = date, y = mspifoc, group = DOWLKNUM))
      
      #plot selected data
      p <- p + geom_line(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
                         aes(x = date, y = mspifoc), size=2)+
        geom_point(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
                   aes(x = date, y = mspifoc), size=1, color = "white")
      
    } else
      p <- p+ geom_line(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25)+
      geom_point(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM))
    
    ggplotly(p)
  })
})



































#' 



# mapping MN lakes: -------------------------------------------------------

#other data for fig
usa <- map_data("usa")
canada <- map_data("world", region = "canada")
states <- map_data("state")
mn_df <- subset(states, region == c("minnesota"))

mn_lakes <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/curlyleaf_mgmt/data/spatial_data/MN_lakesonly_trimmed_strytrk.shp")
mn_lk_DT <- as.data.table(mn_lakes)

lakesutm <- SpatialPointsDataFrame(mn_lk_DT[,.(center_utm, center_u_1)], mn_lk_DT, proj4string=CRS("+proj=utm +zone=15N +datum=WGS84"))  
locsgeo <- spTransform(lakesutm, CRS("+proj=longlat +datum=WGS84"))
str(locsgeo)
locs_sf <- st_as_sf(locsgeo)




#main
study_map <- ggplot(locs_sf)+
  geom_sf( size = 3, shape = 16, colour = "black",  alpha = 0.3)+
  geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
  coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
  theme(text = element_text(size=15))+
  ylab("Latitude")+
  xlab("Longitude")

  ggplotly(study_map)
  
  


# map from macroniche -------------------------------------------------------------

#' # Map the data
#' Here's all the data we started from:
#plant surveys
plants_map <- fread(file = "data/input/plant_secchi_gdd.csv", drop = 1)

locs <- plants_map[is.na(TAXON) == F , .(.N, unique(center_utm), unique(center_u_1)), by = DOWLKNUM]
weatherlocs <- plants_map[is.na(TAXON) == F , .(.N, longitude = unique(Long), latitude = unique(Lat)), by = STATION.NAME]

#drop added plants layer
rm(plants_map)
summary(locs)
#drop waterbodies w/o any location data (these are NWI numbered wetlands)
locs <- na.omit(locs, cols = c("V2","V3"))
locsutm <- SpatialPointsDataFrame(locs[,.(V2,V3),], locs, proj4string=CRS("+proj=utm +zone=15N +datum=WGS84"))  
locsgeo <- spTransform(locsutm, CRS("+proj=longlat +datum=WGS84"))
str(locsgeo)
locs_sf <- st_as_sf(locsgeo)
summary(locs_sf$N)
locs_sf$bin <- cut(locs_sf$N, breaks = c(0,50, 100, 250, 500, 1000, 5000, 10000, 15000), 
                   labels = c("50", "100", "250", "500", "1000", "5000", "10000", "12000") )
#drop WX station NAs:
weatherlocs <- na.omit(weatherlocs, cols = c("longitude","latitude"))
wxlocsutm <- SpatialPointsDataFrame(weatherlocs[,.(longitude, latitude),], weatherlocs, proj4string=CRS("+proj=longlat +zone=15N +datum=WGS84"))  
wxlocsgeo <- spTransform(wxlocsutm, CRS("+proj=longlat +datum=WGS84"))
str(wxlocsgeo)
wxlocs_sf <- st_as_sf(wxlocsgeo)



#other data for fig
usa <- map_data("usa")
canada <- map_data("world", region = "canada")
states <- map_data("state")
mn_df <- subset(states, region == c("minnesota"))
# mn_lakes <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/curlyleaf_mgmt/data/spatial_data/MN_lakesonly_trimmed_strytrk.shp")

# MN DNR Ecological Classification System: https://gisdata.mn.gov/dataset/geos-ecological-class-system
mn_er <- st_read("data/spatial/shp_geos_ecological_class_system/ecs_provinces_of_mn_v99a.shp")

#TNC ecoregions from resilient sites analysis: https://gisdata.mn.gov/dataset/env-resilient-sites-tnc
tnc_er<- st_read("data/spatial/shp_env_resilient_sites_tnc/Ecoregions.shp")
tnc_er <- tnc_er[c(2,3,4,8) , ]

#main
study_map <- ggplot(locs_sf)+
  geom_sf(data = tnc_er, aes(fill = ECO_NAME), alpha = 0.2 )+
  #geom_sf(data = mn_er, aes(fill = PROVNAME), alpha = 0.2 )+
  scale_fill_viridis_d()+
  geom_sf( size = 3, shape = 16, colour = "black",  alpha = 0.3)+
  geom_sf(data = wxlocs_sf, shape = 10 , colour = "red", alpha = 1.0, size = 5)+
  geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
  coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
  theme(text = element_text(size=15))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme(legend.position = c(.755,.29),
        legend.background = element_rect(fill = "white"))+
  guides(fill=guide_legend(title="Ecoregion"))

#inset
inset <- ggplotGrob(
  ggplot() +
    geom_polygon(data = usa,
                 aes(x=long, y = lat, group = group),
                 fill = "gray90", color = "gray50", size = 0.3) +
    # geom_polygon(data = canada, aes(x=long, y = lat, group = group),
    # fill = "gray90", color = "gray50", size = 0.3) +
    coord_fixed(xlim = c(-110, -55),  ylim = c(25, 60), ratio = 1.2) +
    geom_polygon(data = mn_df, aes(x = long, y = lat), color = "black", size = 0.3,
                 fill = "gray40") +
    geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
    theme_bw() +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())+
    coord_map("polyconic")
)    

#combine
g3 <- study_map +
  north(mn_df, anchor = c(x = -88.15, y = 44)) +
  annotation_custom(grob = inset, xmin = -91.5, xmax = -87.75,
                    ymin = 42.7)+
  scalebar(mn_df, dist = 50, dist_unit = "km",
           transform = TRUE, model = "WGS84", st.size = 3)
#figure construction
#Map
tiff("Map.tiff", res = 600, width = 9, height = 12, units = "in", compression = "lzw", type = "cairo")
plot(g3) # Make plot
dev.off()




# map our metadata --------------------------------------------------------

#link it to MN lakes df

metadata <- fread(input = "data/input/MN_SurveyMetadata_11Jan2021.csv")      


metadata[ , summary(dow)]   
mn_lk_DT[ , dowlknum := as.numeric(as.character(dowlknum))]
mn_lk_DT[ , summary(dowlknum) ]

spatial_mnlakesdata <- mn_lk_DT[ , .(dowlknum,center_utm,center_u_1)]

spatial_metadata <- merge.data.table(metadata, spatial_mnlakesdata, by.x = "dow", by.y = "dowlknum")

#map it


metadatutm <- SpatialPointsDataFrame(spatial_metadata[,.(center_utm, center_u_1)], spatial_metadata, proj4string=CRS("+proj=utm +zone=15N +datum=WGS84"))  
mdgeo <- spTransform(metadatutm, CRS("+proj=longlat +datum=WGS84"))
str(mdgeo)
md_sf <- st_as_sf(mdgeo)


study_map <- ggplot(md_sf)+
  geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
  # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
  # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
  # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
  theme(text = element_text(size=15))+
  ylab("Latitude")+
  xlab("Longitude")

ggplotly(study_map)





# plop that into a shiny app ----------------------------------------------



#Spin up a shiny app:
shinyApp(ui=fluidPage(plotlyOutput("myplot")), server=function(input, output, session){
  
  output$myplot = renderPlotly({
    p <- ggplot(md_sf)+
      geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
      # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
      # geom_polygon(data = states, aes(x=long, y = lat, group = group), color = "black", alpha = .0, lwd = .75)+
      # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
      theme(text = element_text(size=15))+
      ylab("Latitude")+
      xlab("Longitude")  
    
    # what to do upon click
    if(!is.null(hover_line)){
      
      selected <- plotdata[date %in% hover_line[[3]] & mspifoc %in% hover_line[[4]], .(DOWLKNUM) ]
      
      print(selected)
      #print(hover_line)
      
      # plot bkgd data          
      p <- p+ geom_line(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
                        aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25) +
        geom_point(data = plotdata[!(DOWLKNUM %in% selected$DOWLKNUM),],
                   aes(x = date, y = mspifoc, group = DOWLKNUM))
      
      #plot selected data
      p <- p + geom_line(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
                         aes(x = date, y = mspifoc), size=2)+
        geom_point(data = plotdata[DOWLKNUM %in% selected$DOWLKNUM,],
                   aes(x = date, y = mspifoc), size=1, color = "white")
      
    } else
      p <- p+ geom_line(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM), alpha = .25)+
      geom_point(data = plotdata, aes(x = date, y = mspifoc, group = DOWLKNUM))
    
    ggplotly(p)
  })
})












