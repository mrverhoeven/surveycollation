
library(sf)
library(maps)
library(rgdal)
library(ggsn)
library(moments)
library(data.table)
library(shiny)
library(plotly)
library(ggspatial)
library(broom)

#' mapping lake survey metadata using Rshiny and plotly 
#' 


# map the survey points form a single lake --------------------------------


#' map a single lake:
#' 
#' 

cedar_PI <-  st_read("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/Statewide management/data_contributions/Archive/raw_data/april_londo/DNR R3S IAPM/Cedar_Scott/point intercept/Map/GIS/Cedar_130m_Survey_points/130m_Survey_points.shp")

cedar_PI_df <- fortify(cedar_PI)


cedar_PI <- readOGR( 
  dsn= "G:/My Drive/Documents/UMN/Grad School/Larkin Lab/Statewide management/data_contributions/Archive/raw_data/april_londo/DNR R3S IAPM/Cedar_Scott/point intercept/Map/GIS/Cedar_130m_Survey_points/130m_Survey_points.shp" )

plot(cedar_PI, pch = 19)

# # 1. Open jpeg file
# png("cedar_PI_pts.png", width = 850, height = 1100, units = "px" )
# # 2. Create the plot
# par(bg=NA)
# plot(cedar_PI, pch = 19, bg = 'transparent')
# # 3. Close the file
# dev.off()



# map our metadata --------------------------------------------------------

#link it to MN lakes df

metadata <- fread(input = "data/input/MN_SurveyMetadata_11Jan2021.csv")      

#other data for fig
usa <- map_data("usa")
canada <- map_data("world", region = "canada")
states <- map_data("state")
mn_df <- subset(states, region == c("minnesota"))

mn_lakes <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/curlyleaf_mgmt/data/spatial_data/MN_lakesonly_trimmed_strytrk.shp")
mn_lk_DT <- as.data.table(mn_lakes)



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
  # geom_sf(data = mn_df, color = "black", alpha = .0, lwd = .75)+
  # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
  theme(text = element_text(size=15))+
  ylab("Latitude")+
  xlab("Longitude")
  

ggplotly(study_map)



# # pull records by georef for MCWD: -------------------------------------------------
# 
#   #here I tried to export the file as a shapefile--no worky
#   # str(md_sf)
#   # write.sf(md_sf, file = "")
#   # st_write(md_sf, "data/output/shapefile/metdata_TEST.shp")
# 
#   #pull in the MCWD shapefile:
#   mcwd_bound <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/GIS Data/MCWD Bound/MCWD_LegalBoundary/MCWD_LegalBoundary/MCWD_LEGAL_BNDRY.shp")
#   
#   #checkCRS alignment
#   st_crs(md_sf)
#   md_sf <- st_transform(md_sf, crs = st_crs(mcwd_bound))
# 
#   #surveys in MCWD:
#   surveys_in_dist <- st_join(md_sf, mcwd_bound, join = st_within)
#   
#   MCWD_metadata <- subset(surveys_in_dist, !is.na(Shape_STLe))
#   
#   
#   #map those data:
#   study_map <- ggplot(MCWD_metadata)+
#     geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
#     # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
#     geom_sf(data = mcwd_bound,  color = "black", alpha = .0, lwd = .75)+
#     # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
#     theme(text = element_text(size=15))+
#     ylab("Latitude")+
#     xlab("Longitude")
#   
#   metadata_for_MCWD <- data.table(MCWD_metadata)
#   
#   metadata_for_MCWD[ , c(31:36) := NULL , ]
#   
#   metadata_for_MCWD[ , .N  , contributor ]
#   
#   #observation level data
#   plants <- fread(input = "data/output/plant_surveys_mn.csv")
#   
#   plants[ , .N , DATASOURCE]
#   
#   plants[DATASOURCE %in% c("Jill Sweet", "Jill_Sweet_19"), .N, SURVEY_ID]
#   
#   plants[SURVEY_ID %in% metadata_for_MCWD[, surveyident], .N, DATASOURCE ]
#   plants[SURVEY_ID %in% metadata_for_MCWD[, surveyident], length(unique(SURVEY_ID)), DATASOURCE ]
#   
#   
#   plants[ , .N , DATASOURCE]
#   
#   #map mn lakes
#   
  st_sf(mn_df)
  
  #map those data:
  study_map <- ggplot(mn_lakes)+
    geom_sf( size = 3, shape = 16, colour = "red",  alpha = 0.3)+
    # geom_sf( data = locs_sf, size = 1, shape = 16, colour = "black",  alpha = 0.3)+
    geom_sf(data = mn_df, x = "lat", y = "long", color = "black", alpha = .0, lwd = .75)+
    # coord_sf(xlim = c(-98, -87.5),  ylim = c(43, 49.5))+
    theme(text = element_text(size=15))+
    ylab("Latitude")+
    xlab("Longitude")
    
  
  # write.csv(metadata_for_MCWD, file = "data/output/MCWD_metadata.csv")
  # 
  # write.csv( plants[SURVEY_ID %in% metadata_for_MCWD[, surveyident], , ], file = "data/output/MCWD_observations.csv")
  # 
  # 

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












