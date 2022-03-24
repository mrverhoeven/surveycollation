#'---
#' title: "Minnesota Macrophytes - Hello world"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull the observation level data for the aquatic plant point
#' intercept survey database. 
#' 
#' With the data, we will run through a description
#' of the fields, plot and summarize to familiarize, and use these exercises to
#' review the nuances, extent, and limitations of the data. 
#' 
#' Finally, this script
#' includes code to aggregate the data up to the survey level (i.e., compute survey
#' statistics), and to aggregate up to the lake level (to create lake level
#' descriptors like species lists or max occurrence depths).
#' 


#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
  # Load Libraries
  library(data.table)
  library(ggplot2)
  library(stringr)
  # 
  # library(sf)
  # library(maps)
  # library(rgdal)
  # library(ggsn)
  # library(moments)
  # library(shiny)
  # library(plotly)
  # library(ggspatial)
  # library(broom)
  # 
  # library(vegan)


# load in functions -------------------------------------------------------

#moves through a matrix replacing NA with zeroes 
f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}



# load in data -------------------------------------------------

  #observation level data
  plants <- fread(input = "z_data/output/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers

  
# summarize whats there ---------------------------------------------------
  
  str(plants) #what data formats?
  names(plants) #field names
  
  plants[ , length(unique(SURVEY_ID)) , ] #how many surveys in all?
  plants[ INDATABASE == T , length(unique(SURVEY_ID))] #how many surveys do we have the data in our db for?
  plants[ , length((unique(DOW))) , ] #how many lake in all?
  plants[ , length(unique(YEAR)) , ] #how many years of data?
  plants[ , length(unique(POINT_ID)),] #how samples pulled from the lake?
  plants[!is.na(TAXON) , length(unique(OBS_ID))] # how many times was a plant identified in these data? 

  
#' Lets see how many surveys we have been given by each contributor
  plants[ , unique(DATASOURCE) ,] #we need to clean these up
  plants[ , sort(unique(tolower( word(DATASOURCE, sep = fixed("_"))))) , ] #check
  plants[ , DATASOURCE := tolower( word(DATASOURCE, sep = fixed("_"))) , ] #DO
  plants[DATASOURCE == "fieldseth" , DATASOURCE := "eric fieldseth"  , ] # Combine
  plants[DATASOURCE == "crwd" , DATASOURCE := "britta belden"  , ] # Combine
  plants[DATASOURCE == "fieldseth" , DATASOURCE := "eric fieldseth"  , ] # Combine
  
  ggplot(plants[ , .N, .(SURVEY_ID, DATASOURCE, INDATABASE)], aes(DATASOURCE, fill = INDATABASE))+
    geom_bar(stat = "count", position = "stack" )+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(label = "n surveys by contributor")
    
#' The database has all the surveys we know exist for MN in it, including those
#' for which we do not have the data. It is often useful to snip those no-data
#' ones off right away to avoid running any calcs using all those rows w/o any
#' species data.
#' 

  missing_data_surveys <- plants[ INDATABASE == F] 
  
  plants <- plants[INDATABASE == T]

#' Now "plants" is only those surveys for which we were able to gather and
#' collate the data. 
  
  #how many surveys and how many points were sampled in each?
  plants[ , length(unique(POINT_ID)) , SURVEY_ID]
  hist(plants[ , length(unique(POINT_ID)) , SURVEY_ID][ , V1])
  
  #how many unique TAXA?
  unique(plants$TAXON)
  # N taxa per survey:
  plants[ , .("Ntaxa" = length(unique(TAXON))) , SURVEY_ID] #if you want to name cols on the fly you need to wrap in .() which makes list from them 
    hist(plants[ , length(unique(TAXON)) , SURVEY_ID][ , V1], main = "N taxa per survey")
    hist(plants[ , length(unique(TAXON)) , POINT_ID][ , V1], main = "N taxa per point")
  
#' That is a good intro to the structure of these data. Some things to keep in 
#' mind:
#'  - Each row is an observation of a species at a point. 
#'  - If there was no species observed, the row is a placeholder for that point,
#'  and that row will have NO_VEG_FOUND set to TRUE, but TAXON and density will 
#'  be blank
#'  - If there are no data for a survey, that row is a placeholder for the
#'  survey, and all of the point-level data will be NAs
#'   


# build up a survey level table -------------------------------------------


    surveys <- plants[ , .(tot_n_samp = length(unique(POINT_ID)))  , SURVEY_ID ]
    
    #richness, ensuring we aren't counting NAs:
    plants[ , length(unique(TAXON))  , SURVEY_ID ]
    plants[!is.na(TAXON) , length(unique(TAXON))  , SURVEY_ID ]
    plants[ , sum(is.na(TAXON)), SURVEY_ID][V1<1]#not every survey has at least 1 NA in the TAXON column
    plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID]
    
    #we can subtract 1 if the taxon column contained NA
    plants[ , length(unique(TAXON))   , SURVEY_ID ][ , V1] - plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID][,V1]
    
    #add that to the surveys dataset
    surveys[  , taxa_richness := #take the "taxon count" and subtract one if the survey includes NAs (see next two lines)
                plants[ , length(unique(TAXON))   , SURVEY_ID ][ , V1]-# ("total richness", but counts NAs as a taxon) minus
                plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID][,V1],]#  (each survey get a 0 if no NAs or a 1 if contains NA's)
    
    # extent of vegetation in survey (prop vegetated)
    plants[NO_VEG_FOUND == F, length(unique(POINT_ID)) , SURVEY_ID ]
    surveys <- merge(surveys,plants[NO_VEG_FOUND == F, .(n_points_vegetated=length(unique(POINT_ID))) , SURVEY_ID ], by = "SURVEY_ID", all.x = TRUE)
    f_dowle3natozeros(surveys, c("n_points_vegetated"))
    surveys[ , prop_veg := n_points_vegetated/tot_n_samp ,]
    
    
    
    #create a plant observation matrix (species abund by survey)
    plants[!is.na(TAXON) , .("count" = .N) , .(SURVEY_ID,TAXON)]
    a <- dcast(plants[!is.na(TAXON) , .("count" = .N) , .(SURVEY_ID,TAXON)], SURVEY_ID ~ TAXON, value.var = "count")
    f_dowle3natozeros(a, c(2:236))
    
    
    #diversity metrics go here
    names(a)
    a[ , shannon_div := diversity(a[,2:236],index = "shannon") ]
    a[ , simpsons_div := diversity(a[,2:236],index = "simpson") ]
    setcolorder(a, neworder = c(1,237,238))
    
    
    
    #max depth surveyed:
    plants[ ,max(DEPTH_FT) , SURVEY_ID]
    surveys[ , max_depth_surveyed := plants[ ,max(DEPTH_FT, na.rm = T) , SURVEY_ID][,V1]  , ]
    
    #other depth surveyed stats:
    surveys[ , min_depth_surveyed := plants[ ,min(DEPTH_FT, na.rm = T) , SURVEY_ID][,V1]  , ]
    surveys[ , mean_depth_surveyed := plants[ ,mean(DEPTH_FT, na.rm = T) , SURVEY_ID][,V1]  , ]
    surveys[ , median_depth_surveyed := plants[ ,median(DEPTH_FT, na.rm = T) , SURVEY_ID][,V1]  , ]
    surveys[ , max_depth_surveyed := plants[ ,IQR(DEPTH_FT, na.rm = T) , SURVEY_ID][,V1]  , ]
    
    
    #vegetated depths data
    #max depth vegetated:
    plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT)) , SURVEY_ID]
    surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT, na.rm = T)) , SURVEY_ID] , by = "SURVEY_ID" , all.x =TRUE )
    
    #other depth vegetated stats:
    surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("min_depth_vegetated" = min(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
    surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("mean_depth_vegetated" = mean(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
    surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("median_depth_vegetated" = median(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
    surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("IQR_depth_vegetated" = IQR(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
    
    #species matrix for lakes
    surveys <- merge(surveys, a, by = "SURVEY_ID", all.x = T)
    
    names(surveys)
    summary(surveys[,1:16])
    
    #append lake data (basic data from plants db) to these
    names(plants)
    
    surveys <- merge(plants[ , .("nobs" = .N) , .(SURVEY_ID, DATASOURCE, LAKE_NAME, DOW, DATESURVEYSTART, SUBBASIN, MULTIPARTSURVEY) ],surveys,  by = "SURVEY_ID")
    
    names(surveys)
    

    #add in spatial locations & more lake data:
    mn_lakes <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/GIS Data/Starry Trek 2017/shp_water_dnr_hydrography (1)/dnr_hydro_features_all.shp ")
    mn_lk_DT <- as.data.table(mn_lakes)
    mn_lk_DT[ , dowlknum := as.numeric(dowlknum) ,]
    summary(mn_lk_DT)
    
    mn_lk_DT <- mn_lk_DT[!is.na(dowlknum)]
    
    duplicated(mn_lk_DT$dowlknum)
    mn_lk_DT <- mn_lk_DT[!duplicated(mn_lk_DT$dowlknum)]
    
    
    surveys <- merge(surveys, mn_lk_DT, by.x = "DOW", by.y = "dowlknum", all.x = T)
    
    names(surveys)
    setcolorder(surveys, c(1:4, 259:285 ))
    
    
    mn_lk_DT_plants <- mn_lk_DT[dowlknum %in% plants[,unique(DOW) , ] & !is.na(dowlknum)]
    
    mn_lk_plants_utm <- SpatialPointsDataFrame(mn_lk_DT_plants[,.(center_utm, center_u_1)], mn_lk_DT_plants, proj4string=CRS("+proj=utm +zone=15N +datum=WGS84"))  
    mdgeo <- spTransform(mn_lk_plants_utm, CRS("+proj=longlat +datum=WGS84"))
    str(mdgeo)
    md_sf <- st_as_sf(mdgeo)
    
    lake_geo_area <- data.table(st_coordinates(st_centroid(md_sf$geometry)), dow = md_sf$dowlknum, area_ac = md_sf$acres)
    
    surveys <- merge(surveys, lake_geo_area, by.x = "DOW", by.y = "dow", all.x = T)
    
    names(surveys)
    setcolorder(surveys, c(1:31, 286:288 ))
    
    names(surveys)
    
    
    
    
    #visualization and exploration
    
    ggplot(surveys, aes(acres, shannon_div))+
      geom_point()+
      scale_x_log10()+
      geom_smooth(method = "lm")
    
    ggplot(surveys, aes(Y, shannon_div))+
      geom_point()+
      geom_smooth(method = "lm")
    
    ggplot(surveys, aes(acres, Y))+
      geom_point()+
      scale_x_log10()
    
    
    ggplot(surveys, aes(`Myriophyllum spicatum`/tot_n_samp, shannon_div))+
      geom_point()+
      geom_smooth()
    ggplot(surveys, aes(`Myriophyllum sibiricum`/tot_n_samp, shannon_div))+
      geom_point()+
      geom_smooth()
    ggplot(surveys, aes(`Potamogeton crispus`/tot_n_samp, shannon_div))+
      geom_point()+
      geom_smooth()
    
    # ggplot(surveys, aes(`Myriophyllum spicatum`/tot_n_samp, shannon_div, group = DOW))+
    #   geom_line()
    # 

    
    

    
    
    

# figures for DL's LCCMR presentation ---------# figures for DL's LCCMR presentation ---------# figures for DL's LCCMR presentation -------------------------------------

    # 
    # # var(richness) ~ latitude, area
    # 
    # plants[ , length(unique(TAXON)) , SURVEY_ID] 
    # 
    # ggplot( plants[ , length(unique(TAXON)) , SURVEY_ID]  , aes(V1) , )+
    #   geom_histogram( bins = 49)
    # 
    # #need to draw in the lattitude and area data:
    # 
    #other data for fig
    # usa <- map_data("usa")
    # canada <- map_data("world", region = "canada")
    # states <- map_data("state")
    # mn_df <- subset(states, region == c("minnesota"))
    
    mn_lakes <- st_read("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/GIS Data/Starry Trek 2017/shp_water_dnr_hydrography (1)/dnr_hydro_features_all.shp ")
    mn_lk_DT <- as.data.table(mn_lakes)
    
    #we are going to lose 118k observations in this join:
    sum(is.na(match(surveys$DOW, mn_lk_DT$dowlknum)))
    sum(!is.na(match(plants$DOW, mn_lk_DT$dowlknum)))
    
    mn_lakes_plants <- subset(mn_lakes, dowlknum %in% plants[,unique(DOW) , ] & !is.na(dowlknum))
    mn_lk_DT_plants <- mn_lk_DT[dowlknum %in% plants[,unique(DOW) , ] & !is.na(dowlknum)]
    
    mn_lk_plants_utm <- SpatialPointsDataFrame(mn_lk_DT_plants[,.(center_utm, center_u_1)], mn_lk_DT_plants, proj4string=CRS("+proj=utm +zone=15N +datum=WGS84"))  
    mdgeo <- spTransform(mn_lk_plants_utm, CRS("+proj=longlat +datum=WGS84"))
    str(mdgeo)
    md_sf <- st_as_sf(mdgeo)
    
    lake_geo_area <- data.table(st_coordinates(st_centroid(md_sf$geometry)), dow = md_sf$dowlknum, area = md_sf$acres)
    
    
    plants[!is.na(TAXON) , length(unique(TAXON)) , .(SURVEY_ID, DOW) ]
    
    lake_geo_area[ , dow := as.numeric(dow) ,]
    
    lake_lvl <- lake_geo_area[plants[!is.na(TAXON) , length(unique(TAXON)) , .(SURVEY_ID, DOW) ], on = .(dow = DOW)]
    
    lake_lvl[!is.na(X), , ]
    
    # richness ~ lat
    ggplot( lake_lvl, aes(Y, V1) )+
      geom_point()+
      geom_smooth(method = "lm")
    # richness ~ area
    ggplot( lake_lvl, aes(log(area), V1) )+
      geom_point()+
      geom_smooth(method = "lm")
    
    
    #lakewide foc v. rake density
    
    plants[  , length(unique(POINT_ID)) , SURVEY_ID ]
    
    plants[ TAXON == "Myriophyllum spicatum" , .(N_EWM = length(unique(POINT_ID)), Rake_abund = mean(REL_ABUND)) , SURVEY_ID   ][plants[  , .(N_samples = length(unique(POINT_ID))) , SURVEY_ID ], on = .(SURVEY_ID = SURVEY_ID) ]
    
    EWMabunds <- plants[ TAXON == "Myriophyllum spicatum" , .(N_EWM = length(unique(POINT_ID)), Rake_abund = mean(REL_ABUND)) , SURVEY_ID   ][plants[  , .(N_samples = length(unique(POINT_ID))) , SURVEY_ID ], on = .(SURVEY_ID = SURVEY_ID) ][ , prop := N_EWM/N_samples , ]
    
    
    ggplot(EWMabunds, aes(prop, Rake_abund))+
      geom_point()
    
    ggplot(EWMabunds, aes(prop))+
      geom_histogram()
    ggplot(EWMabunds, aes(Rake_abund))+
      geom_histogram()
    
    #better rake density data: These have been adjusted to 
    rake_dens <- fread(input = "data/output/rake_abund_surveys_mn.csv", drop = 1)
    
    
    rake_dens[ , prop_rd := REL_ABUND/3]
    
    rake_dens[ TAXON == "Myriophyllum spicatum" , .(N_EWM = length(unique(POINT_ID)), Rake_abund = mean(REL_ABUND)) , SURVEY_ID   ]
    
    EWMabunds[SURVEY_ID %in%
                rake_dens[ TAXON == "Myriophyllum spicatum" , .(N_EWM = length(unique(POINT_ID)), Rake_abund = mean(REL_ABUND)) , SURVEY_ID   ][,SURVEY_ID,],
              
              Rake_abund := rake_dens[ TAXON == "Myriophyllum spicatum" , .(N_EWM = length(unique(POINT_ID)), Rake_abund = mean(REL_ABUND)) , SURVEY_ID   ][ , Rake_abund,]
              
              
              ]
    
    ggplot(EWMabunds, aes(prop, Rake_abund))+
      geom_point()+
      geom_abline(slope = 3, intercept = 1)+
      ylim(c(1,3))
    
    #scale dependency of invader/native relationships
    
    plants[TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), length(unique(TAXON)) , SURVEY_ID  ][ , hist(V1), ]
    
    #invasive species count
    plants[TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(inv_count = length(unique(TAXON))) , SURVEY_ID  ]
    
    #richness per survey
    plants[!TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(inv_count = length(unique(TAXON))) , SURVEY_ID  ]
    
    #combine these
    inv_nat_rich_lake <- merge(plants[!TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(nat_count = length(unique(TAXON))) , SURVEY_ID  ],plants[TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(inv_count = length(unique(TAXON))) , SURVEY_ID  ], by = "SURVEY_ID", all = T)
    
    inv_nat_rich_lake[is.na(nat_count), nat_count := 0]
    inv_nat_rich_lake[is.na(inv_count), inv_count := 0]
    
    ggplot(inv_nat_rich_lake, aes(jitter(nat_count), jitter(inv_count)))+
      geom_point()+
      geom_smooth()
    
    
    #combine these
    inv_nat_rich_point <- merge(plants[!TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(nat_count = length(unique(TAXON))) , POINT_ID  ],plants[TAXON %in% c("Myriophyllum spicatum", "Potamogeton crispus", "Najas minor", "Typha glauca"), .(inv_count = length(unique(TAXON))) , POINT_ID  ], by = "POINT_ID", all = T)
    
    inv_nat_rich_point[is.na(nat_count), nat_count := 0]
    inv_nat_rich_point[is.na(inv_count), inv_count := 0]
    
    ggplot(inv_nat_rich_point, aes(jitter(nat_count), jitter(inv_count)))+
      geom_point()+
      geom_smooth(method = "lm")
    
    inv_nat_rich_lake <- merge(inv_nat_rich_lake, EWMabunds, by = "SURVEY_ID")
    
    inv_nat_rich_lake[is.na(prop), prop := 0]
    
    ggplot(inv_nat_rich_lake, aes(nat_count, prop))+
      geom_point()
    
    ggplot(inv_nat_rich_lake, aes(nat_count, Rake_abund))+
      geom_point()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    hist(plants[ , length(unique(TAXON)) , SURVEY_ID][ , V1])

  plants[TAXON == "Ruppia cirrhosa", .N, .(SURVEY_ID,SURVEY_DATE,DATASOURCE,SURVEYOR,LAKE_NAME,DOW)]  
  
  plants[TAXON == "Potamogeton perfoliatus", .N, .(SURVEY_ID,SURVEY_DATE,DATASOURCE,SURVEYOR,LAKE_NAME,DOW)] 
  
  taxalist <- plants[ , .N , TAXON]
  
#' Aggregate the data up to the survey level
   
  
