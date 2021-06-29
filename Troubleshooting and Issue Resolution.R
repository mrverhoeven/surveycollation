#'---
#' title: "Troubleshooting and Issue Solving"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will tackle issues in the dataset as they come up. 


#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
# Load Libraries
library(data.table)
library()

# load in data -------------------------------------------------

#observation level data
plants <- fread(input = "data/output/plant_surveys_mn.csv", drop = 1) #import, dropping the exported row numbers



#' Explore problem lakes from Saby  


plants[TAXON == "Myriophyllum spicatum" & DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(EWMobsN  = .N) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ]

plants[DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ]


merge(plants[TAXON == "Myriophyllum spicatum" & DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(EWMobsN  = .N) , .( SURVEY_ID) ],
      plants[DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ],
      by = "SURVEY_ID", all.y = T  )


a <- merge(plants[TAXON == "Myriophyllum spicatum" & DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(EWMobsN  = .N) , .( SURVEY_ID) ],
           plants[DOW %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ],
           by = "SURVEY_ID", all.y = T  )


plants[ DATASOURCE == "lund", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ]
missing_data_surveys[ DATASOURCE == "lund", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ]

plants[ LAKE_NAME == "silver", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID)][order(SURVEY_DATE)]

plants[LAKE_NAME == "cedar", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID)][order(SURVEY_DATE)]


plants[TAXON == "Myriophyllum spicatum" & DATASOURCE == "DNR Fisheries" , .(EWMobsN  = .N) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ][order(LAKE_NAME, SURVEY_DATE)]

# see if there are other surveys where M spicatum seems to be a mis-entry

DNR_IW_list <- fread(input = "data/input/infested-waters (3).csv")

DNR_IW_list[ , `DOW number`]
DNR_IW_list[ , DOW := gsub("-" , "" , `DOW number`)]
DNR_IW_list[ , DOW] 
DNR_IW_list[ , downum := as.numeric(DOW)] 
DNR_IW_list[ downum < 999999, downum :=  downum * 100]

plants[ , sort(unique(DOW)) , ]  

DNR_FISH_EWM <- plants[TAXON == "Myriophyllum spicatum" & DATASOURCE == "DNR Fisheries" , .(EWMobsN  = .N) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ][order(LAKE_NAME, SURVEY_DATE)]

DNR_FISH_EWM[ , inf_waters := match(DNR_FISH_EWM[ , DOW, ],DNR_IW_list[ , downum])]  

DNR_IW_list[ downum == 13003200]

plants[ DOW == 69129100 & TAXON == "Myriophyllum spicatum", ]

plantsurveysEWM <- plants[TAXON == "Myriophyllum spicatum" , .(EWMobsN  = .N) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID, SURVEYOR) ][order(LAKE_NAME, SURVEY_DATE)]

plantsurveysEWM[ , parentDOW := round(DOW/100, digits = 0)*100]

plantsurveysEWM[ ,inf_waters := as.logical(match(plantsurveysEWM[ , parentDOW, ],DNR_IW_list[ , downum])) , ]
plantsurveysEWM[]

plants[ TAXON == "Myriophyllum spicatum" & DOW %in% c(70002200, 69129100, 62008300, 62004700, 10005400) ,  , ]


# write.csv(plantsurveysEWM[ ,inf_waters := as.logical(match(plantsurveysEWM[ , parentDOW, ],DNR_IW_list[ , downum])) , ], file = "data/output/ewm_surveys.csv ")
# write.csv(plants[ TAXON == "Myriophyllum spicatum" & DOW %in% c(70002200, 69129100, 62008300, 62004700, 10005400) ,  , ], file = "data/output/ewm_unlistedDOW_points.csv ")

plant_secchi_gdd_DRUM <- fread("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/macropheno/UMN_DRUM_upload/plants_DRUM.csv")

plant_secchi_gdd_reduced <- fread("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/macropheno/plant_secchi_gdd_reduced.csv")

plant_secchi_gdd_reduced[TAXON == "Myriophyllum spicatum" & DOWLKNUM %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(EWMobsN  = .N) , .(DATASOURCE, LAKE_NAME, DOWLKNUM, SURVEY_DATE, SURVEY_ID.x) ]

a <- merge(plant_secchi_gdd_reduced[TAXON == "Myriophyllum spicatum" & DOWLKNUM %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(EWMobsN  = .N) , .( SURVEY_ID.x) ],
           plant_secchi_gdd_reduced[DOWLKNUM %in% c(49014000, 62004700, 62008300, 70002200, 7005300,  73003500, 82010300), .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOWLKNUM, SURVEY_DATE, SURVEY_ID.x) ],
           by = "SURVEY_ID.x", all.y = T  )

plant_secchi_gdd_DRUM[ DATASOURCE == "lund", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOWLKNUM, SURVEY_DATE, SURVEY_ID.x) ]
missing_data_surveys[ DATASOURCE == "lund", .(nsamp = length(unique(POINT_ID))) , .(DATASOURCE, LAKE_NAME, DOW, SURVEY_DATE, SURVEY_ID) ]