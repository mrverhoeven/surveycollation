
#'---
#' title: "Pulling data from dataset for a few specific waterbodies"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' 15 May 2020

#' This script will pull out the data for only a few lakes, as requested by 
#' 
#' 
#' 
#' 


# load libraries ----------------------------------------------------------

library(data.table)


# load in data ------------------------------------------------------------

king <- fread(file = "data/output/plant_surveys_mn.csv")



# grab records for requested lakes ----------------------------------------


names(king)
unique(king$LAKE_NAME)

king[LAKE_NAME %in% c("Christmas", "christmas", "Otter", "otter")]


#' # EWM infestation issues:

# exploring issues with EMW infestation classification
#1446
king[DOWLKNUM==27005400, ]#has EWM

#1340 & 1339
king[DOWLKNUM==27018600, ]#has EWM

#1755
king[DOWLKNUM==40000200, ]# the EWM in this survey appears to be a data entry mistake 

#2108
king[DOWLKNUM==49014000 & 
       TAXON == "Myriophyllum spicatum", ] # This survey is actually of Silver Lake
king[LAKE_NAME == "Silver", .N , .(DATASOURCE, SURVEY_ID) ]
king[DOWLKNUM == 62000100, .N, .(SURVEY_DATE, SURVEY_ID) ] # the survey is already accounted for--drop the bad one from Adrianna's list

#2445
king[SURVEY_DATE == "2015-08-26", .N, .(SURVEY_ID, LAKE_NAME, DATASOURCE)]
king[SURVEY_ID == 2445, .N, STA_NBR] # It appears that EWM was indeed observed in Crosby in this survey


#2662
king[SURVEY_ID == 2662, .N, .(STA_NBR, DATASOURCE, SURVEY_DATE)]
king[SURVEY_ID == 2662 &
       TAXON == "Myriophyllum spicatum", .N, .(STA_NBR, DATASOURCE, SURVEY_DATE)] # this is LIKLEY an entry error, but we cannot tell from this, the data appear to have been "region signed"

#' Thes




