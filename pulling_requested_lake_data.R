
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
