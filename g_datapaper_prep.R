#'---
#' title: "Minnesota Macrophytes - Data Paper"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will do final cleaning and prep of the statewide dataset for
#' sending it out the door with the data paper.
#' 
#' To Do List:
#' -  Drop surveys with no data (hung on import/ not PI data/ no data shared with us)
#' -  Drop surveys that were just CLP
#' -  Incorporate Surveyor feedback
#' -  Re-drop the zeroes from the dataset
#' -  Update rake abundances, rescale all good rake data, change all non abund data to 0/1
#' -  Append Secchi data



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