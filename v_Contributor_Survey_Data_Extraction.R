#'---
#' title: "Minnesota Macrophytes - 2021 Contributor Survey"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will aggregate the survey data so that it can be shared with
#' contributors for them to verify some basic info in preparation for the
#' publication of the database. 
#' 


#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
# Load Libraries
library(data.table)


# load in data -------------------------------------------------

#observation level data
plants <- fread(input = "data/output/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers


# summarize whats there ---------------------------------------------------

str(plants)
names(plants)

plants[ INDATABASE == TRUE, USEABLE := "Y"]
plants[ INDATABASE == TRUE, CLEANED := "Y"]

# aggregate ---------------------------------------------------------------

metadata <- plants[ , .(NPOINTS = length(unique(POINT_ID)),
                        MAXDEPTHOBS_FT = max(DEPTH_FT),
                        TAXA_OBSERVED = paste(unique(TAXON), collapse = "; " ),
                        TAXA_COUNT = length(unique(TAXON))     ), 
                    .(DATASOURCE, COHORT, LAKE_NAME, SUBBASIN, DOW, SURVEYOR, DATEINFO, DAY, MONTH, YEAR, USEABLE, CLEANED, INDATABASE, SURVEY_ID, INVENTORY_NOTES, SUBMISSION_NOTES   ) ]



#extract rake density scales:

RakeDensScales <- plants[!is.na(REL_ABUND), .(MAX_RAKE_DENS = max(REL_ABUND)), SURVEY_ID]

metadata <- merge(metadata, RakeDensScales, by = "SURVEY_ID", all.x = TRUE)


# write.csv(metadata, file = "data/output/Contributor_Survey_Metadata.csv", row.names = F)


# cursor catcher ----------------------------------------------------------




# footer ------------------------------------------------------------------
#' ## Document footer 
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
