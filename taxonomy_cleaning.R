#'---
#' title: "Taxonomy Cleaning"
#' author: "Dan Larkin, Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull the taxonomic names used in the submitted surveys and
#' check them against the accepted names for each taxon. 


#' ## Document Preamble
#+ warning = FALSE
  # load libraries ------------------------------------------------------------------
  # Load Libraries
  library(data.table)



# load in data -------------------------------------------------

  #DNR plant surveys (from DNR databases)
  psD <- fread(input = "data/output/DNR_PI_Data_Combined.csv")
  
  #Surveys from other sources
  ps <- fread(input = "data/output/Surveys_cleaning4Dec.csv")

# pull taxon lists off of each
  psD[ , .N , TAXON]
  DNRtax <- psD[ , .N , TAXON]
  names(ps)
  OTtax <- names(ps)[28:length(names(ps))]
  
  DNRtax[TAXON!= "No Veg Found" , TAXON , ]
  
  taxa <- sort(unique(c(OTtax,DNRtax[TAXON!= "No Veg Found" , TAXON , ])))
  
  taxalist <- data.table(id = NA, taxon = taxa)
  
  taxalist[ , id := .I, ]
  
  write.csv(taxa, file = "data/output/taxalist.csv" )
  
  a <- fread(input = "data/output/taxalist.csv", skip = 1)
  