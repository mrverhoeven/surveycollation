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
  library(taxize)



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


# Use taxize to run TNRS -------------------------------------------------
  
  # Sourcing TNRS from iPlant Collaborative
  tnrs <- tnrs(query = taxa, source = "iPlant_TNRS")
  
  # Choosing well-matched names
  tnrs.final <- tnrs
  setDT(tnrs.final)
  tnrs.final[, species := ifelse(score >= 0.5, matchedname, "")]
    
  # Manually revising names
  tnrs.final[species == "Acorus calamus", species := "Acorus americanus"]
  tnrs.final[species == "Calamagrostis", species := "Calamagrostis canadensis"]
  tnrs.final[species == "Phalaris", species := "Phalaris arundinacea"]
  tnrs.final[species == "Phragmites", species := "Phragmites australis"]
  tnrs.final[submittedname == "Characea", species := "Characeae"]
  tnrs.final[submittedname == "Potamogeton sp. Narrow", species := "Potamogeton (narrow)"]  
  tnrs.final[submittedname == "Potamogeton sp. Broad", species := "Potamogeton (broad)"]  
  tnrs.final[submittedname == "Rare Species", species := "DELETE"]  
  tnrs.final[submittedname == "Chara canescens", species := "Chara canescens"]  
  tnrs.final[submittedname == "Chara globularis", species := "Chara globularis"]  
  tnrs.final[species == "Schoenoplectus acutus var. acutus", species := "Schoenoplectus acutus"] 
  tnrs.final[species == "Andromeda polifolia var. latifolia", species := "Andromeda polifolia"] 
  tnrs.final[species == "Asclepias incarnata var. incarnata", species := "Asclepias incarnata"]
  tnrs.final[species == "Dulichium arundinaceum var. arundinaceum", species := "Dulichium arundinaceum"]
  tnrs.final[species == "Potamogeton foliosus subsp. foliosus", species := "Potamogeton foliosus"]
  tnrs.final[species == "Schoenoplectus acutus var. acutus", species := "Schoenoplectus acutus"]
  tnrs.final[species == "Labiatae", species := "Lamiaceae"]
  tnrs.final[species == "Nitella flexilis", species := "Nitella flexilis"]
  tnrs.final[species == "Nitella tenuissima", species := "Nitella tenuissima"]
  tnrs.final[species == "Nitellopsis obtusa", species := "Nitellopsis obtusa"]
  tnrs.final[submittedname == "Valisneria americana", species := "Vallisneria americana"]
  tnrs.final[submittedname == "Lychnothamnus barbatus", species := "Lychnothamnus barbatus"]
  tnrs.final[submittedname == "Tolypella intricata", species := "Tolypella intricata"]
  tnrs.final[submittedname == "Sedge spp.", species := "Carex"]
  tnrs.final[submittedname == "Drepanocladus or Fontinalis spp.", species := "Drepanocladus"]
  tnrs.final[submittedname == "Fern spp.", species := "DELETE"]
  tnrs.final[submittedname == "Gramineae/Poaceae Family", species := "Poaceae"]
  tnrs.final[submittedname == "Nuphar sp. or Nymphaea sp.", species := "Nymphaeaceae"]
  tnrs.final[submittedname == "Schoenoplectus x oblongus", species := "Schoenoplectus x oblongus"]
  tnrs.final[submittedname == "Sparganium species - emergent type", species := "Sparganium (emergent)"]
  tnrs.final[submittedname == "Sparganium species - floating-leaf type", species := "Sparganium (floating)"]
  tnrs.final[submittedname == "Water Cress spp.", species := "Nasturtium officinale"]
  tnrs.final[species == "Alga", species := "DELETE"] 
  
  # Reduced by 50 taxa 
  length(unique(tnrs.final$submittedname))
  length(unique(tnrs.final$species))
  
  write.csv(taxa, file = "data/output/tnrs.final.csv" )
  