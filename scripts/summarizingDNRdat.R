# header ------------------------------------------------------------------

#' # Compiling statewide PI data for MACRONICHE project
#' ### Author: Mike Verhoeven
#' ### Date: 11 Oct 2019

#' # Preamble
#' Load libraries
#+warning=FALSE, message=FALSE 
  library(knitr)
  library(ezknitr)
  library(bit64)




# load in existing databases ----------------------------------------------

#' MN DNR has 3 primary db's with these data in them. First is shallow lakes. 
#' We can get most of that from a 2018 Muthukrishnan Et al. data archive. There
#' are also data collected by Fisheries--these were shared by Donna Perleberg. 
#' Finally, data from the Lakes and Rivers or Lakes Habitat (a.k.a Perleberg 
#' and Radomski) were acquired through a formal data request (which only turned
#' up data collected before 2013).

  # shallow lakes data
  
  library(data.table)
  
  sldat <- fread(file = "data/contributor_data/macroniche_adds/muthukrishnan/Lake_plant_diversity_data.csv")
  
  names(sldat)
  #how many points per survey
  sldat[ , .N , .(lake_name, lake_id, survey_date)]
  
  #can we pull out the wild rice occurrences?
  sldat[ veg_code == "ZIP", .N , .(lake_name, lake_id, survey_date)]
  sldat[ veg_code == "ZIP", .N , .(lake_name, lake_id)]
  sldat[ veg_code == "ZIP", .N , ]
  
  # fisheries data
  
  fshdat <- fread(file = "data/contributor_data/macroniche_adds/dustin/FishDat.csv")

  #need to melt this guy
  #' Now we need to reshape this behemoth...
  names(fshdat)
  #drop species count cols
  fshdat[ ,c(340:347):= NULL , ]
  
  # retain the point ID chars and the depth, then make data long (new row for every observation of a species)
  fshdat_1 = melt(fshdat, id.vars = c(1:31),
               variable.name = "taxon", value.name = "pres")
  fshdat_1

  #rows with Zizania palu:
  fshdat_1[ taxon == "E_ZIP" & pres == "1", .N, ]
  fshdat_1[ taxon == "E_ZIP" & pres == "1",, ]
  fshdat_1[ taxon == "E_ZIP" & pres == "1",.N,.(LAKE_NAME, DOWLKNUM, SURVEYDATE) ]
  fshdat_1[ taxon == "E_ZIP" & pres == "1",.N,.(LAKE_NAME, DOWLKNUM) ]  
  
  # lnr data
  
  lnrdat <- fread(file = "data/contributor_data/macroniche_adds/perleberg/LakePlant export 20190701 finalql.csv")
  
  names(lnrdat)
  
  lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]
  
  summary <- lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]
  
  
  write.csv(summary, file = "lnrsurveysummary.csv")  
  
  lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]
  
  unique(lnrdat[ , splittaxa:= strsplit(lnrdat$OBSERVED_TAXA, ","), ])


 
 
 # footer ------------------------------------------------------------------
 
 
 #' ## Document footer 
 #' 
 #' Document spun with: datestamp <- Sys.Date();ezspin("scripts/g_p_crispus_analysis_mrv.R", out_dir = paste("html_outputs/g_p_crispus_analysis_mrv",datestamp, sep = ""), fig_dir = "figures", keep_md=FALSE)
 #' 
 #' Session Information:
 #+ sessionInfo
 sessionInfo()
  