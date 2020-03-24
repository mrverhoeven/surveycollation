#'---
#' title: "Compiling DNR PI data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---


# header ------------------------------------------------------------------


  strttime <- Sys.time()
  getwd()
#' # Preamble
#' Load libraries
#+warning=FALSE, message=FALSE 
  # library(knitr)
  # library(ezknitr)
  # library(bit64)
  library(data.table)
  library(tidyr)
  library(stringr)




#' # Load in DNR databases
#' MN DNR has 3 primary db's with these data in them. First is shallow lakes. 
#' We can get most of that from a 2018 Muthukrishnan Et al. data archive. There
#' are also data collected by Fisheries--these were shared by Donna Perleberg. 
#' Finally, data from the Lakes and Rivers or Lakes Habitat (a.k.a Perleberg 
#' and Radomski) were acquired through a formal data request (which only turned
#' up data collected before 2013).


# shallow lakes -----------------------------------------------------------

#' ## Shallow Lakes Program
  # shallow lakes data
  
  sldat <- fread(file = "data/input/contributor_data/macroniche_adds/muthukrishnan/Lake_plant_diversity_data.csv") # use data.table to pull in dataset
  
  names(sldat) #view the names within the sldats datatable

#' We can see that the shallow lakes data are in a long form, with each
#' record representing a plant occurrence, and sites with no plant occurrences
#' having a single record with a *no vegetation present* for the species name.
#' Occurrences with unsurveyed points are marked *no* in the
#' *sample_point_surveyed* field. 
  
  #how many points per survey
  sldat[ , .N , .(lake_name, lake_id, survey_date)]
  
  #can we pull out the wild rice occurrences?
  sldat[ veg_code == "ZIP", .N , .(lake_name, lake_id, survey_date)]
  sldat[ veg_code == "ZIP", .N , .(lake_name, lake_id)]
  sldat[ veg_code == "ZIP", .N , ]

  
# fisheries div -----------------------------------------------------------

#' ## Fisheries Program
#' Now we want to pull in the next dataset. These are PIs from the Fisheries
#' division. 
  
  # fisheries data
  
  fshdat <- fread(file = "data/input/contributor_data/macroniche_adds/dustin/FishDat.csv")

  #need to melt this guy
  #' Now we need to reshape this behemoth...
  names(fshdat)
  #drop species count cols
  fshdat[ ,c(265, 340:347):= NULL , ]
  
  #find an indicator col for sample taken
  fshdat[, summary(as.factor(HAS_DATA)), ] #according to the metadata record, X, Y, Z all were sampled 
  fshdat[ , summary(DEPTH_FT), ]
  fshdat[ , 32:338 , ][ ,sum() , ]
  fshdat[ , vegfound := rowSums(.SD), .SDcols = 32:338]
  fshdat[, summary(vegfound)]
  fshdat[vegfound < 1 ,summary(as.factor(HAS_DATA)),]
  # fshdat[, vegfound, ]
  fshdat[ vegfound == 0 , novegfound := 1, ]
  fshdat[is.na(novegfound) == T , novegfound := 0, ]
  fshdat[, vegfound := NULL, ]

  # retain the point ID chars and the depth, then make data long (new row for every observation of a species)
  fshdat_1 = melt(fshdat, id.vars = c(1:31),
               variable.name = "taxon", value.name = "pres" )
  
#' Alright. Now we have the Fisheries data in a long form. We now want to 
#' remove the species with a null occurrence, and somehow retain a line for even
#' those points which have a sample with nothing found or with no sample taken.
#' We can do this by saying remove rows with zeroes for presence. Because we
#' have added the no veg found as a taxon, this will retain a line with no 
#' vegfound ofr each case where a sample was taken and no plants recovered on
#' rake. One tricky and seemingly impossible thing that we see in these data is 
#' that it appears as though **every** sample point was actually sampled--we can
#' almost certainly assume this is not the case.
#' 
#' 
 
  # find out what landed in taxon & pres cols
  fshdat_1[ , summary(taxon) , ]
  fshdat_1[ , summary(pres) , ]
  
  #drop all of the zeros (not observed) in this plant 
  fshdat_2 <- fshdat_1[pres > 0,] # retain only rows with pres > 0
  str(fshdat_2)
  
  #now convert these codes to taxonomic names
  fshdat_2[ , sort(unique(taxon)), ]
  
  fshdat_2[ , taxon := word(taxon, start = -1, sep = fixed("_")), ]
  
  fish_codes <- fread(file = "data/input/contributor_data/macroniche_adds/dustin/fisheries_codes_MRV.csv")
  
  #these fish data have a field for no species found
  a <- data.table(SCIENTIFIC_NAME = "No Veg Found", PLANT_SPECIES_ABBREV = "novegfound")
  fish_codes <- rbind(fish_codes[, 3:4],a)
  
  # CHECK MATCHES
  # match(fshdat_2$taxon, fish_codes$PLANT_SPECIES_ABBREV)
  
  fshdat_2[ , taxonfull := fish_codes$SCIENTIFIC_NAME[match(fshdat_2$taxon, fish_codes$PLANT_SPECIES_ABBREV)] , ]
  
  fshdat_2[ , sort(unique(taxonfull)), ]
  
  #remove previous versions datafiles from workspace
  rm(fshdat, fshdat_1,fish_codes,a)


# lakes and rivers --------------------------------------------------------

#' ### Lakes and Rivers Program
  # lnr data
  
  lnrdat <- fread(file = "data/input/contributor_data/macroniche_adds/perleberg/LakePlant export 20190701 finalql.csv")
  
#' ## Updated datset:
  v2lnr <- fread(file = "data/input/contributor_data/2020_submission/Lake_Ecology_Perleberg/LakePlant export 20190806_notfinal_JK.csv")
  
  # compare the two datasets:
  names(lnrdat)
  names(v2lnr)  
  
  old <- lnrdat[ , .N , .(LAKE_NAME, SURVEY_ID, DOWLKNUM)]
  new <- v2lnr[ , .N , .(LAKE_NAME, SURVEY_ID, DOWLKNUM)]
  new_v_old_lnr <- merge(old,new, c("LAKE_NAME", "SURVEY_ID", "DOWLKNUM"), all = T)
  
  # write.csv(new_v_old_lnr, file = "new_lnr_comparison.csv")
  
  
  names(v2lnr)
  
  v2lnr[ , .N , .(LAKE_NAME, SURVEY_ID, SURVEY_DATE, DOWLKNUM)]
  
  summary <- v2lnr[ , .N , .(LAKE_NAME, SURVEY_ID, DOWLKNUM)]
  
  # write.csv(summary, file = "lnrv2surveysummary.csv")  
  
  v2lnr[ , .N , .(LAKE_NAME, SURVEY_ID, DOWLKNUM)]

  int <- data.table(sp = rep("sp", 16), num = 1:16)
  
  int[ , spnum := paste(sp,num, sep = "")]
  
  # lnrdat_1 <- separate(v2lnr, OBSERVED_TAXA, into = c(int$spnum) )
  # names(lnrdat_1)
  
  # try to preserve relative rake abundance vals if any are there
  summary(v2lnr$OBSERVED_TAXA_REL_ABUND)
  
  v2lnr[ , OBSERVED_TAXA_REL_ABUND := gsub("}", "", gsub("{", "", OBSERVED_TAXA_REL_ABUND, fixed = TRUE), fixed = TRUE), ]
  v2lnr[ , OBSERVED_TAXA_REL_ABUND := gsub('"', "", OBSERVED_TAXA_REL_ABUND, fixed = TRUE), ]
  
  lnrdat_1 <- separate(v2lnr, OBSERVED_TAXA_REL_ABUND , into = c(int$spnum), sep = "," )
  
  lnrdat_1[sp1 == "" , summary(as.factor(VEG_REL_ABUNDANCE_DESCR)) , ]# deal with locs where no veg was found.
  
  names(lnrdat_1)
  lnrdat_2 <-  melt(lnrdat_1, id.vars = c(1:20))
  
  # move the abundance data out to a new column (anything after the colon)
  unique(lnrdat_2$value) # There are some abundance data.
  lnrdat_2 <- separate(lnrdat_2, value , into = c("TAXON","REL_ABUND" ), sep = ":" )
  
#' Now we have some cleaning to do. What we have is a generic species column 
#' (variable) that has important info because it is a placeholder for all points
#' and where no species were observed at a point, the TAXON column shows "".
#' For all other points, the TAXON column shows the species code, and the 
#' REL_ABUND column shows the relative abundance for that species (if recorded).
#' 
#' So, we've got three things to do: 1) Mark TAXON as NA where currently "" and
#' variable == Sp1. 2) Delete the "variable" column. 3) Convert word abundances
#' to the 1-3 (per Josh Knopik's description of the assignmnet scheme). 

#' We'll want to simplify these and drop unsampled sites from the data: 
#' unsampled sites should not be used in calculating lake stats, and we haven't
#' got species data for those sites.  

  lnrdat_2[ , SAMPLE_TYPE_DESCR := as.factor(SAMPLE_TYPE_DESCR), ]
  lnrdat_2[, summary(SAMPLE_TYPE_DESCR) , ]
  lnrdat_2[, summary(as.factor(VEG_REL_ABUNDANCE_DESCR)) , ]
  lnrdat_2[ SAMPLE_TYPE_DESCR != "sampled", summary(as.factor(VEG_REL_ABUNDANCE_DESCR)) ]
  lnrdat_3 <- lnrdat_2[SAMPLE_TYPE_DESCR == "sampled" , , ] 
  

#' Mark TAXON blanks as NA for no veg detected  
  lnrdat_3[ ,.N , VEG_REL_ABUNDANCE_DESCR]
  lnrdat_3[VEG_REL_ABUNDANCE_DESCR == "vegetation not detected", TAXON:= "novegfound", ]
  lnrdat_3[ , .N , TAXON]
  lnrdat_3[ , summary(as.factor(TAXON)),]
  
  #these are indeed null species find locations
  lnrdat_3[ TAXON == "#N/A" , TAXON := OBSERVED_TAXA]
  lnrdat_3[TAXON== "", TAXON := OBSERVED_TAXA]
  lnrdat_3[TAXON== "", .N, VEG_REL_ABUNDANCE_DESCR]
  lnrdat_3[TAXON== "", TAXON := NA ]
  lnrdat_3[ , summary(as.factor(TAXON)),]
  
  #point level non-detection:
  lnrdat_3[, summary(as.factor(VEG_REL_ABUNDANCE_DESCR))]
  #whats in those loc's
  lnrdat_3[VEG_REL_ABUNDANCE_DESCR == "vegetation not detected" , summary(as.factor(TAXON)) ,]
  #dun
  

#'2) 
  lnrdat_3[variable == "sp1" , summary(as.factor(TAXON))]
  lnrdat_3[variable != "sp1" , summary(as.factor(TAXON))]
  # drop the "variable" col  
  lnrdat_3[, variable := NULL, ]

#'3)
  
  #convert chr abundances to numeric 1-3
  lnrdat_3[ , summary(as.factor(REL_ABUND)) , ]
  lnrdat_3[ REL_ABUND == "sparse", REL_ABUND := "1" , ]
  lnrdat_3[ REL_ABUND == "common", REL_ABUND := "2" , ]
  lnrdat_3[ REL_ABUND == "abundant", REL_ABUND := "3" , ]
  # whats the other stuff in there?
  lnrdat_3[ REL_ABUND == "few individuals" | REL_ABUND == "many individuals" | REL_ABUND == "single" |    REL_ABUND == "surface matted" , .(TAXON, OBSERVED_TAXA, REL_ABUND) ]
  
  #drop zebra mussel observations
  lnrdat_3[ TAXON == "ZM" , TAXON := NA ]
  lnrdat_3[ REL_ABUND == "few individuals" | REL_ABUND == "many individuals" | REL_ABUND == "single" |    REL_ABUND == "surface matted" , .(TAXON, OBSERVED_TAXA, REL_ABUND) ]
  lnrdat_3[ is.na(REL_ABUND) ,.N, .(REL_ABUND, TAXON) ]
  
  #inconsistent noting of these metrics means we'll delete them
  #mark all blanks as NAs (if a survyeor did not mark abundance, they weren't estimating those)
  lnrdat_3[ !REL_ABUND %in% c("1","2", "3") , REL_ABUND := NA]
  lnrdat_3[ , summary(as.factor(REL_ABUND)) , ]
  

  
  lnrdat_3[ , SAMPLE_NOTES := as.factor(SAMPLE_NOTES), ] 
  lnrdat_3[ SAMPLE_NOTES != "", .(SAMPLE_NOTES), ] 
  
  lnrdat_3[ , VEG_REL_ABUNDANCE_DESCR := as.factor(VEG_REL_ABUNDANCE_DESCR),  ]
  lnrdat_3[ , summary(VEG_REL_ABUNDANCE_DESCR),]
  lnrdat_3[ , .N, VEG_REL_ABUNDANCE_DESCR]
  lnrdat_3[ VEG_REL_ABUNDANCE_DESCR == "sparse", VEG_REL_ABUNDANCE_DESCR := "1" , ]
  lnrdat_3[ VEG_REL_ABUNDANCE_DESCR == "common/frequent/occasional", VEG_REL_ABUNDANCE_DESCR := "2" , ]
  lnrdat_3[ VEG_REL_ABUNDANCE_DESCR == "abundant/matted", VEG_REL_ABUNDANCE_DESCR := "3" , ]
  lnrdat_3[ , .N, VEG_REL_ABUNDANCE_DESCR]
  
#' We need to delete all of the na's that come from expanding our data, but we
#' don't want to lose a line of data for points where no veg was found. 
#' 
#' Luckily 
#' we have a "" (blank) for species one from our melt function. So, every row 
#' with value == NA can get hucked out.     
  
  lnrdat_3[VEG_REL_ABUNDANCE_DESCR == 'vegetation not detected', .N , TAXON]#locs labeled as veg not det
  
  lnrdat_4 <- lnrdat_3[is.na(TAXON) == F]
  
  # lookup scientific names
  lnrdat_4[ , sort(unique(TAXON)), ]
  
  lnrtaxa <- fread(file = "data/input/contributor_data/macroniche_adds/perleberg/taxalist.csv")
  #mndnr exported all rare species as "X" so we'll need to add this to the taxalist
  # lnrtaxa <- rbind(lnrtaxa,data.table(SCIENTIFIC_NAME = "Rare Species", TAXA_CODE = "X"))

  # # check matches
  match(unique(lnrdat_4$TAXON), lnrtaxa$TAXA_CODE)
  unique(lnrdat_4$TAXON)[is.na(match(unique(lnrdat_4$TAXON), lnrtaxa$TAXA_CODE))] # need to change NITO to NITEOBTU to match the lnrtaxa scheme
  lnrdat_4[TAXON == "NITO", TAXON := "NITEOBTU" ]
  
  lnrtaxa$SCIENTIFIC_NAME[match(lnrdat_4$TAXON, lnrtaxa$TAXA_CODE)]

  lnrdat_4[ , taxon := lnrtaxa$SCIENTIFIC_NAME[match(lnrdat_4$TAXON, lnrtaxa$TAXA_CODE)] , ]
  
  lnrdat_4[ , sort(unique(taxon)), ]
  
  lnrdat_4[ VEG_REL_ABUNDANCE_DESCR == "vegetation not detected" , summary(as.factor(taxon)), ] # now we have NA in the taxon col for no veg det
  lnrdat_4[ VEG_REL_ABUNDANCE_DESCR == "vegetation not detected" , summary(as.factor(TAXON)), ]
  
  
  #remove datafiles from substeps
  rm(lnrdat_1, lnrdat_2, lnrdat_3, lnrdat, lnrtaxa, int, summary)
  

# clean datasets to prep for merge ----------------------------------------

#' ## Prep datasets for merge   
  
# drop unused or re-creatable columns  -------------------------------------------------------
#' ### Drop unneeded columns

  summary(fshdat_2)
  names(fshdat_2)
  #drop unused columns
  fshdat_2[ , c("OBJECTID","SORT_NAME", "SC_ID", "SS_ID", "FW_ID", "DNR_OFFICE", "STA_CODE", "STA_ID0", "TARGET_LOC", "ACTUAL_LOC", "UTM_SOURCE", "DATA_SOURCE", "SURVEY_STATUS", "PROJECT", "SAMPLETYPE", "LOC_TYPE", "pres","SLICE_lake","UTM_DATUM", "UTM_ZONE", "YEAR") := NULL , ]

  summary(lnrdat_4)
  names(lnrdat_4)
  #drop unused columns
  lnrdat_4[ , c("Project::STATUS", "GPScoords::POINT_SPACING_M", "GPScoords::SURVEY_ID", "DEPTH_STRATA_ACTUAL", "SAMPLE_TYPE_DESCR") := NULL , ]
  lnrdat_4[, SURVEYORS := paste(SURVEYOR_A,SURVEYOR_B,SURVEYOR_C, sep ="," )]
  lnrdat_4[ , unique(SURVEYORS),]
  lnrdat_4[ , c("SURVEYOR_A", "SURVEYOR_B", "SURVEYOR_C", "OBSERVED_TAXA") := NULL ,]
  
  
  
  summary(sldat)
  names(sldat)
  #drop unused columns
  sldat[ , c("record_num","County_code", "county", "Lake_acres", "survey_year","vegetation_common_name") := NULL , ]
  

# clean column names -------------------------------------------------------

#' ### Clean column names
  cbind(names(sldat), names(fshdat_2),names(lnrdat_4) )

  setcolorder(sldat,  c(  1,2,3,4,5,7,9, 11, 6,10,8))
  setcolorder(fshdat_2,c(1,2,6,7,3,9,10,13,11,12, 4, 5,8))
  setcolorder(lnrdat_4, c(1,2,8,7,5,
                          3,6,14,9,12,
                          10,11,15,13,4))
  
  cbind(names(fshdat_2),names(lnrdat_4), names(sldat))  
  
  
  # matchnames
  names(sldat)[1] <- "DOWLKNUM"
  names(sldat)[2] <- "LAKE_NAME"
  names(sldat)[3] <- "SURVEY_ID"
  names(sldat)[4] <- "SURVEY_DATE"
  names(fshdat_2)[4] <- "SURVEY_DATE"
  names(sldat)[5] <- "STA_NBR"
  names(sldat)[6] <- "DEPTH_FT"
  names(lnrdat_4)[6] <- "DEPTH_FT"
  names(sldat)[7] <- "SUBSTRATE"
  names(sldat)[10] <- "TAXACODE"
  names(fshdat_2)[10] <- "TAXACODE"
  names(lnrdat_4)[10] <- "TAXACODE"
  names(sldat)[8] <- "TAXON"
  names(fshdat_2)[8] <- "TAXON"
  names(lnrdat_4)[8] <- "TAXON"
  names(lnrdat_4)[13] <- "SURVEYOR"
  names(lnrdat_4)[11] <- "UTMX"
  names(lnrdat_4)[12] <- "UTMY"

  cbind(names(fshdat_2),names(lnrdat_4), names(sldat))  
  
  str(sldat)
  str(lnrdat_4)
  
  lnrdat_4[ , DOWLKNUM :=  as.character(DOWLKNUM), ]
  sldat[ , SURVEY_ID :=  as.character(SURVEY_ID), ]
  lnrdat_4[ , VEG_REL_ABUNDANCE_DESCR :=  as.character(VEG_REL_ABUNDANCE_DESCR), ]
  
  str(lnrdat_4)
  str(fshdat_2)
  
  fshdat_2[ , DOWLKNUM :=  as.character(DOWLKNUM), ]
  fshdat_2[ , SURVEY_ID :=  as.character(SURVEY_ID), ]
  
  #are all data types aligned
  str(sldat)
  str(lnrdat_4)
  str(fshdat_2)
  
  sldat[ , DATASOURCE := "Muthukrishnan Et al", ]
  lnrdat_4[ , DATASOURCE := "DNR Lakes and Rivers", ]
  fshdat_2[ , DATASOURCE := "DNR Fisheries", ]
  
    
# merge datasets ----------------------------------------------------------
  
#' ## Merge datasets
  
  dnrdat <- merge(lnrdat_4,fshdat_2, all = T )
    str(dnrdat)
    str(sldat)
    dnrdat <- merge(dnrdat,sldat, by = c("DOWLKNUM", "LAKE_NAME", "SURVEY_ID", 
                                         "SURVEY_DATE", "STA_NBR", "DEPTH_FT", 
                                         "SUBSTRATE", "TAXON", "TAXACODE", "DATASOURCE"),  all = T)

# align the no veg found notes ------------------------------------------

#' ### Clean No Veg Found notes
    
  names(dnrdat)
    str(dnrdat)
    
    dnrdat[ , "TAXON" := as.factor(TAXON), ]
    dnrdat[ , .N , TAXON ]
    dnrdat[ TAXON == "" , .N , ]
    # number of obs per taxon
    dnrdat[  , sort(summary(TAXON)) , ]
    
    dnrdat[ , summary(as.factor(VEG_REL_ABUNDANCE_DESCR)),] #from lnr
    dnrdat[ , summary(as.factor(HAS_DATA)),] #from fsh ( X, Y == sampled found veg; Z = sampled, no veg found)
    dnrdat[ , summary(as.factor(sample_point_surveyed)),] #from sl
    
    #drop points where no sample was taken:
    dnrdat <- subset(dnrdat, is.na(sample_point_surveyed) == T |
                       sample_point_surveyed == "yes" , )
    dnrdat[ , sample_point_surveyed := NULL, ]
    
    dnrdat[HAS_DATA == "Z" , summary(TAXON)] #from fsh ( X, Y == sampled found veg; Z = sampled, no veg found)
    
    
#' These are observations of plants that are conflicting with other fields. We 
#' will assign all of these points a TAXON of no veg found.
    dnrdat[ TAXON == "No Veg Found"|
              TAXON == "No Vegetation Present"|
              TAXON == ""|
              HAS_DATA == "Z" |
              VEG_REL_ABUNDANCE_DESCR == "vegetation not detected",summary(TAXON),]
    # assign taxon as No Veg Found
    dnrdat[ TAXON == "No Veg Found"|
              TAXON == "No Vegetation Present"|
              TAXON == ""|
              HAS_DATA == "Z" |
              VEG_REL_ABUNDANCE_DESCR == "vegetation not detected",
            TAXON := "No Veg Found",]
    
    #drop HAS_DATA 
    dnrdat[ , HAS_DATA := NULL, ]
    
    dnrdat[ TAXON == "No Veg Found",summary(TAXON) ,]
    dnrdat[ ,summary(TAXON) ,]
    dnrdat[ is.na(TAXON), ,] 
    
    # Filamentous Algae! (Add names)
    dnrdat[TAXACODE == "FA" ,  ,]
    dnrdat[TAXACODE == "FA" , TAXON := "Filamentous Algae spp." ,]
    
    
# resolve date data -------------------------------------------------------
  
#' ### Clean dates
    
    dnrdat[ DATASOURCE == "Muthukrishnan Et al", .(SURVEY_DATE) ,]
    dnrdat[ DATASOURCE == "DNR Lakes and Rivers", .(SURVEY_DATE) ,]
    dnrdat[ DATASOURCE == "DNR Fisheries", .(SURVEY_DATE) ,]
    
#'we will caress these all into a yyyy-mm-dd, ex: 1992-01-31    
    
    dnrdat[ DATASOURCE == "Muthukrishnan Et al", SURVEY_DATE := as.character(as.Date(SURVEY_DATE, format = "%d-%b-%y")) ,]
    dnrdat[ DATASOURCE == "DNR Lakes and Rivers", SURVEY_DATE := as.character(as.Date(SURVEY_DATE, format = "%m/%d/%Y")) ,]
    dnrdat[ DATASOURCE == "DNR Fisheries", SURVEY_DATE := as.character(as.Date(word(SURVEY_DATE, 1, sep = " "), format = "%m/%d/%Y")),]
    
    dnrdat[ , .N , SURVEY_DATE]
    dnrdat[ , summary(as.Date(SURVEY_DATE)),]
    summary(dnrdat[ ,  as.POSIXlt(SURVEY_DATE, format = "%Y-%m-%d")[, "yday"],])
    hist(dnrdat[ ,  as.POSIXlt(SURVEY_DATE, format = "%Y-%m-%d")[, "yday"],])
    

# resolve substrates ------------------------------------------------------

#' ### Review substrates data
       
    dnrdat[ , str(SUBSTRATE) ,]
    dnrdat[ , SUBSTRATE := as.factor(tolower(SUBSTRATE)) ,]
    dnrdat[ , summary(SUBSTRATE) ,]
    
#' We'll leave this alone for now until we decide it is useful or needed data.

# resolve depth data ------------------------------------------------------
    
#' ### Review depth data
    
    dnrdat[ , summary(DEPTH_FT), ]
    dnrdat[ DEPTH_FT > 99 , , ]
    dnrdat[ is.na(DEPTH_FT), , ]

#' SOme of these points are good, some appear to be incorrect data entries.
#' We'll leave them be for now, with the assumption that we will only use
#' complete entries in our analysis. 


# resolve taxa names ------------------------------------------------------

#' ### Clean taxa names    
    
    dnrdat[ , sort(unique(TAXON)),]
    dnrdat[ TAXON == "Bidens species"|
             TAXON == "Bidens sp.", TAXON := "Bidens spp.",]
    dnrdat[ TAXON == "Carex"|
              TAXON == "Carex sp.", TAXON := "Carex spp.",]
    dnrdat[ TAXON == "Chara"|
              TAXON == "Chara sp.", TAXON := "Chara spp.",]
    dnrdat <- subset(dnrdat, TAXON != "Dreissena polymorpha")
    dnrdat[ TAXON == "Drepanocladus; Fontinalis; etc", TAXON := "Drepanocladus or Fontinalis spp.",]
    dnrdat[ TAXON == "Elatine sp.", TAXON := "Elatine spp.",]
    dnrdat[ TAXON == "Eleocharis" | TAXON == "Eleocharis sp.", TAXON := "Eleocharis spp.",]
    dnrdat[ TAXON == "Elodea sp." , TAXON := "Elodea spp.",]
    dnrdat[ TAXON == "Equisetum sp.", TAXON := "Equisetum spp.",]
    dnrdat[ TAXON == "Impatiens sp.", TAXON := "Impatiens spp.",]
    dnrdat[ TAXON == "Iris sp." | TAXON == "Iris species" | TAXON == "Iris", TAXON := "Iris spp.",]
    dnrdat[ TAXON == "Isoetes sp." | TAXON == "Isoetes", TAXON := "Isoetes spp.",]
    dnrdat[ TAXON == "Juncus sp." , TAXON := "Juncus spp.",]
    dnrdat[ TAXON == "Labiatae Family" , TAXON := "Labiatae spp.",]
    dnrdat[ TAXON == "Lemna sp." | TAXON == "Lemna", TAXON := "Lemna spp.",]
    dnrdat[ TAXON == "Myriophyllum sp." | TAXON == "Myriophyllum species"| TAXON == "Myriophyllum", TAXON := "Myriophyllum spp.",]
    dnrdat[ TAXON == "Myriophyllum verticullatum" , TAXON := "Myriophyllum verticillatum",]
    dnrdat[ TAXON == "Najas sp." | TAXON == "Najas species"| TAXON == "Najas", TAXON := "Najas spp.",]
    dnrdat[ TAXON == "Nitella sp." | TAXON == "Nitella", TAXON := "Najas spp.",]
    dnrdat[ TAXON == "Nuphar sp." | TAXON == "Nuphar species", TAXON := "Nuphar spp.",]
    dnrdat[ TAXON == "Nymphaea sp.", TAXON := "Nymphaea spp.",]
    dnrdat[ TAXON == "Persicaria species - floating-leaf type" | 
              TAXON == "Persicaria species" | 
              TAXON == "Persicaria sp.", TAXON := "Persicaria spp.",]
    dnrdat[ TAXON == "Potamogeton Friesii"  , TAXON := "Potamogeton friesii",]
    dnrdat[ TAXON == "Potamogeton Richardsonii"  , TAXON := "Potamogeton richardsonii",]
    dnrdat[ TAXON == "Potamogeton Robbinsii"  , TAXON := "Potamogeton robbinsii",]
    dnrdat[ TAXON == "Potamogeton sp." |
              TAXON == "Potamogeton", TAXON := "Potamogeton spp.",]
    dnrdat[ TAXON == "Potamogeton species - broadleaf type"  , TAXON := "Potamogeton sp. Broad",]
    dnrdat[ TAXON == "Potamogeton species - narrowleaf type"  , TAXON := "Potamogeton sp. Narrow",]
    dnrdat[ TAXON == "Ranunculus" | TAXON == "Ranunculus sp."  , TAXON := "Ranunculus spp.",]
    dnrdat[ TAXON == "Sagittaria" | TAXON == "Sagittaria species" | TAXON == "Sagittaria sp."  , TAXON := "Sagittaria spp.",]
    dnrdat[ TAXON == "Salix sp." | TAXON == "Salix species"  , TAXON := "Salix spp.",]
    dnrdat[ TAXON == "Schoenoplectus" | TAXON == "Schoenoplectus species"  , TAXON := "Schoenoplectus spp.",]
    dnrdat[ TAXON == "Solidago sp." , TAXON := "Solidago spp.",]
    dnrdat[ TAXON == "Sparganinm sp." | TAXON == "Sparganium" |
              TAXON == "Sparganium sp." | TAXON == "Sparganium species"  , TAXON := "Sparganium spp.",]
    dnrdat[ TAXON == "Stuckenia (Potamogeton) vaginata" , TAXON := "Stuckenia vaginata",]
    dnrdat[ TAXON == "Stuckenia species" , TAXON := "Stuckenia spp.",]
    dnrdat[ TAXON == "Typha" | TAXON == "Typha species" | TAXON == "Typha sp.", TAXON := "Typha spp.",]
    dnrdat[ TAXON == "Utricularia sp." | TAXON == "Utricularia species" , TAXON := "Utricularia spp.",]
    dnrdat[ TAXON == "Wolffia sp." | TAXON == "Wolffia" , TAXON := "Wolffia spp.",]
    dnrdat[ TAXON == "Zanichellia palustris" , TAXON := "Zannichellia palustris",]
    dnrdat[ TAXON == "Unknown emergent" , TAXON := "Unknown emergent species",] 
    dnrdat[ TAXON == "Alnus sp." | TAXON == "Alnus species" , TAXON := "Alnus spp.",]
    dnrdat[ TAXON == "Graminae"  , TAXON := "Gramineae/Poaceae Family",]
    dnrdat[ TAXON == "Phragmites australis (communis)"  , TAXON := "Phragmites australis",]
    dnrdat[ TAXON == "Schoenoplectus pungens/torreyi"  , TAXON := "Schoenoplectus pungens or torreyi",]
    dnrdat[ TAXON == "Sedge sp."  , TAXON := "Carex spp.",]
    
    dnrdat[ TAXON == "Schoenoplectus pungens/torreyi"  , TAXON := "Schoenoplectus pungens or torreyi",]
    
    
    
    
    dnrdat [ , TAXON := droplevels(TAXON),]
    dnrdat[ , sort(unique(as.character(TAXON))),]
    
    
# review dataset and export -----------------------------------------------

#' ## Review and Export dataset    
    
    str(dnrdat)
    dnrdat [ , SUBSTRATE := droplevels(SUBSTRATE),]
    dnrdat [ , SAMPLE_NOTES := droplevels(SAMPLE_NOTES),]
    
    dnrdat[ , .N , TAXON] #n occurrences by spp
    dnrdat[ , .N , c("SURVEY_ID","DATASOURCE")] #n obseravtion in each survey (total lines = n surveys)
    dnrdat[ , length(unique(SURVEY_ID)) , DATASOURCE ] #nsurveys submitted by each DNR contributor
    
    names(dnrdat)[3] <- "SURVEY_ID_DATASOURCE"
    names(dnrdat)[17] <- "POINT_LVL_SECCHI"
    
    #create new, unique survey ID for each survey
    dnrdat[ , .N , c("SURVEY_ID_DATASOURCE","DATASOURCE")]
    nrow(dnrdat[ , .N , c("SURVEY_ID_DATASOURCE","DATASOURCE")])
    dnrdat[, SURVEY_ID := .GRP, by = c("SURVEY_ID_DATASOURCE","DATASOURCE")] 
    
    #and a unique ID for each sample point (groups the plant obs)
    dnrdat[, POINT_ID := .GRP, by = c("STA_NBR", "SURVEY_ID_DATASOURCE", "DATASOURCE")]
    
    #and finally a unique ID for each observation in the dataset
    dnrdat[, OBS_ID := .I]


# metadata for this dataset -----------------------------------------------

#' ### Metadata    
    
#' MN DNR has 3 primary db's with these data in them. First is shallow lakes. 
#' We can get most of that from a 2018 Muthukrishnan Et al. data archive. There
#' are also data collected by Fisheries--these were shared by Donna Dustin. 
#' Finally, data from the Lakes and Rivers or Lakes Habitat (a.k.a Perleberg 
#' and Radomski) were acquired through a formal data request (which only turned
#' up data collected before 2013). 
#' 
#' Muthukrishnan, R., Hansel-Welch, N., & Larkin, D. J. (2018). Environmental
#' filtering and competitive exclusion drive biodiversity-invasibility 
#' relationships in shallow lake plant communities. Journal of Ecology, 106(5),
#' 2058–2070. https://doi.org/10.1111/1365-2745.12963
#' 
#' We joined these three datasets together, resulting in the following:    
    str(dnrdat)
#' Where: 
#'  **DOWLKNUM** - MNDNR Dept of Waters unique waterbody identifier, or an 
#'   identifier from NWI or other to distinguish each waterbody  
#'  **LAKE_NAME** - Lake name as provided by data contributor, has redundancies, 
#'   lake identification should default to DOWLKNUM  
#'  **SURVEY_ID_DATASOURCE** - Unique survey IDs from each data contributor 
#'   (only unique within each contributor group)  
#'  **SURVEY_DATE** - date of observation in yyyy-mm-dd  
#'  **STA_NBR** - identifier for each point within a point-intercept survey (not
#'   all repeated surveys are guaranteed to be repeated samples of the same geo-
#'   locations if looking for repeat sampling of individual points through time,
#'   carefully consider X,Y coords where avail and total *n* points in each 
#'   survey to be sure repeated samples targeted same locs)  
#'  **DEPTH_FT** - depth observed at each sample location, measured in various 
#'   methods including depth pole/probe, weighted rope, sonar, etc. These data
#'   have not been cleaned and may contain errors (see *Review depth data*
#'   section above)  
#'  **SUBSTRATE** - sustrate observations from surveys collected IAW:
#'   Perleberg, D., P. Radomski, S. Simon, K. Carlson, and J. Knopik. 2016. 
#'   Minnesota Lake Plant Survey Manual, for use by MNDNR Fisheries Section and
#'   EWR Lake Habitat Program. Minnesota Department of Natural Resources. 
#'   Ecological and Water Resources Division. Brainerd, MN. 128 pages including
#'   Appendices A-E.  
#'   ![](figs/DNR Substrates_text.png)
#'   ![](figs/DNR Substrates.png)  
#'  **TAXON** - Scientific name of taxa observed. Note that higher taxa classes
#'   are used when surveyors are unable to identify taxa to the species level.
#'   Each taxon observation is classified to the lowest level possible by
#'   surveyor  
#'  **TAXACODE** - These are codes used by each datasource for taxa names. If
#'   using these codes, note that they may differ among datasources and this 
#'   script has used keys supplied by each datasource to develop the **TAXON**
#'   field from those keys  
#'  **DATASOURCE** - Group supplying the data for this collation effort  
#'  **UTMX** - X component of UTM location (datum: 83, zone: 15). We have not 
#'   evaluated the quality or consistency of these data  
#'  **UTMY** -  Y component of UTM location (datum: 83, zone: 15). We have not 
#'   evaluated the quality or consistency of these data  
#'  **SURVEYOR** - Name of surveyor or surveying entity, where provided  
#'  **SAMPLE_NOTES** - Notes associated with each observation  
#'  **SAMPLE_TYPE_DESCR** - used in Lakes and Rivers data to identify shoreline
#'   plots and regular PI plots. ![](figs/DNR Nearshore site.png) See:  
#'   Perleberg, D., P. Radomski, S. Simon, K. Carlson, and J. Knopik. 2016. 
#'   Minnesota Lake Plant Survey Manual, for use by MNDNR Fisheries Section and
#'   EWR Lake Habitat Program. Minnesota Department of Natural Resources. 
#'   Ecological and Water Resources Division. Brainerd, MN. 128 pages including
#'   Appendices A-E.  
#'  **VEG_REL_ABUNDANCE_DESCR** - whole rake rake abundance
#'  **REL_ABUND** - single species rake abundace
#'  **POINT_LVL_SECCHI** - Secchi observations taxen at samling locations as
#'   collected only in the Muthukrishnan Et al dataset.  
#'  **SURVEY_ID** - Unique ID assigned to each survey in this dataset  
#'  **POINT_ID** - Unique ID asisgned to each point in this dataset (aggregates 
#'   osbervations of taxa within each point)  
#'  **OBS_ID** - Unique ID for each observation in this dataset  
#'    
#'  

#' ### Export data 
    
    # write.csv(dnrdat, file = "data/output/DNR_PI_Data_Combined.csv")
      
  # footer ------------------------------------------------------------------
 
 
#' ## Document footer 
#' 
#' 
#' Session Information:
  #script runtime
  Sys.time() - strttime 
  # sessionInfo
  sessionInfo()
  