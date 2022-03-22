#'---
#' title: "Collating Survey Metadata"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will combine survey metadata from multiple sources to develop a
#' set of critical info about each of the surveys we have in the database.

# load libraries ------------------------------------------------------------------
#' ## Document Preamble
    #+ warning = FALSE
    
    # Load Libraries
    library(data.table)
    library(stringr)

# load in data ------------------------------------------------------------

    king <- fread(file = "data/output/plant_surveys_mn.csv")



# filling metadata from submission, main data, and inventory --------------
    
    # grab up some metadata from KING
    
    plantsmeta <- king[, .("npoints" = length(unique(POINT_ID)), "maxsurvdep_ft" = max(DEPTH_FT), "datesurveystart" = range(SURVEY_DATE)[1], "surveyors" = paste(unique(SURVEYOR), collapse = ";"))  , .(SURVEY_ID, DATASOURCE, LAKE_NAME , DOWLKNUM) ]
    
    #which survey's had > 1 combo of SURVEY_ID, DATASOURCE, LAKE_NAME , DOWLKNUM?
    plantsmeta[SURVEY_ID %in% plantsmeta[ , .N , SURVEY_ID][N>1, SURVEY_ID]]
    
#' ## Issue: we have some surveys with multiple lakes in a single DOW. Some of 
#' these are issues.. some are not. I added a github issue to address and track
#' this problem
    
    
    # pull in data from our import and inventory process:
    submissions <- fread(file = "data/input/Data Import and Inventory - DataSubmissionLedger.csv")
    
    inventory <- fread(file = "data/input/Data Import and Inventory - ImportProofing.csv")
    
    
#' Our Goal: a spreadsheet showing known surveys, the data that may or may not have been submitted to describe those surveys and basic meta-data for those surveys.
#' 
#' 
#' 

  # start work on dates-- we need clean dates to match on 
    names(submissions)
    #change column name to avoid using backtick quotes
    names(submissions)[8] <- "survey_date"
    
    #are there any zany dates in the batch?
    submissions[ , unique(survey_date) , ]
    submissions[survey_date == "", survey_date:= NA]
    submissions[ , summary(survey_date) , ]
    
    # remove any spaces in dates:
    submissions[ , survey_date := gsub(" ", "", survey_date) , ]
    
    #Reformat dates into IDate to allow extraction of date parts (months, years, etc.) 
    submissions[ , datesurveystart := as.IDate(survey_date, format = "%m/%d/%Y") , ]
    inventory[ , datesurveystart := as.IDate(`survey_date(m-d-yyyy)`, format = "%m/%d/%Y" ) , ]
    
  # tidy up lake names -- 
    submissions[ , lake := tolower(survey_lake) ,] 
    inventory[ , lake := tolower(survey_lake) ,]   
    plantsmeta[ , lake := tolower(LAKE_NAME) ,] 
    
  # tidy up dows
    submissions[ , unique(DOWLKNUM) , ]
    # fix two wierd ones:
    submissions[DOWLKNUM %in% c("private lake south of Parley","27013306/27013307/27013308"), DOWLKNUM := c("", "27013313", "27013313") ]
    
    inventory[ , unique(survey_dow)]
    inventory[survey_dow == "private lake south of Parley", survey_dow := ""]
    
    plantsmeta[ , unique(DOWLKNUM)]
    
    submissions[ , dow := as.integer(DOWLKNUM), ]
    inventory[ , dow := as.integer(survey_dow), ]
    plantsmeta[ , dow := as.integer(DOWLKNUM), ] # we are okay with a few weird NAs here -- we know that there are Muthukrishnan lakes with NWI numbers instead of DOWs (and those contain numbers)
    
  # contributor is messy in plantsmeta
    # whats in each?
    plantsmeta[ , unique(DATASOURCE) , ]
    submissions[, unique(survey_contributor)]
    inventory[  , unique(survey_contributor), ]
    
    # drop the year tags on a few of the names in plantmeta
    plantsmeta[ , DATASOURCE := word(DATASOURCE, 1, sep = fixed("_"))  , ]
    
    # to lower all
    plantsmeta[ , DATASOURCE := tolower(DATASOURCE) , ]
    submissions[, survey_contributor := tolower(survey_contributor)]
    inventory[  , survey_contributor := tolower(survey_contributor), ]
    
    cbind(plantsmeta[ , sort(unique(DATASOURCE)) , ],
          submissions[, sort(unique(survey_contributor)), ],
          inventory[  , sort(unique(survey_contributor)), ] )

    # now fix any weird names
    submissions[ survey_contributor == "dnr r3s ais" , survey_contributor := "april londo" ,]
    
    inventory[ survey_contributor == "fieldseth" , survey_contributor := "eric fieldseth" ]
    submissions[ survey_contributor == "fieldseth" , survey_contributor := "eric fieldseth" ]
    plantsmeta[ DATASOURCE == "fieldseth" , DATASOURCE := "eric fieldseth" ]
    
    
    inventory[ survey_contributor == "gamble" , survey_contributor := "allison gamble" ]
    submissions[ survey_contributor == "gamble" , survey_contributor := "allison gamble" ]
  
    inventory[ survey_contributor == "johnson" , survey_contributor := "james johnson" ]
    submissions[ survey_contributor == "johnson" , survey_contributor := "james johnson" ]
    
    inventory[ survey_contributor == "rattei" , survey_contributor := "meg rattei" ]
    submissions[ survey_contributor == "rattei" , survey_contributor := "meg rattei" ]
    
    plantsmeta[ DATASOURCE == "sblood" , DATASOURCE := "s blood" ]
    
    
  # field names should be matched
    #we want the fields in the match to reflect the name of parent dataset:
    
    names(submissions)
    
    #clean up submissions table and names
    
    names(submissions)[2] <- "submission_staff"
    names(submissions)[3] <- "submission_staffdate"
    names(submissions)[4] <- "cohort"
    names(submissions)[5] <- "contributor"
    names(submissions)[7] <- "dateinfo"
    names(submissions)[8] <- "date"
    names(submissions)[9] <- "month"
    names(submissions)[10] <- "day"
    names(submissions)[11] <- "year"
    names(submissions)[12] <- "county"
    names(submissions)[13] <- "surveyor"
    names(submissions)[14] <- "dowlknum"
    names(submissions)[15] <- "submission_notes"
    
    submissions[ , submission_notes := paste( submission_notes , NOTES2 , sep = ";" )]
    submissions[ , staff_action := NULL]
    submissions[ , NOTES2 := NULL]
    submissions[ , survey_lake := NULL]


    #cleanup inventory table and names

    names(inventory)
    
    names(inventory)[1] <- "cohort"
    names(inventory)[2] <- "contributor"
    names(inventory)[4] <- "subbasin"
    names(inventory)[5] <- "dateinfo"
    names(inventory)[6] <- "date"
    names(inventory)[7] <- "month"
    names(inventory)[8] <- "day"
    names(inventory)[9] <- "year"
    names(inventory)[10] <- "dowlknum"
    names(inventory)[12] <- "inventory_staff"
    names(inventory)[13] <- "inventory_staffdate"
    names(inventory)[14] <- "useable"
    names(inventory)[15] <- "cleaned"
    names(inventory)[16] <- "indatabase"
    names(inventory)[17] <- "surveyor"
    names(inventory)[18] <- "inventory_notes"
    names(inventory)[20] <- "county"
    
    inventory[ , inventory_notes := paste( inventory_notes , NOTES2 , sep = ";" )]
    inventory[ , staff_action := NULL]
    inventory[ , NOTES2 := NULL]
    inventory[ , survey_lake := NULL]

    # clean up plantsmeta
    
    names(plantsmeta)
    
    names(plantsmeta)[1] <- "surveyident"
    names(plantsmeta)[2] <- "contributor"
    names(plantsmeta)[4] <- "dowlknum"
    names(plantsmeta)[5] <- "npoints"
    names(plantsmeta)[6] <- "maxsurveydepth_ft"
    names(plantsmeta)[8] <- "surveyors"

    plantsmeta[  , LAKE_NAME := NULL , ]
    
    
  # now match inventory and submissions
    
    # inventory and submissions
    
    is <- merge.data.table(inventory,submissions, by = c("cohort", "contributor", "dateinfo", "date", "month", "day", "year", "dowlknum", "datesurveystart", "lake", "dow" ), all = T)
    
    
    # get inv-sub table (is) tidied up:
    is[ , county.y := tolower(county.y), , ]
    is[  , sort(unique(county.y)) ]
    
    is[ county.y %in% c("", "?" ) , county.y := NA , ]
    is[ county.y == "henepin", county.y := "hennepin" ]
    is[ county.y == "kanabac", county.y := "kanabec" ]
    
    is[ , county.x := tolower(county.x), , ]
    is[  , sort(unique(county.x)) ]
    is[ county.x == "kanabac", county.x := "kanabec" ]
    is[county.x == "", county.x :=  NA ]
    
    #where county.x is blank, assign it whatever is in county.y
    is[ is.na(county.x), county.x := county.y]
    
    # any bad alignment in county?
    is[ !(county.x == county.y)] #nope
    
    #drop county.y and rename county.x
    is[ , county.y := NULL]
    names(is)[20] <- "county"
    
    # surveyor
    # review and swap blanks for NAs
    is[ , sort(unique(surveyor.x)) ,]
    is[ surveyor.x == "", surveyor.x := NA]
    
    # review and swap balnks for NAs
    is[ , sort(unique(surveyor.y)) ,]
    is[ surveyor.y == "", surveyor.y := NA]
    
    # where x is blank, insert y
    is[is.na(surveyor.x), surveyor.x := surveyor.y]
    
    # any mismatches?
    is[ !(surveyor.x == surveyor.y), surveyor.y ] # don't care about this one
    
    #drop surveyor.y and rename surveyor.x
    is[ , surveyor.y := NULL]
    names(is)[18] <- "surveyor"
    
  # tidy up notes
    
    is[ , unique(submission_notes) ,]
    is[ submission_notes == ";" , submission_notes := NA ]
    
    is[ , unique(inventory_notes) ,]
    is[ inventory_notes == ";" , inventory_notes := NA ]
    
    
#' Submission and Inventory sheets are now combined and likely contain some
#' messiness and issues. We'll match that set up to our plant db metadata and 
#' then work through all once compiled:

    isk <- merge.data.table(is, plantsmeta, by = c("datesurveystart", "lake", "dow", "contributor"), all = T)
    
    #write to fiile
    # write.csv(isk, file = "data/output/inventory_db_matched.csv")
   
    
    
#' We have now sorted and re-sorted and organized the dataset in Excel to do our
#' best to catch any issues that remained (duplicated surveys or multiplicative
#' inventory, primarily). Let's draw our mega-macrophyte inventory back into
#' R!
#' 
    
     metadata <- fread(input = "data/input/MN_SurveyMetadata_11Jan2021.csv")      

     
     
     
     metadata    

#' Needs a bit of cleaning (of course it does...)

    # note got placed in an incorrect cell. move over and delete unneeded column
    metadata[V31 == "Duplicate, Delete" , `Notes from MV Inventory`:= "Duplicate, Delete" , ]
    metadata[ , V31 := NULL , ]

#' So what did I accomplish here?
#' 
#'     1. surveys are labelled with notes
#'     2. we have a master list of all of the surveys that we know exist, 
#'     including many that we do nt have data for.
#'     
#'     We need to connect our plant survey db to this one and use our notes to 
#'     start cleaning up that database. 
#'     
    metadata[, unique(`Notes from MV Inventory`)]
    metadata[, .N, indatabase ]
    # before inventory, 1109 had been marked as "in database"
    
    metadata[ is.na(surveyident) == F, indatabase := 'Y']
    metadata[ is.na(surveyident) == T, indatabase := 'N']
    metadata[, .N, indatabase ]
    # after inventory, we've got 3312 "in database
    
    # let's consolidate the "delete, duplicate" identifier from the metadata:
    metadata[, unique(`Notes from MV Inventory`)]
    metadata[ `Notes from MV Inventory` %in% c( "Duplicate, Delete", "Delete, Duplicate"),
              `Notes from MV Inventory` := "Duplicate, Delete"
              ]
    # corrected lake name note
    metadata[ `Notes from MV Inventory` %in% c( "Misspelled lake name fixed", "lake names spelling error corrected"),
              `Notes from MV Inventory` := "lake names spelling error corrected"
              ]
    
    #possible duplicates
    metadata[ `Notes from MV Inventory` %in% c( "Matched Dates, N varied", "Check for duplicate--close dates in same lake from fisheries data"),
              `Notes from MV Inventory` := "possible duplicate"
              ]
    metadata[ `Notes from MV Inventory` == "possible duplicate", .(dow,year)]
    
    #pull all surveys from same dow as possible dups
    metadata[ dow %in% metadata[ `Notes from MV Inventory` == "possible duplicate", c(dow)]]
    #looks like all are actual duplicates except for survey no 523
    metadata[ `Notes from MV Inventory` == "possible duplicate" & !surveyident == 523, `Notes from MV Inventory` := "Duplicate, Delete"]
    metadata[ `Notes from MV Inventory` == "possible duplicate" & surveyident == 523, `Notes from MV Inventory` := ""]
    
    # we altered some lake names that were misspelled
    metadata[ `Notes from MV Inventory` == "lake names spelling error corrected", .(datesurveystart,lake, dow),  ]
    #but we dont care all that much because we also re-wrote many lake names that were plainly wrong (esp. the sub-basin stuff)
    metadata[ `Notes from MV Inventory` == "lake names spelling error corrected", `Notes from MV Inventory` := "",  ]
    
    # now some sub-basin an multipart survey work
    metadata[, unique(`Notes from MV Inventory`)]
    metadata[ `Notes from MV Inventory` == "Sub-lake extent unknown", , ] # because this survye is on Tonka, it will recieve special handling w/ respect to sub-basins. So we can delete the Note
    metadata[ `Notes from MV Inventory` == "Sub-lake extent unknown", `Notes from MV Inventory` := "" , ] 
    
    #mark all of the multi-part surveys as such in sub-basin:
    metadata[ `Notes from MV Inventory`%in% metadata[, unique(`Notes from MV Inventory`)][3:68], multipartsurvey := `Notes from MV Inventory` , ]
    #drop multi=part coding from Notes
    metadata[ `Notes from MV Inventory`%in% metadata[, unique(`Notes from MV Inventory`)][3:68], `Notes from MV Inventory` := "", ]
    
    #combine surveyor data into single column
    metadata[ surveyor == "" & !surveyors == "" & !surveyors == "DNR Fisheries", surveyors] # these should all move over to "surveyor" 
    metadata[ surveyor == "" & !surveyors == "" & !surveyors == "DNR Fisheries", surveyor := surveyors]
    metadata[ surveyor == "" & surveyors == "DNR Fisheries", surveyor := surveyors] # then copy DNR fisheries (no person named) over as well
    #now we've rescued all of those surveyor data, we can delete the surveyors column
    metadata[ , surveyors := NULL ,]
    
    
    # Alrighty... we've got a metadata set that we can push back over to link up to our main dataset. 
    metadata[, unique(`Notes from MV Inventory`)]
    metadata[ `Notes from MV Inventory` == "Duplicate, Delete", duplicatesurvey := T ]
    metadata[ ,`Notes from MV Inventory` := NULL]
    
    names(metadata)
    #clean up:
    metadata[ , c("V1", "date") := NULL]
    
#' 
#' 
#' # Survey Metadata
#' 
#' This seems like a good spot to pause and talk about what exactly this dataset
#' now comprises. It is a record of all of the surveys that have been identified
#' as existing for MN by our team (and we know that there are even more out
#' there). We have marked 'indatabase' as Y for surveys that have made it all
#' the way through inventory, cleaning, import, and QA/QC. For surveys not inda-
#' tabase, we have some options: as of 26 Jan 2021, 129 additonal surveys are 
#' awaiting import (already cleaned and inventoried by staff), 159 surveys were
#' not submitted in useable formats, and 742 have not been shared in the raw
#' data format with us (we know of these 742, but there are certainly additional
#' surveys that exist for us to rake up as weel--e.g., RMB Labs data).
#' 
#'  Here's some code to get those completion metrics:
    
    #indatabase
    metadata[ ,.N , indatabase ]
    
    # for not in db, were the data submitted useable and did staff clean them up already? 
    metadata[ indatabase == "N", .N , .(useable, cleaned)]
    # this mean 129 additonal surveys are awaiting import, 159 surveys were not submitted in useable formats, and 742 have not been shared in the raw data format with us.
    
#' So at this point, we'll kick these metadata back over to the "king" dataset
#' and link them. We'll want to be careful to note that once merged, we will
#' have a database with both surveys with and without raw data. Do we split the
#' data? Maybe not... maybe we can simply add a new set of survey id's to
#' unnumbered surveys, and we can use the "indatabase column" to remove those
#' records without survey level data. 
#' 
#' Let's export the metadata:
     
    # write.csv(metadata, file = "data/output/metadata_surveylvl_26Jan2021.csv")
    
    