#'---
#' title: "Survey Metadata and Point Data Merging & Cleaning"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull the metadata and observation level data for the plant
#' database into a single dataset, allowing cleaning of the point data based on
#' inventory results. In addition, this script will work to clean the dataset to
#' resolve some of the issues in github, especially the sub-basin/survey extent
#' problems.
#' 
#' 27 May 2021 - working to alter the script to make all plants db changes 
#' happen in one fell swoop.


#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
# Load Libraries
  library(data.table)

# load in data -------------------------------------------------
  # metadata
  metadata <- fread( input = "data/output/metadata_surveylvl_26Jan2021.csv", drop = 1)
  
  #observation level data
  plants <- fread(input = "data/output/plant_surveys_mn.csv")

# numbering the surveys: -----------------------------------------------------------

  #need to add "surveyident" to every survey in the metdata DT
  metadata[ is.na(surveyident) == F , max(surveyident), ]
  #want to make sure we start post-plants db numbering:
  plants[ , max(SURVEY_ID), ]
  #create new, unique survey ID for each survey in the metadata surveys starting with 3309:
  metadata[ , oldsurveyident := surveyident]
  metadata[ is.na(surveyident) , .I , ] #number each by row position
  metadata[ is.na(surveyident), surveyident := plants[ , max(SURVEY_ID), ] + .I , ] # assign those each their row number plus the max number from the plants db 
  
# duplicated surveys ------------------------------------------------
  
  #tag the surveys that will be dropped with NA for surveyident
  metadata[duplicatesurvey ==T, surveyident := NA]

#' For each of our plant records, we'll eventually update their data based on
#' the metadata file. At that time, each observation will be re-assigned a new 
#' survey ID (often the same as previously assigned), or, if the data should be
#' dropped, the survey ID will be set to NA. Then all rows with survey ID == NA 
#' will implement the drops from the plants data. 
  
  
              # #drop surveys marked for deletion:
              # # plants <- plants[ !SURVEY_ID %in% metadata[duplicatesurvey==TRUE, surveyident], ] 
              # #now drop also from metadata
              # metadata[ , summary(duplicatesurvey) , ]
              # metadata <- metadata[ is.na(duplicatesurvey), ]
              # metadata[ , duplicatesurvey := NULL , ] 

# multipart surveys -------------------------------------------------------

  #how about multipart surveys?
  metadata[ , summary(multipartsurvey) ,]
  metadata[!is.na(multipartsurvey), surveyident  ] #multipart surveys
  duplicated(metadata[!is.na(multipartsurvey), surveyident  ]) #why are some multipart within a single survey? And does it matter? 
  metadata[!is.na(multipartsurvey), surveyident  ][duplicated(metadata[!is.na(multipartsurvey), surveyident  ])]
  #these are surveys that need to be parsed apart (from single into 2) and that happens later in this script!
  metadata[surveyident %in% metadata[!is.na(multipartsurvey), surveyident  ][duplicated(metadata[!is.na(multipartsurvey), surveyident  ])]]
  
  # how many rows in the plants with IDs labelled as multipart in metadata:
  plants[ SURVEY_ID %in% metadata[!is.na(multipartsurvey), surveyident  ] , .N  ]
              # #mark a new column in plants data as a multipart survey
              # plants[ SURVEY_ID %in% metadata[!is.na(multipartsurvey), surveyident  ] , 
              #         multipartsurvey := T]

#' ## Github Issue #9 multipart surveys and variable survey extents 
#' Where a survey is multipart or multi-basin, etc. we want each sub-survey to 
#' have a unique identifier, but then all surveys comprising the whole-lake 
#' composite should have a field linking them to one another:
#' https://github.umn.edu/verh0064/surveycollation/issues/9
  
  # We don't want any of our survey idents shared between lakes (these are "actual" different surveys)
  metadata[ , length(unique(lake)) , .(surveyident) ][V1>1]
  metadata[!is.na(surveyident) , length(unique(dow)) , .(surveyident) ][V1>1]
  metadata[ surveyident %in% metadata[!is.na(surveyident) , length(unique(dow)) , .(surveyident) ][V1>1][,surveyident,], ,]
  
  #crosby lakes are going to be an issue. For now we'll clean up just the multipart bit:
              # plants[ SURVEY_ID == 2462, .N , .(LAKE_NAME) ]
              # plants[SURVEY_ID == 2462 & LAKE_NAME == "lower crosby", .N , .(LAKE_NAME)]
              # plants[SURVEY_ID == 2462 & LAKE_NAME == "lower crosby", SURVEY_ID := max(metadata$surveyident)+1, ]#implement in plants
  metadata[surveyident == 2462 , npoints, lake ]
  metadata[surveyident == 2462 & lake == "upper crosby", surveyident := max(metadata$surveyident, na.rm = T)+1, ]#implement in metadata

  
  #scan metadata for remaining multipart surveys  (sssp-single survey split across two lakes):
  metadata[ !is.na(surveyident), length(unique(lake)) , .(surveyident) ][V1>1]
  metadata[!is.na(surveyident) , length(unique(dow)) , .(surveyident) ][V1>1]
  metadata[ , .N, .(surveyident)][N>1] #multiple metadata records for a single survey ident
  metadata[surveyident %in% 
             metadata[ !is.na(surveyident) , .N, .(surveyident)][N>1][,surveyident]#idents with >1 record
             ]
  
#' First, we'll work through these 18 records making sure that we have numbered the 
#' metadata records appropriately.
  #view multi-line surveys in metadata (these we want to assign new survey Ident values to)
  metadata[surveyident %in% 
             metadata[!is.na(surveyident) , .N, .(surveyident)][N>1][,surveyident]#idents with >1 record
           ]
  #ossawinamakee and south twin were accidental two-lines (allocated data to two sep lines) because of lake name misspellings-- consolidate to one line:
  metadata[ surveyident %in% c(718,1922), , ]
  metadata[ surveyident %in% c(718,1922), surveyor, surveyident ] #need to collapse these surveyors uder each ID.
  
  #ossawinnamakee
  paste(metadata[ surveyident == 718, c(surveyor), ,][1] , metadata[ surveyident == 718, c(surveyor), ,][2], sep = ";")
  metadata[ surveyident == 718 & npoints > 300, surveyor := paste(metadata[ surveyident == 718, c(surveyor), ,][1] , metadata[ surveyident == 718, c(surveyor), ,][2], sep = ";") ]
  metadata[ surveyident == 718 & npoints <300, surveyident := 9999]
  
  #southtwin
  paste(metadata[ surveyident == 1922, c(surveyor), ,][1] , metadata[ surveyident == 1922, c(surveyor), ,][2], sep = ";")
  metadata[ surveyident == 1922 & npoints > 300, surveyor := paste(metadata[ surveyident == 1922, c(surveyor), ,][1] , metadata[ surveyident == 1922, c(surveyor), ,][2], sep = ";") ]
  metadata[ surveyident == 1922 & npoints <300, surveyident := 9999]
  #drop the old rows that aren't useful to us. 
  metadata <- metadata[is.na(surveyident) == T | surveyident != 9999]
  
  
  #view multi-line surveys in metadata (these we want to assign new survey Ident values to)
  metadata[surveyident %in% 
             metadata[ !is.na(surveyident) , .N, .(surveyident)][N>1][,surveyident]#idents with >1 record
  ]
  
  # each pair of surveys should be labelled with multipart field
  #only saunders is unlabelled
  metadata[surveyident == 1335 , , ]
  metadata[surveyident == 1335 & subbasin == "East" , multipartsurvey := 1335.1  , ]
  metadata[surveyident == 1335 & subbasin == "West" , multipartsurvey := 1335.2  , ]
  metadata[surveyident == 1335 & subbasin == "West" , surveyident := max(metadata$surveyident, na.rm = T)+1  , ]
  
  
#' ## Objectives re-orient. 
#' We are labeling all of the multi-part surveys in the metadata set. So far I
#' have labeled only those that were identified in the QA/QC. But we know that
#' there are a few lakes where we might be able to dig up more examples.
#' 
  metadata[ ! subbasin == "", .N , lake ] #15 lakes with subbasins
  metadata[ !is.na(multipartsurvey), .N, lake] # 7 lakes with multipart surveys
  
  #let's start with the lakes already coded as multipart:
  

#' Coon lake is a large two-basin system connected by a channel. Each survey
#' that was done at the sub-basin level will get it's own survey ID. And we'll
#' be able to connect E/W surveys together using the multipart survey key.
  metadata[dow == 2004200, .N, .(datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  metadata[multipartsurvey == 827.2, surveyident := max(metadata$surveyident, na.rm = T) + 1 ]
  metadata[surveyident == 829, multipartsurvey := 829.1 ]
  metadata[surveyident == 3365, multipartsurvey := 829.2 ]
  metadata[surveyident == 3367, multipartsurvey := 3367.1 ]
  metadata[surveyident == 3368, multipartsurvey := 3367.2 ]
  
#' Norway looks like it has a "pine river south bay" subset in one of the
#' surveys (2008). We'll assume that every other survey included that in the 
#' 
  metadata[lake == "norway", .N, .(datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  metadata[multipartsurvey == 467.2, surveyident := max(metadata$surveyident, na.rm = T) + 1 ]

#' Zumbra is a lake with a back-bay called sunny. Technically the lake is 
#' zumbra-sunny and has a single dow number.
#' 
  metadata[lake %in% c("Zumbra-Sunny","zumbra","sunny"), .N, .(dow, datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  #renumbering
  metadata[multipartsurvey == 314.2, surveyident := max(metadata$surveyident, na.rm = T) + 1]
  metadata[surveyident == 320, multipartsurvey := 320.1]
  metadata[surveyident == 315, multipartsurvey := 320.2]
  
  metadata[surveyident == 321, multipartsurvey := 321.1]
  metadata[surveyident == 316, multipartsurvey := 321.2]
  
  #renaming
  metadata[surveyident == 320, lake := "Zumbra-Sunny"]
  metadata[surveyident == 320, subbasin := "Zumbra"]
  metadata[surveyident == 315, lake := "Zumbra-Sunny"]
  metadata[surveyident == 315, subbasin := "Sunny"]
  
  metadata[surveyident == 321, lake := "Zumbra-Sunny"]
  metadata[surveyident == 321, subbasin := "Zumbra"]
  metadata[surveyident == 316, lake := "Zumbra-Sunny"]
  metadata[surveyident == 316, subbasin := "Sunny"]
  
#' Lundsten lake has two basins, connected by a culvert. DNR name has a "d" in
#' it, on google you'll find "lunsten"
  
  metadata[lake %in% c("Lunsten","Lundsten") | dow == 10004300, .N, .(dow, datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  #renumbering
  metadata[multipartsurvey == 327.2, surveyident := max(metadata$surveyident, na.rm = T) + 1]
  metadata[multipartsurvey == 329.2, surveyident := max(metadata$surveyident, na.rm = T) + 1]
  
#' saunders is a small lake, on East side has a small back-bay connected by
#' channel w/ a dock over it. 
#' 
  metadata[lake %in% c("saunders") | dow == 27018500 , .N, .(dow, datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  #renumbering
  metadata[multipartsurvey == 1335.2, surveyident := max(metadata$surveyident, na.rm = T) + 1]
  
#' Gleason lake has a N basin that is separated by narrows w/ a bike trail over
#' them
#' 
  metadata[lake %in% c("gleason") | dow == 27009501 , .N, .(dow, contributor, datesurveystart, lake, subbasin, surveyident, multipartsurvey)]
  
  #renumbering
  metadata[multipartsurvey == 1191.1, multipartsurvey := 1199.1]
  metadata[multipartsurvey == 1191.2, multipartsurvey := 1199.2]
  metadata[multipartsurvey == 0.2, multipartsurvey := 3643.1]
  metadata[multipartsurvey == 0.1, multipartsurvey := 3643.2]
  
#' Minnetonka is a W metro lake with a ridiculous amount of bays. The lake hsn't
#' to my knowledge been surveyed in it's entirety, (ever?). Survey work is usua-
#' lly targeted at a single bay or lake sub-part of interest.  
#' 
  metadata[lake %in% c("minnetonka")|dow %in% c(27013300:27013399) , .N, .(dow , day , month , year , lake, subbasin, contributor, surveyident, oldsurveyident, npoints , multipartsurvey, duplicatesurvey)][order(subbasin, year, month, day)][76:100]
  #found duplicated surveys
  metadata[surveyident %in% c(1319, 1306, 1312, 3621, 3623, 3624), duplicatesurvey := T , ]
  metadata[surveyident %in% c(1319, 1306, 1312, 3621, 3623, 3624) , surveyident := NA , ]

  
#' These Minnetonka surveys are a mess and are not directly able to be mapped 
#' onto a clean set of DOWs. Because of this, we're going to keep them organized
#' as DOW and lake set at the major basin level, and we'll just retain the
#' nominal subbasin as a way to ID where a survey was (roughly--more precise loc
#' can be devised by plugging into the coords within the point level data).
#'   
  metadata[(lake %in% c("minnetonka")|dow %in% c(27013300:27013399)), .N, .(dow , day , month , year , lake, subbasin, contributor, surveyident, npoints , multipartsurvey, duplicatesurvey)][order(as.factor(subbasin),year,month,day)]
  
  #one DNR survey is unlabelled, but I mapped it to determine is a Carsons and St. Louis Bay survey
  metadata[surveyident == 1290, subbasin := "Carsons & St. Louis Bays"]
  
  #turns out A Londo sent in a duplicate of that survey (but hey, its got surveyor data!)
  metadata[surveyident %in% c(3434, 1290)]
  metadata[surveyident %in% c(3434, 1290), surveyor := "S. Sisler, T. Ohmann, B. Hummel, A. Doll, H. Oliverus" ]
  metadata[surveyident == 3434, duplicatesurvey := T]
  metadata[surveyident == 3434, surveyident := NA]
  
  
  
#' Now moving on to the subbasin lakes. Let's start by only looking at the lakes
#' that have at least one survey that included a subbasin level note:
#' 
  metadata[ ! subbasin == "", .N , lake ] #15 lakes with subbasins
  
#' from these lakes, we'll want to try to infer if the surveys were whole lake
#' or subbasin. Some of these were finished in the last step (coon, minnetonka,
#' zumbra, lundsten, saunders, gleason, norway). And some are just fine as-is (example
#' below, island lake). Others we'll work through and do our best to clean up    
  metadata[lake == "island" & dow == 62007500]

  #anderson looks good, might consider labelling as multipart surveys
  metadata[lake == "anderson", .N , .(dow , day , month , year , lake, subbasin, contributor, surveyident, npoints , multipartsurvey)][order(year,month,day)]
  metadata[surveyident == 1151, multipartsurvey := 1151.1]
  metadata[surveyident == 3403, multipartsurvey := 1151.2]
  metadata[surveyident == 3404, multipartsurvey := 3404.1]
  metadata[surveyident == 3412, multipartsurvey := 3404.2]
  metadata[surveyident == 3405, multipartsurvey := 3405.1]
  metadata[surveyident == 3413, multipartsurvey := 3405.2]
  metadata[surveyident == 1152, multipartsurvey := 1152.1]
  metadata[surveyident == 3406, multipartsurvey := 1152.2]
  metadata[surveyident == 3407, multipartsurvey := 3407.1]
  metadata[surveyident == 3610, multipartsurvey := 3407.2]
  metadata[surveyident == 3408, multipartsurvey := 3408.1]
  metadata[surveyident == 3414, multipartsurvey := 3408.2]
  metadata[surveyident == 3409, multipartsurvey := 3409.1]
  metadata[surveyident == 3611, multipartsurvey := 3409.2]
  metadata[surveyident == 3410, multipartsurvey := 3410.1]
  metadata[surveyident == 3612, multipartsurvey := 3410.2]
  metadata[surveyident == 3411, multipartsurvey := 1153.1]
  metadata[surveyident == 1153, multipartsurvey := 1153.2]
  #check dow-subbasin match
  metadata[lake %in% c("anderson")& dow %in% c(27006200:27006299), .N , .(subbasin,dow)]
  
  
  # sarah surveys look good: DOWs are good, subbasins all labelled
  metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) , .N, .(dow , day , month , year , lake, subbasin, contributor, surveyident, npoints , multipartsurvey)][order(year,month,day)]
  #check dow-subbasin match
  metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199), .N , .(subbasin,dow)]
  #label multi-part
      # metadata[surveyident == 1347, multipartsurvey := 1347.1]
      # metadata[surveyident == 3547, multipartsurvey := 1347.2]
      # metadata[surveyident == 1348, multipartsurvey := 1348.1]
      # metadata[surveyident == 3548, multipartsurvey := 1348.2]
      # metadata[surveyident == 1349, multipartsurvey := 1349.1]
      # metadata[surveyident == 1367, multipartsurvey := 1349.2]
      # metadata[surveyident == 1368, multipartsurvey := 1368.1]
      # metadata[surveyident == 1350, multipartsurvey := 1368.2]
      # metadata[surveyident == 1351, multipartsurvey := 1351.1]
      # metadata[surveyident == 1369, multipartsurvey := 1351.2]
      # metadata[surveyident == 1352, multipartsurvey := 1352.1]
      # metadata[surveyident == 1370, multipartsurvey := 1352.2]
      # This is a pain in the ass to write...
      # metadata[surveyident == 3409, multipartsurvey := 3409.1]
      # metadata[surveyident == 3611, multipartsurvey := 3409.2]
      # metadata[surveyident == 3410, multipartsurvey := 3410.1]
      # metadata[surveyident == 3612, multipartsurvey := 3410.2]
      # metadata[surveyident == 3411, multipartsurvey := 1153.1]
      # metadata[surveyident == 1153, multipartsurvey := 1153.2]
      # 
  # can i number them using code?
  metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) , .N, .(dow ,  month , year , lake, subbasin, contributor, surveyident, npoints , multipartsurvey)][order(year,month)]
  
  metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) & year != 2010 , first(surveyident), .(month , year , lake)][order(year,month)]
  metadata[lake %in% c("sarah")& dow %in% c(27019101) & year != 2010, duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)]
  metadata[lake %in% c("sarah")& dow %in% c(27019102) , duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)]
  
  #check alignment for easy numbering
  cbind(
    metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) & year != 2010 , first(surveyident), .(month , year , lake)][order(year,month)],
    metadata[lake %in% c("sarah")& dow %in% c(27019101) & year != 2010, duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)][,.(month,year, surveyident)],
    metadata[lake %in% c("sarah")& dow %in% c(27019102) , duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)][,.(month,year, surveyident)]
  )
  
  #number the month/yr combos:
  a <- cbind(metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) & year != 2010 , first(surveyident), .(month , year , lake)][order(year,month)],
             c(paste(metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) & year != 2010 , first(surveyident), .(month , year , lake)][order(year,month)][,V1], 1, sep = ".")),
             metadata[lake %in% c("sarah")& dow %in% c(27019101) & year != 2010, duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)][,.(surveyident)],
             c(paste(metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) & year != 2010 , first(surveyident), .(month , year , lake)][order(year,month)][,V1], 2, sep = ".")),
             metadata[lake %in% c("sarah")& dow %in% c(27019102) , duplicatesurvey, .(dow ,  month , year , subbasin, surveyident)][order(year,month)][,.(surveyident)])
  
  colnames(a)[c(6,8)] <- c("west","east")
  #lookup code from table a
  a$V2[match(metadata[ lake %in% c("sarah")& dow %in% c(27019101) & year != 2010, surveyident],  a$west)]
  a$V4[match(metadata[ lake %in% c("sarah")& dow %in% c(27019102) & year != 2010, surveyident],  a$east)]
  
  #assign to surveys
  metadata[lake %in% c("sarah")& dow %in% c(27019101) & year != 2010 , multipartsurvey :=  a$V2[match(metadata[ lake %in% c("sarah")& dow %in% c(27019101) & year != 2010, surveyident],  a$west)] ,  ]
  metadata[lake %in% c("sarah")& dow %in% c(27019102) & year != 2010 , multipartsurvey :=  a$V4[match(metadata[ lake %in% c("sarah")& dow %in% c(27019102) & year != 2010, surveyident],  a$east)] ,  ]
  
  #check work
  metadata[lake %in% c("sarah")& dow %in% c(27019100:27019199) , .N, .(dow , day , month , year , lake, subbasin, contributor, surveyident, npoints , multipartsurvey)][order(year,month,day)]
  
  #marie doesn't have subbasins (but there is a weird marie-louisa connection)
  metadata[lake %in% c("marie")]
  metadata[surveyident == 2776, subbasin := ""]
  
  #leech does have subbasins, but like minneotonka the surveys don't follow them
  metadata[lake %in% c("leech")]
  metadata[lake %in% c("leech") & year == 2002]
  metadata[lake %in% c("leech") & year == 2002, multipartsurvey := as.numeric(paste("447", .I, sep = "."))]
  metadata[lake %in% c("leech") & year == 2003]
  metadata[lake %in% c("leech") & year == 2003, multipartsurvey := as.numeric(paste("452", .I, sep = "."))]
  metadata[lake %in% c("leech") & year == 2005]
  metadata[lake %in% c("leech") & year == 2005, multipartsurvey := as.numeric(paste("451", .I, sep = "."))]
  
  #mille lacs has no subbasins... Each of these are a multi-part survey within year
  metadata[lake %in% c("mille lacs")]
  metadata[lake %in% c("mille lacs") & year == 2009]
  metadata[lake %in% c("mille lacs") & year == 2009, multipartsurvey := as.numeric(paste("2027", .I, sep = "."))]
  metadata[lake %in% c("mille lacs") & year == 2010]
  metadata[lake %in% c("mille lacs") & year == 2010, multipartsurvey := as.numeric(paste("2028", .I, sep = "."))]
  metadata[lake %in% c("mille lacs") & year == 2014]
  metadata[lake %in% c("mille lacs") & year == 2014, multipartsurvey := as.numeric(paste("2029", .I, sep = "."))]
  metadata[lake %in% c("mille lacs") & year == 2017]
  metadata[lake %in% c("mille lacs") & year == 2017, multipartsurvey := as.numeric(paste("2040", .I, sep = "."))]
  
  #koronis
  metadata[lake == "koronis"]
  metadata[lake %in% c("koronis") & year == 2018]
  #not sure where the 2018 non-subbasin points fall:
  plants[ SURVEY_ID %in% c(2821,2823)]
  #export to map in ArcGIS
  # write.csv(plants[ SURVEY_ID %in% c(2821,2823)], file = "data/output/koronis_trouble.csv")
  # all of these points are from the same survey, fix up survey 2821 to match other
  metadata[surveyident == 2823, subbasin := "main lake"]
  metadata[surveyident == 2823, dow := 73020002]
  metadata[oldsurveyident == 2823, surveyor := paste(
    metadata[oldsurveyident == 2821, surveyor], metadata[oldsurveyident == 2823, surveyor], sep = ";"
  )]
    metadata[oldsurveyident == 2821]
  metadata[oldsurveyident == 2821, surveyident := 2823]

  #armstrong looks good
  metadata[lake == "armstrong"]
  
  # check for unique key in data
  metadata[ , .N, surveyident][N>1]
  
  metadata[!is.na(surveyident) & surveyident %in% metadata[ , .N, surveyident][N>1, surveyident]]
  #whats up with this Minnetonka bay?
  metadata[ dow == 27013300 & (subbasin %in% c("West Phelps Bay", "East Phelps Bay"))][order(year,month)]
  metadata[surveyident==1291 & subbasin == "West Phelps Bay", multipartsurvey := 1291.1]
  metadata[surveyident==1291 & subbasin == "East Phelps Bay", multipartsurvey := 1291.2]
  metadata[surveyident==1291 & subbasin == "East Phelps Bay", surveyident := max(metadata$surveyident, na.rm = T)+1]

  
  #' So we have all of these buttoned up. We'll now update the plants db with
#' the data from the metadata file:
#' 
#' 
#' 
  
  # Change survey IDs to reflect new numbers:
  metadata[match(plants$SURVEY_ID, metadata$oldsurveyident), surveyident] #nab up the new surveyidents based on old survey ids
  
  #change column names case for easy post merge cleaning
  names(plants) <- toupper(names(plants))
  names(metadata) <- tolower(names(metadata))
  
  #everything we're going to drop:
  metadata[is.na(surveyident)]
  metadata[is.na(surveyident), sum(npoints, na.rm = T)] # 21293 points dropped (so expect more than that number for observations dropped)
  
  #want to retain the old IDs just in case. 
  plants[ , OLD_SURVEY_ID := SURVEY_ID ]
  plants[ , SURVEY_ID := metadata[match(plants$OLD_SURVEY_ID, metadata$oldsurveyident), surveyident]]
  plants[ , summary(SURVEY_ID)] #32,270 observations will be dropped
  # drop all obs without any new survey numbers
  plants[is.na(SURVEY_ID), .N, .(LAKE_NAME, DATASOURCE, SURVEY_DATE)]
  metadata[lake== "carlos"] # an example of what we are dropping
  
  #drop all duplicates and other unneeded data from both
  plants <- plants[!is.na(SURVEY_ID)]
  metadata <- metadata[!is.na(surveyident)]
  
  # check plants for intact-ness
  plants[ , summary(POINT_ID)]
  plants[ , .N, .(SURVEY_ID, DATASOURCE, LAKE_NAME) ][N<10]
  plants[ , .N, SURVEY_ID ][, hist(N, breaks = 5000, xlim = c(0,300))]
  
  sum(is.na(match(plants$SURVEY_ID, metadata$surveyident))==F)
  
  
  #need to make sure that the surveys we are splitting into multiple parts are going to be recognized 
  # the issue here is that when I changed the lake names in the metdata set and move some of the names of "lakes" into the subbasin field, I left myself with a many:one join that won't choose the new survey ident correctly using only the old survey ID. So, to fix this I need to ensure that the lake names and subbasin names can be used for the merge of the two datasets. 
  #which surveys are multi-part-ers
  plants[OLD_SURVEY_ID %in% metadata[ !is.na(multipartsurvey), oldsurveyident , ], unique(LAKE_NAME)]
  #which of those are a single survey ID getting split up?
  plants[OLD_SURVEY_ID %in% metadata[ !is.na(multipartsurvey), oldsurveyident , ],
       unique(LAKE_NAME),
       .(SURVEY_ID, LAKE_NAME, SURVEY_DATE)]
  
  #we'll just hack through these one at a time -- doing it after we merge?
  #' 1. merge to metdata, overwriting the new survey ID with what is actually a "bad" one
  #' 2. extract all records with a "multipartsurvey" designation
  #' 3. make a list showing OLD ID, LAKENAME, new id, lake, subbaisn, multipart code
  #' 4. comparing across rows we can wipe out the first merged metadata and paste in a bit of metadat that we have manually chosen based on the tabling in 3. 
  #'
  
  #merge the easy bits
  metadata[is.na(multipartsurvey) & subbasin == ""]
  
  plants.ez <- merge(plants, metadata[is.na(multipartsurvey) & subbasin == ""] , by.x = c("SURVEY_ID"), by.y = c("surveyident"), all =T)
  plants.ez[is.na(lake)]
  
  unpaired <- merge( plants.ez[is.na(lake)], metadata, by.x = "OLD_SURVEY_ID", by.y = "oldsurveyident", allow.cartesian = T)
  
  plants.ez <- plants.ez[!is.na(lake)]
  
  
  #drop the rows created by the bad 1:many nature of the merge
  sum(duplicated(unpaired[ , OBS_ID ,]))
  unpaired <- unpaired[!duplicated(unpaired[ , OBS_ID ,]), ]
  
  unpaired[ , .N, .(OLD_SURVEY_ID, LAKE_NAME, SURVEY_DATE, SURVEYOR, surveyident, lake.y, subbasin.y, multipartsurvey.y) ]
  
  metadata[oldsurveyident==314]
  unpaired[OLD_SURVEY_ID == 314, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 314 & 
             LAKE_NAME == "zumbra", surveyident:= 4337]
  
  metadata[oldsurveyident==327]
  unpaired[OLD_SURVEY_ID == 327, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 327 & 
             LAKE_NAME == "s. lundsten", surveyident:= 4338]
  
  metadata[oldsurveyident==329]
  unpaired[OLD_SURVEY_ID == 329, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 329 & 
             LAKE_NAME == "south lundsten", surveyident:= 4339]
  
  metadata[oldsurveyident==1335]
  unpaired[OLD_SURVEY_ID == 1335, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 1335 & 
             LAKE_NAME == "saunders-west", surveyident:= 4340]
  
  metadata[oldsurveyident == 827]
  unpaired[OLD_SURVEY_ID == 827, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 827 & 
             LAKE_NAME == "coon west", surveyident:= 4335]
  
  metadata[oldsurveyident==1291]
  unpaired[OLD_SURVEY_ID == 1291, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 1291 & 
             LAKE_NAME == "phelps bay", surveyident:= 1291]
  
  metadata[oldsurveyident==1335]
  unpaired[OLD_SURVEY_ID == 1335, .N, .(SURVEY_ID, LAKE_NAME)]
  unpaired[OLD_SURVEY_ID == 1335 & 
             LAKE_NAME == "saunders-west", surveyident:= 4340]
  
  metadata[oldsurveyident==467]
  unpaired[OLD_SURVEY_ID == 467, .N, .(SURVEY_ID, LAKE_NAME)] 
  unpaired[OLD_SURVEY_ID == 467 & 
             LAKE_NAME == "Norway/Pine River", surveyident:= 4336]
  
  #fix up the multipart surveys for which id was not an easy match (we've manually updated the surveyident field, now want to overwrite the other metadata)
  names(unpaired)
  #dump all the current "metadata" columns
  unpaired[ , c(29:78, 80:84 ):=NULL]
  unpaired.fixed <- merge(unpaired, metadata, by = "surveyident", all.x = T)
  
  unpaired.fixed[ , .N, .(OLD_SURVEY_ID, LAKE_NAME, SURVEY_DATE, SURVEYOR, surveyident, lake, subbasin, datesurveystart, multipartsurvey) ][order(lake,subbasin,datesurveystart)]
  
  unpaired.fixed[ , SURVEY_ID := surveyident]
  unpaired.fixed[ , surveyident :=  NULL]
  
  newsurveylisting <- unpaired.fixed[ , .N, .(OLD_SURVEY_ID, LAKE_NAME, SURVEY_DATE, SURVEYOR, SURVEY_ID, lake, subbasin, datesurveystart, multipartsurvey) ][order(lake,subbasin,datesurveystart)]
  #write.csv(newsurveylisting, file = "data/output/datacheck.csv")
  
#' These look good as far as I can tell. 
  

  #can we join the two merged files?
  cbind(names(plants.ez),names(unpaired.fixed))
  
  plants.o <- rbind(plants.ez, unpaired.fixed)
  
  plants.o[ , unique(indatabase)]
    plants.o[indatabase == "Y"  , indatabase := T ,]
    plants.o[indatabase == "N"  , indatabase := F ,]
    plants.o[ , indatabase := as.logical(indatabase)]
    plants.o[ , summary(indatabase)]
      plants.o[ indatabase == F , .N, SURVEY_ID ][N>1] # EACH of the indatabase == F are repped by a single row. GOOD.
    plants.o[ indatabase == T, summary(SURVEY_ID) ]
    plants.o[ indatabase == T, summary(POINT_ID) ] # I spy an issue...
        
    #whats driving the issue?
        plants.o[ indatabase == T & is.na(POINT_ID)] #its an issue with crosby lakes... #whatgives
        plants.o[ OLD_SURVEY_ID %in% plants.o[ indatabase == T & is.na(POINT_ID)][,oldsurveyident], .N , .(DATASOURCE, contributor, LAKE_NAME, lake, subbasin, OLD_SURVEY_ID , SURVEY_ID, datesurveystart, multipartsurvey ) ]
        plants.o[ OLD_SURVEY_ID %in% plants.o[ indatabase == T & is.na(POINT_ID)][,oldsurveyident],]
        plants.o[ is.na(POINT_ID)]
        plants.o[ oldsurveyident == 2462][order(LAKE_NAME)]
        
        metadata[oldsurveyident == 2462]
        plants.o[SURVEY_ID == 4333] #drop the misaligned metadata
        plants.o <- plants.o[!SURVEY_ID == 4333]
        plants.o[SURVEY_ID == 2462 & LAKE_NAME == "lower crosby", SURVEY_ID := 4333]
        
        # split these two
        upper_crosby_fix <- plants.o[SURVEY_ID == 4333, ]
        plants.o <- plants.o[!SURVEY_ID == 4333, ]
        
        #rematch the crosby data:
        names(upper_crosby_fix)
        upper_crosby_fix[ , 29:56 := NULL]
        upper_crosby_fix <- merge(upper_crosby_fix, metadata, by.x = "SURVEY_ID", by.y = "surveyident", all.x = T)
        
        #reappend to big data
        plants.o <- rbind(plants.o, upper_crosby_fix)
    
    #re-checking    
        
    plants.o[ , unique(indatabase)]
    plants.o[indatabase == "Y"  , indatabase := T ,]
    plants.o[indatabase == "N"  , indatabase := F ,]
    plants.o[ , indatabase := as.logical(indatabase)]
    plants.o[ , summary(indatabase)]
    plants.o[ indatabase == F , .N, SURVEY_ID ][N>1] # EACH of the indatabase == F are repped by a single row. GOOD.
    plants.o[ indatabase == T, summary(SURVEY_ID) ]
    plants.o[ indatabase == T, summary(POINT_ID) ]
    
    sum(duplicated(plants.o[ !is.na(OBS_ID), OBS_ID]))
    
  #remove intermediates
  plants <- plants.o
  rm(plants.ez, plants.o, unpaired, unpaired.fixed, upper_crosby_fix, a, newsurveylisting)
  
  #review plants and clean up columns
  names(plants)
  plants[, oldsurveyident := NULL] # drop metadata old survey ident
  plants[, summary(duplicatesurvey)] # no more dup tagged surveys
    plants[, duplicatesurvey := NULL]
  #some survey lvl metric should always be regenerated out of the data:
  plants[, maxsurveydepth_ft := NULL]
  plants[, npoints := NULL]
  plants[, dowlknum.db := NULL] # redundant with DOWLKNUM
  plants[, dowlknum.inv := NULL] # old dow from team as they inventoried
  plants[, DOWLKNUM := NULL] # dow assigned on original import
  plants[, summary(dow)] # these are the dows assigned during QA/QC
    plants[is.na(dow), .N , .(lake)] #these genuinely don't have a dow
    names(plants)[names(plants) == "dow"] <- "DOW" #change case
  plants[, county :=  NULL]
  plants[SURVEYOR == "" & surveyor != ""]
    plants[SURVEYOR == "", SURVEYOR := surveyor]
  plants[, surveyor := NULL]
  plants[ , unique(DATASOURCE) ]
  plants[ , unique(contributor) ]
    plants[is.na(DATASOURCE) & !is.na(contributor)]
    plants[is.na(DATASOURCE) & !is.na(contributor), DATASOURCE := contributor]
    plants[ , contributor := NULL ,]
  plants[ , LAKE_NAME := lake]
  plants[ , lake := NULL]  
  
  plants    
    
    
    
  
  
# cursor catcher ----------------------------------------------------------
getwd()


# footer ------------------------------------------------------------------
#' ## Document footer 
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()























