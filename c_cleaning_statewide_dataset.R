#' # Cleaning statewide dataset
#' ### Author: Mike Verhoeven
#' ### Date: 10 Sep 2017
#'

#' # Preamble
#' Load libraries
  #+warning=FALSE, message=FALSE 
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(ezknitr)


#' Remove anything in memory
  rm(list = ls())

#' Load custom functions

#' Set working directory to project location
  setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis")  

#' Load in statewide dataset (with clean names, messy data):
  ps <- read.csv(file = "data/output_data/state_clp_comp_namescleaned.csv") 

#' # Check out dataset
  str(ps)
  summary(ps)
  names(ps)

  
#' ## delete (variable)
#' delete the "delete" column (actually, dont do this right away)
  # ps <- ps[,-37]
  # length(names(ps))

#' #' datemv has some goofed up 2015s (e.g., i named the file 12-31-15 and it saw Year = 15)
#'   sort(unique(ps$datemv)) # the first four are out of whack
#'   unique(ps[ps$datemv=="0015-05-22"|
#'        ps$datemv=="0015-07-15"|
#'        ps$datemv=="0015-06-08"|
#'        ps$datemv=="0015-09-01"
#'        ,c("lknamemv", "datasourcemv")])
  
  
#' ## point.id
#####
  summary(ps$point.id)

#' do any blank point id rows have data in other columns?
  sum(ps$point.id == "")
  
  sum(ps$point.id == "" & ps[,2]!= "") # data in the depth.meter column?
  ps[ps$point.id == "" & ps[,2]!= "",1:2] # these are obviously bogus!

#' These are seeemingly actual observations. Unfortunately we dont have point IDs for them...
  
  sum(ps$point.id == "" & ps[,19]!= "") # data in the depth.feet column?
  sum(ps$point.id == "" & ps[,17]!= "") # data in the feet.unkn column?
  ps[ps$point.id == "" & ps[,17]!= "",c(1:17)]

#' delete all rows w/o point.id:
  ps <- ps[ps$point.id != "",]

#' How many point do I have for sblood kohlman 2010-8-25 (I got counted 140 from the survey data, so should be 140)
  sum(ps$datasourcemv == "sblood" & ps$lknamemv == "kohlman" & ps$datemv == "2010-08-25")

#' get rid of the "VP#" point that were in the data
  sum(str_detect(ps$point.id, "VP"))
  ps %>%
  filter(str_detect(ps$point.id, "VP"))
  
  ps$point.id <- str_replace(ps$point.id, "VP", "")
  
  unique(ps$point.id)
  unique(ps$point.id)[1000:length(unique(ps$point.id))]

#' delete rows where rando words came in as "point.id":
  nrow(ps[ps$point.id == "Secchi Depth:" | 
          ps$point.id == "Secchi Depth: 2m (6.5ft)" |
          ps$point.id == "Secchi: 1.5(m) 5ft"|
          ps$point.id == "occur"|
          ps$point.id == "% occurrence"|
          ps$point.id == "September 12, 2011 - Round Lake - Eden Prairie"|
          ps$point.id == "count" |
          ps$point.id == "frequ" |
          ps$point.id == "Total Stations" |
          ps$point.id == "Species present at points" | 
          ps$point.id == "Littoral Stations" |
          ps$point.id == "Vegetation Abundance %" |
          ps$point.id == "Average" |
          ps$point.id == "Other:",])

  ps.1 <- subset(ps, ps$point.id != "Secchi Depth:"  & 
                   ps$point.id != "Secchi Depth: 2m (6.5ft)" &
                   ps$point.id != "Secchi: 1.5(m) 5ft"&
                   ps$point.id != "occur"&
                   ps$point.id != "% occurrence"&
                   ps$point.id != "September 12, 2011 - Round Lake - Eden Prairie"&
                   ps$point.id != "count" &
                   ps$point.id != "frequ" &
                   ps$point.id != "Total Stations" &
                   ps$point.id != "Species present at points" & 
                   ps$point.id != "Littoral Stations" &
                   ps$point.id != "Vegetation Abundance %" &
                   ps$point.id != "Average" &
                   ps$point.id != "Other:" ) 
  
  
  summary(ps.1$point.id)
#' Any words or other erroneus junk left in the point.id data?  
  unique(ps.1$point.id)
  unique(ps.1$point.id)[1000:length(unique(ps.1$point.id))]

#' drop any secondary numbers (in the future this will be a more calculated step, where i will be looking to match the 
#' numbers in the point id column to any spatial data, which would ensure that the connection to spatial referencing is stable/reliable:
  unique(word(string = ps.1$point.id, start = 1, end = 1, sep = ",")) # check fn before applying it
  unique(word(string = ps.1$point.id, start = 1, end = 1, sep = ","))[1000:1223]
  ps.1$point.id <- word(string = ps.1$point.id, start = 1, end = 1, sep = ",")

  # check result:
  unique(ps.1$point.id)
  unique(ps.1$point.id)[1000:length(unique(ps.1$point.id))]

#' remove spaces from the point ids. Note that we are keeping the letters (e.g., 69 and 69A because these denote two separate samples that were collected)
  ps.1$point.id <- gsub(" ", "", ps.1$point.id)

#' set point ids as a factor variable
  ps.1$point.id <- as.factor(ps.1$point.id)
  summary(ps.1$point.id)


#' point.id is largely cleaned up, assign the new data the name ps:
  ps <- ps.1
  nrow(ps)
  names(ps)
#####

#' ## depth.meter
#####
  summary(ps$depth.meter)
  unique(ps$depth.meter)
  
#' how many blanks? 65330
  sum(ps$depth.meter == "")

#' rows with no depth data at all, and not marked as unsampled points:8827
  sum(ps$depth.meter == "" &
      ps$depth.feet == "" &
      ps$depth.unkn == "" &
      ps$not.sampled == "")
  
#' maybe the depth was in another column? 6909
  sum(ps$depth.meter == "" &
        ps$depth.feet == "" &
        ps$depth.unkn == "" &
        ps$not.sampled == ""&
        ps$comments == "" |
        ps$delete == "")
  
#' check out the delete data for those 1918 where theres data in comments or delete columns
  unique(ps[ps$depth.meter == "" &
       ps$depth.feet == "" &
       ps$depth.unkn == "" &
       ps$not.sampled == ""&
       (ps$comments != "" |
       ps$delete != ""), c ("comments", "delete")])
  
#' It would be possible to look back through these data to check if the fulll tabular data had hints as to what these numbers were.There may be
#' cases where my compiling methods (likely the manip in excel) have shifted field headings to the wrong rows.For example you can see that some
#' of the delete data was marks for "too deep" or "too shallow" which hints that the column that I'm deleteing there was actually conting depth data.
  
#' now see how many are missing data for depth
  sum(ps$depth.meter == "") # missing depth.meter
  sum(ps$depth.meter == "" &  # missing depth.meter & .feet
        ps$depth.feet == "")
  sum(ps$depth.meter == "" &  # missing depth.meter & .feet & .unkn
        ps$depth.feet == "" &
        ps$depth.unkn == "")
  sum(ps$depth.meter == "" & # missing depth and not marked as "not.sampled"
        ps$depth.feet == "" &
        ps$depth.unkn == "" &
        ps$not.sampled == "")
  sum(ps$depth.meter == "" & # missing depth info, not marked as unsampled, and not having anything in the comments section
        ps$depth.feet == "" &
        ps$depth.unkn == "" &
        ps$not.sampled == ""&
        ps$comments == "" )
  sum(ps$depth.meter == "" & # all that, and having no info in the "delete" column
        ps$depth.feet == "" &
        ps$depth.unkn == "" &
        ps$not.sampled == ""&
        ps$comments == "" &
        ps$delete == "")

#' My vote is that we delete all rows where these are all blank (depth.meter, .feet, .unkn, not.sampled) and
#' since I'm the only one that gets to vote: (this will remove 8850 rows)
  # if you have no depth info, & you're not marked as unsampled you go bye-bye
  ps <- ps[ps$depth.meter != "" | 
       ps$depth.feet != "" |
       ps$depth.unkn != "" |
       ps$not.sampled != "",]

#' Next lets move markings that indicated "not.sampled" from this column into the not.sampled one
#' What to do with rows within feet.meter that have erroneous things written in them (too deep, too shallow, etc)? I think that these rows need to be blanked out (or NA),
#' then a 1 added into the "not.sampled" column.
  sort(unique(ps$depth.meter))
  
  # how many cases? 246
  sum(ps$depth.meter == "Land" |
        ps$depth.meter ==  "land" |
        ps$depth.meter ==  "Unable to Sample"|
        ps$depth.meter ==  "sandbar"|
        ps$depth.meter ==  "Missed"|
        ps$depth.meter ==  "x"|
        ps$depth.meter ==  "too shallow"|
        ps$depth.meter ==  "shallow"|
        ps$depth.meter ==  "shoreline"|
        ps$depth.meter ==  "ISLAND"|
        ps$depth.meter ==  "TS"|
        ps$depth.meter ==  "*"|
        ps$depth.meter ==  "Island"|
        ps$depth.meter ==  "No sample"|
        ps$depth.meter ==  "E"|
        ps$depth.meter ==  "Too Shallow"|
        ps$depth.meter ==  "Too shallow"|
        ps$depth.meter ==  "Bulrush"|
        ps$depth.meter ==  "island"|
        ps$depth.meter ==  "Cattails"|
        ps$depth.meter ==  "Channel"|
        ps$depth.meter ==  "channel"|
        ps$depth.meter ==  "Shore"|
        ps$depth.meter ==  "Shallow"|
        ps$depth.meter ==  "Deep"|
        ps$depth.meter ==  "shore")

  # mark those into the not.sampled column as "X"
  ps[ps$depth.meter == "Land" |
       ps$depth.meter ==  "land" |
       ps$depth.meter ==  "Unable to Sample"|
       ps$depth.meter ==  "sandbar"|
       ps$depth.meter ==  "Missed"|
       ps$depth.meter ==  "x"|
       ps$depth.meter ==  "too shallow"|
       ps$depth.meter ==  "shallow"|
       ps$depth.meter ==  "shoreline"|
       ps$depth.meter ==  "ISLAND"|
       ps$depth.meter ==  "TS"|
       ps$depth.meter ==  "*"|
       ps$depth.meter ==  "Island"|
       ps$depth.meter ==  "No sample"|
       ps$depth.meter ==  "E"|
       ps$depth.meter ==  "Too Shallow"|
       ps$depth.meter ==  "Too shallow"|
       ps$depth.meter ==  "Bulrush"|
       ps$depth.meter ==  "island"|
       ps$depth.meter ==  "Cattails"|
       ps$depth.meter ==  "Channel"|
       ps$depth.meter ==  "channel"|
       ps$depth.meter ==  "Shore"|
       ps$depth.meter ==  "Shallow"|
       ps$depth.meter ==  "Deep"|
       ps$depth.meter ==  "shore",
    "not.sampled"] <- "X"
  
# change their values to blank in the depth.meter column
  ps[ps$depth.meter == "Land" |
       ps$depth.meter ==  "land" |
       ps$depth.meter ==  "Unable to Sample"|
       ps$depth.meter ==  "sandbar"|
       ps$depth.meter ==  "Missed"|
       ps$depth.meter ==  "x"|
       ps$depth.meter ==  "too shallow"|
       ps$depth.meter ==  "shallow"|
       ps$depth.meter ==  "shoreline"|
       ps$depth.meter ==  "ISLAND"|
       ps$depth.meter ==  "TS"|
       ps$depth.meter ==  "*"|
       ps$depth.meter ==  "Island"|
       ps$depth.meter ==  "No sample"|
       ps$depth.meter ==  "E"|
       ps$depth.meter ==  "Too Shallow"|
       ps$depth.meter ==  "Too shallow"|
       ps$depth.meter ==  "Bulrush"|
       ps$depth.meter ==  "island"|
       ps$depth.meter ==  "Cattails"|
       ps$depth.meter ==  "Channel"|
       ps$depth.meter ==  "channel"|
       ps$depth.meter ==  "Shore"|
       ps$depth.meter ==  "Shallow"|
       ps$depth.meter ==  "Deep"|
       ps$depth.meter ==  "shore",
    "depth.meter"] <- ""
    
  # check for leftover words or erroneous
    sort(unique(ps$depth.meter))

#' depth 3..3 should be 3.3
  sum(ps$depth.meter =="3..3")
  ps[ps$depth.meter =="3..3", "depth.meter"] <- 3.3

#' depth = 1 should be 1
  sum(ps$depth.meter == "= 1") # 32 cases.. who wrote this and in what surveys?
  ps[ps$depth.meter == "= 1", c("datemv", "datasourcemv")]
  # it looks like in rich brasch's surveys on these dates he wrote "<= 1", but my setup pulled in "= 1"
  # I will set these to 1 for depth
  ps[ps$depth.meter == "= 1", "depth.meter"] <- "1"

#' Brasch also used a bunch of these NI and NI* things.... = "Not Interested?"
  ps[ps$depth.meter== "NI*" |
       ps$depth.meter== "NI" ,c("datemv", "datasourcemv")]
  #what is in the N/A, NI, NI* rows
  ps[ps$depth.meter == "N/A"|
       ps$depth.meter == "NI"|
       ps$depth.meter == "NI*", c("lknamemv", "depth.meter", "myriophyllum.spicatum", "ceratophyllum.demersum")]
  #can i populate these with depths from another survey?
  ps[ps$datasourcemv == "brasch" &
       ps$datemv == "2006-09-14" &
       ps$depth.meter == "NI*", c("point.id", "depth.unkn")]
  #maybe, but not going to do that for now...
  
#' I think that NI and NI* are points where the surveyors were unable to record depths. In the future I could try to fill them in by using another survey date's depth data... for
#' not I am going to delete these 4 surveys
   #set depth.meter to "" (we will delete the lines with no depth in a later step)
   ps[ps$depth.meter== "NI*" |
        ps$depth.meter== "NI" , "depth.meter"] <- ""

#' what else do we need to tackle?  
  sort(unique(ps$depth.meter))

#' how many "0" for depth.meter?
  sum(ps$depth.meter == "0")
  # who wrote that?
  ps[ps$depth.meter== "0" ,c("datemv", "datasourcemv", "lknamemv")]
  nrow(ps[ps$depth.meter== "0" ,c("datemv", "datasourcemv", "lknamemv")])

#' It looks like they meant not.sampled. We'll move these over to that column
  ps[ps$depth.meter== "0" ,"not.sampled"] <- "X"
  # and set depth.meter = ""
  ps[ps$depth.meter== "0" ,"depth.meter"] <- ""

#' how many with no depth in any of the three (249) and marked as not.sampled (249) )
  ps$depth.meter <- as.character(ps$depth.meter)
  length( ps[ps$depth.meter== "" &
     ps$depth.feet == "" &
     ps$depth.unkn == "" &
     ps$not.sampled == "X", "depth.meter"])

#' what else do we need to tackle?  
  sort(unique(ps$depth.meter))
  
#' delete the "N/A" row
  ps[ps$depth.meter == "N/A", "datasourcemv"]
  ps <- ps[ps$depth.meter != "N/A",]
  
  length(ps$depth.meter == "")
  
#' Set depth.meter as a numeric:
  ps$depth.meter <- as.numeric(ps$depth.meter)
  summary(ps$depth.meter)
  
#' Visualize results  
  hist(ps$depth.meter, breaks = seq(0, 100, .1), freq = T , xlim = c(0,20))
  hist(ps$depth.meter, breaks = seq(0, 100, .1), freq = T , xlim = c(0,50), ylim = c(0,5))
  
#' who input depths in meters exceeding 10?
  ps[is.na(ps$depth.meter) == F &
    ps$depth.meter > 10 , c("depth.meter", "datemv", "datasourcemv", "lknamemv")]
  # johnson 46 and 37 were intended to be 4.6 and 3.7, all the rest look legit to me.
  ps[is.na(ps$depth.meter) == F &
       ps$depth.meter == 46 , "depth.meter"] <- 4.6
  ps[is.na(ps$depth.meter) == F &
       ps$depth.meter == 37 , "depth.meter"] <- 3.7

#' Visualize results  
  summary(ps$depth.meter)
  hist(ps$depth.meter, breaks = seq(0, 100, .1), freq = T , xlim = c(0,20))
  hist(ps$depth.meter, breaks = seq(0, 100, .1), freq = T , xlim = c(0,50), ylim = c(0,5))

#' I guess that pretty much wraps up the depth.meter column
#####
  
#' ## depth.feet
#####
#' Let's take a look:  
  summary(ps$depth.feet)
  sort(unique(ps$depth.feet))
#' start with blanks. How many rows have no depth.feet
  sum(ps$depth.feet == "") #73279

#' Lets do some not.sampled assignment
#' How many cases of not.sampled show up?
  # how many cases (130)
  sum(
        ps$depth.feet ==  "land" |
        ps$depth.feet ==  "LAND" |
        ps$depth.feet ==  "S"|
        ps$depth.feet ==  "TD"|
        ps$depth.feet ==  "TS"|
        ps$depth.feet ==  "x"|
        ps$depth.feet ==  "X"|
        ps$depth.feet ==  "beach"|
        ps$depth.feet ==  "deep"|
        ps$depth.feet ==  "under dock"|
        ps$depth.feet ==  "dock"
        )
#' how many of those are already marked not.sampled
  ps[
      ps$depth.feet ==  "land" |
      ps$depth.feet ==  "LAND" |
      ps$depth.feet ==  "S"|
      ps$depth.feet ==  "TD"|
      ps$depth.feet ==  "TS"|
      ps$depth.feet ==  "x"|
      ps$depth.feet ==  "X"|
      ps$depth.feet ==  "beach"|
      ps$depth.feet ==  "deep"|
      ps$depth.feet ==  "under dock"|
      ps$depth.feet ==  "dock", c("depth.feet","depth.meter", "not.sampled", "depth.unkn")]

#' Assign these "not.sampled" values of "X"
  ps[ ps$depth.feet ==  "land" |
      ps$depth.feet ==  "LAND" |
      ps$depth.feet ==  "S"|
      ps$depth.feet ==  "TD"|
      ps$depth.feet ==  "TS"|
      ps$depth.feet ==  "x"|
      ps$depth.feet ==  "X"|
      ps$depth.feet ==  "beach"|
      ps$depth.feet ==  "deep"|
      ps$depth.feet ==  "under dock"|
      ps$depth.feet ==  "dock", "not.sampled" ] <- "X"

#' assign them a blank for depth.feet
  ps[ 
      ps$depth.feet ==  "land" |
      ps$depth.feet ==  "LAND" |
      ps$depth.feet ==  "S"|
      ps$depth.feet ==  "TD"|
      ps$depth.feet ==  "TS"|
      ps$depth.feet ==  "x"|
      ps$depth.feet ==  "X"|
      ps$depth.feet ==  "beach"|
      ps$depth.feet ==  "deep"|
      ps$depth.feet ==  "under dock"|
      ps$depth.feet ==  "dock", "depth.feet"] <- ""

#' How'd we do?
  sort(unique(ps$depth.feet))

#' Let's check out those zeroes 
  sum(ps$depth.feet == "0") 
  ps[ps$depth.feet == "0",c("datemv","datasourcemv","lknamemv")]

#' again, these zeroes appear to be being used as not.sampled indicators. Let's mark those rows as such:
  
  ps[ps$depth.feet == "0","not.sampled"] <- "X"

  # and delete the zeroes from depth.feet column
  ps[ps$depth.feet == "0","depth.feet"] <- ""
  
#' How'd we do?
  sort(unique(ps$depth.feet))
  
#' Whats the "3v" about
  ps[ps$depth.feet == "3v",c("datemv","datasourcemv","lknamemv")]
  # lets change them back to 3
  ps[ps$depth.feet == "3v","depth.feet"] <- "3"
  
#' depth.feet to character
  ps$depth.feet <- as.character(ps$depth.feet)

  # cases where depth.feet has a value for an NA marked .meter (we will want to fill these in (.meter blanks) with transformed ft values )
  sum(ps$depth.feet != "" &
        is.na(ps$depth.meter) == T )

#' what else do we need to tackle?  
  sort(unique(ps$depth.feet))
  length(ps$depth.feet == "")
  
#' Set depth.feet as a numeric:
  ps$depth.feet <- as.numeric(ps$depth.feet)
  summary(ps$depth.feet)
 
#' Visualize results  
  hist(ps$depth.feet, freq = T, breaks = seq(0,120,1))
  hist(ps$depth.feet, freq = T, breaks = seq(0,120,1), xlim = c(0,40) )
  hist(ps$depth.feet, freq = T, breaks = seq(0,120,1), xlim = c(0,60), ylim = c(0,10) )

#' who input depths greater than 50 feet?
  ps[is.na(ps$depth.feet) == F &
       ps$depth.feet > 50 , c("depth.feet", "datemv", "datasourcemv", "lknamemv")]
  # the 100' from kohlman was an indicator for no depth measured, lets delete those data (set as NA to be deleted later)
  ps[is.na(ps$depth.feet) == F &
       ps$depth.feet == 100 , "depth.feet"] <- NA
  # the 119.8 from brasch should be 19.8
  ps[is.na(ps$depth.feet) == F &
       ps$depth.feet == 119.8 , "depth.feet"] <- 19.8
  
#' Visualize results  
  summary(ps$depth.feet)
  hist(ps$depth.feet, freq = T, breaks = seq(0,43,1))
  
  
#' I guess that pretty much wraps up the depth.feet column 
  
#####
  
#' ## depth.unk
##### 
#' 
#' Let's take a look:  
  summary(ps$depth.unkn)
  sort(unique(ps$depth.unkn))
  ps$depth.unkn <- as.character(ps$depth.unkn)

#' where there are two values, drop the second. It appears that the second value (after the comma) it used to either show either the same value, or to show a zero. Confirmed by visual inspection of the data (seems like they are coming from Newman's "depth with plants" column). 
  sort(unique(word(string = ps$depth.unkn, start = 1, end = 1, sep = ","))) # check fn before applying it
  ps$depth.unkn <- word(string = ps$depth.unkn, start = 1, end = 1, sep = ",")

#' whats in the not.sampled column for the cases where folks annotated reasons to not sample?
  sort(unique(ps$depth.unkn)) # we see 2,3 and 551:563 are these cases
  for (i in c(2:3,551:563) ) { print(ps[ps$depth.unkn == sort(unique(ps$depth.unkn))[i], "not.sampled"])}
  ps[ps$depth.unkn==".",c("datasourcemv", "lknamemv")]
  
  # we want to assign "X" for the not.sampled values in those rows
  for (i in c(2:3,551:563) ) { ps[ps$depth.unkn == sort(unique(ps$depth.unkn))[i], "not.sampled"] <- "X"}
 
  # now we want to delete all of those data from the depth.unkn col
  for (i in c(1:2) ) {ps[ps$depth.unkn == sort(unique(ps$depth.unkn))[2], "depth.unkn"] <- ""} # the sorted set will collapse as I remove values, so 2 iterations at 2 hits "." and "0"
  for (i in c(561:548) ) {ps[ps$depth.unkn == sort(unique(ps$depth.unkn))[i], "depth.unkn"] <- ""} #  delete values starting at tail to avoid troubles as above

#' where did the dates all come from
  ps[ grep("-", ps$depth.unkn), "depth.unkn" ]
  unique(ps[ grep("-", ps$depth.unkn), c("datasourcemv", "lknamemv", "datemv") ]) # from a dustin survey in a column that should have been marked delete
  ps[ grep("-", ps$depth.unkn), "depth.unkn" ] <- ""

#' convert depth.unkn to a numeric
  sort(unique(ps$depth.unkn))
  str(ps$depth.unkn) # needs to be chr for the as.numeric conv to go work (otherwise assigns level # as numeric value).
  ps$depth.unkn <- as.numeric(ps$depth.unkn)
  
#' right.. now what?
  summary(ps$depth.meter)
  summary(ps$depth.feet)
  summary(ps$depth.unkn)
  hist(ps$depth.unkn, breaks = seq(0,240), xlim = c(0,40))
  
#' who did the unlabeled depths come from?
  nrow(unique(ps[is.na(ps$depth.unkn) == F,c ("lknamemv", "datasourcemv", "datemv")]))
  unique(ps[is.na(ps$depth.unkn) == F,c ("lknamemv", "datasourcemv", "datemv")])
  unique(ps[is.na(ps$depth.unkn) == F,c ("depth.feet", "depth.meter")]) # all of the depth.unkns have no data for other depths

#'Tease out who used meters and feet:  
 #crwd at mccarron: looks footy to me
  hist(ps[ps$datasourcemv == "crwd" &
       is.na(ps$depth.unkn) == F, "depth.unkn"])
  ps[ps$datasourcemv == "crwd" &
       is.na(ps$depth.unkn) == F, "depth.feet"] <- ps[ps$datasourcemv == "crwd" &
                                                        is.na(ps$depth.unkn) == F, "depth.unkn"]
  #newman
  ps[ps$datasourcemv == "newman" &
       is.na(ps$depth.unkn) == F, "depth.unkn"]   %>%
  hist()
  summary(ps[ps$datasourcemv == "newman" &
               is.na(ps$depth.unkn) == F, "depth.unkn"])
  ps[ps$datasourcemv == "newman" &
       is.na(ps$depth.unkn) == F, "depth.meter"] <- ps[ps$datasourcemv == "newman" &
                                                        is.na(ps$depth.unkn) == F, "depth.unkn"]
  #mccomas
  ps[ps$datasourcemv == "mccomas" &
       is.na(ps$depth.unkn) == F, "depth.unkn"]   %>%
    hist( breaks = seq(0,7,1))
    summary( ps[ps$datasourcemv == "mccomas" &
                  is.na(ps$depth.unkn) == F, "depth.unkn"])
    ps[ps$datasourcemv == "mccomas" &
         is.na(ps$depth.unkn) == F, "depth.meter"] <- ps[ps$datasourcemv == "mccomas" &
                                                          is.na(ps$depth.unkn) == F, "depth.unkn"]
  #dustin
    ps[ps$datasourcemv == "dustin" &
         is.na(ps$depth.unkn) == F, "depth.unkn"]   %>%
      summary()
      hist( ps[ps$datasourcemv == "dustin" &
                 is.na(ps$depth.unkn) == F, "depth.unkn"], xlim = c(0,40), breaks = seq(0,240,1) )
      ps[ps$datasourcemv == "dustin" &
           is.na(ps$depth.unkn) == F, "depth.feet"] <- ps[ps$datasourcemv == "dustin" &
                                                            is.na(ps$depth.unkn) == F, "depth.unkn"]
  #brasch
      ps[ps$datasourcemv == "brasch" &
           is.na(ps$depth.unkn) == F, "depth.unkn"] %>% hist()

      summary(ps[ps$datasourcemv == "brasch" &
                   is.na(ps$depth.unkn) == F, "depth.unkn"])
      ps[ps$datasourcemv == "brasch" &
           is.na(ps$depth.unkn) == F, "depth.meter"] <- ps[ps$datasourcemv == "brasch" &
                                                             is.na(ps$depth.unkn) == F, "depth.unkn"]
#' delete the depth.unkn column:
  #do any rows have a depth in the .unkn but not in .feet or .meter? NO! HOORAY!
  unique(ps[is.na(ps$depth.unkn) == F &
        (is.na(ps$depth.meter) == T &
          is.na(ps$depth.feet) == T ), c("datemv", "lknamemv", "datasourcemv") ]) 
  ps[,"depth.unkn"] <- NULL
  
  ##### 
  
#' ## combining depths
######
#' Now we want to retify the depth columns into one. I will use depth.meter as my preferred. Let start by checking out the agreeance between the two columns:
#' 
  # where we have a foot value and a meter value, do they agree?
  plot(ps$depth.meter*3.28084~ps$depth.feet, xlim = c(0,50), ylim = c(0,50))
  abline(a = 0, b = 1, col = "red") # good agreeance

  summary(ps$depth.meter)
  hist(ps$depth.meter)
  
  summary(ps$depth.feet)
  hist(ps$depth.feet)
  hist(ps$depth.feet, ylim = c(0,10))
  #check out values >50
  ps[is.na(ps$depth.feet) == F &
       ps$depth.feet > 50, c("depth.feet", "lknamemv", "datasourcemv", "datemv") ]
  # all legit except the carrie depth, delete that one
  ps[is.na(ps$depth.feet) == F &
       ps$depth.feet == 185.0 , "depth.feet"] <- NA
  
#' Based on this, if I overwrite the meter depth in here, I don't care (they will be the same anyways)
  summary(ps$depth.meter)
  
  for (i in 1:nrow(ps)) {
    # i = 1
  if (is.na(ps$depth.meter[i]) == T) {
    ps[i, "depth.meter"] <- (ps[i, "depth.feet"]*.3048)}
  }
  summary(ps$depth.meter)
  
  hist(ps$depth.meter, breaks = seq(0,73,1))
  hist(ps$depth.meter, breaks = seq(0,73,1), ylim = c(0,10))
  
  ps[,"depth.feet"] <- ps[,"depth.meter"]*3.28084
  
  summary(ps$depth.feet)
  hist(ps$depth.feet, breaks = seq(0,239.000,1))
  hist(ps$depth.feet, breaks = seq(0,239.000,1), xlim = c(0,30))
  
#####
  
#' not.sampled
#####
  unique(ps$not.sampled)
  ps$not.sampled <- as.character(ps$not.sampled)
  ps[ps$not.sampled != "","not.sampled"] <- "1"
  ps[ps$not.sampled == "","not.sampled"] <- "0"
  ps$not.sampled <- as.factor(ps$not.sampled)
  unique(ps$not.sampled)
  summary(ps$not.sampled)
  
#' are there places where depth.meter is NA and we havent marked the point as "not.sampled"?  
  nrow(ps[is.na(ps$depth.meter) == T &
            ps$not.sampled == "0", ])
#' it looks like these are the four brasch surveys with the "NI*" written in for depth and one dustin survey that is 
#' failing to pull in the depths (they are the ones that come in as a date-- see depth.unkn section above).Therefor we will be removing 
#' these rows from the dataset. 
  unique(ps[is.na(ps$depth.meter) == T &
              ps$not.sampled == "0", c("lknamemv", "datasourcemv", "datemv")]) #
  ps <- subset(ps, is.na(ps$depth.meter) != T |
                  ps$not.sampled == "1" )
#' Whats next?  
  names(ps)
#####
  
#' ## sample.taken
#####
  summary(ps$sample.taken)
   sum(ps$sample.taken== "yes" & ps$not.sampled == "1") #sample taken is not useful to us...
  ps[,"sample.taken"] <- NULL
#####

#' ## potamogeton.crispus
#####
  ps$potamogeton.crispus <- factor(ps$potamogeton.crispus)
  summary(ps$potamogeton.crispus)
  sort(unique(ps$potamogeton.crispus))
  hist(as.numeric(as.character(ps$potamogeton.crispus)))
#' who used the X and x?  sblood and mccomas and I think both are zeroes based on looking at the data. 
  ps[ps$potamogeton.crispus == "X" |
       ps$potamogeton.crispus == "x", c( "lknamemv", "datemv", "datasourcemv")]
# ' and the other goofy values - half integers, #v, etc? it looks like mccomas is our main culprit, and the numbers should all be rounded up
  ps[ps$potamogeton.crispus == "0.5" |
       ps$potamogeton.crispus == "1.01" |
       ps$potamogeton.crispus == "1.3" |
       ps$potamogeton.crispus == "1.5" |
       ps$potamogeton.crispus == "1.8" |
       ps$potamogeton.crispus == "1V" |
       ps$potamogeton.crispus == "2.3" |
       ps$potamogeton.crispus == "2.8" |
       ps$potamogeton.crispus == "3.5" |
       ps$potamogeton.crispus == "4.5" ,
     c( "potamogeton.crispus", "lknamemv", "datemv", "datasourcemv")]
  # does mccomas us a 1-4 or 1-5 scale? one to 5
  max(as.numeric(as.character(ps[ps$datasourcemv == "mccomas","potamogeton.crispus"])), na.rm = T)
  hist(as.numeric(as.character(ps[ps$datasourcemv == "mccomas","potamogeton.crispus"])))
  summary(ps$potamogeton.crispus) 
#' replace wierd vals with correct ones:
  ps[ps$potamogeton.crispus == "0.5" |
       ps$potamogeton.crispus == "1.01", "potamogeton.crispus"] <- "1"
  ps[ps$potamogeton.crispus == "1.3" |
       ps$potamogeton.crispus == "1.5"|
       ps$potamogeton.crispus == "1.8" |
       ps$potamogeton.crispus == "1V", "potamogeton.crispus"] <- "2"
  ps[ps$potamogeton.crispus == "2.3" |
       ps$potamogeton.crispus == "2.5" |
       ps$potamogeton.crispus == "2.8", "potamogeton.crispus" ] <- "3"
  ps[ps$potamogeton.crispus == "3.5" , "potamogeton.crispus"] <- "4"
  ps[ps$potamogeton.crispus == "4.5", "potamogeton.crispus"] <- "5"
  ps[ps$potamogeton.crispus == "dead" |
       ps$potamogeton.crispus == "Dead" |
       ps$potamogeton.crispus == "V" |
       ps$potamogeton.crispus == "x" |
       ps$potamogeton.crispus == "X" , "potamogeton.crispus"] <- "0"
  ps[ps$potamogeton.crispus == "", "potamogeton.crispus"] <- "0"
  
  # where not.sampled, assign p.cri NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
       ps$potamogeton.crispus != "", c( "datasourcemv", "potamogeton.crispus", "datemv")])
  ps[ps$not.sampled == "1", "potamogeton.crispus"] <- NA
  
  # re factor and view
  ps$potamogeton.crispus <- factor(ps$potamogeton.crispus, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$potamogeton.crispus)
  sort(unique(ps$potamogeton.crispus))
    hist(as.numeric(as.character(ps$potamogeton.crispus)))
  plot(depth.meter~potamogeton.crispus, data = ps)
  
  
#####
  
#' ## ceratophyllum.demersum
#' 
#####
  ps$ceratophyllum.demersum <- factor(ps$ceratophyllum.demersum)
  summary(ps$ceratophyllum.demersum)
  sort(unique(ps$ceratophyllum.demersum))
  ps[ps$ceratophyllum.demersum == "", "ceratophyllum.demersum"] <- "0"
  hist(as.numeric(as.character(ps$ceratophyllum.demersum)), breaks = c(0:5, 100), xlim = c(0,10))
#' who used the X? sblood done it and I think both are zeroes based on potamogeton.crispus parallel situation. 
  ps[ps$ceratophyllum.demersum == "X" |
       ps$ceratophyllum.demersum == "x", c( "lknamemv", "datemv", "datasourcemv")]
  ps[ps$ceratophyllum.demersum == "X","ceratophyllum.demersum"] <- "0"
#' and the other goofy values - half integers, 55, 58? it looks like mccomas is our main culprit, and the numbers should all be 
#' rounded up (see explanation in potamogeton.crispus, above). the 55 came from brasch and 58 from newman - both should be 5 via
#' looking at datasheets. 
  ps[ps$ceratophyllum.demersum == "0.5" |
       ps$ceratophyllum.demersum == "1.5" |
       ps$ceratophyllum.demersum == "2.5" |
       ps$ceratophyllum.demersum == "3.5" |
       ps$ceratophyllum.demersum == "4.5" |
       ps$ceratophyllum.demersum == "55" |
       ps$ceratophyllum.demersum == "58" ,
     c( "ceratophyllum.demersum", "lknamemv", "datemv", "datasourcemv")]
#' corrrecting the weird vals:
  ps[ps$ceratophyllum.demersum == "0.5" , "ceratophyllum.demersum"] <- "1"
  ps[ps$ceratophyllum.demersum == "1.5" , "ceratophyllum.demersum"] <- "2"
  ps[ps$ceratophyllum.demersum == "2.5" , "ceratophyllum.demersum"] <- "3"
  ps[ps$ceratophyllum.demersum == "3.5" , "ceratophyllum.demersum"] <- "4"
  ps[ps$ceratophyllum.demersum == "4.5" , "ceratophyllum.demersum"] <- "5"
  ps[ps$ceratophyllum.demersum == "55" |
       ps$ceratophyllum.demersum == "58" , "ceratophyllum.demersum"] <- "5"
  
#' where not.sampled, assign cer.dem NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$ceratophyllum.demersum != "", c( "datasourcemv", "ceratophyllum.demersum", "datemv")])
  ps[ps$not.sampled == "1", "ceratophyllum.demersum"] <- NA
  
  # re factor and view
  ps$ceratophyllum.demersum <- factor(ps$ceratophyllum.demersum, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$ceratophyllum.demersum)
  
  sort(unique(ps$ceratophyllum.demersum))
  hist(as.numeric(as.character(ps$ceratophyllum.demersum)))
  plot(depth.meter~ceratophyllum.demersum, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= ceratophyllum.demersum, y = depth.meter))+
    geom_violin(aes(ceratophyllum.demersum))

#####  
  
  
#' ## myriophyllum.spicatum
#' 
#####
  ps$myriophyllum.spicatum <- factor(ps$myriophyllum.spicatum)
  summary(ps$myriophyllum.spicatum)
  sort(unique(ps$myriophyllum.spicatum))
  hist(as.numeric(as.character(ps$myriophyllum.spicatum)))
  
#' whats up with the v and the half values?
  ps[ps$myriophyllum.spicatum == "v" |
       ps$myriophyllum.spicatum == "0.5", c( "myriophyllum.spicatum", "lknamemv", "datemv", "datasourcemv")]
  # the v's are going to become 0s... I think they are visual obs. And the half integers are steve's... will correct as in p.cri
  ps[ps$myriophyllum.spicatum == "v" , "myriophyllum.spicatum"] <- "0"
  ps[ps$myriophyllum.spicatum == "0.5",  "myriophyllum.spicatum"] <- "1"
  ps[ps$myriophyllum.spicatum == "1.5",  "myriophyllum.spicatum"] <- "2"
  ps[ps$myriophyllum.spicatum == "2.5",  "myriophyllum.spicatum"] <- "3"
  ps[ps$myriophyllum.spicatum == "3.5",  "myriophyllum.spicatum"] <- "4"
  ps[ps$myriophyllum.spicatum == "4.5",  "myriophyllum.spicatum"] <- "5"
  
  #' where not.sampled, assign value of NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$myriophyllum.spicatum!= "", c( "datasourcemv", "myriophyllum.spicatum", "datemv")])
  ps[ps$not.sampled == "1", "myriophyllum.spicatum"] <- NA
  
  # re factor and view
  ps$myriophyllum.spicatum <- factor(ps$myriophyllum.spicatum, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$myriophyllum.spicatum)
  ps[is.na(ps$myriophyllum.spicatum) == F &
    ps$myriophyllum.spicatum == "", "myriophyllum.spicatum"] <- "0"
  ps$myriophyllum.spicatum <- factor(ps$myriophyllum.spicatum, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$myriophyllum.spicatum) 
  sort(unique(ps$myriophyllum.spicatum))
  hist(as.numeric(as.character(ps$myriophyllum.spicatum)))
  plot(depth.meter~myriophyllum.spicatum, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= myriophyllum.spicatum, y = depth.meter))+
    geom_violin(aes(myriophyllum.spicatum))
#####
  
#' ## elodea.canadensis
#' 
#####
  ps$elodea.canadensis <- factor(ps$elodea.canadensis)
  summary(ps$elodea.canadensis)
  sort(unique(ps$elodea.canadensis))
  hist(as.numeric(as.character(ps$elodea.canadensis)))
  ps[is.na(ps$elodea.canadensis) == T , "elodea.canadensis"] <- "0"

    #' where not.sampled, assign value of NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$elodea.canadensis!= "", c( "datasourcemv", "elodea.canadensis", "datemv")])
  ps[ps$not.sampled == "1", "elodea.canadensis"] <- NA
  
  # re factor and view
  ps$elodea.canadensis <- factor(ps$elodea.canadensis, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$elodea.canadensis)
  sort(unique(ps$elodea.canadensis))
  hist(as.numeric(as.character(ps$elodea.canadensis)))
  plot(depth.meter~elodea.canadensis, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= elodea.canadensis, y = depth.meter))+
    geom_violin(aes(elodea.canadensis))
#####

#' ## algae.filamentous
#' 
#####
  ps$algae.filamentous <- factor(ps$algae.filamentous)
  summary(ps$algae.filamentous)
  sort(unique(ps$algae.filamentous))
  hist(as.numeric(as.character(ps$algae.filamentous)))
  ps[ps$algae.filamentous == "", "algae.filamentous"] <- "0"
  
  # get rid of half-integers
  ps[ps$algae.filamentous == "0.5", "algae.filamentous"] <- "1"
  ps[ps$algae.filamentous == "1.5", "algae.filamentous"] <- "2"
  ps[ps$algae.filamentous == "2,2", "algae.filamentous"] <- "3"
  ps[ps$algae.filamentous == "4.5", "algae.filamentous"] <- "5"

#' where not.sampled, assign value of NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$algae.filamentous!= "", c( "datasourcemv", "algae.filamentous", "datemv")])
  ps[ps$not.sampled == "1", "algae.filamentous"] <- NA
  
  # re factor and view
  ps$algae.filamentous <- factor(ps$algae.filamentous, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$algae.filamentous)
  sort(unique(ps$algae.filamentous))
  hist(as.numeric(as.character(ps$algae.filamentous)))
  plot(depth.meter~algae.filamentous, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= algae.filamentous, y = depth.meter))+
    geom_violin(aes(algae.filamentous))
  
##### 
  
#' ## nymphaeaceae
#' 
#####
ps$nymphaeacae <- as.factor(ps$nymphaeaceae)
summary(ps$nymphaeaceae)
sort(unique(ps$nymphaeaceae))
hist(as.numeric(as.character(ps$nymphaeaceae)))
ps[is.na(ps$nymphaeaceae) == T, "nymphaeaceae"] <- "0"

#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$nymphaeaceae!= "", c( "datasourcemv", "nymphaeaceae", "datemv")])
ps[ps$not.sampled == "1", "nymphaeaceae"] <- NA

# re factor and view
ps$nymphaeaceae <- factor(ps$nymphaeaceae, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$nymphaeaceae)
sort(unique(ps$nymphaeaceae))
hist(as.numeric(as.character(ps$nymphaeaceae)))
plot(depth.meter~nymphaeaceae, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= nymphaeaceae, y = depth.meter))+
  geom_violin(aes(nymphaeaceae))

#####   
  
#' ## potamogeton.foliosus
#' 
#####
ps$potamogeton.foliosus <- as.factor(ps$potamogeton.foliosus)
summary(ps$potamogeton.foliosus)
sort(unique(ps$potamogeton.foliosus))
hist(as.numeric(as.character(ps$potamogeton.foliosus)))


#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$potamogeton.foliosus!= "", c( "datasourcemv", "potamogeton.foliosus", "datemv")])
ps[ps$not.sampled == "1", "potamogeton.foliosus"] <- NA
ps[is.na(ps$potamogeton.foliosus) == T, "potamogeton.foliosus"] <- "0"
ps[ps$not.sampled == "1", "potamogeton.foliosus"] <- NA

# re factor and view
ps$potamogeton.foliosus <- factor(ps$potamogeton.foliosus, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$potamogeton.foliosus)
sort(unique(ps$potamogeton.foliosus))
hist(as.numeric(as.character(ps$potamogeton.foliosus)))
plot(depth.meter~potamogeton.foliosus, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= potamogeton.foliosus, y = depth.meter))+
  geom_violin(aes(potamogeton.foliosus))

##### 

#' ## najas.flexilis
#' 
#####
ps$najas.flexilis <- as.factor(ps$najas.flexilis)
summary(ps$najas.flexilis)
sort(unique(ps$najas.flexilis))
hist(as.numeric(as.character(ps$najas.flexilis)))


#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$najas.flexilis!= "", c( "datasourcemv", "najas.flexilis", "datemv")])
ps[is.na(ps$najas.flexilis) == T, "najas.flexilis"] <- "0"
ps[ps$not.sampled == "1", "najas.flexilis"] <- NA

# re factor and view
ps$najas.flexilis <- factor(ps$najas.flexilis, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$najas.flexilis)
sort(unique(ps$najas.flexilis))
hist(as.numeric(as.character(ps$najas.flexilis)))
plot(depth.meter~najas.flexilis, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= najas.flexilis, y = depth.meter))+
  geom_violin(aes(najas.flexilis))

##### 

#' ## vallisneria.americana
#' 
#####
ps$vallisneria.americana <- as.factor(ps$vallisneria.americana)
summary(ps$vallisneria.americana)
sort(unique(ps$vallisneria.americana))
hist(as.numeric(as.character(ps$vallisneria.americana)))


#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$vallisneria.americana!= "", c( "datasourcemv", "vallisneria.americana", "datemv")])
ps[is.na(ps$vallisneria.americana) == T, "vallisneria.americana"] <- "0"
ps[ps$not.sampled == "1", "vallisneria.americana"] <- NA

# re factor and view
ps$vallisneria.americana <- factor(ps$vallisneria.americana, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$vallisneria.americana)
sort(unique(ps$vallisneria.americana))
hist(as.numeric(as.character(ps$vallisneria.americana)))
plot(depth.meter~vallisneria.americana, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= vallisneria.americana, y = depth.meter))+
  geom_violin(aes(vallisneria.americana))

##### 

#' ## brasenia.schreberi
#' 
#####
ps$brasenia.schreberi <- as.factor(ps$brasenia.schreberi)
summary(ps$brasenia.schreberi)
sort(unique(ps$brasenia.schreberi))
hist(as.numeric(as.character(ps$brasenia.schreberi)))


#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$brasenia.schreberi!= "", c( "datasourcemv", "brasenia.schreberi", "datemv")])
ps[is.na(ps$brasenia.schreberi) == T, "brasenia.schreberi"] <- "0"
ps[ps$not.sampled == "1", "brasenia.schreberi"] <- NA

# re factor and view
ps$brasenia.schreberi <- factor(ps$brasenia.schreberi, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$brasenia.schreberi)
sort(unique(ps$brasenia.schreberi))
hist(as.numeric(as.character(ps$brasenia.schreberi)))
plot(depth.meter~brasenia.schreberi, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= brasenia.schreberi, y = depth.meter))+
  geom_violin(aes(brasenia.schreberi))

##### 

#' ## potamogeton.richardsonii
#' 
#####
  ps$potamogeton.richardsonii <- as.factor(ps$potamogeton.richardsonii)
  summary(ps$potamogeton.richardsonii)
  sort(unique(ps$potamogeton.richardsonii))
  hist(as.numeric(as.character(ps$potamogeton.richardsonii)))
  ps[ps$potamogeton.richardsonii == "", "potamogeton.richardsonii"] <- "0"
  
  # v's are zeros (visual obs in sblood dataset)
  ps[ps$potamogeton.richardsonii == "v", "potamogeton.richardsonii"] <- "0"

#' where not.sampled, assign value of NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$potamogeton.richardsonii!= "", c( "datasourcemv", "potamogeton.richardsonii", "datemv")])
  ps[is.na(ps$potamogeton.richardsonii) == T, "potamogeton.richardsonii"] <- "0"
  ps[ps$not.sampled == "1", "potamogeton.richardsonii"] <- NA

# re factor and view
  ps$potamogeton.richardsonii <- factor(ps$potamogeton.richardsonii, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$potamogeton.richardsonii)
  sort(unique(ps$potamogeton.richardsonii))
  hist(as.numeric(as.character(ps$potamogeton.richardsonii)))
  plot(depth.meter~potamogeton.richardsonii, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= potamogeton.richardsonii, y = depth.meter))+
    geom_violin(aes(potamogeton.richardsonii))
  
#####
  
#' ## spirodela.polyrhiza
#' 
#####
  ps$spirodela.polyrhiza <- factor(ps$spirodela.polyrhiza)
  summary(ps$spirodela.polyrhiza)
  sort(unique(ps$spirodela.polyrhiza))
  hist(as.numeric(as.character(ps$spirodela.polyrhiza)))
  ps[ps$spirodela.polyrhiza == "", "spirodela.polyrhiza"] <- "0"
  
  # whats up with all of the weird values
  ps[ps$spirodela.polyrhiza == "0,1"|
     ps$spirodela.polyrhiza == "0,2" |
     ps$spirodela.polyrhiza == "11", c("lknamemv","datasourcemv","datemv","spirodela.polyrhiza")]
  # Newmans's are 1's and 2's and brasch's is 1.
  ps[ps$spirodela.polyrhiza == "0,1", "spirodela.polyrhiza"] <- "1"
  ps[ps$spirodela.polyrhiza == "0,2" , "spirodela.polyrhiza"] <- "2"
  ps[ps$spirodela.polyrhiza == "11", "spirodela.polyrhiza"] <- "1"
  ps[is.na(ps$spirodela.polyrhiza) == F & 
       ps$spirodela.polyrhiza== "0,0", "spirodela.polyrhiza"] <- "0"
  
  
#' where not.sampled, assign value of NA. Notice that this is nixing some data...
  unique(ps[ps$not.sampled == "1" &
              ps$spirodela.polyrhiza!= "", c( "datasourcemv", "spirodela.polyrhiza", "datemv")])
  ps[is.na(ps$spirodela.polyrhiza) == T, "spirodela.polyrhiza"] <- "0"
  ps[ps$not.sampled == "1", "spirodela.polyrhiza"] <- NA
  
  # re factor and view
  ps$spirodela.polyrhiza <- factor(ps$spirodela.polyrhiza, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$spirodela.polyrhiza)
  ps[is.na(ps$spirodela.polyrhiza) == F & 
       ps$spirodela.polyrhiza== "0,0", "spirodela.polyrhiza"] <- "0"
  ps$spirodela.polyrhiza <- factor(ps$spirodela.polyrhiza, ordered = is.ordered(c(0,1,2,3,4,5)))
  summary(ps$spirodela.polyrhiza)
  sort(unique(ps$spirodela.polyrhiza))
  hist(as.numeric(as.character(ps$spirodela.polyrhiza)))
  plot(depth.meter~spirodela.polyrhiza, data = ps, ylim = c(0,20))
  ggplot(ps, aes(x= spirodela.polyrhiza, y = depth.meter))+
    geom_violin(aes(spirodela.polyrhiza))
  
#####  

 
  
   str(ps)[1:60]
  
#' ## nymphaea.odorata
#' 
#####
ps$nymphaea.odorata <- factor(ps$nymphaea.odorata)
summary(ps$nymphaea.odorata)
sort(unique(ps$nymphaea.odorata))
hist(as.numeric(as.character(ps$nymphaea.odorata)))
ps[ps$nymphaea.odorata == "" , "nymphaea.odorata"] <- "0"
# round half integers up:
ps[ps$nymphaea.odorata == "0.5", "nymphaea.odorata"] <- "1"
ps[ps$nymphaea.odorata == "1.5", "nymphaea.odorata"] <- "2"
ps[ps$nymphaea.odorata == "2.5", "nymphaea.odorata"] <- "3"
ps[ps$nymphaea.odorata == "3.5", "nymphaea.odorata"] <- "4"
ps[ps$nymphaea.odorata == "4.5", "nymphaea.odorata"] <- "5"

# delete any non integer values

ps[is.na(ps$nymphaea.odorata) == F & 
     (ps$nymphaea.odorata == "" |
        ps$nymphaea.odorata != "0"&
        ps$nymphaea.odorata != "1"&
        ps$nymphaea.odorata != "2"&
        ps$nymphaea.odorata != "3"&
        ps$nymphaea.odorata != "4"&
        ps$nymphaea.odorata != "5"), "nymphaea.odorata"] <- "0"

ps$nymphaea.odorata <- factor(ps$nymphaea.odorata)
summary(ps$nymphaea.odorata)

#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$nymphaea.odorata!= "", c( "datasourcemv", "nymphaea.odorata", "datemv")])
ps[is.na(ps$nymphaea.odorata) == T, "nymphaea.odorata"] <- "0"
ps[ps$not.sampled == "1", "nymphaea.odorata"] <- NA

# re factor and view
ps$nymphaea.odorata <- factor(ps$nymphaea.odorata, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$nymphaea.odorata)
sort(unique(ps$nymphaea.odorata))
hist(as.numeric(as.character(ps$nymphaea.odorata)))
plot(depth.meter~nymphaea.odorata, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= nymphaea.odorata, y = depth.meter))+
  geom_violin(aes(nymphaea.odorata))

#####
  
#' ## chara.spp
#' 
#####
ps$chara.spp <- factor(ps$chara.spp)
summary(ps$chara.spp)
sort(unique(ps$chara.spp))
hist(as.numeric(as.character(ps$chara.spp)))
ps[ps$chara.spp == "" , "chara.spp"] <- "0"
# round half integers up:
ps[ps$chara.spp == "0.5", "chara.spp"] <- "1"
ps[ps$chara.spp == "1.5", "chara.spp"] <- "2"
ps[ps$chara.spp == "2.5", "chara.spp"] <- "3"
ps[ps$chara.spp == "3.5", "chara.spp"] <- "4"
ps[ps$chara.spp == "4.5", "chara.spp"] <- "5"

# delete any non integer values
ps[is.na(ps$chara.spp) == F & 
     (ps$chara.spp == "" |
        ps$chara.spp != "0"&
        ps$chara.spp != "1"&
        ps$chara.spp != "2"&
        ps$chara.spp != "3"&
        ps$chara.spp != "4"&
        ps$chara.spp != "5"), "chara.spp"] <- "0"

ps$chara.spp <- factor(ps$chara.spp)
summary(ps$chara.spp)

#' where not.sampled, assign value of NA. Notice that this is nixing some data...
unique(ps[ps$not.sampled == "1" &
            ps$chara.spp!= "", c( "datasourcemv", "chara.spp", "datemv")])
ps[is.na(ps$chara.spp) == T, "chara.spp"] <- "0"
ps[ps$not.sampled == "1", "chara.spp"] <- NA

# re factor and view
ps$chara.spp <- factor(ps$chara.spp, ordered = is.ordered(c(0,1,2,3,4,5)))
summary(ps$chara.spp)
sort(unique(ps$chara.spp))
hist(as.numeric(as.character(ps$chara.spp)))
plot(depth.meter~chara.spp, data = ps, ylim = c(0,20))
ggplot(ps, aes(x= chara.spp, y = depth.meter))+
  geom_violin(aes(chara.spp))

##### 

#' #Automate the rest:
#' 
#####
str(ps)
names (ps)
ps.rem.names <- names(ps)[c(21:26,28:29,31:34,36:67,69:91,93:96,
                            100:102,104:108,113:113,116:137,140:146,148:154,158:160 )]
for (i in (ps.rem.names)) { 
                
                # i = "stuckenia.pectinata"
                # i = "najas.spp"
                ps[,i] <- as.character(ps[,i])
                ps[is.na(ps[,i]) == F & ps[,i] == "" , i] <- "0"
                # round half integers up:
                ps[is.na(ps[,i]) == F & ps[,i] == "0.5", i] <- "1"
                ps[is.na(ps[,i]) == F & ps[,i] == "1.5", i] <- "2"
                ps[is.na(ps[,i]) == F & ps[,i] == "2.5", i] <- "3"
                ps[is.na(ps[,i]) == F & ps[,i] == "3.5", i] <- "4"
                ps[is.na(ps[,i]) == F & ps[,i] == "4.5", i] <- "5"
                
                # delete any non integer values
                ps[is.na(ps[,i]) == F & 
                     (ps[,i] == "" |
                        ps[,i] != "0"&
                        ps[,i] != "1"&
                        ps[,i] != "2"&
                        ps[,i] != "3"&
                        ps[,i] != "4"&
                        ps[,i] != "5"), i] <- "0"
                
                # ps[,i] <- factor(ps[,i])
                # summary(ps[,i])
                # 
                #' where not.sampled, assign value of NA. Notice that this is nixing some data...
                unique(ps[ps$not.sampled == "1" &
                            ps[,i]!= "", c( "datasourcemv", i, "datemv")])
                ps[is.na(ps[,i]) == T, i] <- "0"
                ps[ps$not.sampled == "1", i] <- NA
                
                # re factor and view
                ps[,i] <- factor(ps[,i], ordered = is.ordered(c(0,1,2,3,4,5)))
                # summary(ps[,i])
                # sort(unique(ps[,i]))
                hist(as.numeric(as.character(ps[,i])))
                plot(ps$depth.meter~ps[,i], ylim = c(0,20), main = i)
                print(i)
              }

par(mfrow = c(3,3))
for (i in names(ps)) {
  plot(ps$depth.meter~ps[,i], ylim = c(0,20), main = i)
}

##### 
    
#' Save progress as a .csv file in the clp_surveys folder  
write.csv(ps, file = "data/output_data/state_clp_comp_cleaned.csv", row.names = F)  
  
######

#' ## Document footer 
#' 
#' Document spun with: ezspin("cleaning_statewide_dataset.R", out_dir = "coding_html_outputs/cleaning_statewide_dataset", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
