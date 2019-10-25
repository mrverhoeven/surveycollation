# header ------------------------------------------------------------------

#' # Calculating plant foc data for each point in the statewide dataset
#' ### Author: Mike Verhoeven
#' ### Date: 24 May 2019

#' This script will calculate survey-level statisitics for frequency of occurrence
#'  and mean rake density (at points where each species is found) for lake data
#'  that are in a point-level format. 
#'  
#'  On 10 September 2018 I added coded to specify removing/ignoring NA's in the 
#'  calculation of rake densities and foc because of errors found in compiling
#'  native response data/analysis. Following this, the code took ~16 hours to run.
#'  
#'  On 12 August 2019 I edited this code to run with data.table (in an effort to cut
#'  calculation times). This process also included recalculating all lak level stats
#'  both including all points surveyed, and including only points within the littoral 
#'  zone of each specific lake's maximum ever in our dataset observed (secchi)
#'  littoral area (secchi*2). 
#'  
#'  This script is not put into a Rmd file, mainly because it is entirely a
#'  calculation script (no analysis or visualization)
#'  
#'  UNCOMMENT "SAVE OUTPUT" SECTION TO RESAVE OUTPUT FILE

#' # Preamble
#' Load libraries
#+warning=FALSE, message=FALSE 
library(ggplot2)
library(knitr)
library(ezknitr)
library(data.table)
library(reshape2)

#' Remove anything in memory
#rm(list = ls())

#' Load custom functions

#' Set working directory to project location

# load in data ------------------------------------------------------------



#' Load in statewide dataset (cleaned):
ps <- fread(file = "data/output_data/loaded_data/state_clp_comp_cleaned.csv") 

names(ps)



# do it with data.table ---------------------------------------------------

#bring point level values to front (leaves all sp obs to fall in after selected cols)
setcolorder(ps, c(10:12,1,2,18,27,30,35,55,68,92,97:99,103,109:111,114:115,138:139,147,155:157))
names(ps)

# delete Fieldseth's added points
ps  <- subset(ps, added.point != "x")

# delete points marked as unsampled
unique(ps[,"not.sampled"])
ps <- subset(ps, not.sampled != 1)


# delete unneeded/not useful cols
names(ps)
str(ps)
ps[,c(7:9,11,13:27):=NULL]

# populate the no veg found
ps[, "no.veg.found":= as.factor(no.veg.found)] 
summary(ps[,"no.veg.found"])
ps[, "no.veg.found":= as.character(no.veg.found)] 
ps[no.veg.found == "2", no.veg.found:= "0"]
ps[no.veg.found == ""  , no.veg.found:= "0"]
ps[no.veg.found != "0"  , no.veg.found:= "1"]
ps[, "no.veg.found":= as.factor(as.character(no.veg.found))] 
names(ps)
ps[ is.na(lemna.spp)==T , lemna.spp] # all in the lemna 
ps[ is.na(lemna.spp)==T , lemna.spp:= 0 ,] # make NAs into zeros

str(ps)


# match lake names to ease secchi data join -------------------------------


# from this plot we can see that littoral nsamp is always less than total, but not significantly different.
# calculate using lake specific depths:
# Step One is to get a list of the lake names:

AKdat <- fread(file = "data/output_data/pcrispus_wDNRtrtdat.csv", colClasses=c(Lake.ID="character"))
AKdat[ ,.N, by = .(Lake.ID,Lake.x) ]
ps[ ,.N, by = .(lknamemv) ]

cbind(AKdat[ ,.N, by = .(Lake.ID,Lake.x) ],
ps[ ,.N, by = .(lknamemv) ])

sort(unique(AKdat$Lake.x))
sort(unique(ps$lknamemv))

# what lakes in Adams set are unmatched?
A <- sort(unique(AKdat$Lake.x))
B <- sort(unique(ps$lknamemv))
A[is.na(match(A,B))==T]


# fix lake names ----------------------------------------------------------


#' Baldeagle: no data in a tabular format
#' Benton: no data in a tabular format (only summary stats)
#' Clear lakes (2) each from dif datasource:
ps[lknamemv=="clearm", c("lknamemv", "datasourcemv" )]
ps[lknamemv=="clear"&
     datasourcemv =="newman", lknamemv := "clearm"]
ps[lknamemv=="clear"&
     datasourcemv =="mccomas", lknamemv := "clearw"]
#'Tonka's bays
ps[lknamemv=="graystonka", lknamemv:= "grays"]
ps[lknamemv=="halsteadtonka", lknamemv := "halsted"]
ps[lknamemv=="stubbstonka", lknamemv := "stubbs"]
ps[lknamemv=="stalbanstonka", lknamemv := "stalban"]
ps[lknamemv=="maxwelltonka", lknamemv := "maxwell" ]
ps[lknamemv=="northarmtonka", lknamemv := "northarm"  ]
ps[lknamemv=="libbstonka", lknamemv := "libbs" ]
#'Little crosby
ps[lknamemv=="crosbyupper"|
     lknamemv=="crosbylower", .N, by = c("lknamemv", "datemv")] #match inferred by smaller N on smaller lake and pairing with Ray's notes on lake names and DOWs
ps[lknamemv=="crosbylower", lknamemv := "littlecrosby"]
#' Long lakes (2)
ps[lknamemv=="long", c("lknamemv", "datasourcemv")]
ps[lknamemv=="long"&
     datasourcemv =="mccomas", lknamemv := "longd" ]
ps[lknamemv=="long"&
     datasourcemv =="newman", lknamemv := "longi" ]
#'mccarrons
ps[lknamemv=="mccarron", lknamemv := "mccarrons"]
#' Sarah east and west
ps[lknamemv=="saraheast", lknamemv := "sarahe"]
ps[lknamemv=="sarahwest", lknamemv := "sarahw"]
#' southcenter
ps[lknamemv=="southcentr", lknamemv := "southcenter" ]
#' sweeney: no useable tabular data
#' twin: no useable tabular data
ps[lknamemv=="crosbyupper", lknamemv:= "uppercrosby"]
ps[lknamemv=="crook", lknamemv:="crookneck"]

# what lakes in Adams set are unmatched?
A <- sort(unique(AKdat$Lake.x))
B <- sort(unique(ps$lknamemv))
A[is.na(match(A,B))==T]

#we now have unique IDs to link these two datasets based on lake names.

# calc lake stats ---------------------------------------------------------

#' This next section could be run with varying depths
# calculate nsamp
summary(ps[ depth.meter<4.6 ,.N , by = c("datemv","datasourcemv","lknamemv")])
ps[ depth.meter<4.6 , lnsamp := .N , by = c("datemv","datasourcemv","lknamemv")]
ps[is.na(lnsamp) == T, c("datemv","datasourcemv","lknamemv","lnsamp"),]

summary(ps[datasourcemv=="brasch" & lknamemv=="medicine" , .(depth.meter,datasourcemv, datemv, lnsamp)]) #NA asisgned to rows deeper than littoral

# calculate totnsamp
ps[ ,.N , by = c("datemv","datasourcemv","lknamemv")]
ps[ , tnsamp := .N , by = c("datemv","datasourcemv","lknamemv")]
plot(ps$lnsamp~ps$tnsamp)


# add max secchi data to ps -----------------------------------------------
maxlit <- AKdat[is.na(PrevAUGSecchi)==F, max(PrevAUGSecchi), by = c("Lake.x", "Lake.ID")]
maxlit

#' max secchi (lake) to ps(lake-date) based on year & lakename

names(AKdat)
names(ps)
names(maxlit)[3] <- "maxsecchi"

ps[ , .N , by = c("lknamemv","datasourcemv","datemv")]

ps <- merge(ps, maxlit, by.x = "lknamemv", by.y = "Lake.x", all.x = T)

#bring point level values to front (leaves all sp obs to fall in after selected cols)
names(ps)
setcolorder(ps, c(1:3,142:145,4:141))
names(ps)

# here are the lakes with data, but having no secchi data for even one year.
ps[is.na(maxsecchi) == T, .N, by = c("lknamemv","datasourcemv","datemv") ] 
#checked this list usingthe following code to look at the secchi data for each lake name. 
# AKdat[Lake.x=="vetstonka", .(Year,Lake.x, PrevAUGSecchi)]

# calculate lslnsamp
summary(ps$maxsecchi)
ps[is.na(maxsecchi)==T]
summary(ps[ depth.meter < 2*maxsecchi, .N , by = c("datemv","datasourcemv","lknamemv")])
ps[ depth.meter < 2*maxsecchi, lslnsamp := .N , by = c("datemv","datasourcemv","lknamemv")]
summary(ps$lslnsamp)
plot(ps$lslnsamp~ps$tnsamp)
plot(ps$lslnsamp~ps$lnsamp)




# reshape data ------------------------------------------------------------


#' Now we need to reshape this behemoth...
names(ps)
#bring point level values to front (leaves all sp obs to fall in after selected cols)
names(ps)
setcolorder(ps, c(1:7,146,8:145))
names(ps)

# retain the point ID chars and the depth, then make data long (new row for every observation of a species)
ps_r1 = melt(ps, id.vars = c(1:13),
             variable.name = "taxon", value.name = "rake.dens")

# test er' out
str(ps_r1) # a cool 11 million rows..
ps_r1[ , max(rake.dens), by = datasourcemv]


#drop all of the zeros (not observed) in this plant 
ps_r2 <- ps_r1[rake.dens>0,]
str(ps_r2)


# calculate more lake stats ----------------------------------------------------



# calculate mean rake densities:
ps_r2[ , mean_rd := mean(rake.dens) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
#create a sep file for these:
rd <- ps_r2[ , mean(rake.dens) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(rd)[6] <- "rd"
rd[ , .N, taxon] # how many taxa do we have represented and in how many surveys are each found?
rd[ , .N, c("datemv","datasourcemv","lknamemv","Lake.ID")] # how many taxa do we have represented in each survey?

#calculate nocc by species
# littoral area
ps_r2[ depth.meter<4.6, lnocc:= .N , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
ps_r2[ depth.meter<4.6, .(lnocc,lnsamp) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
ps_r2[ depth.meter<4.6, lfoc := lnocc/lnsamp , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]

# for total lake area
ps_r2[ , tnocc:= .N , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
summary(ps_r2$tnocc)
ps_r2[ , .(tnocc,tnsamp) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
ps_r2[ , tfoc := tnocc/tnsamp , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]

# for lake specific littoral
ps_r2[ depth.meter< 2*maxsecchi, lslnocc:= .N , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
summary(ps_r2$lslnocc)
summary(ps_r2$lslnsamp)
ps_r2[ depth.meter< 2*maxsecchi, .(lslnocc,lslnsamp) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
ps_r2[ depth.meter< 2*maxsecchi, lslfoc := lslnocc/lslnsamp , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
summary(ps_r2$lslfoc)
#



# compile into a dataset --------------------------------------------------



#peel focs off at the taxa-survey level
summary(ps_r2[ depth.meter<4.6, mean(lfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
foc <- ps_r2[ depth.meter<4.6, mean(lfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(foc)[6] <- "litfoc"

summary(ps_r2[, mean(tfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
totfoc <- ps_r2[, mean(tfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(totfoc)[6] <- "totfoc"

#lake specific littoral foc
summary(ps_r2[is.na(lslfoc)==F, max(lslfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
lslfoc <- ps_r2[is.na(lslfoc)==F, mean(lslfoc) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(lslfoc)[6] <- "lslfoc"

#max occ depth by survey-taxon
summary(ps_r2[, max(depth.meter) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
maxdepth <- ps_r2[, max(depth.meter) , by = c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(maxdepth)[6] <- "maxdep"


#n samples
summary(ps_r2[, mean(tnsamp), c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
tot_samp <- ps_r2[, mean(tnsamp), c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(tot_samp)[6] <- "tnsamp"

summary(ps_r2[is.na(lnsamp)==F,mean(lnsamp), c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
lit_samp <- ps_r2[is.na(lnsamp)==F,mean(lnsamp), c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(lit_samp)[6] <- "lnsamp"

summary(ps_r2[is.na(lslnsamp)==F ,mean(lslnsamp) , c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")])
lsl_samp <- ps_r2[is.na(lslnsamp)==F , mean(lslnsamp) , c("datemv","datasourcemv","lknamemv","Lake.ID","taxon")]
names(lsl_samp)[6] <- "lslsamp"



surveylaketaxastat <- merge(merge(merge(merge(merge(merge(merge(foc,rd),maxdepth),totfoc),tot_samp),lit_samp),lsl_samp),lslfoc)


surveylaketaxastat[,.N,  by = c("datemv","datasourcemv","lknamemv","Lake.ID")] #number of species per lake


surveylaketaxastat[taxon == "potamogeton.crispus",]



# save output -------------------------------------------------------------



#' save progress as a .csv file in the clp_surveys folder  
    # write.csv(surveylaketaxastat, file = "data/output_data/surveylktaxastat.csv", row.names = F)  
  

    
    
    
    