# lakes and rivers --------------------------------------------------------

#' ### Lakes and Rivers Program
# lnr data

lnrdat <- fread(file = "LakePlant export 20190806_notfinal.csv")

names(lnrdat)

lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]

summary <- lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]

# write.csv(summary, file = "lnrsurveysummary.csv")  

lnrdat[ , .N , .(LAKE_NAME, SURVEY_DATE, DOWLKNUM)]

int <- data.table(sp = rep("sp", 20), num = 1:20)

int[ , spnum := paste(sp,num, sep = "")]

lnrdat_1 <- separate(lnrdat, OBSERVED_TAXA, into = c(int$spnum) )

lnrdat_1[] <-  lapply(lnrdat_1[], factor)
summary(lnrdat_1)#which are blank completely? 16:20

lnrdat_1 <- lnrdat_1[,which(unlist(lapply(lnrdat_1, function(x)!all(is.na(x))))),with=F]

lnrdat_1[sp1 == "" , summary(as.factor(VEG_REL_ABUNDANCE_DESCR)) , ]# deal with locs where no veg was found.

names(lnrdat_1)

lnrdat_2 <-  melt(lnrdat_1, id.vars = c(1:5, 22:35))

#' We'll want to simplify these and drop unsampled sites from the data, also
#' reevaluate what to do with the "sampled subjective sites",  "shoreline 
#' surveys." For now, I have dropped all of these
lnrdat_2[ , SAMPLE_TYPE_DESCR := as.factor(SAMPLE_TYPE_DESCR), ]
lnrdat_2[, summary(SAMPLE_TYPE_DESCR) , ]

lnrdat_3 <- lnrdat_2[SAMPLE_TYPE_DESCR == "sampled" , , ] 

lnrdat_3[ , SAMPLE_NOTES := as.factor(SAMPLE_NOTES), ] 
lnrdat_3[ SAMPLE_NOTES != "", .(SAMPLE_NOTES), ] 

lnrdat_3[ , VEG_REL_ABUNDANCE_DESCR := as.factor(VEG_REL_ABUNDANCE_DESCR),  ]
lnrdat_3[ , summary(VEG_REL_ABUNDANCE_DESCR),]

#' We need to delete all of the na's that come from expanding our data, but we
#' don't want to lose a line of data for points where no veg was found. Luckily 
#' we have a "" (blank) for spoecies one from our melt function. So, every row 
#' with value ==NA can get hucked out.     

lnrdat_3[VEG_REL_ABUNDANCE_DESCR != 'vegetation not detected', ,]#locs not labeled as veg not det

#reatin only rows where "value" is not NA
lnrdat_4 <- lnrdat_3[is.na(value) == F, , ]


# lookup scientific names
lnrdat_4[ , variable := NULL , ]

lnrdat_4[ , sort(unique(value)), ]



lnrtaxa <- fread(file = "E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/contributor_data/macroniche_adds/perleberg/taxalist.csv")

#mndnr exported all rare species as "X" so we'll need to add this to the taxalist
lnrtaxa <- rbind(lnrtaxa,data.table(SCIENTIFIC_NAME = "Rare Species", TAXA_CODE = "X"))

# # check matches
# match(lnrdat_4$value, lnrtaxa$TAXA_CODE)

lnrdat_4[ , taxon := lnrtaxa$SCIENTIFIC_NAME[match(lnrdat_4$value, lnrtaxa$TAXA_CODE)] , ]

lnrdat_4[ , sort(unique(taxon)), ]

lnrdat_4[ VEG_REL_ABUNDANCE_DESCR == "vegetation not detected" , summary(as.factor(value)), ]

#remove datafiles from substeps
rm(lnrdat_1, lnrdat_2, lnrdat_3, lnrdat, lnrtaxa, int, summary)