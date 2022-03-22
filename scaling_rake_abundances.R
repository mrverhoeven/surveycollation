
#'---
#' title: "Reviewing Rake Abundances "
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This code will clean the relative rake density data from the whole PI dataset 
#' 
#' 
#' 
#' # Clean Up Rake Density Data?
#' 
#' Presence/absence use of these data has been a sound apporach, and has been
#' how these data have been used to-date. But the relative rake abundances
#' contain useful, point-level dat about species specific biomass (even if they
#' are an imperfect, subjective data type). 
#' 
#' On 30 Oct 2020 Mike Verhoeven, Dan Larkin, Shyam Thomas, and Jake Walsh had a
#' meeting to discuss how to clean up rake abundance data, and how to handle 
#' some of the quirkiness of the datasets.
#' 
#' The first issue we tackled was how to infer which ranking system a surveyor 
#' was using. This issue arises because different surveyors use ranking systems
#' that vary (most commonly for veg on rake a scale of 1-3, 1-4, or 1-5 is
#' used) but they do not include in their data contributions any indication of
#' what scale they are using. We decided that we could reasonably infer the
#' scale used by reviewing the max value recorded in a given year from each
#' surveyor. While we are finishing up cleaning our surveyor data, we will use
#' the data contributor instead. Thus, for each contributor-year of surveys we 
#' will set the scale they used based on the max rake density observed. 
#' 
#' The second issue we tackled was how to stretch each of these different rake
#' density scales to fit on the same scale. To do this, we have decided to 
#' convert the all rake density data to be a 1,2,3 scale from 1-4 and 1-5. This
#' is done by setting endpoints of all scales to 1 and 3, then lumping all mid-
#' values into the category of 2. We feel that this fits the truncation of how
#' much biomass can be picked up on a rake (each scale's max values are a "fully
#' covered" rake), and that all scales define their lowest category (1) as
#' something along the lines of "a single fragment or very small amount of a
#' species picked up on the rake."
#' 
#' Here we implement these changes and we review the N of presence absence
#' versus rel rake density surveys.
#' 
#' ##'Load Workspace

# load libraries ----------------------------------------------------------

library(data.table)
library(ggplot2)
library(gridExtra)


# load in data ------------------------------------------------------------

king <- fread(file = "data/output/plant_surveys_mn.csv", drop = c(1:2))

# grab records with rake abundance data ----------------------------------------

#' ## Review dataset and issue in question 

    names(king)
    
    #how many total surveys in the dataset?
    king[ , .N , SURVEY_ID] # all of the surveys can be considered "presence/absence"
    
    #how many surveys submitted seem to be rel rake abundance surveys?
    king[!is.na(REL_ABUND) , .N , SURVEY_ID] # 1057 surveys
    
    #what is the distribution of max values observed in those surveys?
    #by survey
    hist(king[!is.na(REL_ABUND) , max(REL_ABUND) , SURVEY_ID][,V1])
    
    # and by contibutor-era
    hist(king[!is.na(REL_ABUND) , max(REL_ABUND) ,
              .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d")))][,V1])
    
    #append contrib-era-max field to data (prep for later sorting and manip)
    king[!is.na(REL_ABUND)  , CONTERAMAXRAKE:= max(REL_ABUND) , 
         .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))) # contributor-era
         ]
    
    #now plot these by survey:
    king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID]
    hist(king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID][,V1])
    #and for comparison, this is what we would have said is we'd used max vals w/in surveys:
    hist(king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID][,V1])

#' ## Determining the rake scale used
#' 
#' Contributor-era max and and survey max produce different results.
#' 
#' We'll compile these surveys both ways, noting that contributor-era
#' max rake densities suffer from the contributor not always being the surveyor.
#'  At the end, we'll review what each approach gave us and pick one.
#' 
#' ## Survey level max (method 1)

# implement changes by survey -------------------------------------------

    #drop surveys with max vals of 1s and 1-2s
    rakes1 <- king[SURVEY_ID %in% king[!is.na(REL_ABUND)  ,
                                       max(REL_ABUND) , SURVEY_ID  ][V1 %in% c(3,4,5),SURVEY_ID], ]
    
    #check that we've dropped the 1's and 2's
    hist(rakes1[!is.na(REL_ABUND)  , max(as.numeric(REL_ABUND)) , SURVEY_ID  ][,V1])
    
    #how many surveys in these categories?
    rakes1[  , .N  , SURVEY_ID] #932
    
    #Now shift/ realign data per discussion above
    
    #1-4 survey shifted to 1-3
    rakes1[SURVEY_ID %in% king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID ][V1 == 4,SURVEY_ID] &
              REL_ABUND == 3 , REL_ABUND := 2 ]
    rakes1[SURVEY_ID %in% king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID ][V1 == 4,SURVEY_ID] &
             REL_ABUND == 4 , REL_ABUND := 3 ]
    
    #1-5 surveys shifted to 1-3
    rakes1[SURVEY_ID %in% king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID ][V1 == 5,SURVEY_ID] &
             (REL_ABUND == 3 |REL_ABUND == 4) , REL_ABUND := 2 ]
    rakes1[SURVEY_ID %in% king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID ][V1 == 5,SURVEY_ID] &
             REL_ABUND == 5 , REL_ABUND := 3 ]
    
    #check that the max vals are all 3's
    hist(rakes1[ !is.na(REL_ABUND) , max(REL_ABUND) , SURVEY_ID  ][,V1])
    
    #and all data are distributed in 1-3 rake density framework
    hist(rakes1[ !is.na(REL_ABUND) , REL_ABUND ,  ])
    
    # count the number of surveys we've got
    rakes1[ , .N , SURVEY_ID ] # N points per survey (includes NA's--points where no species were observed)
    

# do changes by contributor-era -------------------------------------------

#' ## Contributor-era maximums used to det rake scale (method 2)
    
    #how many contributor-eras and whats max rake dens for each?
    king[!is.na(REL_ABUND)  , max(REL_ABUND) , 
           .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d")))
           ]
    #hist max vals
    hist(king[!is.na(REL_ABUND)  , max(REL_ABUND) , 
              .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d")))
              ][,V1])
    
    #drop surveys with max vals of 1s and 1-2s
    rakes2 <- king[SURVEY_ID %in% king[!is.na(REL_ABUND), mean(CONTERAMAXRAKE) , SURVEY_ID][V1 %in% c(3,4,5),SURVEY_ID], ]
    
    #check that we've dropped the 1's and 2's
    hist(rakes2[!is.na(REL_ABUND)  , max(as.numeric(REL_ABUND)) , 
                .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d")))][,V1])
    
    #how many surveys in these categories?
    rakes2[  , .N  , SURVEY_ID]
    #distribution of densities:
    hist(rakes2[!is.na(REL_ABUND), REL_ABUND, ])
    
    #Now shift/ realign data per discussion at top of code
    
    #1-4 survey shifted to 1-3
    rakes2[SURVEY_ID %in% king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID ][V1 == 4,SURVEY_ID] &
             REL_ABUND == 3 , REL_ABUND := 2 ]
    rakes2[SURVEY_ID %in% king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID ][V1 == 4,SURVEY_ID] &
             REL_ABUND == 4 , REL_ABUND := 3 ]
            
    #1-5 surveys shifted to 1-3
    rakes2[SURVEY_ID %in% king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID ][V1 == 5,SURVEY_ID] &
             (REL_ABUND == 3 |REL_ABUND == 4) , REL_ABUND := 2 ]
    rakes2[SURVEY_ID %in% king[!is.na(REL_ABUND)  , mean(CONTERAMAXRAKE) , SURVEY_ID ][V1 == 5,SURVEY_ID] &
             REL_ABUND == 5 , REL_ABUND := 3 ]
    
    #check that the max vals are all 3's in each contributor-era
    hist(rakes2[ !is.na(REL_ABUND) , max(REL_ABUND) , .(DATASOURCE,year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d")))  ][,V1])
    
    #and all data are distributed in 1-3 rake density framework
    hist(rakes2[ !is.na(REL_ABUND) , REL_ABUND ,  ])
    
    # count the number of surveys we've got
    rakes2[ , .N , SURVEY_ID ] # N points per survey (includes NA's--points where no species were observed)
      
#' # Plots to visualize data cleaning outcomes
#' 
#' We've implemented our changes in multiple ways. Let's throw up a few plots to
#' visualize what we've done.  

    #choose a cutoff N for samples to warrant inclusion (by species)
    
    #survey points sampled by year
    king_points <- ggplot(king[ , .N , .(SURVEY_DATE, SURVEY_ID ,POINT_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N points sampled")+
      labs(title = "Pres/Abs points sampled per year")
    
    #surveys conducted by year
    king_surveys <- ggplot(king[ , .N , .(SURVEY_DATE, SURVEY_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N surveys")+
      labs(title = "Pres/Abs surveys conducted per year")
    
    #rake density survey points sampled by year
    rd_points_method1 <- ggplot(rakes1[ , .N , .(SURVEY_DATE, SURVEY_ID ,POINT_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N points sampled")+
      labs(title = "Rake points sampled per year (method1)")
    
    #rake density survey points sampled by year
    rd_points_method2 <- ggplot(rakes2[ , .N , .(SURVEY_DATE, SURVEY_ID ,POINT_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N points sampled")+
      labs(title = "Rake points sampled per year (method2)")
    
    #rake surveys conducted by year
    rd_surveys_method1 <- ggplot(rakes1[ , .N , .(SURVEY_DATE, SURVEY_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N surveys")+
      labs(title = "rake surveys conducted per year (method1)")
    
    #rake surveys conducted by year
    rd_surveys_method2 <- ggplot(rakes2[ , .N , .(SURVEY_DATE, SURVEY_ID)], aes(year(as.POSIXct(SURVEY_DATE, format = "$Y-$m-$d"))))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+
      xlab("Year")+
      ylab("N surveys")+
      labs(title = "rake surveys conducted per year (method2)")

#' ## Sample sizes

    grid.arrange(king_points,rd_points_method1, rd_points_method2,king_surveys,rd_surveys_method1, rd_surveys_method2, ncol = 3, nrow = 2)






    #distribution of rake densities
    
    uncorrected_desities <- 
      ggplot( king[!is.na(REL_ABUND), ], aes(REL_ABUND))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+ 
      ylim(c(0,160000))+
      labs(title = "uncorrected")
    
    # uncorrected_desities_3scale_contrib <- 
    #   ggplot( king[!is.na(REL_ABUND) & CONTERAMAXRAKE == 3, ], aes(REL_ABUND))+
    #   geom_histogram(binwidth = 1, color = "gray", fill = 1 )+ 
    #   ylim(c(0,11000))+ 
    #   labs(title = "uncorrected_scale1-3_method2")
    
    uncorrected_desities_3scale_survey <- 
      ggplot( king[!is.na(REL_ABUND) &
                     SURVEY_ID %in% king[!is.na(REL_ABUND)  , max(REL_ABUND) , SURVEY_ID ][V1 == 3, SURVEY_ID ], ,
                   ],
                   aes(REL_ABUND))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+ 
      ylim(c(0,18000))+ 
      labs(title = "uncorrected_scale1-3surveys")
    
    corrected_method1 <- 
      ggplot( rakes1[!is.na(REL_ABUND), ], aes(REL_ABUND))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+ 
      ylim(c(0,160000))+
      labs(title = "method1")
    
    corrected_method2 <- 
      ggplot( rakes2[!is.na(REL_ABUND), ], aes(REL_ABUND))+
      geom_histogram(binwidth = 1, color = "gray", fill = 1 )+ 
      ylim(c(0,160000))+
      labs(title = "method2")

#' ## Density distributions 

    grid.arrange(uncorrected_desities,corrected_method1,corrected_method2,uncorrected_desities_3scale_survey, ncol = 4)

#' # Which method of determining max rake density was best?
#' 
#' Here we can see that the use of max values within a survey to determine rake 
#' density scales are a slightly better match to those surveys originally
#' surveyed on a 1-3 scale. I made this determination by comparing the
#' ratio of 1:2:3 in each of method 1, method 2, and originally submitted as 1-3
#' surveys. We could or should use a statistical test to evaluate this more 
#' empirically-- but I'm not certain I know what test would serve us best.
#' 
#' Given this conclusion (method 1 is better of the two options), let's export
#' the dataset for use in other projects:
#' 
#' #' #3 Output cleaned data

    str(rakes1)
    
    # write.csv(rakes1, file = "data/output/rake_abund_surveys_mn.csv" )
    
    
    
    rakes1[ DEPTH_FT <= 15, .(nsamp = .N)  , SURVEY_ID ] #number of samples in <15 feet for each survey
    
    rakes1[REL_ABUND ==3 & DEPTH_FT <= 15, .(nmaxRD = .N), SURVEY_ID] #number of samples with maxRD in <15feet for each survey
    
    #combine those 2 things: 
    maxRD <- merge(rakes1[ DEPTH_FT <= 15, .(nsamp = .N)  , SURVEY_ID ],rakes1[REL_ABUND ==3 & DEPTH_FT <= 15, .(nmaxRD = .N), SURVEY_ID], )
    
    merge(rakes1,maxRD, by = "SURVEY_ID")[ , .(nmaxRD = unique(nmaxRD), SURVEY_ID, LAKE_NAME ) , SURVEY_ID]
