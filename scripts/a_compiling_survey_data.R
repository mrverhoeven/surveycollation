#' # Data compilation from multiple PI surveys
#' ### Author: Mike Verhoeven
#' ### Date: 16 Mar 2017
#' a




#' ## Document Preamble
#+ warning = FALSE

  # set working directory  
  setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis")

  # Clear Environment
  remove(list = ls())
  
  # Load Libraries
  library(knitr)
  library(ezknitr)
  library(readxl)
  library(Hmisc)
  library(stringr)
  library(dplyr)
  library(tools)
 
  # Set knitr options
  opts_chunk$set(fig.width = 6, fig.height = 5, warning = FALSE, message = FALSE)
  
#' ### Assign Custom Functions
#'   
#'Function for cleaning fieldnames. Edited from Dan Larkin's original version.  Makes spaces, periods dashes, fwd slashes and parentheses into underscore. 
  
  #tidyName()
  tidyName <- function(x) tolower(gsub(" ", "_", trimws(gsub("__", " ",gsub("  ", " ", gsub(".", " ", gsub("(", " ", gsub(")", " ", gsub("-", " ", gsub("/", " ", gsub("?", " ", 
                                                          x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), "both"), fixed = TRUE))

#' # A new approach:
#' Combine all of the surveys, then do the lookup table at the end 
#'    
#' "ps" (point scale) will be our dataset. Let's fill er' up!

                  # create an empty dataframe 
  ps <- data_frame(NA,"")   
  
#' ## Brasch Surveys:
#####
  # set working Dir to sourcefile loc:
  setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/brasch")
 
  # List file extensions that you'd like to cover
  files = list.files(pattern= "*.xls")# Get the files names for extension j

  # for those files, go through, clean up headings and compile them
      for  (i in c(1:length(files))) {
       
        # load in survey FILEi, name it processingtable:
        processingtable <- read_excel(files[i])
        
        # add a column for datasource and populate with SOURCEi
        processingtable$datasourcemv <- rep("brasch", length(processingtable[,1]))
        
        # add a column for lake name
        processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
        
        # add a column for date YEAR-MO-DA (version 2)
        processingtable$datemv <- rep(as.character(as.Date(paste(word(sub(" ","-", file_path_sans_ext(files[i])),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                      length(processingtable[,1]))

        # clean up fieldnames
        names(processingtable) <- tidyName(names(processingtable))
        
        # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
        processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
        
        # name columns with no headings. If all are named, print "no missing names for:"
        ifelse(sum(names(processingtable) == "") > 0 ,
               processingtable <- tbl_df(data.frame(processingtable)),
               print("no missing names for:"))
        
        # make all columns into factors before compiling?
        processingtable[] <- lapply(processingtable[], factor)

        # save that into the big set
        ps <- bind_rows(processingtable, ps)
        
        # print iteration # and survey name
        print(c(i,files[i]))
        
        }
     
#' Check for input:
  
  # Original ps has 1 row (NAs) + brasch has 88 surveys, should see 1 + 88 = 89 unique combinations of data source, lake, survey date.
  nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

#####    
     
#' ### Dustin Surveys:
#####
#' For these data I used a Kutools for Excel file conversion tool to correct the file extensions
#' because the read excel function didn't like the way they were written (I think that the file 
#' format didnt match the file extension label). So you'll see that the files are pulled from a
#' subfolder "converted."
     
     # set working Dir to sourcefile loc:
     setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/dustin/converted")
     
     # List file extensions that you'd like to cover
     files = list.files(pattern= "*.xls")# Get the files names for files with .xls (also grabs .xlsx) extensions
     
     # for those files, go through-- clean up, add headings-- and compile them
     for  (i in c(1:length(files))) {
       
       # load in survey FILEi, name it processingtable:
       processingtable <- read_excel(files[i])
       
       # add a column for datasource and populate with SOURCEi
       processingtable$datasourcemv <- rep("dustin", length(processingtable[,1]))
       
       # add a column for lake name
       processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
       
       # add a column for date YEAR-MO-DA (version 2)
       processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-", file_path_sans_ext(files[i])),c(-2,-1), sep = "-"), collapse = ""), format = "%Y%m%d")), 
                                     length(processingtable[,1]))
       
       # clean up fieldnames
       names(processingtable) <- tidyName(names(processingtable))
       
       # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
       processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
       
       # name columns with no headings. If all are named, print "no missing names for:"
       ifelse(sum(names(processingtable) == "") > 0 ,
              processingtable <- tbl_df(data.frame(processingtable)),
              print("no missing names for:"))
       
       # make all columns into factors before compiling?
       processingtable[] <- lapply(processingtable[], factor)
      
       #save that into the big set
       ps <- bind_rows(processingtable, ps)
       
       # print iteration # and survey name
       print(c(i,files[i]))
       
     }
     
#' Check for input: 
      # (1 row original, 88 from brasch,  104 from dustin should give 193 total)
      nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

#####

#' ### Fieldseth Surveys:
#####
#' For these data I used a Kutools for Excel workbook splitting tool to split tabs from workbooks into separate 
#' .xlsx files. I also went through the surveys and deleted a header row from them to get the fieldnames into row # 1. 
#' Only the Excel data from the "CLP_Fieldseth" folder were used (some survey data remained in pdf or word doc form).
#' Only the modified files are included in the "fieldseth" folder.
#' 

     # set working Dir to sourcefile loc:
     setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/fieldseth")
     
     # List file extensions that you'd like to cover
     files = list.files(pattern= "*.xls")# Get the files names for extension j
     
     # for those files, go through, clean up headings and compile them
     for  (i in c(1:length(files))) {
       
       # load in survey FILEi, name it processingtable:
       processingtable <- read_excel(files[i])
       
       # add a column for datasource and populate with SOURCEi
       processingtable$datasourcemv <- rep("fieldseth", length(processingtable[,1]))
       
       # add a column for lake name
       processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
       
       # add a column for date YEAR-MO-DA (version 2)
       processingtable$datemv <- rep(as.character(as.Date(paste(word(sub(" ","-", file_path_sans_ext(files[i])),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                     length(processingtable[,1]))

       # clean up fieldnames
       names(processingtable) <- tidyName(names(processingtable))
       
       # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
       processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
       
       # name columns with no headings. If all are named, print "no missing names for:"
       ifelse(sum(names(processingtable) == "") > 0 ,
              processingtable <- tbl_df(data.frame(processingtable)),
              print("no missing names for:"))
       
       # make all columns into factors before compiling
       processingtable[] <- lapply(processingtable[], factor)
      
       #save that into the big set
       ps <- bind_rows(processingtable, ps)
       
       # print iteration # and survey name
       print(c(i,files[i]))
       
     }
     
#' Check for input:
    # (193 prior, 12 in fieldseth = 205)
    nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

###### 

#' Johnson Surveys:
#####
#' For these data in Excel: collapse fieldnamess into single cell, delete header rows & sub fieldname rows, delete extra rows (populated with zeros), delete extra columns
#'  (populated with excess calculations), name tabs as lake date, export tabs with Kutools for Excel workbook splitting tool
#'      
     # set working Dir to sourcefile loc:
     setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/johnson")
     
     # List file extensions that you'd like to cover
     files = list.files(pattern= "*.xls")# Get the files names for extension j
     
     # for those files, go through, clean up headings and compile them
     for  (i in c(1:length(files))) {
       
       # load in survey FILEi, name it processingtable:
       processingtable <- read_excel(files[i])
       
       # add a column for datasource and populate with SOURCEi
       processingtable$datasourcemv <- rep("johnson", length(processingtable[,1]))
       
       # add a column for lake name
       processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
       
       # add a column for date YEAR-MO-DA (version 2)
       processingtable$datemv <- rep(as.character(as.Date(paste(word(sub(" ","-", file_path_sans_ext(files[i])),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                     length(processingtable[,1]))
       
       # clean up fieldnames
       names(processingtable) <- tidyName(names(processingtable))
       
       # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
       processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
       
       # name columns with no headings. If all are named, print "no missing names for:"
       ifelse(sum(names(processingtable) == "") > 0 ,
              processingtable <- tbl_df(data.frame(processingtable)),
              print("no missing names for:"))
       
       # make all columns into factors before compiling
       processingtable[] <- lapply(processingtable[], factor)
       
       #save that into the big set
       ps <- bind_rows(processingtable, ps)
       
       # print iteration # and survey name
       print(c(i,files[i]))
       
     }
     
#' Check for input 
      # (205 prior, 85 in johnson = 290)
      nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
     
###### 

#' Lund Surveys:
#####
 #' For these data in Excel:delete header rows, delete extra rows (populated with zeros), delete extra columns
 #'  (populated with excess calculations), name tabs as "lake m-d-year", export tabs with Kutools for Excel workbook splitting tool
 #'      
 # set working Dir to sourcefile loc:
 setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/lund")
 
 # List file extensions that you'd like to cover
 files = list.files(pattern= "*.xls")# Get the files names for extension j
 
 # for those files, go through, clean up headings and compile them
 for  (i in c(1:length(files))) {
   
   # load in survey FILEi, name it processingtable:
   processingtable <- read_excel(files[i])
   
   # add a column for datasource and populate with SOURCEi
   processingtable$datasourcemv <- rep("lund", length(processingtable[,1]))
   
   # add a column for lake name
   processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
   
   # add a column for date YEAR-MO-DA (version 2)
   processingtable$datemv <- rep(as.character(as.Date(paste(word(sub(" ","-", file_path_sans_ext(files[i])),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                 length(processingtable[,1]))
   
   # clean up fieldnames
   names(processingtable) <- tidyName(names(processingtable))
   
   # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
   processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
   
   # name columns with no headings. If all are named, print "no missing names for:"
   ifelse(sum(names(processingtable) == "") > 0 ,
          processingtable <- tbl_df(data.frame(processingtable)),
          print("no missing names for:"))
   
   # make all columns into factors before compiling
   processingtable[] <- lapply(processingtable[], factor)
   
   #save that into the big set
   ps <- bind_rows(processingtable, ps)
   
   # print iteration # and survey name
   print(c(i,files[i]))
   
 }
 
 #' Check for input 
   # (290 prior, 3 in lund = 293)
   nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
 
######      
 
#' McComas Surveys:
#####
#' For these data in Excel:use kutools to combine multirow headers, delete extra header rows, delete non-data rows, delete extra columns,
#' name tabs as "lake m-d-year", rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l mods done in excel:
#' Gleason:  split into N. and S. Basin because of data point i.d. duplication (manually copy N basin to new tab)
#'           rename "clp stems1,2,3..." (this is the only CLP data in 4-21-15) which I belive represent quadrat data?
#' Long:     deleted some oddly place and incomplete data in 6_23_13 survey tab (seems to not be from this lake (points don't match))
#' Round:    most years in transect form... yikes, 2006 data has no survey date (set to 1_1_06)  
 
   # set working Dir to sourcefile loc:
   setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/mccomas")
   
   # List file extensions that you'd like to cover
   files = list.files(pattern= "*.xls")# Get the files names for extension j
   
   # for those files, go through, clean up headings and compile them
   for  (i in c(1:length(files))) {
     
         # load in survey FILEi, name it processingtable:
         processingtable <- read_excel(files[i])
         
         # add a column for datasource and populate with SOURCEi
         processingtable$datasourcemv <- rep("mccomas", length(processingtable[,1]))
         
         # add a column for lake name
         processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
         
         # add a column for date YEAR-MO-DA (version 2)
         processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-",gsub(" ","-", file_path_sans_ext(files[i]))),c(-1,-3,-2), sep = "-"), collapse = "-"), "%y-%m-%d")), 
                                       length(processingtable[,1]))
         
         # clean up fieldnames
         names(processingtable) <- tidyName(names(processingtable))
         
         # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
         processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
         
         # name columns with no headings. If all are named, print "no missing names for:"
         ifelse(sum(names(processingtable) == "") > 0 ,
                processingtable <- tbl_df(data.frame(processingtable)),
                print("no missing names for:"))
         
         # make all columns into factors before compiling
         processingtable[] <- lapply(processingtable[], factor)
         
         #save that into the big set
         ps <- bind_rows(processingtable, ps)
         
         # print iteration # and survey name
         print(c(i,files[i]))
        }
 
 #' Check for input 
     # (293 prior, 112 in mccomas = 405)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
 
###### 
 
#' Newman Surveys:
#####
#' For these data in Excel:use kutools to combine multirow headers, delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l notes for mods done in excel:
#' Used the GIS data tabs when available, which allowed me to avoid combining header rows and deleting extra rows.
#' Lucy:  unknown "?,?" lake, date survey not included
#' RMN Misc: in "RMN Misc" files with _DNR labels were all locked, and I was unable to import them
#'           AV June Samples 2009 1 - Blueberry had no date, set to 1_1_2009; 
#'           set surveys with month but no day to 15th of month. 
#'           where no date metadata was in the file, I set the date in new file name as the date in the file name
#'           

     # set working Dir to sourcefile loc:
     setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/newman")
     
     # List file extensions that you'd like to cover
     files = list.files(pattern= "*.xls")# Get the files names for extension j
     
     # for those files, go through, clean up headings and compile them
     for  (i in c(1:length(files))) {
            
               # load in survey FILEi, name it processingtable:
               processingtable <- read_excel(files[i])
               
               # add a column for datasource and populate with SOURCEi
               processingtable$datasourcemv <- rep("newman", length(processingtable[,1]))
               
               # add a column for lake name
               processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
               
               # add a column for date YEAR-MO-DA (version 2)
               processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-",gsub(" ","-", file_path_sans_ext(files[i]))),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                             length(processingtable[,1]))
               
               # clean up fieldnames
               names(processingtable) <- tidyName(names(processingtable))
               
               # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
               processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
               
               # name columns with no headings. If all are named, print "no missing names for:"
               ifelse(sum(names(processingtable) == "") > 0 ,
                      processingtable <- tbl_df(data.frame(processingtable)),
                      print("no missing names for:"))
               
               # make all columns into factors before compiling
               processingtable[] <- lapply(processingtable[], factor)
               
               #save that into the big set
               ps <- bind_rows(processingtable, ps)
               
               # print iteration # and survey name
               print(c(i,files[i]))
             }
     
     #' Check for input 
     # (405 prior, 121 in newman = 526)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   

#####

#' SBlood Surveys:
#####
#' For these data in Excel: delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)

     # set working Dir to sourcefile loc:
     setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/sblood")
     
     # List file extensions that you'd like to cover
     files = list.files(pattern= "*.xls")# Get the files names for extension j
     
     # for those files, go through, clean up headings and compile them
     for  (i in c(1:length(files))) {
       
               # load in survey FILEi, name it processingtable:
               processingtable <- read_excel(files[i])
               
               # add a column for datasource and populate with SOURCEi
               processingtable$datasourcemv <- rep("sblood", length(processingtable[,1]))
               
               # add a column for lake name
               processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
               
               # add a column for date YEAR-MO-DA (version 2)
               processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-",gsub(" ","-", file_path_sans_ext(files[i]))),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                             length(processingtable[,1]))
               
               # clean up fieldnames
               names(processingtable) <- tidyName(names(processingtable))
               
               # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
               processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
               
               # name columns with no headings. If all are named, print "no missing names for:"
               ifelse(sum(names(processingtable) == "") > 0 ,
                      processingtable <- tbl_df(data.frame(processingtable)),
                      print("no missing names for:"))
               
               # make all columns into factors before compiling
               processingtable[] <- lapply(processingtable[], factor)
               
               #save that into the big set
               ps <- bind_rows(processingtable, ps)
               
               # print iteration # and survey name
               print(c(i,files[i]))
             }
     
     #' Check for input 
     # (526 prior, 30 in sblood = 556)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   
     
     #####     
          
#' CRWD Surveys:
#####
#' For these data in Excel:delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l notes for mods done in excel:
#'           2012 are first years with excel data

   # set working Dir to sourcefile loc:
   setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis/clp_surveys/crwd")
   
   # List file extensions that you'd like to cover
   files = list.files(pattern= "*.xls")# Get the files names for extension j
   
   # for those files, go through, clean up headings and compile them
   for  (i in c(1:length(files))) {
          
           # load in survey FILEi, name it processingtable:
           processingtable <- read_excel(files[i])
           
           # add a column for datasource and populate with SOURCEi
           processingtable$datasourcemv <- rep("crwd", length(processingtable[,1]))
           
           # add a column for lake name
           processingtable$lknamemv <- rep(tolower(first.word(files[i])), length(processingtable[,1]))
           
           # add a column for date YEAR-MO-DA (version 2)
           processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-",gsub(" ","-", file_path_sans_ext(files[i]))),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                         length(processingtable[,1]))
           
           # clean up fieldnames
           names(processingtable) <- tidyName(names(processingtable))
           
           # drop empty columns (here we make a new object of only the columns with less NAs than the total column length of the table)
           processingtable <- processingtable[,colSums(is.na(processingtable))<nrow(processingtable)]
           
           # name columns with no headings. If all are named, print "no missing names for:"
           ifelse(sum(names(processingtable) == "") > 0 ,
                  processingtable <- tbl_df(data.frame(processingtable)),
                  print("no missing names for:"))
                  
           # make all columns into factors before compiling
           processingtable[] <- lapply(processingtable[], factor)
           
           #save that into the big set
           ps <- bind_rows(processingtable, ps)
           
           # print iteration # and survey name
           print(c(i,files[i]))
         }
     
     #' Check for input 
     # (556 prior, 19 in CRWD = 575)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   
     
     #####     

     
#' ## Check out product, save progress, and reset working directory
#' 

# Set working directory back to project location
setwd("C:/Users/Mike Verhoeven/Google Drive/Documents/UMN/Grad School/Larkin Lab/Curlyleaf_retro_analysis")  

#what is the size of the dataset?
print(c(ncol(ps),nrow(ps))) 

# convert all columns back to factors (made into character when using compile.rows function) this allows me to cruze the str(ps) to see what each column consists of
ps[] <- lapply(ps[], factor)

# print one example of what i mean ^^ 
names(ps)
str(ps[,"datasourcemv"]) 
unique(ps[,"datasourcemv"])
# check to see that the number of NAs in every column is less than the length of the column if column is all NAs, tell me the name of the column
names(ps[,which(as.vector(colSums(is.na(ps)) == nrow(ps)))])

# save progress as a .csv file in the clp_surveys folder  
write.csv(ps, file = "data/output_data/state_clp_comp.csv", row.names = F)    

   

#' ## Document footer 
#' 
#' Document spun with: ezspin(file = "scripts/a_compiling_survey_data.R", out_dir = "html_outputs/compiling survey data", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
