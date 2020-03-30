#'---
#' title: "Compiling stakeholder PI data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' 30 March 2020

#' This script will compile PI data for a bunch of different surveyors who have
#' submitted data.The script pulls data (points in rows, species in columns) in
#' and merges columns together where names match. Where a new column name comes 
#' in with a dataset, that column is retained. In this way, all submitted 
#' surveys are assembled into a single dataset. 

# load libraries ------------------------------------------------------------------
#' ## Document Preamble
#+ warning = FALSE

  # Load Libraries
  library(readxl)
  library(Hmisc)
  library(stringr)
  library(dplyr)
  library(tools)
  library(data.table)
  library(xtable)
  library(tidyr)
  library(devtools)


# assign custom functions -------------------------------------------------

  
#' ### Assign Custom Functions
#'   
#'Function for cleaning fieldnames. Edited from Dan Larkin's original version.  Makes spaces, periods dashes, fwd slashes and parentheses into underscore. 
  
  #tidyName()
  tidyName <- function(x) tolower(gsub(" ", "_", trimws(gsub("__", " ",gsub("  ", " ", gsub(".", " ", gsub("(", " ", gsub(")", " ", gsub("-", " ", gsub("/", " ", gsub("?", " ", 
                                                          x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), "both"), fixed = TRUE))
  
  # for removing NA's
  na.remove <-  function (x) {gsub("NA", replacement = "", 
                                   gsub("NA,", replacement = "", x))}
  # extra commas from the above unite function
  TrimMult <- function(x, char=" ") {return(gsub(paste0("^", char, "*|(?<=", char, ")", char, "|", char, "*$"),"", x, perl=T))}



# CLP - Brasch ------------------------------------------------------------
  
#' "ps" (point scale) will be our dataset. Let's fill er' up!

ps <- data.table(NULL)
  
  #' ## Brasch Surveys:

  # set working Dir to sourcefile loc:
  setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/brasch")
 
  # List file extensions that you'd like to cover
  files = list.files(pattern= "*.xls")# Get the files names for extension j

  # for those files, go through, clean up headings and compile them
      for  (i in c(1:length(files))) {
       # i = 13
        # load in survey FILEi, name it processingtable:
        processingtable <- read_excel(files[i], trim_ws = T)
        
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
  
  # brasch has 88 surveys, 88 unique combinations of data source, lake, survey date.
  nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))




# CLP - Dustin ------------------------------------------------------------
     
#' ### Dustin Surveys:

#' For these data I used a Kutools for Excel file conversion tool to correct the file extensions
#' because the read excel function didn't like the way they were written (I think that the file 
#' format didnt match the file extension label). So you'll see that the files are pulled from a
#' subfolder "converted."
     
     # set working Dir to sourcefile loc:
  setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/dustin/converted")
     
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
      # (88 from brasch,  104 from dustin should give 192 total)
      nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))



# CLP- Fieldseth ----------------------------------------------------------

      
      
#' ### Fieldseth Surveys:

#' For these data I used a Kutools for Excel workbook splitting tool to split tabs from workbooks into separate 
#' .xlsx files. I also went through the surveys and deleted a header row from them to get the fieldnames into row # 1. 
#' Only the Excel data from the "CLP_Fieldseth" folder were used (some survey data remained in pdf or word doc form).
#' Only the modified files are included in the "fieldseth" folder.
#' 

     # set working Dir to sourcefile loc:
      setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/fieldseth/")
     
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
    # (192 prior, 12 in fieldseth = 204)
    nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))




# CLP - Johnson -----------------------------------------------------------

    
#' Johnson Surveys:

#' For these data in Excel: collapse fieldnamess into single cell, delete header rows & sub fieldname rows, delete extra rows (populated with zeros), delete extra columns
#'  (populated with excess calculations), name tabs as lake date, export tabs with Kutools for Excel workbook splitting tool
#'      
     # set working Dir to sourcefile loc:
    setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/johnson/")
     
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
      # (204 prior, 85 in johnson = 289)
      nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
     
 

      

# CLP -  Lund -------------------------------------------------------------

      
#' Lund Surveys:

 #' For these data in Excel:delete header rows, delete extra rows (populated with zeros), delete extra columns
 #'  (populated with excess calculations), name tabs as "lake m-d-year", export tabs with Kutools for Excel workbook splitting tool
 #'      
 # set working Dir to sourcefile loc:
      setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/lund/")
 
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
   # (289 prior, 3 in lund = 292)
   nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
 
      

   

# CLP - McComas -----------------------------------------------------------

   
#' McComas Surveys:

#' For these data in Excel:use kutools to combine multirow headers, delete extra header rows, delete non-data rows, delete extra columns,
#' name tabs as "lake m-d-year", rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l mods done in excel:
#' Gleason:  split into N. and S. Basin because of data point i.d. duplication (manually copy N basin to new tab)
#'           rename "clp stems1,2,3..." (this is the only CLP data in 4-21-15) which I belive represent quadrat data?
#' Long:     deleted some oddly place and incomplete data in 6_23_13 survey tab (seems to not be from this lake (points don't match))
#' Round:    most years in transect form... yikes, 2006 data has no survey date (set to 1_1_06)  
 
   # set working Dir to sourcefile loc:
   setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/mccomas/")
   
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
     # (292 prior, 112 in mccomas = 404)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
 
 
 
     

# CLP - Newman ------------------------------------------------------------

     
#' Newman Surveys:

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
     setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/newman/")
     
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
     # (404 prior, 121 in newman = 525)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   



     

# CLP - SBlood ------------------------------------------------------------

     
#' SBlood Surveys:

#' For these data in Excel: delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)

     # set working Dir to sourcefile loc:
     setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/sblood/")
     
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
     # (525 prior, 30 in sblood = 555)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   
     
    

     

# CLP - CRWD --------------------------------------------------------------

     
#' CRWD Surveys:

#' For these data in Excel:delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l notes for mods done in excel:
#'           2012 are first years with excel data

   # set working Dir to sourcefile loc:
     setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/crwd/")
   
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
     # (555 prior, 19 in CRWD = 574)
     nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))   
     
    


     
     
# Review and output CLP data ----------------------------------------------
#' ## Next Steps:
#' So at this point, we will want to add DOW numbers to the lake IDs. We also 
#' need to process these data according to how the CLP dataset was processed.  
  
#' Check out product, save progress, and reset working directory
#' 

# Set working directory back to project location
     setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")  

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



# progress checkpoint -----------------------------------------------------


# save progress as a .csv file in output data folder  
# write.csv(ps, file = "G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/output/clp_proj_surveys.csv", row.names = F)    
# Set working directory back to project location
# setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")  
# ps <- fread(file = "data/output/clp_proj_surveys.csv")



# add DOWs to CLP lakes ---------------------------------------------------


#' ## add the DOW ids to these lakes:
#' Adam Kautza had compiled the lake Info for these lakes in the CLP project
#' 

AKdat <- fread(file = "data/input/pcrispus_wDNRtrtdat.csv", colClasses=c(Lake.ID="character"))
AKdat[ ,.N, by = .(Lake.ID,Lake.x) ]
ps[ ,.N, by = .(lknamemv) ]

cbind(AKdat[ ,.N, by = .(Lake.ID,Lake.x) ],
      ps[ ,.N, by = .(lknamemv) ])

sort(unique(AKdat$Lake.x))
sort(unique(ps$lknamemv))

# what lakes in ps set are unmatched?
A <- sort(unique(AKdat$Lake.x))
B <- sort(unique(ps$lknamemv))
B[is.na(match(B,A))==T]


# fix lake names 

#' Clear lakes (2) each from dif datasource:
ps[lknamemv=="clear", c("lknamemv", "datasourcemv" )]
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
ps[lknamemv=="crosbyupper", lknamemv := "uppercrosby"]
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
ps[lknamemv=="crosbylower", lknamemv:= "bigcrosby"]
AKdat[Lake.x== "littlecrosby", Lake.x := "bigcrosby"]
AKdat[Lake.x== "uppercrosby", Lake.ID := "62022500"]
ps[lknamemv=="crook", lknamemv:="crookneck"]

# what lakes in Adams set are unmatched?
A <- sort(unique(AKdat$Lake.x))
B <- sort(unique(ps$lknamemv))
B[is.na(match(B,A))==T]

#' We'll grab these dow numbers manually:
# make DOW table from Adam's data and add in the missing DOWs:
DOW <- AKdat[ ,.N, by = .(Lake.ID,Lake.x) ]
missingdow <- data.table(Lake.x = B[is.na(match(B,A))==T], 
                         Lake.ID = c("10001200", "06000200", "27009501", "10000600", "02000300", "18038600", "27013322", "27013338"), N = NA)

DOWIds <- rbind(DOW, missingdow)

merge(head(ps), DOWIds, by.x = "lknamemv", by.y = "Lake.x")

match(ps$lknamemv, DOWIds$Lake.x)
dows <- DOWIds[match(ps[,lknamemv,], DOWIds[,Lake.x,]),Lake.ID]

ps[ , dowid := dows  ,  ]

unique(ps$dowid)


# new datasets ----------------------------------------

ps <- tbl_df(ps)
ps[] <- lapply(ps[], factor)

# 2018 - Allison Gamble ----------------------------------------------------


#' ## Gamble Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Allison_Gamble")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 40
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Allison Gamble", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 120 surveys:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))


# 2018 - Andrea Prichard ----------------------------------------------------


#' ## Andrea Prichard Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/andrea_prichard")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 13
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Andrea Prichard", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -3,  sep = "_")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(gsub("_","-", file_path_sans_ext(files[i])),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")),length(processingtable[,1]))
  
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

#' Check for input 683+51 = 734:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - April Londo ----------------------------------------------------


#' ## DNR R3S Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/April_Londo")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("April Londo", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 734+98=832:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Britta Belden ----------------------------------------------------


#' ## CRWD Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Britta Belden")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Britta Belden", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 832+9=841:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Cole Loewen ----------------------------------------------------


#' ## Cole Loewen Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Cole Loewen")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Cole Loewen", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 841+3=844:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Eric Fieldseth ----------------------------------------------------


#' ## Eric Fieldseth Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Eric Fieldseth")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 7
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Eric Fieldseth", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 844+8=852:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - James Johnson ----------------------------------------------------


#' ## Freshwater Scientific Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/James Johnson")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("James Johnson", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 852+85=937 :

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))



# 2018 - Jill Sweet ----------------------------------------------------


#' ## MCWD Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Jill Sweet")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Jill Sweet", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 937+102:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))


# 2018 - Mark Ranweiler ----------------------------------------------------


#' ## MNDNR R2 N Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Mark Ranweiler")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Mark Ranweiler", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 1039+17=1056:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Matt Berg ----------------------------------------------------


#' ## Endangered Resource Services LLC Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Matt Berg")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Matt Berg", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 1056+7=1063:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Meg Rattei ----------------------------------------------------


#' ## Barr Engineering Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Meg Rattei")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Meg Rattei", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 1063+63:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - Rob Brown ----------------------------------------------------


#' ## Minneapolis Parks Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Rob Brown")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Rob Brown", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), length(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
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

#' Check for input 1126+9=1135:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))



# progress checkpoint -----------------------------------------------------


# save progress as a .csv file in output data folder  
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")
# write.csv(ps, file = "data/output/clp_2018_surveys.csv", row.names = F)    
# Set working directory back to project location
# ps <- fread(file = "data/output/clp_2018_surveys.csv")

# append DOW numbers -------------------------------------------------


dow18 <- fread(file = "data/input/Data Import Progress - 2018 Entry.csv")
str(dow18)

# validate matches on survey contributor names -- Greg Graske surveys were dropped for no dates
dow18[ , survey_contributor := as.factor(survey_contributor),]
levels(dow18$survey_contributor)
unique(ps$datasourcemv)
match( levels(dow18$survey_contributor), unique(ps$datasourcemv))

# clean up dates
dow18[ , "survey_date(m-d-yyyy)" := as.Date(`survey_date(m-d-yyyy)`, "%m-%d-%Y" ),  ]
dow18[ , .(`survey_date(m-d-yyyy)`), ]
names(dow18)[5] <- "survey_date"

#' we want to check lake names matches.
sort(unique(dow18$survey_lake))
dow18[ , survey_lake  := tolower(survey_lake),]
# clean up lake names to improve matches
dow18[, survey_lake:= gsub("lake", "", survey_lake) , ]
dow18[ , survey_lake := trimws(survey_lake, which = "both")]

sort(unique(ps$lknamemv))
ps[ ,lknamemv := gsub("lake", "", lknamemv), ]
ps[ ,lknamemv := trimws(lknamemv, which = "both"), ]

#bald eagle has a bad dow--
dow18[survey_lake == "bald eagle", survey_dow:="62000200"]
#kesler should be keslers
dow18[survey_lake == "kesler pond", survey_lake:="keslers pond"]
#louisa
dow18[survey_lake == "lousia", survey_lake := "louisa"]
dow18[survey_lake == "n. lundstern", survey_lake := "n. lundsten"]
dow18[survey_lake == "roemhildts", survey_lake := "roemhildt"]
ps[lknamemv == "roemhildts", lknamemv := "roemhildt"]
dow18[survey_lake == "saunders- west", survey_lake := "saunders-west"]
ps[lknamemv == "wassermann", lknamemv := "wasserman"]
dow18[survey_lake=="wolfeld", survey_lake := "wolsfeld"]

# what lakes in ps are unlabelled with dows
sort(unique(ps[is.na(dowid) == T, lknamemv,]))
sort(unique(dow18$survey_lake)) #any matches in dow18 set

matched <- match(sort(unique(ps[is.na(dowid) == T, lknamemv,])),
      sort(unique(dow18$survey_lake))
      )
#need to nab these lake dows manually:
sort(unique(ps[is.na(dowid) == T, lknamemv,]))[is.na(matched)]

#coal and crookneck are not in 2018 data but are johnson surveys
ps[lknamemv =="coal" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="coal" , dowid := 77004600]
ps[lknamemv =="crookneck" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="crookneck" , dowid := 49013300]
#julia?
ps[lknamemv =="julia" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="julia" , dowid := 71014500]
#long
ps[lknamemv == "long mahtomedi", lknamemv:= "long"]
#lowermission
ps[lknamemv =="lowermission" , dowid := 18024300]
#tonka these names are skate-y so I'll punch them out manually rather than rely on the dows put together by the techs (no offense, techs!):
ps[lknamemv == "minnetonka grays bay" | lknamemv== "minnetonka north arm" |
     lknamemv == "grays" | lknamemv == "northarm", .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv == "minnetonka grays bay" |
     lknamemv == "grays" , dowid := 27013301]
ps[lknamemv == "minnetonka north arm" |
     lknamemv == "northarm" , dowid := 27013318]
#rush
ps[lknamemv=="rush", dowid := 71014700]
#vails?
ps[lknamemv =="vails" ,dowid:= 73015100]
#weaver
ps[lknamemv =="weaver" , dowid := 27011700]

#all issues solved:
matched <- match(sort(unique(ps[is.na(dowid) == T, lknamemv,])),
                 sort(unique(dow18$survey_lake))
)
sort(unique(ps[is.na(dowid) == T, lknamemv,]))[is.na(matched)]



#merge dow18 do vals into ps

str(dow18)
str(ps[ , .("lknamemv", "datasourcemv","datemv",dowid)])

dow18[ , datemv:=as.character(survey_date)]
dow18[ , lknamemv := as.character(survey_lake)]
dow18[ , datasourcemv := as.character(survey_contributor)]

#drop dow18 lines incomplete for these 4
dow18 <- dow18[ , .(datemv, lknamemv, datasourcemv, survey_dow) ,]
dow18[ , survey_dow:=as.integer(survey_dow)]
dow18 <- dow18[complete.cases(dow18), , ]

# ps <- merge( ps, dow18, by = c("lknamemv", "datasourcemv","datemv"), all.x = T)
# copy <- ps
# dt version:
setkeyv(ps, c("lknamemv", "datasourcemv","datemv") )
setkeyv(dow18, c("lknamemv", "datasourcemv","datemv"))

ps[dow18 , dow18:= i.survey_dow]

summary(as.integer(ps$dow18))

 #now check coverage and drop dows from dow18 into the dow column then delete extra cols
summary(ps[ , .(dowid, dow18) , ])
ps[is.na(dowid), .N , c("lknamemv", "datasourcemv","datemv")]

#assign dow to the dowid column
ps[is.na(dowid) == T & is.na(dow18)==F , dowid := dow18, ]

# add in any missing dows manually ----------------------------------------

#fix the last few manually
ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)]

#benton
ps[lknamemv=="benton", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="benton", dowid:= 41004300]
#big marine
ps[lknamemv=="big marine", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="big marine", dowid:= 82005200]
#big sob
ps[lknamemv=="big sob", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="big sob", dowid:= 27009999]# big sob does not have a dow that I can find
#brownie
ps[lknamemv=="brownie", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="brownie", dowid:= 27003800]
#calhoun
ps[lknamemv=="calhoun", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="calhoun", dowid:= 27003100]
#cedar
ps[lknamemv=="cedar", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="cedar" & datasourcemv == "Allison Gamble" , dowid:= 66005200]
ps[lknamemv=="cedar" & datasourcemv == "Allison Gamble" , datemv:= "2014-07-17"]
ps[lknamemv=="cedar" & datasourcemv == "Rob Brown" , dowid:= 27003900]
#christmas
ps[lknamemv=="christmas", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="christmas", dowid := 27013700]
#fish
ps[lknamemv=="fish", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="fish" & datasourcemv == "James Johnson", dowid := 70006900]
#harriet
ps[lknamemv=="harriet", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="harriet" , dowid := 27001600]
#island
ps[lknamemv=="island", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="island" , dowid := 62007500]
ps[lknamemv=="island" & is.na(datemv)==T , datemv := "2010-05-17"]
#isles
ps[lknamemv=="isles", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="isles" , dowid := 27004000]
#loring
ps[lknamemv=="loring", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="loring" , dowid := 27065500]
#medicine
ps[lknamemv=="medicine", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="medicine" , dowid := 27010400]
#rebecca
ps[lknamemv=="rebecca", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="rebecca" , dowid := 27019200]
#sakatah bay
ps[lknamemv=="sakatah bay", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="sakatah bay" , dowid := 40000201]
#wirth
ps[lknamemv=="wirth", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="wirth" , dowid := 27003700]

# any surveys missing dows?
ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)]



# progress checkpoint -----------------------------------------------------
# save progress as a .csv file in output data folder  
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")
# write.csv(ps, file = "data/output/clp_2018dow_surveys.csv", row.names = F)    
# Set working directory back to project location
# ps <- fread(file = "data/output/clp_2018dow_surveys.csv")

# 2019 datasets -----------------------------------------------------------

ps <- tbl_df(ps)
ps[] <- lapply(ps[], factor)

# 2019 - Allison Gamble ----------------------------------------------------


#' ## DNR R4 AIS Surveys:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Allison Gamble")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Allison Gamble_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 1145+13=1158:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))



# 2019 - Meg Rattei ----------------------------------------------------


#' ## Barr Engineering Surveys:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Barr Engineering")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Meg Rattei_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 1158 + 25 :1183

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))


#' ## Freshwater Scientific Surveys:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Freshwater Scientific")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 31
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("James Johnson", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 1183

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2019 - James Johnson ----------------------------------------------------


#' ## Freshwater Scientific Surveys:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Freshwater Scientific")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 8
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("James Johnson_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 1158 + 25 :1183

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2019 - Jill Sweet ----------------------------------------------------

#' ## Minnehaha Creek Watershed Dist:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Minnehaha Creek Watershed District")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 31
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Jill Sweet_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 
nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
# 2019 - April Londo ----------------------------------------------------

#' ## MN DNR R3S:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/MN DNR R3 South/R3 South/R3 South")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 31
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("April Londo_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input
nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))
# 2019 - Andrea Prichard ----------------------------------------------------

#' ## Ramsey Co. Envrionmental:

# set working Dir to sourcefile loc:
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2019_submissions/Ramsey Environmental")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 3
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Andrea Prichard_19", nrow(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], end = -2,  sep = " ")), nrow(processingtable[,1]))
  
  # add a column for date YEAR-MO-DA (version 2)
  processingtable$datemv <- rep(as.character(as.Date(paste(word(word(file_path_sans_ext(files[i]), -1),c(-1,-3,-2), sep = "-"), collapse = "-"), "%Y-%m-%d")), 
                                nrow(processingtable[,1]))
  
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

#' Check for input 1183 + 123 = 1306 (so this inclusion duplicated some previous data)

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))


















# progress checkpoint -----------------------------------------------------
# save progress as a .csv file in output data folder  
setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")
# write.csv(ps, file = "data/output/clp_2018dow_2019_surveys.csv", row.names = F)    
# Set working directory back to project location
# ps <- fread(file = "data/output/clp_2018dow_2019_surveys.csv")

# append DOW numbers -------------------------------------------------


dow19 <- fread(file = "data/input/Data Import Progress - 2019 Entry.csv")
str(dow19)

# validate matches on survey contributor names
dow19[ , survey_contributor:=  as.factor(paste(survey_contributor,"19",sep = "_")),]
levels(dow19$survey_contributor)
unique(ps[ , datasourcemv,])
match( unique(dow19$survey_contributor), unique(ps$datasourcemv))
dow19 <- dow19[survey_contributor!= "_19"] #drop lines that had no labeled datsource

# clean up dates
dow19[ , "survey_date(m-d-yyyy)" := as.Date(`survey_date(m-d-yyyy)`, "%m-%d-%Y" ),  ]
dow19[ , .(`survey_date(m-d-yyyy)`), ]
names(dow19)[5] <- "survey_date"

dow19 <- dow19[is.na(survey_date)==F] #keep only rows with dates
dow19[, survey_dow:=as.integer(survey_dow)]
dow19 <- dow19[is.na(survey_dow)==F] #keep only rows with dows (why retain a row if it doesn't have the data we are fishing for?)


#' we want to check lake names matches.
sort(unique(dow19$survey_lake))
dow19[ , survey_lake  := tolower(survey_lake),]
# clean up lake names to improve matches
dow19[, survey_lake:= gsub("lake", "", survey_lake) , ]
dow19[ , survey_lake := trimws(survey_lake, which = "both")]

sort(unique(ps$lknamemv))
ps[ ,lknamemv := gsub("lake", "", lknamemv), ]
ps[ ,lknamemv := trimws(lknamemv, which = "both"), ]

# what lakes in ps are unlabelled with dows
sort(unique(ps[is.na(dowid) == T, lknamemv,]))
sort(unique(dow19$survey_lake)) #any matches in dow19 set

matched <- match(sort(unique(ps[is.na(dowid) == T, lknamemv,])),
                 sort(unique(dow19$survey_lake))
)


#need to nab these lake dows manually:
sort(unique(ps[is.na(dowid) == T, lknamemv,]))[is.na(matched)]

ps[datasourcemv =="April Londo_19" , .N,.(lknamemv,datemv,datasourcemv, dowid)] #a bunch of the R3 surveys are incorrect b/c natalie did not transpose them 
ps <- ps[datasourcemv !="April Londo_19" , , ]

matched <- match(sort(unique(ps[is.na(dowid) == T, lknamemv,])),
                 sort(unique(dow19$survey_lake))
)
#need to nab these lake dows manually:
needDOW <- sort(unique(ps[is.na(dowid) == T, lknamemv,]))[is.na(matched)]

#east auburn
ps[lknamemv =="east auburn" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="east auburn" , dowid := 10004402]
#"east phelps bay"
ps[lknamemv =="east phelps bay" | lknamemv == "phelps bay" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="east phelps bay" | lknamemv == "phelps bay" , dowid := 27013300]
#east battle lk
ps[lknamemv =="eastbattle" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="eastbattle" , dowid := 56013800]
#"gideons bay"
ps[lknamemv =="gideons bay" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="gideons bay"  , dowid := 27013331]
#more minnetonka
ps[lknamemv == "grays bay" | lknamemv== "stalbansbay" | lknamemv == "northarmbay", .N,.(lknamemv,datemv,datasourcemv, dowid)]
#grays
ps[lknamemv == "grays bay", dowid := 27013301]
ps[lknamemv == "grays bay", lknamemv := "grays"]
#stalbans
ps[lknamemv == "stalbansbay", dowid := 27013304]
ps[lknamemv == "stalbansbay", lknamemv := "st albans"]
#northarm
ps[lknamemv == "northarmbay", dowid := 27013313]
ps[lknamemv == "northarmbay", lknamemv := "north arm"]
#"grey cloud slough"
ps[lknamemv =="grey cloud slough" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="grey cloud slough" , dowid := 19000500]
#""marsh""
ps[lknamemv =="marsh" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="marsh" , dowid := 10005400]
#"mud"
ps[lknamemv =="mud" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="mud" , dowid := 27018600]
#"north lundsten"
ps[lknamemv =="north lundsten" | lknamemv == "n. lundsten", .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="north lundsten" , dowid := 10004300]
#"parley"
ps[lknamemv =="parley" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="parley" , dowid := 10004200]
#pulaski
ps[lknamemv =="pulaski" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="pulaski" , dowid := 	86005300]
#ricemarsh
ps[lknamemv =="ricemarsh" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv =="ricemarsh" , dowid := 	10000100]
#silverwest
ps[lknamemv =="silver" |lknamemv=="silver west" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="silver west" , dowid := 62008300 ]
#snail
ps[lknamemv =="snail" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="snail" , dowid := 62007300 ]
#"south lundsten"
ps[lknamemv =="south lundsten" | lknamemv == "s. lundsten" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="south lundsten" , dowid := 10004300 ]
#steiger
ps[lknamemv =="steiger" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="steiger" , dowid := 10004500 ]
#sunny
ps[lknamemv =="sunny" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="sunny" , dowid := 10004100 ]
#sweeney
ps[lknamemv =="sweeney" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="sweeney" , dowid := 27003501 ]
#turbid
ps[lknamemv =="turbid" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="turbid" , dowid := 10005100 ]
#"wass pond west"
ps[lknamemv =="wass pond west" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="wass pond west" , dowid := 10004802 ]
#"wasseramann"
ps[lknamemv =="wasseramann" | lknamemv == "wasserman", .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="wasseramann" , dowid := 10004800 ]
ps[lknamemv=="wasseramann", lknamemv := "wasserman"]
#west auburn
ps[lknamemv =="west auburn" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="west auburn" , dowid := 10004401 ]
#white bear
ps[lknamemv =="whitebear" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="whitebear" , dowid := 82016700 ]
#zumbra
ps[lknamemv =="zumbra" , .N,.(lknamemv,datemv,datasourcemv, dowid)]
ps[lknamemv=="zumbra" , dowid := 10004100 ]

#' At this point, ever lake should have either a match or at least one or two named surveys following that match.
#' 
#' 
matched <- match(sort(unique(ps[is.na(dowid) == T, lknamemv,])),
                 sort(unique(dow19$survey_lake))
)
#need to nab these lake dows manually:
sort(unique(ps[is.na(dowid) == T, lknamemv,]))[is.na(matched)]
sort(unique(ps$lknamemv))
#all issues solved.

#merge dow18 do vals into ps

str(dow19)
str(ps[ , .("lknamemv", "datasourcemv","datemv",dowid)])

dow19[ , datemv:=as.character(survey_date)]
dow19[ , lknamemv := as.character(survey_lake)]
dow19[ , datasourcemv := as.character(survey_contributor)]

#drop dow19 lines incomplete for these 4
dow19 <- dow19[ , .(datemv, lknamemv, datasourcemv, survey_dow) ,]
dow19[ , survey_dow:=as.integer(survey_dow)]
dow19 <- dow19[complete.cases(dow19), , ]

# dt version:
setkeyv(ps, c("lknamemv", "datasourcemv","datemv") )
setkeyv(dow19, c("lknamemv", "datasourcemv","datemv"))

ps[dow19 , dow19:= i.survey_dow]

summary(as.integer(ps$dow19))

#now check coverage and drop dows from dow18 into the dow column then delete extra cols
summary(ps[ , .(dowid, dow19) , ])
ps[is.na(dowid), .N , c("lknamemv", "datasourcemv","datemv")]

#assign dow to the dowid column
ps[is.na(dowid) == T & is.na(dow19)==F , dowid := dow19, ]
summary(ps[ , .(dowid, dow19) , ])

# add in any missing dows manually ----------------------------------------

#fix the last few manually
ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)]

sort(unique(ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)][,lknamemv,]))

#ann
ps[lknamemv=="ann", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="ann", dowid:= 10001200]
ps[lknamemv=="armstrong", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="armstrong", dowid:= 82011600]
ps[lknamemv=="bass", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="bass" & datasourcemv != "Allison Gamble", dowid:= 86023400]
ps[lknamemv=="beebe", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="beebe", dowid:= 86002300]
ps[lknamemv=="brownie", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="brownie", dowid:= 27003800]
ps[lknamemv=="calhoun", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="calhoun", dowid:= 27003100]
ps[lknamemv=="carsons bay", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="carsons bay", dowid:= 27013303]
ps[lknamemv=="cedar", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="cedar" & datemv == "2016-08-01" , dowid:= 86022700]
ps[lknamemv=="cedar" & is.na(dowid) == T , dowid := 27003900]
ps[lknamemv=="christmas", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="christmas", dowid := 27013700]
ps[lknamemv=="colby", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="colby" , dowid := 82009400]
ps[lknamemv=="crooked", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="crooked" , dowid := 2008400]
ps[lknamemv=="demontreville", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="demontreville" , dowid := 82010100]
ps[lknamemv=="duck", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="duck" , dowid := 27006900]
ps[lknamemv=="eagle", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="eagle" , dowid := 27011101]
ps[lknamemv=="elmo", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="elmo" , dowid := 82010600]
ps[lknamemv=="harriet", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="harriet" , dowid := 27001600]
ps[lknamemv=="howard", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="howard" , dowid := 86019900]
ps[lknamemv=="idlewild", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="idlewild" , dowid := 27007400]
ps[lknamemv=="isles", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="isles" , dowid := 27004000]
ps[lknamemv=="jane", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="jane" , dowid := 82010400]
ps[lknamemv=="john", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="john" , dowid := 86028800]
ps[lknamemv=="la", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="la" , dowid := 82009700]
ps[lknamemv=="long", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="long" & datasourcemv =="James Johnson" , dowid := 29016100]
ps[lknamemv=="long" & datasourcemv =="Meg Rattei_19" , dowid := 82011800]
ps[lknamemv=="lost", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="lost" , dowid := 82013400]
ps[lknamemv=="lotus", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="lotus" , dowid := 10000600]
ps[lknamemv=="louisa", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="louisa" , dowid := 86028200]
ps[lknamemv=="maple", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="maple" , dowid := 86013401]
ps[lknamemv=="marie", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="marie" , dowid := 73001400]
ps[lknamemv=="markgrafs", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="markgrafs" , dowid := 82008900]
ps[lknamemv=="martha", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="martha" , dowid := 86000900]
ps[lknamemv=="maxwell", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="maxwell" , dowid := 27013320]
ps[lknamemv=="olson", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="olson" , dowid := 82010300]
ps[lknamemv=="pike", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="pike" , dowid := 27011100]
ps[lknamemv=="platte", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="platte" , dowid := 18008800]
ps[lknamemv=="powers", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="powers" , dowid := 82009200]
ps[lknamemv=="ravine", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="ravine" , dowid := 82008700]
ps[lknamemv=="red rock", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="red rock" , dowid := 27007600]
ps[lknamemv=="rice", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="rice" , dowid := 27011600]
ps[lknamemv=="riley", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="riley" , dowid := 10000200]
ps[lknamemv=="rush", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="rush" , dowid := 71014700]
ps[lknamemv=="silver", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="silver" & datasourcemv == "Meg Rattei_19" , dowid := 62000100]
ps[lknamemv=="silver" & datasourcemv == "Meg Rattei_19" , datemv := "2018-07-29"]
ps[lknamemv=="silver" & datasourcemv == "James Johnson" , dowid := 27013600]
ps[lknamemv=="silver" & datasourcemv == "Andrea Prichard_19" , dowid := 62000100]
ps[lknamemv=="southeast anderson", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="southeast anderson" , dowid := 27006202]
ps[lknamemv=="stubbs", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="stubbs" , dowid := 27013319]
ps[lknamemv=="sullivan", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="sullivan" , dowid := 49001600]
ps[lknamemv=="susan", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="susan" , dowid := 10001300]
ps[lknamemv=="twin", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="twin" & datasourcemv == "Meg Rattei_19", dowid := 27003502]
ps[lknamemv=="weaver", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="weaver" , dowid := 27011700]
ps[lknamemv=="westwood", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="westwood" , dowid := 27071100]
ps[lknamemv=="wilmes", .N , .(lknamemv, datasourcemv, dowid, datemv)]
ps[lknamemv=="wilmes" , dowid := 82009000]

# any surveys missing dows?
ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)]
sort(unique(ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)][,lknamemv,]))

# progress checkpoint -----------------------------------------------------
# save progress as a .csv file in output data folder  
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")
# write.csv(ps, file = "data/output/clp_2018dow_2019dow_surveys.csv", row.names = F)    
# Set working directory back to project location
# ps <- fread(file = "data/output/clp_2018dow_2019dow_surveys.csv")

# drop duplicated surveys -------------------------------------------------


ps[ , .N , by = .(datemv, dowid)  ]

#create new, unique survey ID for each survey
ps[ , .N , by = .(datemv, dowid, datasourcemv)  ]
nrow(ps[ , .N , by = .(datemv, dowid, datasourcemv)  ])
ps[ , SURVEY_ID:= .GRP , by = .(datemv, dowid, datasourcemv)  ]

surveys <- ps[ , .N , by = .(datemv, dowid, datasourcemv, SURVEY_ID)  ]
# identify then drop duplicated rows
duplicated(surveys[ , .(datemv,dowid) ,])
surveys[duplicated(surveys[ , .(datemv,dowid) ,]), dup:= TRUE , ]
ids_to_drop <- surveys[ dup == T , SURVEY_ID,]

ps[!(SURVEY_ID %in% ids_to_drop), .N , .(datemv, dowid, datasourcemv, SURVEY_ID) ]
ps <- ps[!(SURVEY_ID %in% ids_to_drop), , ]

ps <- tbl_df(ps)
ps[] <- lapply(ps[], factor)

#drop columns with no data:

ps <- ps[,colSums(is.na(ps))<nrow(ps)]


# export old fieldnames ---------------------------------------------------


#' # Old Variable Names
#' 
#' Pull all of the variable names (field names) off of the dataset, also attaching the lake and datasource names to those to help me identify the survey that I need to look at to interpret names
#' 
#Unquote to re-export field names: 

#' fielduse <- data.frame()
#' print(Sys.time())
#'   start <- Sys.time()
#'   for (i in 1:ncol(ps)) { 
#'   
#'   # code for troubleshooting:
#'   #####
#'   # i =1
#'   # colnames(ps)[i] # name for column i
#'   # fieldname = colnames(ps[i]) # fieldnames assigned value of column i
#'   # unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")]) # for column i, which rows have non NA values? of these, what are the unique lkname and datasource combinations?
#'   # str(ps[,i,])
#'   # length(unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")])) == 1
#'   #####
#'   # use rowbind to grow a dataframe that is fieldname=old variable name, and data source, and lake name. Only include unique combinations of each datasource and lkname.
#'   fielduse <-  rbind(fielduse,data.frame("fieldname" = colnames(ps[i]),
#'                                          unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")])))
#'   
#'   }
#' 
#'   Sys.time() - start
#' 
#' #summary(fielduse)
#' str(fielduse)
#' #length(unique(fielduse[,1]))
#' 
#' #' Save the field (or variable) name data as a .csv file in the clp_surveys folder
#' # write.csv(fielduse, file = "data/output/fieldkeystart.csv", row.names = F)



# bring in new field names ------------------------------------------------



#' # New Variable Names
#' 
#' Next, I went through fieldkeystart.csv and added a column for the new name that I would assign to each 
#' old field name. This woudl serve as a lookup key to assign a coordinated set of names to the data.
#' I used Skawinski 2014 (Aq plants of the upper midwest), Chadde 2011 (A great lakes wetland flora),
#' and Borman et al 1997 (Throght the looking glass) to assign latin names to the names in the 
#' submitted datasets. Where unneeded fields were in survey data, I assigned them the name delete to
#' flag them for deletion later on (example total n native species, richness, etc.)
#' I saved this new file as fieldkeydone.csv. (this process took about 10 hours).
#' 
#' Heres a look at what I came up with
# to load in finished field key:
fk3 <- fread(file = "data/input/fieldkeydone.csv") 
str(fk3)

#' Not bad, we went from 1400 to 240 column names. Lets take a look at the new names:
names <- fk3[,.N,newfieldname]

#' # Assign New Variable Names to Data
#' 
#' Now we want to take the "new fieldnames" and pop them in as new columnnames in our statewide dataset. 

#' now check to make sure that we have a full key for each unique column title in fieldkey 3:
length(colnames(ps))
length(unique(fk3$fieldname))

#' test the lookup from the fieldkey 3 table
# save the old names
pst <- colnames(ps)
length(pst)
# colnames(ps) <- pst # used to reassign old names to ps

#' Do all columnames have a match? Are they all unique matches?
# # testing
# a <- c(1,2,3,4,5)
# key <- data.frame(old = c(1:5), new = c(3,3,3,5,5))
# match(a,key$new)
sum(is.na(match(pst, fk3[,fieldname]))) # 0 NA matches
length(unique(match(pst, fk3[,fieldname]))) # 1976 unique matches

#' make a new columnname vector that has my new variable names in the order of the way they match the old dataset
# make a vector that matches ps column names to my old names and assigns each object the new name (order of old dataset is maintained)
pst1 <- fk3$newfieldname[match(pst, fk3[,fieldname])] 
paste(pst1[345],names(ps[345]), sep= "---") # test one match


# collapse duplicated columns ---------------------------------------------



#' # Collating Multiple Variables Into A Single Column
#' 
#' All columns contain data--yikes. How do we unite those duplicates into single columns (for example, how do we combine all of our point.id columns?)
#' 1. take all columns named "x" and combine them, separating their values with commas
#' 2. remove all NAs
#' 3. Review the data in each column, removing erroneous rows... This will be a big step.
#' Let's start with 1&2


#' Assign numbers to the new names as well (e.g., point.id_1, point.id_2, ..., point.id_i)
# link up new and old names
names <- data.frame(pst1, pst)
length(names[,1])
# alphabetize
head(order(names[,1]))
head(names[order(names[,1]),1:2])
# alphabetize and tack on numbers
names <- names[order(names[,1]),1:2] # make "names" into the ordered version
names$pst3 <- paste(names[,1],1:length(names[,1]),sep = "--") # add numbers to the ordered version
# keep the old names and the numbered new names:
fk4 <- names[,2:3]

#' now need to reorder these names based on the ps dataset
# check matches
sum(is.na(match(pst, fk4[,1]))) # how many NA matches?
length(unique(match(pst, fk4[,1]))) # how many unique matches?
# reorder and reassign names of ps dataset to be numbered new names
pst2 <- fk4[match(pst, fk4[,1]),2] #change order back to the ps dataset order
colnames(ps) <- pst2
#' Define the new matches (pst are original names)!
sort(paste(colnames(ps),pst, sep = "---"))

#' Combine all rows with point.id_i (to develop the code for NA rm and collation of columns)  

#' to pull the names w/o any numbers:
head(word(string = names(ps), start = -2, sep = "--"))
tail(word(string = names(ps), start = -2, sep = "--"))

#' combine all columns with the same name
head(names(ps)=="STA_NBR") #which ps are named STA_NBR
head(word(string = names(ps), start = -2, sep = "--")=="STA_NBR") # and after cutting off the number label?
head(ps[,word(string = names(ps), start = -2, sep = "--")=="STA_NBR"]) # call out the columns with STA_NBR in them
names(ps[,word(string = names(ps), start = -2, sep = "--")=="STA_NBR"]) # the names of those columns


#' At this point we have a point id variable that contains many NAs (to be deleted) and multiple erroneous (also need to delete these)
#' We also need to verify that there are no "#,#" or "#,etc" left in here.
#' Finally, we need to do this unite and subsequent NA deletion for all of the other variables in our dataset. 
#' 
#' A loop to unite all columns with the same name, then delete all of the NA's in em'(takes ~ 3 minutes to run)
psb <- ps #save a copy of ps unedited

library(devtools)
devtools::install_github("hadley/tidyr") # need to use devlopment version to have functionality of the na.rm = TRUE call within unite


# ps <- psb
start <- Sys.time()
for (j in unique(pst1)) {
  
  print(match(j, unique(pst1)))
  # testdat <- ps[1:100,] # used for testing             
  # j = unique(pst1)[32] # used for testing 
  # unite to paste together all point id column data
  ps <- unite(ps, #dataframe
              col = !! paste(j, "a", sep = "--"), # new title for created column--must keep "_a" ending to ensure that word() fn doesnt break in the next line
              names(ps)[word(string = names(ps), start = -2, sep = "--") == j ], # old column titles to be united
              sep = ",", # separate data from multiple columns with commas
              remove = TRUE,# delete the old field headings
              na.rm = TRUE
  )
  # trim out NAs and commas
  # ps[,paste(j,"a",sep = "_")] = TrimMult(na.remove(ps[,paste(j,"a",sep = "_")]), char = ",")
  # progress:
}
Sys.time() - start



# # a messy dplyr attempt
# ps[ , id := .I]
# 
# ps <- 
#   ps %>%
#   gather("col_ID", "Value",-id)%>%
#   separate(col_ID,c("col_group"),sep="--") %>%
#   group_by(id, col_group) %>%
#   summarize(new_value=paste0(Value,collapse = ",")) %>%
#   spread(col_group,new_value)%>%
#   head()


#' # Check Result, Save Progress
#' examine the new variables
head(sort(names(ps)))
str(ps)
summary(ps$`STA_NBR--a`)

#' strip "_a" from the new field names (used in the collating process)
# to pull the names w/o any "_a":
# word(string = names(ps), start = -2, sep = "_")
colnames(ps) <- word(string = names(ps), start = -2, sep = "--")
sort(names(ps))


# progress checkpoint -----------------------------------------------------

#' save progress as a .csv file in the output folder

# write.csv(ps, file = "data/output/surveys_columns_united.csv")
# ps <- fread(input = "data/output/surveys_columns_united.csv")


# clean out cells in each column ------------------------------------------

#' We need to go through each column and drop or edit cells with errors. For
#' example, some cells in the depth column will have words. Some cells migt have
#' two values after collapsing columns... we need to deal with those issues.
#' 
#'    
#'  Start by re-arranging the columns:

names(ps)

setcolorder(ps, c(1,222,30,31,2,39,33,3,151,191,190,171,
                  172,196,223,218,187,168,159,154,155,68,
                  52,34,32,29,28))
setcolorder(ps, c(names(ps)[1:27],sort(names(ps)[28:223])))# alphabet sort plants

names(ps)      

setnames(ps, "V1", "rowident")


#' now start cleaning!!!

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



# STA_NBR -----------------------------------------------------------------



#' ## STA_NBR
#'In the future this could be a more calculated step,  looking to match the 
#' numbers in the point id column to any spatial data, which would ensure that 
#' the connection to spatial referencing is stable/reliable:

summary(ps$STA_NBR)

#' do any blank point id rows have data in other columns?
sum(ps$STA_NBR == "")

sum(ps$STA_NBR == "" & ps[,depth_m]!= "") # data in the depth.meter column?
ps[ps$STA_NBR == "" & ps[,depth_m]!= "", .(STA_NBR,depth_m)] # these are obviously bogus!

#' These are seeemingly actual observations. Unfortunately we dont have point IDs for them...

sum(ps$STA_NBR == "" & ps[,DEPTH_FT]!= "") # data in the DEPTH_FT column?
sum(ps$STA_NBR == "" & ps[,depth_unk]!= "") # data in the feet.unkn column?
ps[ps$STA_NBR == "" & ps[,depth_unk]!= "",c(1:27)]

#rescue STA NUM from delete col?
ps[ps$STA_NBR == "" & ps[,depth_unk]!= "", delete]
ps[ps$STA_NBR == "" & ps[,depth_unk]!= "", STA_NBR := word(delete, sep = ",")]

a <- ps[ps$STA_NBR == "" & ps[,depth_m]!= "", .(delete, SURVEY_ID)] #looks like our trick will not work on survey # 249
ps[ps$STA_NBR == "" & ps[,depth_m]!= "" & SURVEY_ID != "249", word(delete, sep = ",")]
ps[ps$STA_NBR == "" & ps[,depth_m]!= "" & SURVEY_ID != "249", STA_NBR := word(delete, sep = ",")]

a <- ps[ps$STA_NBR == "" & ps[,DEPTH_FT]!= "", .(delete, SURVEY_ID)] #looks like our trick will not work on survey # 585
ps[ps$STA_NBR == "" & ps[,DEPTH_FT]!= "" & SURVEY_ID != "585", word(delete, sep = ",")]
ps[ps$STA_NBR == "" & ps[,DEPTH_FT]!= "" & SURVEY_ID != "585", STA_NBR := word(delete, sep = ",")]

#' delete all rows w/o STA_NBR:
ps[STA_NBR == "", 1:27,]
ps <- ps[STA_NBR != "",]

#' How many point do I have for sblood kohlman 2010-8-25 (I got counted 140 from the survey data, so should be 140)
sum(ps$DATASOURCE == "sblood" & ps$LAKE_NAME == "kohlman" & ps$SURVEY_DATE == "2010-08-25")

#' get rid of the "VP#" point that were in the data
sum(str_detect(ps$STA_NBR, "VP"))
ps %>%
  filter(str_detect(ps$STA_NBR, "VP"))

ps$STA_NBR <- str_replace(ps$STA_NBR, "VP", "")

# unique(ps$STA_NBR)
# unique(ps$STA_NBR)[1000:length(unique(ps$STA_NBR))]

#' retain only first values, drop extra words, etc.
# sort(unique(gsub( "TD", "", word(word(word(ps$STA_NBR,sep = ","), sep = "/"), sep = " "), )))
# sort(unique(gsub( "TD", "", word(word(word(ps$STA_NBR,sep = ","), sep = "/"), sep = " "), )))[1000:1691]

ps[ , STA_NBR := gsub( "TD", "", word(word(word(STA_NBR,sep = ","), sep = "/"), sep = " ")), ]



#drop words & other erroneous things
sort(unique(ps$STA_NBR) ) #1:5 are bad, 652 is bad, 
sort(unique(ps$STA_NBR) )[1000:1691]
sort(unique(ps$STA_NBR) )[c(1:5,652, 1527:1691)]

sum(is.na(match(ps$STA_NBR, sort(unique(ps$STA_NBR) )[c(1:5,652, 1527:1691)] )) == F)
sum(is.na(match(ps$STA_NBR, sort(unique(ps$STA_NBR) )[c(1:5,652, 1527:1691)] )) == T)


ps <- ps[ is.na(match(ps$STA_NBR, sort(unique(ps$STA_NBR) )[c(1:5,652, 1527:1691)] )) == T, , ]


summary(ps$STA_NBR)
#' Any words or other erroneus junk left in the STA_NBR data?  
unique(ps$STA_NBR)
unique(ps$STA_NBR)[1000:length(unique(ps$STA_NBR))]
ps <- ps[STA_NBR != "Average", , ]
ps[ , STA_NBR := word(STA_NBR, sep = "-") ,]

# check result:
unique(ps$STA_NBR)
unique(ps$STA_NBR)[1000:length(unique(ps$STA_NBR))]

# write.csv(ps , file = "data/output/Surveys_cleaning4Dec.csv", row.names = F)

# depth meters ------------------------------------------------------------------


#' ## depth_m

summary(ps$depth_m)
unique(ps$depth_m)

#' how many blanks? 65330
sum(ps$depth_m == "")

#' rows with no depth data at all, and not marked as unsampled points:8827
sum(ps$depth_m == "" &
      ps$DEPTH_FT == "" &
      ps$depth_unk == "" &
      ps$not_sampled == "")

#' maybe the depth was in another column? 
sum(ps$depth_m == "" &
      ps$DEPTH_FT == "" &
      ps$depth_unk == "" &
      ps$not_sampled == ""&
      ps$SURVEY_NOTES == "" &
      ps$delete == "")

#' check out the delete data for those lines where theres data in comments or delete columns
unique(ps[ps$depth_m == "" &
            ps$DEPTH_FT == "" &
            ps$depth_unk == "" &
            ps$not_sampled == "" &
            (ps$SURVEY_NOTES != "" |
               ps$delete != ""), c ("SURVEY_NOTES", "delete")])

#' There's really not many lines with relevant info there

#' now see how many are missing data for depth
sum(ps$depth_m == "") # missing depth_m
sum(ps$depth_m == "" &  # missing depth_m & .feet
      ps$DEPTH_FT == "")
sum(ps$depth_m == "" &  # missing depth_m & .feet & .unkn
      ps$DEPTH_FT == "" &
      ps$depth_unk == "")
sum(ps$depth_m == "" & # missing depth and not marked as "not_sampled"
      ps$DEPTH_FT == "" &
      ps$depth_unk == "" &
      ps$not_sampled == "")
sum(ps$depth_m == "" & # missing depth info, not marked as unsampled, and not having anything in the comments section
      ps$DEPTH_FT == "" &
      ps$depth_unk == "" &
      ps$not_sampled == ""&
      ps$SURVEY_NOTES == "" )
sum(ps$depth_m == "" & # all that, and having no info in the "delete" column
      ps$DEPTH_FT == "" &
      ps$depth_unk == "" &
      ps$not_sampled == ""&
      ps$SURVEY_NOTES == "" &
      ps$delete == "")

#' My vote is that we delete all rows where these (depth_m, .feet, .unkn, not_sampled) are all blank  and
#' since I'm the only one that gets to vote... (this will remove 14674 rows)
# if you have no depth info, & you're not marked as unsampled you go bye-bye
ps <- ps[ps$depth_m != "" | 
           ps$DEPTH_FT != "" |
           ps$depth_unk != "" |
           ps$not_sampled != "",]

#' Next lets move markings that indicated "not_sampled" from this column into the not_sampled one
#' What to do with rows within feet.meter that have erroneous things written in them (too deep, too shallow, etc)? I think that these rows need to be blanked out (or NA),
#' then a 1 added into the "not_sampled" column.
sort(unique(ps$depth_m))

# how many cases? 475
sum(is.na(match(ps[,depth_m], sort(unique(ps$depth_m))[828:856])) == F) 

# which of these indicate not_sampled?
sort(unique(ps$depth_m))[828:856][c(3:5,7:12, 17:28)]
is.na(match(ps[,depth_m], sort(unique(ps$depth_m))[828:856][c(3:5,7:12, 17:28)])) == F
ps[ is.na(match(ps[,depth_m], sort(unique(ps$depth_m))[828:856][c(3:5,7:12, 17:28)])) == F , depth_m,] #peel of lines that match the params
ps[ is.na(match(ps[,depth_m], sort(unique(ps$depth_m))[828:856][c(3:5,7:12, 17:28)])) == F , not_sampled := "X",] # mark the not sampled column

#' We need to have some idea of what the remainder each of these were used to 
#' denote if we want to do anything with them. Rather than try to infer what the
#' ambiguous ones meant, we'll delete the words from this column
#' (leaving the row in the dataset). 
#' 
ps[ is.na(match(ps[,depth_m], sort(unique(ps$depth_m))[c(2,828:856)])) == F , depth_m := "",] # make ambiguous depth data into blanks


# check for leftover words or erroneous
sort(unique(ps$depth_m))

#' depth = 1 should be 1
sum(ps$depth_m == "= 1") # 32 cases.. who wrote this and in what surveys?
ps[ps$depth_m == "= 1", c("SURVEY_DATE", "DATASOURCE")]
# it looks like in rich brasch's surveys on these dates he wrote "<= 1", but my setup pulled in "= 1"
# I will set these to 1 for depth
ps[ps$depth_m == "= 1", "depth_m"] <- "1"

#' what else do we need to tackle?  
sort(unique(ps$depth_m))

#' how many "0" for depth_m?
sum(ps$depth_m == "0")
# who wrote that?
ps[ps$depth_m== "0" ,c("SURVEY_DATE", "DATASOURCE", "LAKE_NAME")]

#' It looks like they meant not_sampled. We'll move these over to that column
ps[ps$depth_m== "0" ,"not_sampled"] <- "X"
# and set depth_m = ""
ps[ps$depth_m== "0" ,"depth_m"] <- ""

#' how many with no depth in any of the three (136) and marked as not_sampled (136) )
ps$depth_m <- as.character(ps$depth_m)
 ps[ps$depth_m== "" &
             ps$DEPTH_FT == "" &
             ps$depth_unk == "" &
             ps$not_sampled == "X", "depth_m"]

#' what else do we need to tackle?  
sort(unique(ps$depth_m))

#' drop "m" off the depths and make sure there are no erroneous spaces
ps[ ,depth_m := gsub("m","", depth_m) , ]
ps[ ,depth_m := gsub(" ","", depth_m) , ]
sort(unique(ps$depth_m))

ps[ depth_m == "",DOWLKNUM ,]
sum(is.na(as.numeric(ps$depth_m)) == T)

#' Set depth_m as a numeric:
ps[ , depth_m:= as.numeric(depth_m),]
summary(ps$depth_m)

#' Visualize results  
hist(ps$depth_m, breaks = seq(0, 100, .1), freq = T , xlim = c(0,20))
hist(ps$depth_m, breaks = seq(0, 100, .1), freq = T , xlim = c(0,50), ylim = c(0,5))

#' who input depths in meters exceeding 10?
ps[is.na(ps$depth_m) == F &
     ps$depth_m > 10 , c("depth_m", "SURVEY_DATE", "DATASOURCE", "LAKE_NAME")]
# johnson 46 and 37 were intended to be 4.6 and 3.7, all the rest look legit to me.
ps[is.na(ps$depth_m) == F &
     ps$depth_m == 46 , "depth_m"] <- 4.6
ps[is.na(ps$depth_m) == F &
     ps$depth_m == 37 , "depth_m"] <- 3.7

#' Visualize results  
summary(ps$depth_m)
hist(ps$depth_m, breaks = seq(0, 100, .1), freq = T , xlim = c(0,20))
hist(ps$depth_m, breaks = seq(0, 100, .1), freq = T , xlim = c(0,50), ylim = c(0,5))

#' I guess that pretty much wraps up the depth meters column



# depth feet --------------------------------------------------------------


#' ## DEPTH_FT

#' Let's take a look:  
summary(ps$DEPTH_FT)
sort(unique(ps$DEPTH_FT))
#' start with blanks. How many rows have no DEPTH_FT
sum(ps$DEPTH_FT == "") #90807

# these are not depths--
sort(unique(ps$DEPTH_FT))[c(2:3,345:445)]
ps[ is.na(match(ps[,DEPTH_FT], sort(unique(ps$DEPTH_FT))[c(2:3,345:445)])) == F , DEPTH_FT := "",]



#' Lets do some not_sampled assignment
#' How many cases of not_sampled show up?
# how many cases (130)
sort(unique(ps$DEPTH_FT))[1000:1465][439:466][c(1:4,6,7,8,11,13:28)]

#' Assign these "not_sampled" values of "X"
ps[ is.na(match(ps[,DEPTH_FT], sort(unique(ps$DEPTH_FT))[1000:1465][439:466][c(1:4,6,7,8,11,13:28)])) == F , not_sampled := "X" ,]

#' assign any non depth vals a blank for DEPTH_FT
ps[ is.na(match(ps[,DEPTH_FT], sort(unique(ps$DEPTH_FT))[1000:1465][439:466])) == F , DEPTH_FT := "" ,]

#' How'd we do?
sort(unique(ps$DEPTH_FT))

#' Let's check out those zeroes 
sum(ps$DEPTH_FT == "0") 
ps[ps$DEPTH_FT == "0",c("SURVEY_DATE","DATASOURCE","LAKE_NAME")]

#' again, these zeroes appear to be being used as not_sampled indicators. Let's mark those rows as such:

ps[ps$DEPTH_FT == "0", not_sampled := "0"] 

# and delete the zeroes from DEPTH_FT column
ps[ps$DEPTH_FT == "0",DEPTH_FT := ""]

#' How'd we do?
sort(unique(ps$DEPTH_FT))

#' Whats the "3v" about
ps[ps$DEPTH_FT == "3v",c("SURVEY_DATE","DATASOURCE","LAKE_NAME")]
# lets change them back to 3
ps[ps$DEPTH_FT == "3v","DEPTH_FT"] <- "3"

# drop </> and spaces
ps[ ,DEPTH_FT := gsub( ">", "",gsub("<", "", DEPTH_FT)) , ]

ps[ ,DEPTH_FT := gsub(" ", "", DEPTH_FT) , ]


#' what else do we need to tackle?  
sort(unique(ps$DEPTH_FT))
length(ps$DEPTH_FT == "")

#' Set DEPTH_FT as a numeric:
ps$DEPTH_FT <- as.numeric(ps$DEPTH_FT)
summary(ps$DEPTH_FT)

#' Visualize results  
hist(ps$DEPTH_FT, freq = T, breaks = seq(0,203,1))

#' who input depths greater than 50 feet?
ps[is.na(ps$DEPTH_FT) == F &
     ps$DEPTH_FT > 50 , c("DEPTH_FT", "SURVEY_DATE", "DATASOURCE", "LAKE_NAME")]
# the 99.9's from Londo and 100's from kohlman was an indicator for no depth measured, lets delete those data (set as NA to be deleted later)
ps[DEPTH_FT == "100" | DEPTH_FT == "99.9", DEPTH_FT := "0"]


#' Visualize results  
summary(ps$DEPTH_FT)
hist(ps$DEPTH_FT, freq = T, breaks = seq(0,203,1))


#' I guess that pretty much wraps up the DEPTH_FT column 


# depth w/ unknown units --------------------------------------------------

#' ## depth.unk
 
#' 
#' Let's take a look:  
summary(ps$depth_unk)
sort(unique(ps$depth_unk))
sort(unique(ps$depth_unk))[1000:1359]

#' where there are two values, drop the second. 
ps[ , depth_unk := word(depth_unk, sep = ",") ,]

#' whats in the not_sampled column for the cases where folks annotated reasons to not sample?
sort(unique(ps$depth_unk)) 

# we want to assign "X" for the not_sampled values in those rows
sort(unique(ps$depth_unk))[c(918:925,928:936)]
ps[ is.na(match(ps[,depth_unk], sort(unique(ps$depth_unk))[c(918:925,928:936)])) == F ,  not_sampled := "X"]


# now we want to delete all of those data from the depth_unk col
sort(unique(ps$depth_unk))[c(2,918:937)]
ps[ is.na(match(ps[,depth_unk], sort(unique(ps$depth_unk))[c(2,918:937)])) == F ,  depth_unk := ""]

# Move zeros to not_sampled
ps[depth_unk == "99.9", LAKE_NAME]
ps[depth_unk == "0", not_sampled := "X"]
ps[depth_unk == "0", depth_unk := ""]

#' convert depth_unk to a numeric
sort(unique(ps$depth_unk))
ps$depth_unk <- as.numeric(ps$depth_unk)


# convert all depths to feet ----------------------------------------------

#' right.. now what?
summary(ps$depth_m)
summary(ps$DEPTH_FT)
summary(ps$depth_unk)
hist(ps$depth_unk, breaks = seq(0,240), xlim = c(0,40))

#' who did the unlabeled depths come from?
nrow(unique(ps[is.na(ps$depth_unk) == F,c ("LAKE_NAME", "DATASOURCE", "SURVEY_DATE")]))
unique(ps[is.na(ps$depth_unk) == F,c ("DEPTH_FT", "depth_m", "depth_unk")]) # some of these already have a depth.

# if you have a depth in ft, delete others
ps[ is.na(DEPTH_FT) == F , depth_unk := NA ,]
ps[ is.na(DEPTH_FT) == F , depth_m := NA,]

# roll all remaining depths in meters over into feet and drop the meters column form set
summary(ps[ is.na(DEPTH_FT) == T & is.na(depth_m) == F, depth_m ,])
ps[ is.na(DEPTH_FT) == T & is.na(depth_m) == F, DEPTH_FT := 3.28084*depth_m ,]
ps[ is.na(DEPTH_FT) == F , depth_m := NA,]
summary(ps$depth_m)
ps[ , depth_m:= NULL ,]


#'Tease out who used meters and feet in the depth unk column:  
a <- ps[is.na(depth_unk)== F, .("max" = max(depth_unk), "median" = median(depth_unk)) , .( LAKE_NAME, SURVEY_DATE, DATASOURCE)]

# pick a unit by surveyor
a
# units are feet:
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Allison Gamble", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Andrea Prichard", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Andrea Prichard_19", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "April Londo", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Britta Belden", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "crwd", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "dustin", DEPTH_FT := depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Meg Rattei_19", DEPTH_FT := depth_unk]

# units were meters:
ps[is.na(depth_unk)== F & 
     DATASOURCE == "brasch", DEPTH_FT := 3.28084*depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "James Johnson", DEPTH_FT := 3.28084*depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "mccomas", DEPTH_FT := 3.28084*depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Jill Sweet", DEPTH_FT := 3.28084*depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "newman", DEPTH_FT := 3.28084*depth_unk]
ps[is.na(depth_unk)== F & 
     DATASOURCE == "Rob Brown", DEPTH_FT := 3.28084*depth_unk]

# any more depth_unk data that we can use?
ps[is.na(depth_unk) == F & is.na(DEPTH_FT) == T , depth_unk ]

#delete depth_unk
ps[ ,depth_unk := NULL , ]

#these rows have no associated depth data:
ps[ is.na(DEPTH_FT) == T , .N, .( LAKE_NAME, SURVEY_DATE, DATASOURCE) ]


# not_sampled -------------------------------------------------------------

#likely they are "not sampled" points (the points from above[rows have no associated depth data])
ps[ is.na(DEPTH_FT) == T , unique(not_sampled), .( LAKE_NAME, SURVEY_DATE, DATASOURCE) ]
ps[ is.na(DEPTH_FT) == T , unique(not_sampled),  ]

#' not_sampled

unique(ps$not_sampled)
ps[not_sampled != "" &
     not_sampled != "0" ,not_sampled := "1"]
ps[not_sampled != "1",not_sampled := "0" ]
ps$not_sampled <- as.factor(ps$not_sampled)
unique(ps$not_sampled)
summary(ps$not_sampled)

#' are there places where depth is NA and we havent marked the point as "not_sampled"?  
nrow(ps[is.na(ps$DEPTH_FT) == T &
          ps$not_sampled == "0", ])
ps[is.na(ps$DEPTH_FT) == T &
     ps$not_sampled == "0", .N ,.( LAKE_NAME, SURVEY_DATE, DATASOURCE) ]


#' These survey points are failing to pull in the depths in my compiling. 
#' Therefor we will be removing these rows from the dataset. 
ps <- subset(ps, is.na(ps$DEPTH_FT) != T |
               ps$not_sampled == "1" )


NULL# other point level features ----------------------------------------------

#' Whats next?  
names(ps)

#' ## sample_taken

summary(ps$sample_taken)
sum(ps$sample_taken== "yes" & ps$not_sampled == "1") #sample taken is not useful to us...
ps[,sample_taken:= NULL]

#' ## added_points these should be dropped. these are biased locations
#' 
  ps[, unique(Added_point),] #whats in it?
  ps[Added_point == "x", .N ,]
  ps[Added_point == "x", 1:25,]

  
  # points with suffixes on STA_NBR
  ps[ , unique(STA_NBR) , ]
  ps[, STA_NBR_suf := gsub("[0-9]", "", STA_NBR),] #nab any suffixes off station ids
  ps[ , unique(STA_NBR_suf) ,]

  #review the idents for those:
  ps[  is.na(match(ps[,STA_NBR_suf], unique(STA_NBR_suf)[c(2,3,5:9)]  )) == F  , STA_NBR ,]

  #and mark some as added points
  ps[  is.na(match(ps[,STA_NBR_suf], unique(STA_NBR_suf)[c(3,7:9)]  )) == F  , Added_point := "x" ,]
  
  ps[Added_point == "x" , Added_point := "1" ,]
  ps[Added_point != "1" , Added_point := "0" ,]
  ps[ ,Added_point := as.factor(Added_point) ,]
  ps[ , STA_NBR_suf:= NULL]
  
  summary(ps$Added_point)
  ps <- ps[Added_point != "1", ,]
  ps[ ,Added_point:= NULL , ]
  
  
#' ## delete column
 ps[ , delete := NULL , ]  
  
  


# Clean taxa columns up ---------------------------------------------------


 
 names(ps)
 ps.taxa <- data.frame(ps[,23:218])
 ps.taxa[is.na(ps.taxa)] <- "0"
 ps.taxa[ps.taxa==""] <- "0"
 ps.taxa[ps.taxa=="0.5"] <- "1"
 ps.taxa[ps.taxa=="1.5"] <- "2"
 ps.taxa[ps.taxa=="2.5"] <- "3"
 ps.taxa[ps.taxa=="3.5"] <- "4"
 ps.taxa[ps.taxa=="4.5"] <- "5"
 
 ps.taxa[ps.taxa != "0"&
           ps.taxa != "1"&
           ps.taxa != "2"&
           ps.taxa != "3"&
           ps.taxa != "4"&
           ps.taxa != "5"] <- "0"
 
 ps.taxa[] <- lapply(ps.taxa[], factor)
 
 summary(ps.taxa)
 
 ps[ , 23:218 := ps.taxa ,]
 
 str(ps)
 
 
   # summary(ps[,i])
   # sort(unique(ps[,i]))
   hist(as.numeric(ps[,`Myriophyllum spicatum`]))
   plot(ps$DEPTH_FT~ps[,`Myriophyllum spicatum`], main = "EWM")
  


# progress checkpoint -----------------------------------------------------


 #' Save progress 
   
   #write.csv(ps, file = "data/output/surveys_columns_cleaned.csv", row.names = F)
   #ps <- fread(input = "data/output/surveys_columns_cleaned.csv")
 

# melt data to be wide format ---------------------------------------------

   #drop locs that were not sampled
   names(ps)
   ps[ , summary(not_sampled) , ]
   ps[not_sampled == 1 , .N , ]
   ps <- ps[not_sampled != 1 , , ]
   ps[, not_sampled := NULL, ]
   
   # make sure all of the no_veg_found are correct
   ps[ , no_veg_found := as.factor(no_veg_found) , ]
   ps[ , summary(no_veg_found) ,]
   
   # count all of the species columns
   names(ps)
   ps[ , no_taxa_found := as.factor(rowSums(ps[ ,22:217])==0) , ]
   ps[, summary(no_taxa_found) , ]
   
   # how does that compare to no_veg_found
   ps[ no_taxa_found == FALSE , summary(no_veg_found) ,]
   
   #drop old no veg column
   ps[, no_veg_found := NULL,]
   
   #change no taxa to 0,1
   ps[, no_taxa_found := as.character(no_taxa_found)]
   ps[ no_taxa_found == "TRUE", no_taxa_found := "1"]
   ps[ no_taxa_found == "FALSE", no_taxa_found := "0"]
   
   # retain the point ID chars and the depth, then make data long (new row for every observation of a species)
   names(ps)
   ps_1 = melt(ps, id.vars = c(1:20),
                   variable.name = "taxon", value.name = "rake" )
   
   #drop all of the zeros (not observed) in this plant data. because we tossed in a no_veg_found this will keep the veg in rows where they exitst and drop the no_veg_found 
   ps_2 <- ps_1[rake > 0,] # retain only rows with pres > 0
   str(ps_2)
 


   # progress checkpoint -----------------------------------------------------
   
   
   #' Save progress 
   
   # write.csv(ps_2, file = "data/output/surveys_longform.csv", row.names = F)
   # ps <- fread(input = "data/output/surveys_longform.csv", drop = 1)
   

# merge with DNR data -----------------------------------------------------

#' Now we can merge these surveys into the MNDNR datasets.
   dnrdat <- fread(input = "data/output/DNR_PI_Data_Combined.csv", drop = 1)

   #check name alignment
   cbind(names(dnrdat),names(ps))

   
   # fix up names for merge
   setnames(ps, "SURVEY_ID", "SURVEY_ID_DATASOURCE")
   setnames(ps, "taxon", "TAXON")
   setnames(ps, "rake", "REL_ABUND")
   setnames(ps, "whole_rake_density", "VEG_REL_ABUNDANCE_DESCR")
    dnrdat[ , TAXACODE := NULL , ]
   ps[ , SAMPLE_NOTES := paste(SAMPLE_NOTES,SURVEY_NOTES,sep = ";")]
   ps[ , SURVEY_NOTES := NULL , ]
   

   # merge data
   
   ps[] <- lapply(ps[], as.character)
   
   dnrdat[] <- lapply(dnrdat[], as.character)
   
   king <- merge(ps , dnrdat, by = c("DOWLKNUM", "LAKE_NAME", "DATASOURCE", "SURVEY_DATE", "STA_NBR", "DEPTH_FT", "SUBSTRATE","SURVEYOR", "SURVEY_ID_DATASOURCE", "TAXON", "SAMPLE_NOTES", "REL_ABUND", "VEG_REL_ABUNDANCE_DESCR"),  all = T )
   
   
   # clean up product:
   
   str(king)
   names(king)
   setcolorder(king, c(1:6,10,12,13,7:9,11,20,23, 14:19,21:22, 24:26))
   

   # NO veg found:
   sort(unique(king$TAXON))
   king[TAXON == "no_taxa_found", TAXON := "No Veg Found"]
   
   # if no veg found, assign dens as NA
   king[TAXON == "No Veg Found", unique(REL_ABUND)]
   king[TAXON == "No Veg Found", REL_ABUND := NA]
   
   
# review and export dataset -----------------------------------------------

   #any duplicated surveys?
   king[, .N ,.(DOWLKNUM, SURVEY_DATE)]
   unique(duplicated(king[, .N ,.(DOWLKNUM, SURVEY_DATE)])==T) # no duplicates
   
   # samples per taxon
   king[ , .N , TAXON]
   
   #create new, unique survey ID for each survey
   king[, SURVEY_ID := as.integer(SURVEY_ID),]
   king[, SURVEY_ID := .GRP ,.(DOWLKNUM, SURVEY_ID_DATASOURCE)]
   
   #and a unique ID for each sample point (groups the plant obs)
   king[, POINT_ID := as.integer(POINT_ID),]
   king[, POINT_ID := .GRP, by = c("STA_NBR", "SURVEY_ID")]
   
   #and finally a unique ID for each observation in the dataset
   king[, OBS_ID := .I]
   
   king[ , .N , c("SURVEY_ID","DATASOURCE")] #n obseravtion in each survey (total lines = n surveys)
   
  # write.csv(king, file = "data/output/plant_surveys_all.csv")

# clean up  taxonomy ------------------------------------------------------
  
   # pull in taxonomy corrections:
   
   tnrs <- fread( file = "data/output/tnrs.final.csv", drop = 1)

   tnrs[match(king$TAXON, tnrs$submittedname), "species"]
   
   king[, TAXONC := tnrs[match(king$TAXON, tnrs$submittedname), "species"],  ]
   
   names(king)
   setcolorder(king, c(1:11,27))
   
   #change TAXON to NO_VEG_FOUND
   setnames(king, "TAXON", "NO_VEG_FOUND")
   
   # drop Taxa marked for deletion
   king[ , sort(unique(TAXONC)) ,]
   
   # this is ferns, alga, etc
   king[ TAXONC == "DELETE" , NO_VEG_FOUND , ]
   
   #now we need to ensure we retain rows where no useable taxa were found
   king[ NO_VEG_FOUND == "No Veg Found", , ]
   king[ NO_VEG_FOUND == "No Veg Found", NO_VEG_FOUND := "TRUE" , ]
   king[ NO_VEG_FOUND != "TRUE", NO_VEG_FOUND := "FALSE" , ]
   
   #make all the DELETE TAXA into NAs
   king[TAXONC == "DELETE" , TAXONC := NA , ]
   
   #now delete all rows where TAXON == NA & NO_VEG_FOUND == F
   king[is.na(TAXONC) & NO_VEG_FOUND == F, ]
   king <- king[!(is.na(TAXONC) & NO_VEG_FOUND == F), ]

   setnames(king, "TAXONC", "TAXON")
   
   
   # samples per taxon
   king[ , .N , TAXON]
   
   #and finally a new unique ID for each observation in the dataset
   king[, OBS_ID := .I]
   

# export final ------------------------------------------------------------

   
   
#' And here we have it-- the king of all PI databases

   king[ TAXON == "Potamogeton amplifolius" , .N  , .(SURVEY_ID,DOWLKNUM)  ]
   
#' From DOW issue raised by wes:
   DOWfixes <- data.table(dow_original = c( 17004800, 27003500, 27009500, 27011100, 34015000, 41002100, 47004900, 47015401, 47015402, 66000290, 70050000, 81001400, 82011600, 87006001), 
                         dow_new = c( 17004802, 27003502, 27009501, 27011102, 34015100, 41002101, 47004901, 47015400, 47015400, 66002900, 70005000, 81001401, 82011602, 87006000))
   
   summary(match(king$DOWLKNUM,DOWfixes$dow_original))
   is.na(match(king$DOWLKNUM,DOWfixes$dow_original)) == F
   king[is.na(match(king$DOWLKNUM,DOWfixes$dow_original)) == F, DOWLKNUM , ]
   
   king[is.na(match(king$DOWLKNUM,DOWfixes$dow_original)) == F, 
        DOWLKNUM := as.character(DOWfixes[ match(
          king[is.na(match(king$DOWLKNUM,DOWfixes$dow_original)) == F, DOWLKNUM , ],
          DOWfixes$dow_original
        ),dow_new]) , ]
   
   
   
   
   write.csv(king, file = "data/output/plant_surveys_mn.csv", row.names = F)
   
   
   
# footer ------------------------------------------------------------------
#' ## Document footer 
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
