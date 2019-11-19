# header ------------------------------------------------------------------


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

#' This script will compile PI data for a bunch of different surveyors who have
#' submitted data.The script pulls data (points in rows, species in columns) in
#' and merges columns together where names match. Where a new column name comes 
#' in with a dataset, that column is retained. In this way, all submitted 
#' surveys are assembled into a single dataset. 


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

  
#' "ps" (point scale) will be our dataset. Let's fill er' up!

ps <- data.table(NULL)

# CLP - Brasch ------------------------------------------------------------

  
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
  
  # Original ps has 1 row (NAs) + brasch has 88 surveys, should see 1 + 88 = 89 unique combinations of data source, lake, survey date.
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
      # (1 row original, 88 from brasch,  104 from dustin should give 193 total)
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
    # (193 prior, 12 in fieldseth = 205)
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
      # (205 prior, 85 in johnson = 290)
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
   # (290 prior, 3 in lund = 293)
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
     # (293 prior, 112 in mccomas = 405)
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
     # (405 prior, 121 in newman = 526)
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
     # (526 prior, 30 in sblood = 556)
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
     # (556 prior, 19 in CRWD = 575)
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
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")  
ps <- fread(file = "data/output/clp_proj_surveys.csv")



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

# 2018 - AllisonGamble ----------------------------------------------------


#' ## Gamble Surveys:

# set working Dir to sourcefile loc:
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/2018_submissions/Allison_Gamble")

# List file extensions that you'd like to cover
files = list.files(pattern= "*.xls")# Get the files names for extension j

# for those files, go through, clean up headings and compile them
for  (i in c(1:length(files))) {
  # i = 113
  # load in survey FILEi, name it processingtable:
  processingtable <- read_excel(files[i], trim_ws = T)
  
  # add a column for datasource and populate with SOURCEi
  processingtable$datasourcemv <- rep("Allison Gamble", length(processingtable[,1]))
  
  # add a column for lake name
  processingtable$lknamemv <- rep(tolower(word(files[i], sep = " ")), length(processingtable[,1]))
  
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

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))


# 2018 - AndreaPrichard ----------------------------------------------------


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
  processingtable$lknamemv <- rep(tolower(word(files[i], sep = "_")), length(processingtable[,1]))
  
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

#' Check for input:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - AprilLondo ----------------------------------------------------


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

#' Check for input:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))

# 2018 - BrittaBelden ----------------------------------------------------


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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

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

#' Check for input:

nrow(unique(cbind(ps$datemv, ps$datasourcemv, ps$lknamemv)))



# progress checkpoint -----------------------------------------------------


# save progress as a .csv file in output data folder  
# write.csv(ps, file = "data/output/clp_proj_surveys.csv", row.names = F)    
# Set working directory back to project location
setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")  
ps <- fread(file = "data/output/clp_proj_surveys.csv")

# add DOW ids to new data -------------------------------------------------

setwd("G:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")

dow18 <- fread(file = "data/input/Data Import Progress - 2018 Entry.csv")

str(dow18)

dow18[ , survey_contributor := as.factor(survey_contributor),]

levels(dow18$survey_contributor)

unique(ps$datasourcemv)

match( levels(dow18$survey_contributor), unique(ps$datasourcemv))

dow18[ , "survey_date(m-d-yyyy)" := as.Date(`survey_date(m-d-yyyy)`, "%m-%d-%Y" ),  ]

dow18[ , .(`survey_date(m-d-yyyy)`), ]

names(dow18)[5] <- "survey_date"

summary(dow18)
summary(ps$dowid)

# check lake name matches?
A <- sort(unique(dow18$survey_lake))
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



#####

#' ## Document footer 
#' 
#' Document spun with: ezspin(file = "scripts/a_compiling_survey_data.R", out_dir = "html_outputs/compiling survey data", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
