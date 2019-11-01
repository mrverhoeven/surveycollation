
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



# CLP - Brasch ------------------------------------------------------------

  
  #' ## Brasch Surveys:

  # set working Dir to sourcefile loc:
  setwd("data/input/contributor_data/clp_proj/brasch")
 
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

#####    


# CLP - Dustin ------------------------------------------------------------
     
#' ### Dustin Surveys:

#' For these data I used a Kutools for Excel file conversion tool to correct the file extensions
#' because the read excel function didn't like the way they were written (I think that the file 
#' format didnt match the file extension label). So you'll see that the files are pulled from a
#' subfolder "converted."
     
     # set working Dir to sourcefile loc:
  setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/dustin/converted")
     
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


# CLP- Fieldseth ----------------------------------------------------------

      
      
#' ### Fieldseth Surveys:

#' For these data I used a Kutools for Excel workbook splitting tool to split tabs from workbooks into separate 
#' .xlsx files. I also went through the surveys and deleted a header row from them to get the fieldnames into row # 1. 
#' Only the Excel data from the "CLP_Fieldseth" folder were used (some survey data remained in pdf or word doc form).
#' Only the modified files are included in the "fieldseth" folder.
#' 

     # set working Dir to sourcefile loc:
      setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/fieldseth/")
     
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


# CLP - Johnson -----------------------------------------------------------

    
#' Johnson Surveys:

#' For these data in Excel: collapse fieldnamess into single cell, delete header rows & sub fieldname rows, delete extra rows (populated with zeros), delete extra columns
#'  (populated with excess calculations), name tabs as lake date, export tabs with Kutools for Excel workbook splitting tool
#'      
     # set working Dir to sourcefile loc:
    setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/johnson/")
     
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

      

# CLP -  Lund -------------------------------------------------------------

      
#' Lund Surveys:

 #' For these data in Excel:delete header rows, delete extra rows (populated with zeros), delete extra columns
 #'  (populated with excess calculations), name tabs as "lake m-d-year", export tabs with Kutools for Excel workbook splitting tool
 #'      
 # set working Dir to sourcefile loc:
      setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/lund/")
 
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
   setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/mccomas/")
   
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
     setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/newman/")
     
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

     

# CLP - SBlood ------------------------------------------------------------

     
#' SBlood Surveys:

#' For these data in Excel: delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)

     # set working Dir to sourcefile loc:
     setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/sblood/")
     
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

     

# CLP - CRWD --------------------------------------------------------------

     
#' CRWD Surveys:

#' For these data in Excel:delete extra header rows, delete non-data rows, delete extra columns,
#' rename tabs as "lake m_d_yr", export tabs with Kutools for Excel workbook splitting tool (into .xlsx)
#' 
#' Addt'l notes for mods done in excel:
#'           2012 are first years with excel data

   # set working Dir to sourcefile loc:
     setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation/data/input/contributor_data/clp_proj/crwd/")
   
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


# Review and output CLP data ----------------------------------------------

  
#' ## Check out product, save progress, and reset working directory
#' 

# Set working directory back to project location
     setwd("E:/My Drive/Documents/UMN/Grad School/Larkin Lab/R_projects/surveycollation")  

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
write.csv(ps, file = "data/output/clp_proj_surveys.csv", row.names = F)    

#####


# resolve issues with field names -----------------------------------------

#' # Old Variable Names
#' 
#' Pull all of the variable names (field names) off of the dataset, also attaching the lake and datasource names to those to help me identify the survey that I need to look at to interpret names
fielduse <- data.frame()
for (i in 1:(length(ps[1,])-2)) { # dont do the NA or "" columns that I added at the beginning (placeholder columns, both have no data in them; these are columns 599:600)
  # code for troubleshooting:
  #####
  # i =1
  # colnames(ps)[i] # name for column i
  # fieldname = colnames(ps[i]) # fieldnames assigned value of column i
  # unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")]) # for column i, which rows have non NA values? of these, what are the unique lkname and datasource combinations?
  # ps[,is.na(ps[,1]) == FALSE]
  # str(ps[,i])
  # length(unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")])) == 1
  #####
  # use rowbind to grow a dataframe that is fieldname=old variable name, and data source, and lake name. Only include unique combinations of each datasource and lkname.
  fielduse <-  rbind(fielduse,data.frame(fieldname = colnames(ps[i]),
                                         unique(ps[which(is.na(ps[,i])==FALSE),c("datasourcemv","lknamemv")])))
}

#summary(fielduse)
str(fielduse)
#length(unique(fielduse[,1]))

#' Save the field (or variable) name data as a .csv file in the clp_surveys folder
write.csv(fielduse, file = "data/output/fieldkeyver.csv", row.names = F)

#' # New Variable Names
#' 
#' Next, I went through fieldkey 2 and added a column for the new name that I would assign to each 
#' old field name. This woudl serve as a lookup key to assign a coordinated set of names to the data.
#' I used Skawinski 2014 (Aq plants of the upper midwest), Chadde 2011 (A great lakes wetland flora),
#' and Borman et al 1997 (Throght the looking glass) to assign latin names to the names in the 
#' submitted datasets. Where fields were derived from survey data, I assigned them the name delete to
#' flag them for deletion later on. I saved this new file as field key 3. (this process took about 15 hours).
#' 
#' Heres a look at what I came up with
# to load in finished field key:
fk3 <- read.csv(file = "clp_surveys/fieldkeyver3.csv") 
str(fk3)

#' Not bad, we went from 607 variables to 161. Lets take a look at the new names:
sort(unique(fk3[,'mrvname']))

#' # Assign New Variable Names to Data
#' 
#' Now we want to take the "mrvnames" and pop them in as new columnnames in our statewide dataset. 
#' Delete the extra stuff (mrv name, lk name, other info columns) from our lookup table (only need fieldname and mrvname columns)
str(fk3)
fk3[,c(2,3,5,6)] <- NULL
str(fk3)

#' There are also two dummy columns that I used in assembly that we can delete right away( titled: "NA" and "X" )
summary(ps[,'NA.'])
summary(ps[,'X..'])
ps$NA. <- NULL
ps$X.. <- NULL

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
sum(is.na(match(pst, fk3[,1]))) # 0 NA matches
length(unique(match(pst, fk3[,1]))) # 607 unique matches

#' make a new columnname vector that has my new variable names in the order of the way they match the old dataset
# make a vector that matches ps column names to my old names and assigns each object the new name (order of old dataset is maintained)
pst1 <- fk3$mrvname[match(pst, fk3[,1])] 
paste(pst1[345],names(ps[345]), sep= "---") # test one match

#' # Collating Multiple Variables Into A Single Column
#' 
#' All columns contain data--yikes. How do we unite those duplicates into single columns (for example, hw do we combine all of our point.id columns?)
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
names$pst3 <- paste(names[,1],1:length(names[,1]),sep = "_") # add numbers to the ordered version
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
#####
#' to pull the names w/o any numbers:
head(word(string = names(ps), start = -2, sep = "_"))
tail(word(string = names(ps), start = -2, sep = "_"))

#' combine all columns with the same name
head(names(ps)=="point.id") #which ps are named point.id
head(word(string = names(ps), start = -2, sep = "_")=="point.id") # and after cutting off the number label?
head(ps[,word(string = names(ps), start = -2, sep = "_")=="point.id"]) # call out the columns with point.id in them
names(ps[,word(string = names(ps), start = -2, sep = "_")=="point.id"]) # the names of those columns

#' use unite to paste together all point id column data
ps.test <- unite_(ps, #dataframe
                  col = "point.id", #new title
                  names(ps[,word(string = names(ps), start = -2, sep = "_")=="point.id"]), # old titles
                  sep = ",", # separate data with commas
                  remove = TRUE #delete the old field headings
)

ps.test$point.id <- as.character(ps.test$point.id)
# mutate to remove NAs
head(sapply(ps.test,class))
ps.test$point.id <- as.factor(ps.test$point.id)
head(ps.test$point.id)
head(summary(ps.test$point.id))
#####

#' At this point we have a point id variable that contains many NAs (to be deleted) and multiple erroneous (also need to delete these)
#' We also need to verify that there are no "#,#" or "#,etc" left in here.
#' Finally, we need to do this unite and subsequent NA deletion for all of the other variables in our dataset. 
#' 
#' A loop to unite all columns with the same name, then delete all of the NA's in em'(takes ~ 3 minutes to run)
for (j in unique(pst1)) {
  # testdat <- ps[1:100,] # used for testing             
  # j = as.character(unique(pst1)[1]) # used for testing 
  # unite to paste together all point id column data
  ps <- unite_(ps, #dataframe
               col = paste(j,"a",sep = "_") , # new title for created column--must keep "_a" ending to ensure that word() fn doesnt break in the next line
               names(ps)[word(string = names(ps), start = -2, sep = "_") == j ], # old column titles to be united
               sep = ",", # separate data from multiple columns with commas
               remove = TRUE # delete the old field headings
  )
  # trim out NAs and commas
  ps[,paste(j,"a",sep = "_")] = as.factor(TrimMult(na.remove(ps[,paste(j,"a",sep = "_")]), char = ","))
  # progress:
}

#' # Check Result, Save Progress
#' examine the new variables
head(sort(names(ps)))
str(ps)
summary(ps$point.id_a)

#' strip "_a" from the new field names (used in the collating process)
# to pull the names w/o any "_a":
# word(string = names(ps), start = -2, sep = "_")
colnames(ps) <- word(string = names(ps), start = -2, sep = "_")
sort(names(ps))

#' save progress as a .csv file in the clp_surveys folder  
write.csv(ps, file = "data/output_data/state_clp_comp_namescleaned.csv", row.names = F)  
   

#' ## Document footer 
#' 
#' Document spun with: ezspin(file = "scripts/a_compiling_survey_data.R", out_dir = "html_outputs/compiling survey data", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
