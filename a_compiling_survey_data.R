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

# 2018 - AllisonGamble ----------------------------------------------------


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

#' Check for input 734+98=832:

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
dow19 <- dow19[survey_contributor!= "_19"]


# clean up dates
dow19[ , "survey_date(m-d-yyyy)" := as.Date(`survey_date(m-d-yyyy)`, "%m-%d-%Y" ),  ]
dow19[ , .(`survey_date(m-d-yyyy)`), ]
names(dow19)[5] <- "survey_date"

dow19 <- dow19[is.na(survey_date)==F]
dow19[, survey_dow:=as.integer(survey_dow)]
dow19 <- dow19[is.na(survey_dow)==F]


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
sort(unique(dow19$survey_lake)) #any matches in dow18 set

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





###########################





unique(ps[is.na(dowid) == T , .N , .(lknamemv, datasourcemv, dowid, datemv)][, as.character(lknamemv),])

unique(sort(dow18[,survey_lake,]))

ps[lknamemv == "big marine", .N,.(lknamemv, datasourcemv, dowid, datemv) ]

#####

#' ## Document footer 
#' 
#' Document spun with: ezspin(file = "scripts/a_compiling_survey_data.R", out_dir = "html_outputs/compiling survey data", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()
