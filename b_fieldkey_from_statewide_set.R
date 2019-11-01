
  
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
  write.csv(fielduse, file = "clp_surveys/fieldkeyver2.csv", row.names = F)

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
#' Document spun with: ezspin("fieldkey_from_statewide_set.R", out_dir = "coding_html_outputs/fieldkey_from_statewide_set", fig_dir = "figures", keep_md=FALSE)
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()