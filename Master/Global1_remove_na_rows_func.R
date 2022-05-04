## Script for functions to output data frame which removes rows/cases
## with all NA variables
## YB 12/12/18 uses code from RK & MB script from Global1_remove_blank_rows.R

## reviewed by Caroline Mckeon 11/03/19
## checked & minor revision by YB 08/04/19, moved the NA detection directly into the compiler script

## scripts one function & then applies them to "mydata": 

## rem_na_df takes a dataframe as an argument and removes complete rows/cases with all NA's, takes file "mydata" by default
## pass any data frame to the function rem_dat_fr (by default it will take mydata)

rem_na_df <- function(dat_fr = mydata) {
  
  # Create function which counts the number of NA's in each row. 
  countNAs <- function(x) {length(which(is.na(x)))}
  numNAs <- apply(dat_fr,1,countNAs)
  # apply this function to each row (2nd argument = 1) in the dataset using apply, 
  # and store this in a new vector called numNAs 
  
  nrow(dat_fr[numNAs == ncol(dat_fr),])
  
  #  Delete all blank rows by Deleting rows where all cells are NA's 
  #  These will be rows where numNAs is equal to the number col of the dataframe
  dat_fr <- dat_fr[numNAs != ncol(dat_fr),]
  
}
## Run these functions on PPN dataset

mydata <- rem_na_df(mydata)

## Print the number of rows that will be removed 
## original_data <- read.csv("fullPPN_dataset_2018-06-12_.csv")

rows_rem <- function(df_orig = data_orig, df_test = mydata) {
  x <- nrow(df_orig) - nrow(df_test)
  if(x != 0) 
    print(paste(c(x,"rows with only NA's"), collapse=" ")) 
  warning(paste(x, "rows containing only NA's have been removed", sep = "  "))
  
}
rows_rem(data_orig, mydata)
  


## use for example
## source("Global1_remove_na_rows_func.R")


