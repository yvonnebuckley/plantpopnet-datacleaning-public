## Script for functions to read in file and output data frame which removes rows/cases
## with all NA variables
## YB 12/12/18 uses code from RK & MB script from Global1_remove_blank_rows.R

## scripts two functions: 
## 1. id_na_df takes a file name string as an argument and converts various NA 
## types to na
## 2. rem_na_df takes a dataframe as an argument and removes complete rows/cases with all NA's

## 1. id_na_df
id_na_df <- function(file_n) {
 df <- read.csv(file_n, header = T, na.strings = c("NA", "", "Na", "na") )
}
 
## 2. rem_na_df
## pass any data frame to the function rem_dat_fr (by default it will take mydata)
rem_na_df <- function(dat_fr = mydata) {
  
  # Create function which counts the number of NA's in each row. 
  countNAs <- function(x) {length(which(is.na(x)))}
  numNAs <- apply(dat_fr,1,countNAs)
  # apply this function to each row in the dataset using apply, 
  # and store this in a new vector called numNAs 
  
  nrow(dat_fr[numNAs == ncol(dat_fr),])
  
  #  Delete all blank rows by Deleting rows where all cells are NA's 
  #  These will be rows where numNAs is equal to the number col of the dataframe
  dat_fr <- dat_fr[numNAs != ncol(dat_fr),]
  
}
## Run these functions on PPN dataset
mydata <- id_na_df("./Master/fullPPN_dataset_2018-06-12_.csv")
mydata <- rem_na_df(mydata)

## use for example
## source("remove_na_rows_func.R")


