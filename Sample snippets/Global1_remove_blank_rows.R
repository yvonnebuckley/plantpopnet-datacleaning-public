### Code chunk to remove all completely blank rows from the master file.
# Authors: Ruth Kelly + Maude Baudraz + Yvonne Buckley
# started: 29/11/2018
# completed: 10/12/2018 

### set working directory to master to read in csv file


###### Reading in csv file ######

##Use na.strings argument in read.csv to find all strings which should be coded as NA 
## on reading in the data. 

#mydata <- read.csv("fullPPN_dataset_2018-06-12_.csv", na.strings = c("NA", "", "Na", "na"))

# Create function which counts the number of NA's in each row. 
countNAs <- function(x) {length(which(is.na(x)))}

# apply this function to each row in the dataset using apply, 
# and store this in a new vector called numNAs 
numNAs <- apply(mydata,1,countNAs)
##

### This code checks how many blank rows there are
#nrow(mydata[numNAs == ncol(mydata),])
#1364 blank rows.. 

#  Delete all blank rows by Deleting rows where all cells are NA's 
#  These will be rows where numNAs is equal to the number col of the dataframe
mydata <- mydata[numNAs != ncol(mydata),]

### A simple check to make sure there are no more blank rows. 
# Apply the countNAs function to the new dataset to check new dataset 

# numNAs <- apply(mydata,1,countNAs)

### Check that numNAs is never = to the number of column 
### (i.e. this line of code should give a 0 response)
# length(which(numNAs == ncol(mydata)))

### output new mydata file

write.csv(mydata, "fullPPN_dataset_2018-06-12_noBlanks.csv", row.names = FALSE)

###


