## Caroline McKeon 17/01/19
## checking rosette_number column
## reviewed by Alain Finn 08/04/2019
## YB 21/10/2020

## inital exploration of column
table(mydata$rosette_ID)

## rosette number does not matter much as a column, as in the general ppn dataset there is no consistency
## in tracking rosettes within a plant from year to year
## the only thing we need is for the number of rosette numbers to match the 
## numbers from the column "no_rosettes"


## Survival script edits have created instance where survival = "yes" 
## but there no data for the rosette (all NAs). this is because survival
## was measured at rosette level and not plant level as required in PLANTPOPNET. 
## These rows should be removed now. 

mydata <- mydata[!(mydata$survival == "yes" & is.na(mydata$no_leaves) & is.na(mydata$leaf_length)),]
#155 rows removed as of 19/02/2020 - this may change as data is added. 

## Creating warnings for instances where the number of unique rosette IDs per unique plant id
## is not the same as the value for no_rosetes

check_rosette_ID <- c()

for( y in unique(mydata$c_year)){
  for (i in unique(mydata$plant_unique_id)){
    for (j in mydata$no_rosettes[mydata$plant_unique_id == i & mydata$c_year == y]){
    
    check_rosette_ID <- append(check_rosette_ID, 
      (j / (length(unique(mydata$rosette_ID[mydata$plant_unique_id == i & mydata$c_year == y])))))
    }
  }
}

mydata <- cbind(mydata, check_rosette_ID)

## create warnings for and print plant ids where number of unique rosette ids 
## are not the same as the values for no_rosettes per unique plant id

if(any(is.na(mydata$check_rosette_ID))) warning("NA's for")
cat(paste(length(check_rosette_ID[is.na(check_rosette_ID)]), "lines of data"))

if(any(mydata$check_rosette_ID != 1)) warning("incorrect rosette ID's for")
cat(paste(mydata$plant_unique_id[!is.na(mydata$check_rosette_ID) & mydata$check_rosette_ID != 1], 
          max.levels = 0))


    
## if(any(mydata$check_rosette_ID != 1)) warning("incorrect rosettes IDs for")
## print(mydata$plant_unique_id[mydata$check_rosette_ID != 1], max.levels = 0)    this line prints an NA if the plant is recorded as dead 



## clean up
mydata <- droplevels(mydata)
rm(check_rosette_ID)

## FINISHED
