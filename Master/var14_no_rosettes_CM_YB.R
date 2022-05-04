## Caroline Mckeon 08/12/18
## checking no_rosette column
#Reviewed Maude Baudraz
## reviewed & edited by Yvonne Buckley 21/10/2019

## tasks accomplished in this script:
## 1. check that number of rosette values are positive numeric intergers and implement solutions (YB edited)
## 2. check that number of rosettes per plant matches records for that year for that plant

##read in data
#mydata <- read.csv("fullPPN_dataset_2019-01-29_.csv", header = T)

## 1. check that number of rosette values are positive numeric intergers and implement solutions
mydata$no_rosettes <- as.character(mydata$no_rosettes)
## inital exploration of column
##table(mydata$no_rosettes)
table(mydata$no_rosettes[grepl("[[:alpha:]]", mydata$no_rosettes)])  ## YB added to detect non-numeric values

## plants with "NA" in no_rosettes may be plants with a single rosette that hasn't been entered as 1 - fix these
## NOT YET IMPLEMENTED
cat(paste(length(unique(mydata$plant_unique_id[is.na(mydata$no_rosettes)])), "plants have NA in no_rosettes  "))

## some plants with "zero" rosettes (from plants that had died)
## change these to NA's
is.na(mydata$no_rosettes[grep("^0$", mydata$no_rosettes)]) <- TRUE ## YB changed to is.na

## mydata[grep("plenty", mydata$no_rosettes), ]  ## one entry where rosettes couldn't be determined - change to 1
mydata$no_rosettes[grep("plenty", mydata$no_rosettes)] <- 1  ## YB checked & changed

# create warnings for non-numeric entries
x <- grepl("[[:alpha:]]", mydata$no_rosettes)
if(any(x == TRUE)) warning("non-numeric entry")
table(mydata$no_rosettes[grepl("[[:alpha:]]", mydata$no_rosettes)])

## "new plant" in no_rosette means new to the data-set and as length of the unique id = length of the "new plants"
## we can assume these all have one rosette
uid_newbie <- mydata$plant_unique_id[grep("new plant", mydata$no_rosettes)]
new_plant <- mydata$no_rosettes[grep("new plant", mydata$no_rosettes)]
if (length(uid_newbie) == length(new_plant)) {
  cat("new plants are assumed to have 1 rosette, this is fixed")  
  mydata$no_rosettes[grep("new plant", mydata$no_rosettes)] <- 1
} else warning("some new plant attributions in no_rosette seem to have more than one rosette - check")

## "no plant" changed to NA as it contains no useful information
is.na(mydata$no_rosettes[grep("no plant", mydata$no_rosettes)]) <- TRUE
cat("'no plant' has been changed to NA as it contains no useful information")


## "not found" - looks like survival has incorrectly been coded as 1 for these plants
## mydata[mydata$plant_unique_id == mydata$plant_unique_id[grep("not found", mydata$no_rosettes)][1],]
## mydata$no_leaves[grepl("not found", mydata$no_rosettes)]

## chunk below commented out (AF 22/10/20) "not found" values are only present in CDF 2018 and refer to dead individuals, change to NA 
#mydata$plant_survival2 <- mydata$plant_survival
#levels(mydata$plant_survival2) <- levels(mydata$plant_survival)
#mydata$plant_survival2 <- ifelse((is.na(mydata$no_leaves) & grepl("not found", mydata$no_rosettes)), 
#                                       "no", mydata$plant_survival2)
is.na(mydata$no_rosettes[grep("not found", mydata$no_rosettes)]) <- TRUE
#mydata$plant_survival <- mydata$plant_survival2

## 'removed' comment seems to be a non-surviving plant
## chunk commented out for same reason as above (af 22/10/20)
#mydata[mydata$plant_unique_id == mydata$plant_unique_id[grep("removed", mydata$no_rosettes)][1],]
#mydata$plant_survival2 <- mydata$plant_survival 
#mydata$plant_survival2 <- ifelse((is.na(mydata$no_leaves) & grepl("removed", mydata$no_rosettes)), 
#                                 "no", mydata$plant_survival2)
is.na(mydata$no_rosettes[grep("removed", mydata$no_rosettes)]) <- TRUE
#mydata$plant_survival <- mydata$plant_survival2
#cat("All 'not found' & 'removed' comments in no_rosettes changed to NA. Where no_leaves is NA for 'not found' or 'removed' rosettes survival has been changed to no")

## 'no' value seems to be a typo for a non surviving plant - change to NA
is.na(mydata$no_rosettes[grep("no", mydata$no_rosettes)]) <- TRUE

## 'v' value seems to be a typo on a non surviving plant - change to NA
is.na(mydata$no_rosettes[grep("v", mydata$no_rosettes)]) <- TRUE

## 'no' value seems to be a typo for a non surviving plant - change to NA
is.na(mydata$no_rosettes[grep("no", mydata$no_rosettes)]) <- TRUE

## 'v' value seems to be a typo on a non surviving plant - change to NA
is.na(mydata$no_rosettes[grep("v", mydata$no_rosettes)]) <- TRUE


## 2. check that number of rosettes per plant matches records for that year for that plant
mydata$no_rosettes <- as.numeric(mydata$no_rosettes) ## changed to numeric as no other alpha values in vector
freq_table <- as.data.frame(table(mydata$no_rosettes))
freq_table$ratio <- as.numeric(freq_table[,2])/as.numeric(freq_table[,1]) ## if no. of records for a plant within a site/year is the same as the no_rosettes then this will divide evenly (no remainder)
freq_table$integer <- freq_table$ratio%%1==0

if(any(freq_table$integer != TRUE)) warning("incorrect number of rosettes") # they currently do not

## check that within a year, length of plant_id column for "some_unique_id" = values in no_rosettes column for that ID
## run loop to see whether rosette values = length of the plant_id

## YB to CHECK THIS AND IMPLEMENT ANOTHER SOLUTION

check_no_rosettes <- logical()

len_plant_uid_year <- vector()
no_ros_plant_uid_year <- vector()
check_plant_uid <- vector()
check_year <- vector()

suppressWarnings( for (i in unique(mydata$plant_unique_id)){
    w <- mydata[mydata$plant_unique_id == i,]
    for( y in unique(w$c_year)){
      u <- w[w$c_year == y,]
      len_plant_uid_year <- append(len_plant_uid_year, length(u$plant_unique_id))
      no_ros_plant_uid_year <- append(no_ros_plant_uid_year, max(u$no_rosettes, na.rm = TRUE))  
      check_plant_uid <- append(check_plant_uid, i)
      check_year <- append(check_year, y)
    }
  })


chex <- data.frame(len_plant_uid_year, no_ros_plant_uid_year, check_plant_uid, check_year)
is.na(chex$no_ros_plant_uid_year[is.infinite(chex$no_ros_plant_uid_year)]) <- TRUE
## create warnings for and print plant ids where rosette values are not the same length of the plant_id

chex2 <- chex[!is.na(chex$no_ros_plant_uid_year),]  ## take out the NA's where no. rosette is NA
if(any(chex2$len_plant_uid_year != chex2$no_ros_plant_uid_year)) warning("incorrect number of rosettes for")
cat(paste(length(chex2$check_plant_uid[chex2$len_plant_uid_year != chex2$no_ros_plant_uid_year]), "unique plants"))


chex2 <- chex2[chex2$no_ros_plant_uid_year/chex2$len_plant_uid_year != 1,]
print("Sites HV 2014, TO 2015, AC 2015 have all recorded multiple no_rosettes but only measures 1 rosette. These account for 43 of the mismatches listed in chex2")
if(length(chex2$len_plant_uid_year >= 44)) warning("mismatches still present, recheck chex2, ")
cat(paste(length(chex2$len_plant_uid_year)-43, "individuals need to be corrected in the master data"))

#site MACD has recorded full rows for additional flowering stems. Remove these additonal rows but keep the largest fl.stem
#MACD accounts for 413 additionals rows due to this (as of Y2 data)
#Please double check that exrows1 is not removing additonal data
print("site MACD has measured every flowering stem and recorded as duplicated rows. These will be cleaned below")
exrows <- mydata

exrows1 <- exrows %>% 
  group_by(plant_unique_id, unique_plot_id, rosette_ID) %>% 
  filter(fl_stem_height == max(fl_stem_height)|is.na(fl_stem_height)) %>% 
  ungroup() 


chec <- setdiff(exrows, exrows1)  
if(length(chec$rosette_ID >= 416))warning("additional ")
cat(paste(length(chec$rosette_ID)-416, "rows have been removed. Consult 'Chec' object to ensure these have not mistakenly been removed"))

mydata <- exrows1


## clean up
mydata <- droplevels(mydata)
rm(freq_table,check_no_rosettes, chec, chex, chex2, len_plant_uid_year,no_ros_plant_uid_year, check_plant_uid, check_year, exrows, exrows1)

## FINISHED



