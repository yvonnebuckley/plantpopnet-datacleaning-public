## Caroline Mckeon 08/12/18
## checking no_rosette column
#Reviewed Maude Baudraz

## tasks accomplished in this script:
## check that number of rosette values are positive numeric intergers
## check that number of rosettes per plant matches records for that year for that plant

##read in data
#mydata <- read.csv("fullPPN_dataset_2019-01-29_.csv", header = T)


mydata$no_rosettes <- as.character(mydata$no_rosettes)
## inital exploration of column
#table(mydata$no_rosettes)

## some plants with "zero" rosettes (from plants that had died)
## change these to NA's
mydata$no_rosettes[mydata$no_rosettes == 0] <- NA

## supress the plant_id where someone has entered "plenty" (what will we do about this?)
mydata <- mydata[mydata$plant_unique_id %nin% mydata$plant_unique_id[mydata$no_rosettes =="plenty"],] ###review; nice, didn't know nin was a thing! ### review end

# create warnings for non-numeric entries
## NOT WORKING
# x <- mydata$no_rosettes %in% c("[[:digit:]]",NA)
# if(any(x != TRUE)) warning("non-numeric entry")
# print(paste(c(
#   mydata$plant_unique_id[mydata$no_rosettes %nin% c("[[:digit:]]",NA)],
#   mydata$no_rosettes[mydata$no_rosettes %nin% c("[[:digit:]]",NA)]), collapse=": "), max.levels = 0)


## check if values for number of rosettes divid evenly into total rosette numbers 

freq_table <- as.data.frame(table(mydata$no_rosettes))

freq_table$ratio <- as.numeric(freq_table[,2])/as.numeric(freq_table[,1])

freq_table$interger <- freq_table$ratio%%1==0

if(any(freq_table$interger != TRUE)) warning("incorrect number of rosettes") # they currently do not

## check that within a year, length of plant_id column for "some_unique_id" = values in no_rosettes column for that ID
## run loop to see whether rosette values = length of the plant_id

mydata$no_rosettes <- as.numeric(mydata$no_rosettes) 

check_no_rosettes <- c()

for( y in unique(mydata$c_year)){
  for (i in unique(mydata$plant_unique_id)){
    for (j in mydata$no_rosettes[mydata$plant_unique_id == i & mydata$c_year == y]){ 
    check_no_rosettes <- append(check_no_rosettes, (j/(length(mydata$plant_unique_id[mydata$plant_unique_id == i & mydata$c_year == y]))))
    }
  }
}


mydata <- cbind(mydata, check_no_rosettes)


## create warnings for and print plant ids where rosette values are not the same length of the plant_id

if(any(mydata$check_no_rosettes != 1)) warning("incorrect number of rosettes for")
print(mydata$plant_unique_id[mydata$check_no_rosettes != 1], max.levels = 0)


############## Checking to see if this code works on 1 site for 1 year of data#######################
acr1 <- mydata[which(mydata$site_code == "ACR" & mydata$c_year == "2015"),]

freq_table <- as.data.frame(table(acr1$no_rosettes))

freq_table$ratio <- as.numeric(freq_table[,2])/as.numeric(freq_table[,1])

freq_table$interger <- freq_table$ratio%%1==0

if(any(freq_table$interger != TRUE)) warning("incorrect number of rosettes")


acr1$no_rosettes <- as.numeric(acr1$no_rosettes) 

check_no_rosettes <- c()

for( y in unique(acr1$c_year)){
  for (i in unique(acr1$plant_unique_id)){
    for (j in acr1$no_rosettes[acr1$plant_unique_id == i & acr1$c_year == y]){ 
      check_no_rosettes <- append(check_no_rosettes, (j/(length(acr1$plant_unique_id[acr1$plant_unique_id == i]))))
    }
  }
}

if(any(acr1$check_no_rosettes != 1)) warning("incorrect number of rosettes for")
print(acr1$plant_unique_id[acr1$check_no_rosettes != 1], max.levels = 0)

################### Review##################
## Oct 2019
## acr1 check shows 1 mistake - plant unique id ACR_T1_P10_40 has 2 rosettes but is listed as 1 in no_rosettes column. Woop! the code works
## On the full dataset ~6000 entries are listed with mistakes. How to fix?
## also, there are hundreds of Nas in plant unique id which should not be the case!
## GO BACK AND CHECK PLANT_ID AGAIN - Alain 09/10/2019







## clean up
mydata <- droplevels(mydata)
rm(freq_table,check_no_rosettes)

## FINISHED

