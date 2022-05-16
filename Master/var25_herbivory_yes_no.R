## Caroline McKeon 04/02/19
## checking herbivory column

#Reviewed; Maude Baudraz, 28.03.2019

##read in data
#mydata <- read.csv("fullPPN_dataset_2019_01_21_.csv", header = T)

## There are instances of sites submitting commbinded herbivory/disease columns
## Aim here is to tease them appart, with out loosing any information, so that we end up with 
## 1. binary disease column 
## 2. binary herbivory column 
## 3. disease comments (original column) 
## 4. herbiovry comments (original column)

## inital exploration of column
#table(mydata$herbivory..yes.no.)
#table(mydata$herbivory_comments)
## herbivory comments is a detailed breakdown of percent herbivory by taxa. Leave in.

mydata$herbiv_orig <- mydata$herbivory..yes.no.  #copying column before making any changes 
##standardising column levels

#if i is 0 = "no"
mydata$herbivory..yes.no.[mydata$herbivory..yes.no. == 0] <- "no" 
#Review; 
#this line seems to convert a lot of NAs to "no"
#Enter following line to reproduce the error 
#mydata$herbivory..yes.no.  
#Review end

## if i contains n = "no"
## this needs to be run before the digits line to catch some of the weird examples 
## eg CDF Plant_ID 42 is "?no" which R reads as "<U+0E37>no"
mydata$herbivory..yes.no.[grep("n", mydata$herbivory..yes.no., ignore.case = TRUE)] <- "no"

## if i contains digits = "yes"
mydata$herbivory..yes.no.[grep("[[:digit:]]", mydata$herbivory..yes.no., ignore.case = TRUE)] <- "yes"

## if i contains y = "yes"
mydata$herbivory..yes.no.[grep("y", mydata$herbivory..yes.no., ignore.case = TRUE)] <- "yes"



## if i contains alphabetic charaters = "yes"
mydata$herbivory..yes.no.[mydata$herbivory..yes.no. != "no"] <- "yes"
#Review; this line changes "one rosette outside plot" for "yes", whereas that may mean that no rosette in the plot has herbivory?
#Suggestion; print out the options that will be changed to yes at this stage, and asking the operator to check whether they indeed mean yes. That would make it less likely to have other options in next years that are not a yes 
#Review end

## checking no information was left behind in the herbivory_comments column

herbivory <- c("herb", "deer", "insect", "mollusk", "caterpillar", "chew", "grass", "leaf miner") #Review; what if more options appear in the next years? But these are good options indeed, that will remain valid in future. #Review end

## Change herbivory_yes/no to yes if values from "herbivory" above feature in disease_comments or herbivory_comments
## if i is in "herbivory" -> herbivory..yes.no. = "yes"
mydata$herbivory..yes.no.[grep(paste(herbivory, collapse="|"), 
                               mydata$disease_comments, ignore.case = TRUE)] <- "yes"
mydata$herbivory..yes.no.[grep(paste(herbivory, collapse="|"), 
                               mydata$herbivory_comments, ignore.case = TRUE)] <- "yes"

## create warnings for rows not cleaned by this script
x <- mydata$herbivory..yes.no.%in% c("yes", "no", NA)
if(any(x != TRUE)) warning("unknown level") #Review; Maybe make the error report a bit more user friendly? Like, "level that is not yes, no or na?" #Review end

## create "not in" operator
'%nin%' = Negate('%in%')

## print plant ids where herbivory level is not yes, no or NA
print(mydata$plant_unique_id[mydata$herbivory..yes.no. %nin% c("yes", "no", NA)], max.levels = 0)

## clean up
mydata <- droplevels(mydata)
rm(herbivory,x)

## FINISHED
