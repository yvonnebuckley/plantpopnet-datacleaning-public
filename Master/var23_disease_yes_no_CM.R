## Caroline McKeon 21/01/19
## checking disease..yes.no. column

#### Reviewed Ruth Kelly 08/04/2019

##read in data
#mydata <- read.csv("fullPPN_dataset_2019-01-29_", header = T)

## There are instances of sites submitting combined herbivory/disease columns
## Aim here is to tease them appart, with out loosing any information, so that we end up with 
## 1. binary disease column 
## 2. binary herbivory column 
## 3. disease comments (original column) 
## 4. herbivory comments (original column)

## inital exploration of column
table(mydata$disease..yes.no.)

##standardising column levels

## if i is 0  = no
mydata$disease..yes.no.[mydata$disease..yes.no. == 0] <- "no"

## if i contains a non-zero digit = yes
mydata$disease..yes.no.[grep("[[:digit:]]", mydata$disease..yes.no., ignore.case = TRUE)] <- "yes"

## if i contains a y == yes
mydata$disease..yes.no.[grep("y", mydata$disease..yes.no., ignore.case = TRUE)] <- "yes"

## if i contains a n == no
mydata$disease..yes.no.[grep("n", mydata$disease..yes.no., ignore.case = TRUE)] <- "no"


## checking no information was left behind in the disease_comments column

#table(mydata$disease_comments)
disease <- c("brown", "yellow", "black", 
             "virus", "fungus", "spot", 
             "lesions", "disease", "mildew")

## if i is in disease -> disease..yes.no. = "yes"
mydata$disease..yes.no.[grep(paste(disease, collapse="|"),
                             mydata$disease_comments, ignore.case = TRUE)] <- "yes"

mydata <-droplevels(mydata)
summary(mydata$disease..yes.no.)

## create warnings for rows not cleaned by this script
 x <- mydata$disease..yes.no.%in% c("yes", "no", NA)
 if(any(x = !TRUE)) warning("unknown level") 
 
#### Consider changing warning to "unknown level remaining in disease..yes.no. column" 

## print plant ids where disease level is not yes, no or NA
print(mydata$plant_unique_id[mydata$disease..yes.no. %nin% c("yes", "no", NA)], max.levels = 0)

## clean up
mydata <- droplevels(mydata) 
rm(disease,x)

## FINISHED
  
  
  
  
  
