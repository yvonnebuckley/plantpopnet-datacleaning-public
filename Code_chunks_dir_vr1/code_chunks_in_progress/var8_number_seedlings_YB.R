## data cleaning number_seedlings
## Yvonne Buckley 19/11/18
## ready to source into master file 29/11/2018
## outputs: mydata$number_seedlings 


#mydata <- read.csv("fullPPN_dataset_2018-06-12_.csv", header = T)

## ERROR: factor with text entry for na
# str(mydata$number_seedlings)
# levels(mydata$number_seedlings)

## SOLUTION: change to numeric and coerce text to NA - will give warnings - ignore
mydata$fix_number_seedlings <- as.numeric(as.character(mydata$number_seedlings))

## CHECK: check fix
# str(mydata$fix_number_seedlings)
# mydata$fix_number_seedlings[1:500]
# mydata$number_seedlings[1:500]
# summary(mydata$number_seedlings)
# summary(mydata$fix_number_seedlings)

## tidy up and replace number_seedlings with fixed version
mydata$number_seedlings <- mydata$fix_number_seedlings
mydata <- subset(mydata, select = -fix_number_seedlings)
mydata <- droplevels(mydata)

