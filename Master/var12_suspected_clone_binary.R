## Caroline McKeon 08/12/18
## checking suspected clone colume
## Reviewed by Yvonne Buckley 11/03/19

## tasks accomplished in this script:
## suspected_clone_binary column created 
## suspected_clone column renamed and preserved as suspected_clone_notes

## read in data 
#mydata <- read.csv("fullPPN_dataset_2019-01-29_.csv", header = T)

## initial investigation of data 
#table(mydata$suspected_clone)

## create uncleaned column "suspected_clone_notes"
mydata$suspected_clone_notes <- mydata$suspected_clone

## rename column to be cleaned "suspected_clone_binary"
names(mydata)[names(mydata) == 'suspected_clone'] <- 'suspected_clone_binary'

##standardising column levels 

## if row contains "y" or "Y", row = yes
mydata$suspected_clone_binary[grep("y", mydata$suspected_clone_binary, ignore.case = TRUE)] <- "yes"

## if i contains "clone", "Clone" or "CLONE" = yes
mydata$suspected_clone_binary[grep("clone", mydata$suspected_clone_binary, ignore.case = TRUE)] <- "yes"

## if i contains "n" or derivative then set to no
mydata$suspected_clone_binary[grep("n", mydata$suspected_clone_binary, ignore.case = TRUE)] <- "no"

#if i is 0
mydata$suspected_clone_binary[mydata$suspected_clone_binary == 0] <- "no"

## if i contains digits = yes
mydata$suspected_clone_binary[grep("[[:digit:]]", mydata$suspected_clone_binary, ignore.case = TRUE)] <- "yes"

## check: table should show only levels yes and no
table(mydata$suspected_clone_binary)


## Tidy up
mydata <- droplevels(mydata)

## create warnings for rows not cleaned by this script
x <- mydata$suspected_clone_binary %in% c("yes", "no", NA) ## should be TRUE
if(any(x != TRUE)) warning('unknown level')


## print plant ids where suspected_clone_binary level is not yes, no or NA
print(mydata$plant_id[mydata$suspected_clone_binary %nin% c("yes", "no", NA)], max.levels = 0)
print("if character(0), no unwanted values are present")
## clean up
mydata <- droplevels(mydata)
rm(x)

##FINISHED
