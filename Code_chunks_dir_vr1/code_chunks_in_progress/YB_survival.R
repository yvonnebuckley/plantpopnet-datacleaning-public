## checking survival column
## YB 07/01/2019
#

mydata <- read.csv("./Code_chunks_dir_vr1/code_chunks_in_progress/fullPPN_dataset_2019-01-29_.csv", header = T)

## str(mydata$survival)

## collapse values of factor levels containing y or n to "yes" and "no" - regardless of case
correct_levels <- c("no", "yes")
survival_fix <- mydata$survival
survival_fix[grep("([y])", survival_fix, ignore.case = TRUE)] <- "yes"
survival_fix[grep("([n])", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix <- droplevels(survival_fix)
## will return a warning if levels are not correct
if(any(levels(survival_fix) != correct_levels)) warning("incorrect levels")

## for all Y0 survival should be NA, returns a warning if not
if (all(is.na(mydata$survival[mydata$s_year == "Y0"])) == FALSE) warning("Y0 survival not NA - check data")

## replace survival with the fixed column
mydata$survival <- survival_fix

## check origin of NA's
length(is.na(mydata$survival))
length(is.na(mydata$survival[mydata$s_year == "Y0"]))  ## most NA's are Y0 plants


## NA's also likely to arise from new plants (in first year of recording)
## need to check survival for individual plants (unique id's) - only one "no" per plant, 
## possibly one "NA" in yr preceding a "no"



