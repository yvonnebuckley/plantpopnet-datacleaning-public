## checking survival column
## YB 07/01/2019
## YB 8/4/19
## AF 09/09/19
## YB 21/10/2020

#mydata <- read.csv("./Master/mydata_surv_check.csv", header = T)
##mydata <- mydata_all[mydata_all$s_year == "Y0" | mydata_all$s_year =="Y1", ]


## 1. Fixes structure, factor, checks correct levels are used
## 2. Checks that all Y0 survival is NA
## 3. Creates new plant_survival variable to store unique plant id level survival (one value per unique plant id)
## 4. Year Loop: loops through each year separately and assigns plant_survival <- yes if any values in survival for each unique plant id are "yes". 
## This is done to account for some PI's who record individual rosette survival in "survival"
## 5. Checking individual plants for temporal patterns in "plant_survival" - NOT YET IMPLEMENTED!!

## 1. Fixes structure, factor, checks correct levels are used
## str(mydata$survival)

## collapse values of factor levels containing y or n to "yes" and "no" - regardless of case
correct_levels <- c("no", "yes", NA)
survival_fix <- mydata$survival
levels(survival_fix)

survival_fix[grep("new", survival_fix, ignore.case = TRUE)] <- NA
survival_fix[grep("first_record", survival_fix, ignore.case = TRUE)] <- NA

survival_fix[grep("([yY])", survival_fix, ignore.case = TRUE)] <- "yes"
survival_fix[grep("1", survival_fix, ignore.case = TRUE)] <- "yes"
survival_fix[grep("survive", survival_fix, ignore.case = TRUE)] <- "yes"
survival_fix[grep("([c])", survival_fix, ignore.case = TRUE)] <- "yes"
survival_fix[grep("revived", survival_fix, ignore.case = TRUE)] <- "yes"

survival_fix[grep("([n])", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix[grep("dead", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix[grep("died", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix[grep("0", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix[grep("([?])", survival_fix, ignore.case = TRUE)] <- "no"
survival_fix[grep("([tamp])", survival_fix, ignore.case = TRUE)] <- "no"


survival_fix <- droplevels(survival_fix)
## will return a warning if levels are not correct
#if(any(levels(survival_fix) != correct_levels)) warning("incorrect levels")
levels(survival_fix)

## 2. Checks that all Y0 survival is NA
## for all Y0 survival should be NA, returns a warning if not
if (all(is.na(mydata$survival[mydata$s_year == "Y0"])) == FALSE) warning("Y0 survival not NA - check data")

## replace survival with the fixed column
mydata$survival <- survival_fix

## 3.Create new plant_survival var to store unique plant survival values from year loop below
mydata$plant_survival <- factor(rep(NA, length(mydata$survival)), levels = levels(mydata$survival))

## 4. Year loop
corrections <- list()

for(j in 2:length(levels(mydata$s_year))) {  ## start at 2 to start with Y1 plants, Y0's already checked above
  ## checking plant level survival: Y# = yes (survived), no (dead) or NA (new plants or plants where survival was unsure)
  mydataY1 <- subset(mydata, mydata$s_year == levels(mydata$s_year)[j])
  uid_Y1 <- unique(mydataY1$plant_unique_id)
#relies on a working Plant_ID script otherwise there is no plant_unique_id column
  
  
  ## 1. check within each year that there's only one survival value per plant
  ## 2. where multiple survival values are found it's likely to arise from within-plant rosette survival being recorded
  ## 3. creates new plant_survival var to store a single survival value (NA, "yes", "no") per plant
  ## if any survivals for a unique plant are "yes" then "yes", if all are "no" then "no"
  ## otherwise retain NA 
  
  ##survival_fix_Y1 set to NA
  survival_fix_Y1 <- factor(rep(NA, length(mydataY1$survival)), levels = c("no", "yes"))
  mydataY1 <- data.frame(mydataY1, survival_fix_Y1) ## appends survival_fix_Y1 to mydataY1
  
  ##dat is produced to enable manual checking for survival values
  dat <- subset(mydataY1, select = c(survival, survival_fix_Y1, plant_unique_id)) ##
  
  ## This block commented out as only needed if manually debugging.
 #  ## Diagnose unique id's where a mix of "yes", "no" & NA values are present, stores unique id's in tmp variable y
 #  y<- as.character(NA)
 #  for(i in 1:length(uid_Y1)) {
 #    if(length(unique(mydataY1$survival[mydataY1$plant_unique_id == uid_Y1[i]])) != 1) (y <- append(y, as.character(uid_Y1[i])))
 #  }
 # 
 #  y <- y[-1] ## removes leading zero
 # y <- factor(y)
 #  corrections[[j]] <- y
 # 
 # 
 #  ## pulls out data set just containing plant ids that have a mix of survival values
 #  dat_mix_vals <-
 #    mydataY1 %>% filter(
 #      mydataY1$plant_unique_id %in% y
 #    )

  
  uid_Y1 <- unique(mydataY1$plant_unique_id)
  
  ## set survival_fix_Y1 to "yes" if any rosette has survival = "yes" and to "no" if all rosettes are "no", NA's indicate new plants
  for(i in 1:length(uid_Y1)) {
    sub_tmp <- subset(mydataY1, mydataY1$plant_unique_id == uid_Y1[i])
    sub_tmp$survival_fix_Y1 <- rep(ifelse(any(sub_tmp$survival == "yes"), "yes", 
                                            ifelse(all(sub_tmp$survival == "no"), "no")), length(sub_tmp$survival_fix_Y1))
    mydataY1[mydataY1$plant_unique_id == uid_Y1[i],] <- sub_tmp
  }
  
## Set whole plant survival column to survival_fix_Y1
  mydataY1$plant_survival <- mydataY1$survival_fix_Y1
  mydataY1$survival_fix_Y1 <- NULL
  
  mydata[mydata$s_year == unique(mydata$s_year)[j],] <- mydataY1
 
}

## warning("Survival for the following plant_unique IDs must be fixed in the original data files. Errors will likely be survival recorded at rosette level")
## print(unique(corrections))
#print("survival script complete")
##L29 Year loop

## 5. Checking individual plants for temporal patterns in "plant_survival"
## Not implemented here

## run the second rm line if using the commented debugging block above, line 77-93
rm(mydataY1, sub_tmp, uid_Y1, dat, survival_fix_Y1, survival_fix )
#rm(y, dat_mix_vals)

## NA's will arise for new plants (in first year of recording)
## need to check survival for individual plants (unique id's) - only one "no" per plant, 
## possibly one "NA" in yr preceding a "no"



