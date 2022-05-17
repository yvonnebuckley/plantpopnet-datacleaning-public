####Inflorescence phenology ####
#Maude Baudraz and Caroline Mckeon 
#4.2.2019

## tasks accomplished in this script:
## inflor_phenology comments column created to preserve original inflor_phenology field notes
## new "phenology" column created with standardised levels where descriptions of phenology conform to categories allowed by PPN protocol

## read in the data
## mydata<-read.csv("fullPPN_dataset_2019-01-29_.csv",header=T)

## inital exploration of column
#table(mydata$inflor_phenology)

## There are 300+ levels. Save the information in inflor_phenology column

## create a new column to store standardised levels from inflor_phenology - "phenology"
mydata$inflor_phenology_comments <- mydata$inflor_phenology
names(mydata)[names(mydata) == 'inflor_phenology'] <- 'phenology'
names(mydata)[names(mydata) == 'inflor_phenology_comments'] <- 'inflor_phenology'

## levels allowed on the protocol: 
## bud (prior to fully developed flowers)
## flowering 
## seeds developing
## mature seeds
## seeds dispersed
## no seeds (no signs of seed production in mature inflorescences)

## standardising column levels

#########################################
## reassigning levels with multiple stages based on most mature stage

## if i contains a seeds dispersed == "seeds dispersed"
seeds_dispersed <- c("seeds dispersed","dispersed","disper")
mydata$phenology[grep(paste(seeds_dispersed, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "seeds dispersed"

## if i contains mature seeds == "mature seeds"
mature_seeds <- c("mature seeds","mature", "ripe")
mydata$phenology[grep(paste(mature_seeds, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "mature seeds"

## if i contains a seeds developing == "seeds developing"
seeds_developing <- c("seeds developing","developing","fruiting", "maturing")
mydata$phenology[grep(paste(seeds_developing , collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "seeds developing"

## if i contains a flowering == "flowering"
flowering <- c("flowering","flower", "fl")
mydata$phenology[grep(paste(flowering, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "flowering"

## if i contains a bud == "bud"
mydata$phenology[grep("bud", mydata$inflor_phenology, ignore.case = TRUE)] <- "bud"

## if i contains a early bolt == "early bolt"
early_bolt <- c("early bolt","earlybolt")
mydata$phenology[grep(paste(early_bolt, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "early bolt"

## if i contains a no seeds == "no seeds"
no_seeds <- c("no seed")
mydata$phenology[grep(paste(no_seeds, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "no seeds"

## if i contains a broken == "broken"
broken <- c("damaged", "broken", "eaten", "gone","shrivel", "dried", "dry","abort","inflorescence missing")
mydata$phenology[grep(paste(broken, collapse="|"), mydata$inflor_phenology, ignore.case = TRUE)] <- "damaged"

## if i is 0 = "na"
mydata$phenology[mydata$phenology == 0] <- "na"

## if i contains a non-zero digit = "numeric"
mydata$phenology[grep("[[:digit:]]", mydata$phenology)] <- "numeric"

# if i is Vegetative stage* 
mydata$phenology[mydata$phenology == "Vegetative stage*"] <- "na"


######################################
## now that the levels have been whittled down into more sensible groups, 
## can finalise assignment of some outlier levels without worrying that we are losing information

## if i in pre f = "bud" (as in pre flowering)
mydata$phenology[grep("pre f", mydata$inflor_phenology, ignore.case = TRUE)] <- "bud"

## if i is "not f" = "no seeds" 
mydata$phenology[grep("not f", mydata$inflor_phenology, ignore.case = TRUE)] <- "no seeds" ## YB to check

## if i is ms = "mature seeds"
mydata$phenology[mydata$phenology == "MS"] <- "mature seeds"

## if i is im = "bud" (as in immature)
mydata$phenology[mydata$phenology == "im"] <- "bud"

## if i is f = "floweing" 
mydata$phenology[mydata$phenology == "f"] <- "flowering"

## if i in seeds = "mature seeds"
mydata$phenology[grep("seed", mydata$inflor_phenology, ignore.case = TRUE)] <- "mature seeds"

## final cleaning up of uninformative levels
## if i NOT on meaningful phenological description list = "see comments"
informative <- c("mature seeds", NA, "seeds dispersed", "flowering", "early bolt", 
               "numeric", "bud", "damaged", "seeds developing", 
               "no seeds")
mydata$phenology[-grep(paste(informative, collapse="|"), mydata$phenology)] <- "see comments"

## check levels
table(mydata$phenology)

## create warnings for rows not cleaned by this script
x <- mydata$phenology %in% c("mature seeds", NA, "seeds dispersed", "flowering", "early bolt", 
                             "numeric", "bud", "damaged", "seeds developing", "no seeds", "see comments")
if(any(x != TRUE)) warning("unknown level") 

## create "not in" operator
'%nin%' = Negate('%in%')
## print plant ids where phenology level is not an infomrative level
print(mydata$plant_id[mydata$phenology %nin% c("mature seeds", NA, "seeds dispersed", 
                                                        "flowering", "early bolt", 
                                                        "numeric", "bud", "damaged", 
                                                        "seeds developing", "no seeds", "see comments")])

## clean up
mydata <- droplevels(mydata)
rm(seeds_dispersed,mature_seeds,seeds_developing,flowering,early_bolt,no_seeds,broken,informative)

## FINSIHED 

