### Ruth's code to check popnet dataset gps coordinates.  
### Where are they and do they look correct? 
### With lots of useful code snippets from Maude Baudraz
### 07/01/2019

#### Set working directory for testing

setwd("C:/R/Plantpopnet-Data-Cleaning/Code_chunks_dir_vr1/code_chunks_in_progress")

#
#### Load in current data object
mydata <- read.csv("fullPPN_dataset_2019-01-29_.csv", 
                   na.strings = c("NA", "na", ""))
####
summary(mydata)
names(mydata)


### Summarise columns relating to the coordinate systme

summary(mydata[,c("transect_Lat_start", "transect_Lat_stop",
                  "transect_Lon_start", "transect_Lon_stop")]) 

### Note NA's in the dataset here. 1160 for transect_start (lat and lon)
###    and 1837 for the stop values.. 

### lat stop and lon stop heave more NA's than lat_start and lon_start

#### Looking at the unique values in each column to spot errors. 
unique(mydata$transect_Lat_start)
unique(mydata$transect_Lat_stop)       
unique(mydata$transect_Lon_start)
unique(mydata$transect_Lon_stop)

#### Issues spotted here - ####
        # 1. presence of NA's lat long, both start and  values (more at stop)
        # 2. Odd coordinate system in latitude data "10 S 0607724" 
        # 3. Use of N and E and W to indicate + or - hemispheres.
        # 4. Why are there so many more levels for Lat_start and Lon_stop, 
        #    than the others. Check each transect has only one start and end value
        #    for each of lat and lon. 

##### Checking which sites have NA's for the transect start values #####
#### select rows in mydata where transect_Lat_start = NA
x1 <- which(is.na(mydata$transect_Lat_start))

### dataset containing only those rows with nas in transect coordinates. 
na_coords <- mydata[x1,]
na_coords <- droplevels(na_coords) 

#### vector containing sites with no coordinates given
na_site_coords <- levels(na_coords$site_code)

#### creates dataset containing rows for which transect_Lat_start != NA
has_coords <- droplevels(mydata[-x1,])

#### vector containing sites with no coordinates given
has_site_coords <- levels(has_coords$site_code)
has_site_coords
### 

#### Check which sites have no transect coordinates in either file. 
setdiff(na_site_coords,has_site_coords)
# [1] "ARH" "PM"  "TG"  "TJ"  "ZM" 

##### No transect coordinates were supplied for these 5 sites in the original 
#### files no fix needs to be applied here. Leave data as is. 

### which sites has both transects with and without coordinate data
intersect(na_site_coords,has_site_coords)

#### repeat to check coordinate stop values which are inputted as NA's ####

#### select rows in mydata where transect_Lon_stop = NA
x3 <- which(is.na(mydata$transect_Lon_stop))

### dataset containing only those rows with nas in transect coordinates. 
na_coords <- mydata[x3,]
na_coords <- droplevels(na_coords) 

#### vector containing sites with no coordinates given
na_site_coords <- levels(na_coords$site_code)

#### creates dataset containing rows for which transect_Lon_Stop != NA
has_coords <- droplevels(mydata[-x3,])

#### vector containing sites with no coordinates given
has_site_coords <- levels(has_coords$site_code)
has_site_coords
### 

#### Check which sites have no transect coordinates in either file. 
setdiff(na_site_coords,has_site_coords)
# [1] "ARH" "PA"  "PM"  "TG"  "TJ"  "ZG"  "ZM" 

#### comparing with above 5 sites with no lat_start values the sites
# "PA", and "ZG", have transect start values but no stop values recorded. 

#### take quick look at these. 

summary(droplevels(mydata[mydata$site_code == "PA",])) 
### Note all NA's for transect_Lat_stop and transect_Lon_stop

summary(droplevels(mydata[mydata$site_code == "ZG",])) 
### Note all NA's for transect_Lat_stop and transect_Lon_stop

#### Note: This error needs to be referred to Alain to request for site managers.


#### Extra checking  for site "BG" some rows with coordinates and some without ####

BG <- droplevels(mydata[mydata$site_code == "BG",])
names(BG)
str(BG)

#### check transect related columns in the "BG"
summary(BG[,c("transect_Lat_start", "transect_Lat_stop",
           "transect_Lon_start", "transect_Lon_stop")])
#### Note 170 = NA

###########
table(BG$transect, BG$transect_Lat_start)
table(BG$transect_Lat_start, BG$c_year)

### Looks like the wasn't recorded in 2015, checked with Alain this is true

#### NOTE THERE ARE STILL ROWS IN THIS DATASET FOR INDIVIDUAL FLOWER STEMS 
#### WHERE EVERYTHING ELSE IS NA

BG$transect[BG$c_year == "2015"]

###  Create dataset for just 2016
BG_2016 <- BG[BG$c_year == "2016",]

#### Create dataset of just T1 in 2016
BG_2016_T1 <- droplevels(BG_2016[BG_2016$transect == "T1",])
summary(BG_2016_T1$transect)
levels(BG_2016_T1$transect_Lat_start) # Returns -  "61.44802"
levels(BG_2016_T1$transect_Lon_start) # Returns -  "7.480596"
levels(BG_2016_T1$transect_Lat_stop) # Returns -  "61.448075"
levels(BG_2016_T1$transect_Lon_stop) # Returns -  "7.480565"

### Add coordinates for T1 in BG in mydata in this data. 

#### find rows in mydata for site code BG, transect T1 and year 2015
x2 <- which(mydata$site_code == "BG" & mydata$transect == "T1" & mydata$c_year == "2015")

#### Add info for this transect start stops from 2016 info 

mydata$transect_Lat_start[x2] <-  levels(BG_2016_T1$transect_Lat_start) 
mydata$transect_Lon_start[x2] <- levels(BG_2016_T1$transect_Lon_start) # Returns -  "7.480596"
mydata$transect_Lat_stop[x2] <- levels(BG_2016_T1$transect_Lat_stop) # Returns -  "61.448075"
mydata$transect_Lon_stop[x2] <- levels(BG_2016_T1$transect_Lon_stop) # 

####  Next step repeat above for T2.. 

BG_2016_T2 <- droplevels(BG_2016[BG_2016$transect == "T2",])
levels(BG_2016_T2$transect_Lat_start)
levels(BG_2016_T2$transect_Lon_start)
levels(BG_2016_T2$transect_Lat_stop)
levels(BG_2016_T2$transect_Lon_stop)

####
x3 <- which(mydata$site_code == "BG" & mydata$transect == "T2" & mydata$c_year == "2015")
###
mydata$transect_Lat_start[x3] <-  levels(BG_2016_T2$transect_Lat_start) # "61.448026"
mydata$transect_Lon_start[x3] <- levels(BG_2016_T2$transect_Lon_start) # Returns - "7.480639"
mydata$transect_Lat_stop[x3] <- levels(BG_2016_T2$transect_Lat_stop) # Returns -  "61.448072"
mydata$transect_Lon_stop[x3] <- levels(BG_2016_T2$transect_Lon_stop) # Returns - "7.480614"
####


#### Check California site with wrong coordinate system - site = PC ####

summary(droplevels(mydata[mydata$site_code == "PC",])) 
## Note: For reference only 1 transect at this site. 


#2. What format is 10 S 0607724 4263981 in ?
#A convertor online (http://www.earthpoint.us/Convert.aspx) says it may be in California, at 38.5177007°, -121.7643222: 
#Indeed, the river next to the plot is Putah Creek and the plot ID is PC
#Indeed site name is in California in main site list and coordinates match these. 

### Set transect columns to 'chr' for easier manipulation 

mydata$transect_Lat_start <- as.character(mydata$transect_Lat_start)
mydata$transect_Lat_stop  <- as.character(mydata$transect_Lat_stop)
mydata$transect_Lon_start <- as.character(mydata$transect_Lon_start)
mydata$transect_Lon_stop <- as.character(mydata$transect_Lon_stop)

str(mydata)

### change coords based on online coordinate converter as follows.
# names(mydata)
mydata[mydata$site_code == "PC","transect_Lat_start"] <- "38.5177007"
mydata[mydata$site_code == "PC","transect_Lon_start"]  <- "-121.7643222"
mydata[mydata$site_code == "PC","transect_Lat_stop"] <- "38.5176821"
mydata[mydata$site_code == "PC","transect_Lon_stop"]  <- "-121.7642651"
# 


#### 3. Check for N E etc in the transect coordinates and fix these. ####

unique(mydata$transect_Lat_start)

#### Copied directly from Maude's code.  Thanks Maude. : )
#1: some have the N and E or N and W on.
#52.144308 N            -8.948419W is in Cork, just suppress the N and W
#36.37866 N          -121.56724 W is in estonia
#53.05706 N           009.51672 W is in germany... 
#46.74930            17.23734 is in Romania, in a city?
#All seem likely options; 


#### Grab all rows with an N in the latitude coordinates, for examination
N_issues <-droplevels(mydata[which(grepl("N", mydata$transect_Lat_start)==T),])

unique(N_issues$site_code)

# [1] CH    EE    HAS   HU    IO    OR_SS

#### check each location in turn and match against site level dataset. 

#### read in  site level dataset. ####
sites <- read.csv("Coordinates_Feb2019_site_level.csv")

#### check details of CH , contains letter N ####

CH <- droplevels(mydata[mydata$site_code == "CH",])

### look at coords of site 
sites[sites$site_code == "CH",9:10]
#    latitude longitude
#    52.14431 -8.948419
#

summary(CH)
table(CH$transect,CH$c_year)

table(CH$transect_Lat_start, CH$transect)

#            T1
#52.13891    414
#52.144308 N 193

unique(CH$transect_Lat_start)
# "52.144308 N" "52.13891"  
unique(CH$transect_Lon_start)
# [1] "-8.948419W" "-8.95128" 

#### why did these coords change between years. Ask Alain about this. ####

table(CH$transect_Lat_start,  CH$c_year)
table(CH$transect_Lon_start,  CH$c_year)

### Location matches site location, 
# and also to map location coolclough in Cork
# Drop " N" and keep coordinates (see below)

#### note some of these have " N" and some just "N", 

#### EE check for letters in coordinates ####

EE <- droplevels(mydata[mydata$site_code == "EE",])
summary(EE)
sites[sites$site_code == "EE",]
#  latitude longitude
#  58.71712   23.7716
#

table(EE$transect_Lat_start, EE$transect)
### these are very close to the site coords, but have an " N"

table(EE$transect_Lon_start, EE$transect)
### these are very close to the site coords, but have an " E"

### Remove both the " N" and " E" below. 

####

##  Location matches site location, 
# and also to map location estonia
# Drop " N" and  " E" and keep coordinates (see below)

#### HAS check for letters in coordinates ####


HAS <- droplevels(mydata[mydata$site_code == "HAS",])

sites[sites$site_code == "HAS",]
#  latitude longitude
#  36.37865  -121.5672
#

table(HAS$transect_Lat_start, HAS$transect)
### these are very close to the site coords, but have an " N"

table(HAS$transect_Lon_start, HAS$transect)
### these are very close to the site coords, but have an " W"

### Remove both the " N" and " W" below. 

### Location matches site location, 
# and also to map location HAS in California
# Drop " N" and " W" and keep coordinates (see below)

#### IO check for letters in coordinates ####
IO <- droplevels(mydata[mydata$site_code == "IO",])

sites[sites$site_code == "IO",]
#  latitude longitude
#  53.00805  -9.31444

### Site - coordinates are in County Clare not on inisOirr
#

### transect coordinates are on InisOirr. Site coordinate sheet contains typo?

table(IO$transect_Lat_start, IO$transect)
### these are very close to the site coords, but have an " N" in one of the years

table(IO$transect_Lon_start, IO$transect)
### these are very close to the site coords, but have an " W" in one of the years
### "009.51672 W"  needs to be changed to "-9.51672"

table(IO$transect_Lon_stop, IO$transect)
### these are very close to the site coords, but have an " W" in one of the years
### "009.51671 W"  needs to be changed to "-9.51671"
unique(IO$transect_Lon_stop)

### this is idiosyncratic and won't be bit up by the later general fix. 
### So fix this here instead. 

N_issues <-droplevels(mydata[which(grepl("N", mydata$transect_Lat_start)==T),])

mydata$transect_Lon_stop[which(mydata$transect_Lon_stop == "009.51671 W")] <- "-9.51671"
mydata$transect_Lon_start[which(mydata$transect_Lon_start == "009.51672 W")] <- "-9.51672"

### Remove both the " N" and "E", with general fix below. 

#### OR_SS check for letters in coordinates ####
OR_SS <- droplevels(mydata[mydata$site_code == "OR_SS",])

sites[sites$site_code == "OR_SS",]
#  latitude longitude
#  59.26924  18.10113

### coordinates match to expected location on map - Stockholm Sweden. 
#

table(OR_SS$transect_Lat_start, OR_SS$transect)
### these are very close to the site coords, but have an " N" in one of the years

table(OR_SS$transect_Lon_start, OR_SS$transect)
### these are very close to the site coords, but have an " E" in on of the years

### Remove both the "N" and "E" below. 

#### Code to remove "N", "W", "E", "S" where present.


mydata[,c("transect_Lat_start", "transect_Lon_start", "transect_Lat_stop",
          "transect_Lon_stop")] 

#### Remove spaces from "transect_Lat_start", "transect_Lon_start",
#### "transect_Lon_stop", "transect_Lat_stop"


### create vector of letters and characters to remove, this will be 'j' in the loop
replace1 <- c(" ", "N", "E", "S", "W")

### create vector columns to remove 'replace1' from, this wil be 'i' in the loop
cols_trans <- c("transect_Lat_start", "transect_Lon_start",
                "transect_Lon_stop", "transect_Lat_stop")


##### Loop to replace " " and letters N,S,E,W from coordinates. 

for (j in replace1) {
  
for(i in cols_trans) {
  mydata[,i] <- gsub(pattern = j, 
             x = mydata[,i], 
            "")
  print(i)
}
  print(j)
}

### check no spaces or letters using 
unique(mydata[,cols_trans])
#######

#### return transect cols to numeric 

#### this returns character as the structure for all columns
str(mydata[,cols_trans])

### this converts each transect column to numeric 
for(i in cols_trans) {
  mydata[,i] <- as.numeric( mydata[,i]) 
  print(i)
}

### This check should return as numeric for all columns. 
str(mydata[,cols_trans])

##### Check each Transect within each site has only one set of coordinates ####

library("dplyr")

#### For each transect within each site, 
#### return a table containing the count of unique transect start and stop points 

#### Checking why transects have multiple coordinates ####

check_table <- as.data.frame( mydata %>%
  group_by(site_code, transect) %>%
     summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
               n_trans_lat_stop = n_distinct(transect_Lat_stop),
               n_trans_lon_start = n_distinct(transect_Lon_start),
               n_trans_lon_stop = n_distinct(transect_Lon_stop)))

#### If these are correct, there should only be one set of coordinates 
#### per transect (i.e. the sum of the rows in the above table will be 4)

### To get rows with errors.... 
### Return rows in the table where the number of transect start and stop 
#### combinations is != 4)

err1 <- which(rowSums(check_table[,3:6]) != 4)

tocheck2 <- check_table[err1,]

site_checks <- unique(tocheck2$site_code)

#### View each of these turn in the mydata object 
site_checks[1]
CDF <- droplevels(mydata[mydata$site_code == "CDF",])
table(CDF$transect_Lat_start, CDF$transect)
table(CDF$transect_Lat_stop, CDF$transect)
table(CDF$transect_Lon_start, CDF$transect)

table(CDF$transect_Lat_stop, CDF$c_year)
table(CDF$transect_Lat_start, CDF$c_year)
table(CDF$transect_Lon_start, CDF$c_year)

length(which(CDF$transect_Lat_stop == CDF$transect_Lat_start))
nrow(CDF)

#### 2 different start points very close together. For T1. No obvious reason. 
#### These occur in same census year. Also latitude start and stop values are the same
#### This could mean transects were running directly North/South. 
#### but is odd combined with other errors
#### 
sites[sites$site_code == "CDF",]

##### Flag this for checking with author. 

# site_code native demographics genetics round country         location region latitude
# 9       CDF native            Y        Y     2 Ireland DistilleryFields Europe 51.89961
# longitude coordinator  X X.1 X.2
# 9 -8.485889     Wingler NA  NA    

#### next one is CH - Coolclough

site_checks[2]

#### this is Coolclough in cork. Checking above revealed that T1
#### differs in coordinates between years, to be checked with site coordinator

#### next one - GB

site_checks[3]

GB <- droplevels(mydata[mydata$site_code == "GB",])

GB %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
GB %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

#### looks like transect 2 was moved between years or there's an error. 
### Check with site coordinator? 

#### next one - HR

site_checks[4]

HR <- droplevels(mydata[mydata$site_code == "HR",])

HR %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
HR %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))


table(HR$transect_Lat_start, HR$c_year)

#         2015 2016
# 16.34465  132    0
# 46.34465    0  204
table(HR$transect_Lat_stop, HR$c_year)
#         2015 2016
# 16.34465  132    0
# 46.34465    0  204

sites[sites$site_code == "HR",]

#### typo?  16.34465 should be 46.34465 in 2015. Otherwise it doesn't 
#### match known site location. Transect starts and stops are the same at this site?

#### fix transect latitudes in mydata
mydata[mydata$site_code == "HR","transect_Lat_start"] <- 46.34465
mydata[mydata$site_code == "HR","transect_Lat_stop"]  <- 46.34465

##### next one - SC

site_checks[5]

SC <- droplevels(mydata[mydata$site_code == "SC",])

SC %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
SC %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### issue is with T2 in 2016

SCT2 <- SC[SC$transect == "T2",]

table(SCT2$transect_Lat_start, SCT2$c_year)

#          2015 2016 2017
# 53.07223   66   87   85
# 53.07226    0    1    0

table(SCT2$transect_Lat_stop, SCT2$c_year)


table(SCT2$transect_Lon_start, SCT2$c_year)

#            2015 2016 2017
# -8.99239    0   87   85
# -8.99231    0    1    0
# -8.9923    66    0    0

table(SCT2$transect_Lat_stop, SCT2$c_year)

#           2015 2016 2017
# 53.07221   66   87   85
# 53.07224    0    1    0

#### small location errors in single row in 2016.
#### These could be fixed by rounding, or setting to the most common value?


##### next one - SI
length(site_checks)

SI <- droplevels(mydata[mydata$site_code == "SI",])


SI %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
SI %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### issue is with T2 in 2016 . Examine this..

SIT2 <- SI[SI$transect == "T2",]

table(SIT2$transect_Lat_start, SIT2$c_year)

table(SIT2$transect_Lat_stop, SIT2$c_year)

table(SIT2$transect_Lon_stop, SIT2$c_year)

table(SIT2$transect_Lon_start, SIT2$c_year)

#### Small location errors in single row in 2016.
#### These could be fixed by rounding, or setting to the most common value?
#### Why does this line differ?


##### next one - TNC
site_checks[6]

TNC <- droplevels(mydata[mydata$site_code == "TNC",])


TNC %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
TNC %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### only one transect at this site - T1 in 2016 . Examine this..

unique(TNC$transect_Lat_start)

table(TNC$transect_Lat_start, TNC$c_year)
table(TNC$transect_Lat_stop, TNC$c_year)
### strange coordinates for 14 rows in 2016. Off by 100's of kms!
### Set these to match the other coordinates for this transect?

table(TNC$transect_Lon_start, TNC$c_year)
table(TNC$transect_Lon_stop, TNC$c_year)
### strange coordinates for 14 sites in 2016. Off by a small amount 
### possible error from copying into cells below in excel? 

#### Location errors in 14 rows in 2016. Latitude errors are very large.
#### Fix by setting these to match the other 513 rows?


##### next one - TNM
site_checks[8]

TNM <- droplevels(mydata[mydata$site_code == "TNM",])


TNM %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
TNM %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### only one transect at this site - T1 in 2016 . Examine this..

unique(TNM$transect_Lon_start)

table(TNM$transect_Lon_start, TNM$c_year)
table(TNM$transect_Lon_stop, TNM$c_year)
### TNM lonitude values are missing the '-' (negative direction)
### in 2017 

#### Fix as follows 

mydata[mydata$site_code == "TNM","transect_Lon_start"] <- -7.61744
mydata[mydata$site_code == "TNM","transect_Lon_stop"]  <-  -7.61743


##### next one - TW
site_checks[9]

TW <- droplevels(mydata[mydata$site_code == "TW",])


TW %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
TW %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### only one transect at this site - T1 in 2016 . Examine this..

unique(TW$transect_Lat_start[TW$c_year == "2015"])
unique(TW$transect_Lon_stop[TW$c_year == "2015"])
#### This looks like a 'fill cells' error from excel in 2015 data, 
### numbers are increasing sequentially

#### Replace these with the values to Lat Start and Lon start from 2016

#### Fix as follows 

TW_LatStr <- unique(TW$transect_Lat_start[TW$c_year == "2016"])
TW_LonStp <- unique(TW$transect_Lon_stop[TW$c_year == "2016"])


mydata[mydata$site_code == "TW","transect_Lat_start"] <- TW_LatStr 
mydata[mydata$site_code == "TW","transect_Lon_stop"]  <-  TW_LonStp


##### next one - UR
site_checks[10]

UR <- droplevels(mydata[mydata$site_code == "UR",])


UR %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

UR %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### Issues with all five transects! Seems to happen in 2017 only, but check.. 

UR16 <- UR[UR$c_year == "2016",]
UR17 <- UR[UR$c_year == "2017",]
table(UR16$transect_Lat_start, UR16$transect)
table(UR17$transect_Lat_start, UR17$transect)

table(UR16$transect_Lat_stop, UR16$transect)
table(UR17$transect_Lat_stop, UR17$transect)

table(UR16$transect_Lon_stop, UR16$transect)
table(UR17$transect_Lon_stop, UR17$transect)

table(UR16$transect_Lon_start, UR16$transect)
table(UR17$transect_Lon_start, UR17$transect)

### big mess in 2017.. check if the plantID's also move?

table(UR17$plant_id, UR17$transect)

check1 <- UR %>%
  group_by(plant_id) %>%
  summarise(n_trans = n_distinct(transect))

which(check1[,2] != 1)

check1[116,]

### Fix by assigning transect coordinates from 2016 to transects in 2017. 

#### create vector of transect numbers 
trans_ns <- unique(UR$transect)
trans_ns
#### create vector of columns to fix 

#### loop to start all transect coordinates based on 2016 data. 

#### transect_Lat_start

for(i in trans_ns){
coordval  <- unique(UR$transect_Lat_start[UR$c_year == "2016" & UR$transect == i])
mydata[mydata$site_code == "UR" & mydata$transect == i, "transect_Lat_start"] <- coordval 
print(i)
print(coordval)
}

#### transect_Lat_stop

for(i in trans_ns){
  coordval  <- unique(UR$transect_Lat_stop[UR$c_year == "2016" & UR$transect == i])
  mydata[mydata$site_code == "UR" & mydata$transect == i, "transect_Lat_stop"] <- coordval 
  print(i)
  print(coordval)
}

#### transect_Lon_start

for(i in trans_ns){
  coordval  <- unique(UR$transect_Lon_start[UR$c_year == "2016" & UR$transect == i])
  mydata[mydata$site_code == "UR" & mydata$transect == i, "transect_Lon_start"] <- coordval 
  print(i)
  print(coordval)
}

#### transect_Lon_stop

for(i in trans_ns){
  coordval  <- unique(UR$transect_Lon_stop[UR$c_year == "2016" & UR$transect == i])
  mydata[mydata$site_code == "UR" & mydata$transect == i, "transect_Lon_stop"] <- coordval 
  print(i)
  print(coordval)
}


#### recheck by creating the UR dataset again from mydata and summarising. 
UR <- droplevels(mydata[mydata$site_code == "UR",])

#### This looks good now
UR %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

##### next one - VA

site_checks[11]

VA <- droplevels(mydata[mydata$site_code == "VA",])

VA %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
VA %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

### issue is with transect_Lat_start values in T1 in 2016

table(VA$transect_Lat_start, VA$c_year)

# #                 2016 2017
# 37.95865  123  123
# 37.95866    1    0
# 37.95867    1    0
# 37.95868    1    0
# 37.95869    1    0
# 37.9587     1    0
# 37.95871    1    0
# 37.95872    1    0
# 37.95873    1    0
# 37.95874    1    0
# 37.95875    1    0
# 37.95876    1    0
# 37.95877    1    0
# 37.95878    1    0
# 37.95879    1    0
# 37.9588     1    0
# 37.95881    1    0
# 37.95882    1    0

#### Looks like a fill cells below error from excel again. Fix by using coordinates from 2017. 

#### Fix as follows 

VA_tr_start  <- unique(VA$transect_Lat_start[VA$c_year == "2017"])
mydata[mydata$site_code == "VA" & mydata$transect == "T1", "transect_Lat_start"] <- VA_tr_start 

##### RECHECK WHICH TRANSECTS STILL NEED FIXES


#### Checking why transects have multiple coordinates ####

check_table <- as.data.frame( mydata %>%
                                group_by(site_code, transect) %>%
                                summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
                                          n_trans_lat_stop = n_distinct(transect_Lat_stop),
                                          n_trans_lon_start = n_distinct(transect_Lon_start),
                                          n_trans_lon_stop = n_distinct(transect_Lon_stop)))

#### If these are correct, there should only be one set of coordinates 
#### per transect (i.e. the sum of the rows in the above table will be 4)

### To get rows with errors.... 
### Return rows in the table where the number of transect start and stop 
#### combinations is != 4)

err1 <- which(rowSums(check_table[,3:6]) != 4)

tocheck2 <- check_table[err1,]

site_checks <- unique(droplevels(tocheck2$site_code))

##### Notes 24/02/2018 #### 

#### Fixes have been applied here as far as I can, 
#### but for the following sites more information is needed to apply the fixes. 

unique(site_checks)
### CDF CH  GB  SC  SI  TNC