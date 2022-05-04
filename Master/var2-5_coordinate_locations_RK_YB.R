### Ruth's code to check popnet dataset gps coordinates.  
### Where are they and do they look correct? 
### With lots of useful code snippets from Maude Baudraz
### 07/01/2019
### Reviewed 11/04/19 YMB - new file saved
### Reviewed & edited 20/06/19 YMB


#summary(mydata)
#names(mydata)


### Summarise columns relating to the coordinate systme

#summary(mydata[,c("transect_Lat_start", "transect_Lat_stop",
 #                 "transect_Lon_start", "transect_Lon_stop")]) 

### Note NA's in the dataset here. 
### lat stop and lon stop have more NA's than lat_start and lon_start

#### Looking at the unique values in each column to spot errors. 
unique(as.character(mydata$transect_Lat_start))
unique(as.character(mydata$transect_Lat_stop))      
unique(as.character(mydata$transect_Lon_start))
unique(as.character(mydata$transect_Lon_stop))

#### Issues spotted here - ####
        # Error 1. Sites without any coordinates
        # Error 2. Sites with and without coordinates
        # Error 3. Odd coordinate system in latitude data "10 S 0607724" 
        # Error 4. on Mac (YB laptop) there are some odd transect_lat_start values for VA site e.g. "\xa037.95865" or "\24037.95865" - perhaps a Mac/PC encoding issue? (YB) 
        # Error 5.  Use of N and E and W to indicate + or - hemispheres.
        # Error 6. Why are there so many more levels for  Lon_stop, 
        #    than the others. Check each transect has only one start and end value
        #    for each of lat and lon. Looks like lots of similar values is an excel copy down error.

#################################################################################################################
## Error 1
##### Checking which sites have NA's for the transect start values #####
#### select rows in mydata where transect_Lat_start = NA
x1 <- which(is.na(mydata$transect_Lat_start))

### dataset containing only those rows with nas in transect coordinates. 
na_coords <- mydata[x1,]
na_coords <- droplevels(na_coords) 

#### vector containing site codes with no coordinates given
na_site_coords <- levels(na_coords$site_code)

#### creates dataset containing rows for which transect_Lat_start != NA
has_coords <- droplevels(mydata[-x1,])

#### vector containing sites with coordinates 
has_site_coords <- levels(has_coords$site_code)
#has_site_coords
### 

#### Check which sites have no transect coordinates in either file. 
sites_no_coords <- setdiff(na_site_coords,has_site_coords)
# [1] "ARH"   "CPA"   "PM"    "SBK"   "SW242" "SW729" "TJ"    "ZM"  

##### No transect coordinates were supplied for these sites in the original 
#### files no fix needs to be applied here. Leave data as is. 
print(sites_no_coords)
if(length(sites_no_coords>0)) {
  warning("Error 1: the sites above have no transect coordinates - contact site coordinator. No fix applied.")
  
}

### which sites has both transects with and without coordinate data
sites_with_without <- intersect(na_site_coords,has_site_coords)
print(sites_with_without)
if(length(sites_with_without) > 0) {
  warning("Error 2: these sites have transects both with and without coordinate data")
  
}
#BG SBK

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
#has_site_coords
### 

#### Check which sites have no transect coordinates in either file. 
#setdiff(na_site_coords,has_site_coords)
# [1] "ARH"   "PA"    "PM"    "SW242" "SW729" "TG"    "TJ"    "ZG"    "ZM" 

#### comparing with above sites with no lat_start values the sites
# "PA", and "ZG", have transect start values but no stop values recorded. 

#### take quick look at these. 

#summary(droplevels(mydata[mydata$site_code == "PA",])) 
### Note all NA's for transect_Lat_stop and transect_Lon_stop

# summary(droplevels(mydata[mydata$site_code == "ZG",])) 
### Note all NA's for transect_Lat_stop and transect_Lon_stop

#### Note: YB 17/06/19 Transect stop co-ords not necessary - no further action needed.
#######################################################################################################

#### Error 2: Extra checking  for site "BG" some rows with coordinates and some without ####

BG <- droplevels(mydata[mydata$site_code == "BG",])
BG$transect_Lat_start <- as.factor(BG$transect_Lat_start)
BG$transect_Lon_start <- as.factor(BG$transect_Lon_start)
BG$transect_Lat_stop <- as.factor(BG$transect_Lat_stop)
BG$transect_Lon_stop <- as.factor(BG$transect_Lon_stop)
#names(BG)
#str(BG)

#### check transect related columns in the "BG"
summary(BG[,c("transect_Lat_start", "transect_Lat_stop",
          "transect_Lon_start", "transect_Lon_stop")])

#### Note 170 = NA

###########
#table(BG$transect, BG$transect_Lat_start)
#table(BG$transect_Lat_start, BG$c_year)

### Looks like the transect start coords weren't recorded in 2015, checked with Alain this is true

#### NOTE THERE ARE STILL ROWS IN THIS DATASET FOR INDIVIDUAL FLOWER STEMS 
#### WHERE EVERYTHING ELSE IS NA

# BG$transect[BG$c_year == "2015"]

###  Create dataset for just 2016
BG_2016 <- BG[BG$c_year == "2016",]

#### Create dataset of just T1 in 2016
BG_2016_T1 <- droplevels(BG_2016[BG_2016$transect == "T1",])
#summary(BG_2016_T1$transect)
#levels(BG_2016_T1$transect_Lat_start) # Returns -  "61.44802"
#levels(BG_2016_T1$transect_Lon_start) # Returns -  "7.480596"
#levels(BG_2016_T1$transect_Lat_stop) # Returns -  "61.448075"
#levels(BG_2016_T1$transect_Lon_stop) # Returns -  "7.480565"

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
#levels(BG_2016_T2$transect_Lat_start)
#levels(BG_2016_T2$transect_Lon_start)
#levels(BG_2016_T2$transect_Lat_stop)
#levels(BG_2016_T2$transect_Lon_stop)

####
x3 <- which(mydata$site_code == "BG" & mydata$transect == "T2" & mydata$c_year == "2015")
###
{
  mydata$transect_Lat_start[x3] <-  levels(BG_2016_T2$transect_Lat_start) # "61.448026"
  mydata$transect_Lon_start[x3] <- levels(BG_2016_T2$transect_Lon_start) # Returns - "7.480639"
  mydata$transect_Lat_stop[x3] <- levels(BG_2016_T2$transect_Lat_stop) # Returns -  "61.448072"
  mydata$transect_Lon_stop[x3] <- levels(BG_2016_T2$transect_Lon_stop) # Returns - "7.480614"

  print("Error 2 fixed for BG")
}
##################################################################################################
## ERROR 3 - wrong coordinate systems

#### Check California site with wrong coordinate system - site = PC ####

#summary(droplevels(mydata[mydata$site_code == "PC",])) 
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

#str(mydata)

### change coords based on online coordinate converter as follows.
# names(mydata)
{
  mydata[mydata$site_code == "PC","transect_Lat_start"] <- "38.5177007"
  mydata[mydata$site_code == "PC","transect_Lon_start"]  <- "-121.7643222"
  mydata[mydata$site_code == "PC","transect_Lat_stop"] <- "38.5176821"
  mydata[mydata$site_code == "PC","transect_Lon_stop"]  <- "-121.7642651"
  
  print("Error 3 fixed for PC")
}

# 

###################################################################################################
#### Error 4  on Mac there are some odd transect_lat_start values for VA site e.g. \24037.95865 & JE site
## - perhaps a Mac/PC encoding issue? (YB)
## YB 20/06/19 this problem occurs on YB laptop (Mac) but not on desktop (Mac)!
err4 <- "Error 4: site with strange encoding in transect latitude"
err4b <- "Error 4: site with strange encoding in transect longitude"
x3 <- grepl("\240", mydata$transect_Lat_start)
weird_sites <- droplevels(unique(mydata$site_code[x3]))

if(length(weird_sites) > 0 ) {
  warning(err4)
  print(weird_sites)
  mydata$transect_Lat_start[x3] <- gsub("\240", "", mydata$transect_Lat_start[x3] )
  print("Error 4 fixed for transect_lat_start weird sites above")
}

## all other coordinate columns checked for this error

x5 <- grepl("50\260", mydata$transect_Lat_start)
weird_sites2 <- droplevels(unique(mydata$site_code[x5]))  ##JE

if(length(weird_sites2) > 0) {
  warning(err4)
  print(weird_sites2)
  mydata$transect_Lat_start[x5] <- 50.95192
  print("Error 4 fixed for weird sites above")
  
}

x6 <- grepl("50\260", mydata$transect_Lat_stop)
weird_sites3 <- droplevels(unique(mydata$site_code[x6])) #JE
if(length(weird_sites3) > 0) {
  warning(err4)
  print(weird_sites3)
  mydata$transect_Lat_stop[x6] <- 50.95192
  print("Error 4 fixed for weird sites above")
}

x7 <- grepl("11\260", mydata$transect_Lon_start)
weird_sites4 <- droplevels(unique(mydata$site_code[x7])) #JE
if(length(weird_sites4) > 0) {
  warning(err4b)
  print(weird_sites4)
  mydata$transect_Lon_start[x7] <- 11.62282
  print("Error 4 fixed for weird sites above")
}

x8 <- grepl("11\260", mydata$transect_Lon_stop)
weird_sites5 <- droplevels(unique(mydata$site_code[x8])) #JE
if(length(weird_sites5) > 0) {
  warning(err4b)
  print(weird_sites5)
  mydata$transect_Lon_stop[x8] <- 11.62282
  print("Error 4 fixed for weird sites above")
}

## all other coordinate columns checked for this error

########################################################################################
#### Error 5 . Check for N E etc in the transect coordinates and fix these. ####

#unique(mydata$transect_Lat_start)

#### Copied directly from Maude's code.  Thanks Maude. : )
#1: some have the N and E or N and W on.
#52.144308 N            -8.948419W is in Cork, just suppress the N and W
#36.37866 N          -121.56724 W is in estonia
#53.05706 N           009.51672 W is in germany... 
#46.74930            17.23734 is in Romania, in a city?
#All seem likely options; 


#### Grab all rows with an N in the latitude coordinates, for examination
N_issues <-droplevels(mydata[which(grepl("N", mydata$transect_Lat_start)==T),])


N_issues_sites <- unique(N_issues$site_code)
if(length(N_issues_sites) > 0 ) {
  print(N_issues_sites)
  warning("N included in transect latitude start for the sites above")
}

# [1] BL    EE    GU    HAS   HUFZ  IO    OR_SS

#### check each location in turn and match against site level dataset. 

#### read in  site level dataset. ####

sites <- sitedata

#### EE check for letters in coordinates ####

EE <- droplevels(mydata[mydata$site_code == "EE",])
# summary(EE)
# sites[sites$site_code == "EE",]
#  latitude longitude
#  58.71712   23.7716
#

#table(EE$transect_Lat_start, EE$transect)
### these are very close to the site coords, but have an " N"

#table(EE$transect_Lon_start, EE$transect)
### these are very close to the site coords, but have an " E"

### Remove both the " N" and " E" below. 

####

##  Location matches site location, 
# and also to map location estonia
# Drop " N" and  " E" and keep coordinates (see below)

#### HAS check for letters in coordinates ####


HAS <- droplevels(mydata[mydata$site_code == "HAS",])

#sites[sites$site_code == "HAS",]
#  latitude longitude
#  36.37865  -121.5672
#

#table(HAS$transect_Lat_start, HAS$transect)
### these are very close to the site coords, but have an " N"

#table(HAS$transect_Lon_start, HAS$transect)
### these are very close to the site coords, but have an " W"

### Remove both the " N" and " W" below. 

### Location matches site location, 
# and also to map location HAS in California
# Drop " N" and " W" and keep coordinates (see below)

#### IO check for letters in coordinates ####
IO <- droplevels(mydata[mydata$site_code == "IO",])

# sites[sites$site_code == "IO",]
#  latitude longitude
#  53.00805  -9.31444


# table(IO$transect_Lat_start, IO$transect)
### these are very close to the site coords, but have an " N" in one of the years

# table(IO$transect_Lon_start, IO$transect)
### these are very close to the site coords, but have an " W" in one of the years
### "009.51672 W"  needs to be changed to "-9.51672"

# table(IO$transect_Lon_stop, IO$transect)
### these are very close to the site coords, but have an " W" in one of the years
### "009.51671 W"  needs to be changed to "-9.51671"
# unique(IO$transect_Lon_stop)

### this is idiosyncratic and won't be bit up by the later general fix. 
### So fix this here instead. 

N_issues <-droplevels(mydata[which(grepl("N", mydata$transect_Lat_start)==T),])

{
  mydata$transect_Lon_stop[which(mydata$transect_Lon_stop == "009.51671 W")] <- "-9.51671"
  mydata$transect_Lon_start[which(mydata$transect_Lon_start == "009.51672 W")] <- "-9.51672"
  warning("site IO fixed here, W removed from coordinates")
}


### Remove both the " N" and "E", with general fix below. 

#### OR_SS check for letters in coordinates ####
OR_SS <- droplevels(mydata[mydata$site_code == "OR_SS",])

#sites[sites$site_code == "OR_SS",]
#  latitude longitude
#  59.26924  18.10113

### coordinates match to expected location on map - Stockholm Sweden. 
#

#table(OR_SS$transect_Lat_start, OR_SS$transect)
### these are very close to the site coords, but have an " N" in one of the years

#table(OR_SS$transect_Lon_start, OR_SS$transect)
### these are very close to the site coords, but have an " E" in on of the years

### Remove both the "N" and "E" below. 

#### Code to remove "N", "W", "E", "S" where present.


#mydata[,c("transect_Lat_start", "transect_Lon_start", "transect_Lat_stop",
#            "transect_Lon_stop")] 

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
  # print(i)
 
}
  # print(j)
  warning("Error 5 fixed: all letters and spaces removed from coordinates columns")
}

### check no spaces or letters using 
unique(mydata[,cols_trans])

#Unidentified Issue with site VA transect_Lat_start
VA <- droplevels(mydata[mydata$site_code == "VA",])
#table(VA$transect_Lat_start, VA$transect)
#                    T1
#  <U+00A0>37.95865 263

#table(VA$transect_Lat_stop, VA$transect)
#          T1
#37.958633 263

#replace T1 start with 37.95865
mydata[mydata$site_code == "VA","transect_Lat_start"] <- 37.95865


##########

#### return transect cols to numeric 

#### this returns character as the structure for all columns
# str(mydata[,cols_trans])
 
### this converts each transect column to numeric 
for(i in cols_trans) {
  mydata[,i] <- as.numeric( mydata[,i]) 
 # print(i)
}
### Check remaining NAs
x1 <- which(is.na(mydata$transect_Lat_start))

### dataset containing only those rows with nas in transect coordinates. 
#na_coords <- mydata[x1,]
#na_coords <- droplevels(na_coords)

unique(levels(na_coords$site_code))
#[1] "ARH"   "CPA"   "PM"    "SBK"   "SW242" "SW729" "TG"    "TJ"    "ZM"
########### 9 sites above with no coords, site JE has had NAs cooerced. Problem is resolved further down in site checks. 



### This check should return as numeric for all columns. 
# str(mydata[,cols_trans])

#####################################################################################
##### Error 6. Check each Transect within each site has only one set of coordinates ####

#library("dplyr")

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
# [1]  CDF  CH   HV   MACD PA   SBK  SC   TNC  TNM  TRU  TW   WIN 
# 12 sites 08/06/2021

# make new variable site_checks_resolved which will hold names of sites from site_checks that have been sorted out
site_checks_resolved <- rep(NA, length(site_checks))

#### View each of these turn in the mydata object 

# [1] CDF JE SBK SC  TNC TNM TW  WIN

#site_checks[1]
CDF <- droplevels(mydata[mydata$site_code == "CDF",])
#table(CDF$transect_Lat_start, CDF$transect)
#table(CDF$transect_Lat_stop, CDF$transect)
#table(CDF$transect_Lon_start, CDF$transect)
#table(CDF$transect_Lon_stop, CDF$transect)

#table(CDF$transect_Lat_stop, CDF$c_year)
#table(CDF$transect_Lat_start, CDF$c_year)
#table(CDF$transect_Lon_start, CDF$c_year)

#length(which(CDF$transect_Lat_stop == CDF$transect_Lat_start))
#nrow(CDF)

#### 2 different start points very close together. For T1. No obvious reason. 
#### These occur in same census year. Also latitude start and stop values are the same
#### This could mean transects were running directly North/South. 
#### but is odd combined with other errors
#### 
#sites[sites$site_code == "CDF",]

##### Author contacted 17/06/2019. Transects do in fact run east-west so Lat start and Lat stop are the same. 
# Correct coordinates are
# T1 Lat start and End : 51.899639
# T1 lon start -8.485972, T1 lon_stop -8.485889
# T2 Lat start and end: 51.899611
# T2 lon start -8.485944, T2 lon_stop -8.485889

#Lat
mydata[mydata$site_code == "CDF" & mydata$transect == "T1","transect_Lat_start"] <- 51.899639
mydata[mydata$site_code == "CDF" & mydata$transect == "T1","transect_Lat_stop"] <- 51.899639
mydata[mydata$site_code == "CDF" & mydata$transect == "T2","transect_Lat_start"] <- 51.899611 
mydata[mydata$site_code == "CDF" & mydata$transect == "T2","transect_Lat_stop"] <- 51.899611

#Lon
mydata[mydata$site_code == "CDF" & mydata$transect == "T1","transect_Lon_start"] <- -8.485972
mydata[mydata$site_code == "CDF" & mydata$transect == "T1","transect_Lon_stop"] <- -8.485889
mydata[mydata$site_code == "CDF" & mydata$transect == "T2","transect_Lon_start"] <- -8.485944 
mydata[mydata$site_code == "CDF" & mydata$transect == "T2","transect_Lon_stop"] <- -8.485889


site_checks_resolved[1] <- "CDF"

#### Next one CH
######### Error Resolved in data, fill series errors. Alain 20/06/2019 ###########
#### next one - GB
######### Error Resolved in data, fill series had overwritten T2 coords with T1. Alain 18/06/2019#################

## next one - JE (site_checks[2])
JE <- droplevels(mydata[mydata$site_code == "JE",])
JE %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
JE %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

## detect NA's in transect coordinates
if(any(is.na(c(JE$transect_Lat_start, JE$transect_Lat_stop, JE$transect_Lon_start,JE$transect_Lon_stop))) ) {
  warning("NA's in transect coords detected & fixed for JE")
  #no values for transect in 2016. fill with values from 2018
  mydata[mydata$site_code == "JE","transect_Lat_start"] <- unique(JE$transect_Lat_start)[1]
  mydata[mydata$site_code == "JE","transect_Lat_stop"]  <- unique(JE$transect_Lat_stop)[1]
  mydata[mydata$site_code == "JE","transect_Lon_start"] <- unique(JE$transect_Lon_start)[1]
  mydata[mydata$site_code == "JE","transect_Lon_stop"]  <- unique(JE$transect_Lon_stop)[1]
  #######Values for start and stop are the same but this is not a problem ########
  
  site_checks_resolved[2] <- "JE" 
}
## shows NA's in transect coordinates


## next one - SBK (site_checks[3])
SBK <- droplevels(mydata[mydata$site_code == "SBK",])

SBK %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
SBK %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
SBK1 <- SBK[SBK$transect == "T1",]
#table(SBK1$transect_Lat_start, SBK1$c_year)
#table(SBK1$transect_Lon_start, SBK1$c_year)
#table(SBK1$transect_Lat_stop, SBK1$c_year)
#table(SBK1$transect_Lon_start, SBK1$c_year)
#no values for transect in 2016. fill with values from 2018
mydata[mydata$site_code == "SBK","transect_Lat_start"] <- 47.40025
mydata[mydata$site_code == "SBK","transect_Lat_stop"]  <- 47.40025
mydata[mydata$site_code == "SBK","transect_Lon_start"] <- 19.15878
mydata[mydata$site_code == "SBK","transect_Lon_stop"]  <- 19.15878
#######Values for start and stop are the same but this is not a problem ########

site_checks_resolved[3] <- "SBK"


##### next one - SC

#site_checks[4]

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

### issue is with Transect lon

SCT2 <- SC[SC$transect == "T2",]
SCT1 <- SC[SC$transect =="T1",]
#table(SCT2$transect_Lat_start, SCT2$c_year)
#table(SCT2$transect_Lat_stop, SCT2$c_year)
##Both of these are fine

#table(SCT2$transect_Lon_start, SCT2$c_year)

#             2015 2016 2017 2018
#-008.9923    66    0    0    0
#-008.99239    0    0   85   72
#-8.99239      0   88    0    0
#table(SCT2$transect_Lon_stop, SCT2$c_year)
#           2015 2016 2017 2018
#-008.99235    0    0   85   72
#-8.99235     66   88    0    0


#table(SCT1$transect_Lon_start, SCT1$c_year)
#          2015 2016 2017 2018
#-008.99231   40    0   64   66
#-8.99231      0   57    0    0

#table(SCT1$transect_Lon_stop, SCT1$c_year)
#           2015 2016 2017 2018
#-008.99229    0    0   64   66
#-8.99229     40   57    0    0

#### inconsistencies in Transect_Lon_stop across all years
mydata[mydata$site_code == "SC" & mydata$transect == "T2","transect_Lon_start"] <- -8.99239
mydata[mydata$site_code == "SC" & mydata$transect == "T2","transect_Lon_stop"] <- -8.99235
mydata[mydata$site_code == "SC" & mydata$transect == "T1","transect_Lon_start"] <- -8.99231 
mydata[mydata$site_code == "SC" & mydata$transect == "T1","transect_Lon_stop"] <- -8.99229



site_checks_resolved[4] <- "SC"


##### next one - SI
#### Fixed in excel file - one row mislabelled as T2 when it was actually in T1 - 18/06/2019 Alain



##### next one - TNC
#site_checks[5]

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

#unique(TNC$transect_Lat_start)

#table(TNC$transect_Lat_start, TNC$c_year)
#table(TNC$transect_Lat_stop, TNC$c_year)
### strange coordinates for 14 rows in 2016. Off by 100's of kms!
### Set these to match the other coordinates for this transect?
mydata[mydata$site_code == "TNC","transect_Lat_start"] <- 44.03775
mydata[mydata$site_code == "TNC","transect_Lat_stop"]  <-  44.03775


#table(TNC$transect_Lon_start, TNC$c_year)
#table(TNC$transect_Lon_stop, TNC$c_year)
### strange coordinates for 14 sites in 2016. Off by a small amount 
### possible error from copying into cells below in excel? 

#### Location errors in 14 rows in 2016. Latitude errors are very large.
#### Fix by setting these to match the other 513 rows?
mydata[mydata$site_code == "TNC","transect_Lon_start"] <- -123.1665
mydata[mydata$site_code == "TNC","transect_Lon_stop"]  <-  -123.166467

site_checks_resolved[5] <- "TNC"

##### next one - TNM
#site_checks[6]

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

### only one transect at this site - T1. Examine this..

#unique(TNM$transect_Lon_start)

#table(TNM$transect_Lon_start, TNM$c_year)
#table(TNM$transect_Lon_stop, TNM$c_year)
### TNM lonitude values are missing the '-' (negative direction)
### in 2017 

#### Fix as follows 

mydata[mydata$site_code == "TNM","transect_Lon_start"] <- -7.61744
mydata[mydata$site_code == "TNM","transect_Lon_stop"]  <-  -7.61743

site_checks_resolved[6] <- "TNM"


##### next one - TW
#site_checks[7]

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

### only one transect at this site - T1 . Examine this..

#unique(TW$transect_Lat_start[TW$c_year == "2015"])
#unique(TW$transect_Lon_stop[TW$c_year == "2015"])
#### This looks like a 'fill cells' error from excel in 2015 data, 
### numbers are increasing sequentially

#### Replace these with the values to Lat Start and Lon start from 2016

#### Fix as follows 

TW_LatStr <- unique(TW$transect_Lat_start[TW$c_year == "2016"])
TW_LonStp <- unique(TW$transect_Lon_stop[TW$c_year == "2016"])


mydata[mydata$site_code == "TW","transect_Lat_start"] <- TW_LatStr 
mydata[mydata$site_code == "TW","transect_Lon_stop"]  <-  TW_LonStp

site_checks_resolved[7] <- "TW"

## next one - WIN site_checks[8]
WIN <- droplevels(mydata[mydata$site_code == "WIN",])


WIN %>%
  group_by(transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))
WIN %>%
  group_by(c_year, transect) %>%
  summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
            n_trans_lat_stop = n_distinct(transect_Lat_stop),
            n_trans_lon_start = n_distinct(transect_Lon_start),
            n_trans_lon_stop = n_distinct(transect_Lon_stop))

unique(WIN$transect_Lon_start[WIN$c_year == "2018"])
#missing a minus in transect_Lon_start & transect_Lon_stop in 2018
mydata[mydata$site_code == "WIN","transect_Lon_start"] <- -1.3081
mydata[mydata$site_code == "WIN","transect_Lon_stop"] <- -1.3081

site_checks_resolved[8] <- "WIN"


##### RECHECK WHICH TRANSECTS STILL NEED FIXES

if(sum(is.na(site_checks_resolved)) > 0) {
  warning("These sites require more information before proper GPS fixes can be applied:")
  warning(paste(site_checks[site_checks %nin% site_checks_resolved], "  "))
}
  

###### one last look at check_table
check_table <- as.data.frame( mydata %>%
                                group_by(site_code, transect) %>%
                                summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
                                          n_trans_lat_stop = n_distinct(transect_Lat_stop),
                                          n_trans_lon_start = n_distinct(transect_Lon_start),
                                          n_trans_lon_stop = n_distinct(transect_Lon_stop)))
check_table

###### tidy up
rm(tocheck2, TW_LonStp, TW_LatStr, TW, TNM, TNC, SCT2, SC, CDF, WIN, SBK, SBK1, SCT1, 
   site_checks, err1, check_table, cols_trans, replace1, OR_SS, N_issues, IO, HAS, EE, sites, x3, 
   weird_sites, weird_sites2, weird_sites3, weird_sites4, weird_sites5, BG_2016_T2, x2, BG_2016_T1, BG_2016, BG, 
   has_coords, has_site_coords, na_coords, na_site_coords, x1, site_checks_resolved, x5,x6,x7,x8)
