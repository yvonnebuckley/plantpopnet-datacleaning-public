###### Clean site_code column ######
#That script tries to solve the different issues met with the site_codes

#Maude Baudraz January 2019
## code reviewed by Caroline Mckeon 11/03/19, revised by Yvonne Buckley 11/04/19
#ppn<-read.csv("./Code_chunks_dir_vr1/code_chunks_in_progress/fullPPN_dataset_2019-01-28_.csv")

## YB: lats & longs need to be cleaned before running this script as they can't be matched between
## mydata and sitedata before cleaning (dodgy lats & longs in mydata)

##1. Identify empty site code (NA) and resolve
##2. Checking the site codes in mydata match the Site description codes in sitedata


ppn<-mydata

#### Visualize data ####
levels(ppn$site_code) <- trimws(levels(ppn$site_code))
tt <- table(ppn$site_code)

##1. Identify empty site code (NA) and resolve
#One line has an empty site code: 
tt2 <- ppn[which(is.na(ppn$site_code)),] 

#Extract the coordinate of the transect that has no site code
Coord <- ppn$transect_Lat_start[which(is.na(ppn$site_code))] ## returns nothing if no missing site code
if(length(Coord) >1) warning("more than one empty site code -  solution script needs editing to resolve this")

#Extract the site code of the lines that have the same coordinate:
missing_site_code <- unique(ppn$site_code[which(ppn$transect_Lat_start == Coord)],na.omit = T)
if(length(missing_site_code) == 2){   #If there only is one site that has the same coordinates, plus the NA level you're looking to remove
  ppn$site_code[which(is.na(ppn$site_code))] <- missing_site_code[which(is.na(missing_site_code)==F)]}
#then attribute that code to the line with a NA. 

#Note from alain - is this actually the case?
#if(length(missing_site_code) > 2){  #If there is more than two levels, Two populations have the same North coordinates/latitude/Y 
 # warning("There is an unforseen case in your data. Two populations have the same lattitude, although their site ID is different. var1_site_code_MB_YB.R line 25")
  #}

tt3 <- ppn[which(is.na(ppn$site_code)),] # if no rows are returned then NAs have been resolved
if(length(tt3[,1]) > 0) warning("NA's in site code remain")

#ppn$site_code<-droplevels(ppn$site_code)
tt4 <- table(ppn$site_code) #looks ok now

print("If nothing printed out until now, all rows had site code information (no NA's), or their site could be reattributed! Well done")


##2. Checking the site codes in mydata match the Site description codes in sitedata

#setwd("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Code_chunks_dir_vr1/code_chunks_in_progress")
SiteDescriptionSheet <- sitedata
tt5 <- SiteDescriptionSheet$site_code

print("The script will now see whether all site codes do match those listed in the summary site data.")
print("The script will automatically change the spelling of the codes CDF and LK to match the spelling on the summary sheet.")
print("If another site code appears listed here as a warning, please add a case to correct the spelling of the site code")

## YB 12/10/2020: updated script to show differences between site description and demography files both ways
in_demog_nin_site <- setdiff(levels(ppn$site_code), levels(SiteDescriptionSheet$site_code))
in_site_nin_demog <-  setdiff(levels(SiteDescriptionSheet$site_code), levels(ppn$site_code))

if(all(is.na(in_demog_nin_site))) {
  print("sites in demographic dataframe match site description dataframe")} else {
    warning(paste("the following sites are in the demographic dataframe but 
                 don't match the site description data: "))
    print(in_demog_nin_site)
    }

if(all(is.na(in_site_nin_demog))) {
  print("sites in site description dataframe match sites in demographic dataframe")} else {
    warning("the following sites are in the site description dataframe but 
                 don't match the sites in demographic dataframe")
    print(in_site_nin_demog)
    }
   
         
tt6 <- levels(SiteDescriptionSheet$site_code)
#Check the site codes in the description sheet to know where the difference stem from 
#On June 6th 2019, the site code LK were not matching the site description (should read LK1). 


levels(ppn$site_code) <- trimws(levels(ppn$site_code))


levels(ppn$site_code) <- c(levels(ppn$site_code),"LK1")

ppn$site_code[which(ppn$site_code=="LK")]<-"LK1"
print("LK1 fixed")
print("Site JR is featured in the Genomic analysis but never recorded demographic data so it is not included in mydata")
ppn <- droplevels(ppn)

#### Remove the unused levels now present in the site_code variable, 
#### using droplevels()
mydata<-droplevels(ppn)
print("Sites included in mydata: ")
print(levels(mydata$site_code))

## Tidy up
rm(ppn, tt, tt2, tt3, tt4, tt5, in_site_nin_demog, in_demog_nin_site)






