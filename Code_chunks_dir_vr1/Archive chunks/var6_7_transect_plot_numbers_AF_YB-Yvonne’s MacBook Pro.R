# Transect and Plot
# Alain Finn
# Updated 08/04/2019

# Reviewed by Ruth Kelly - 11/03/2019
# Revised by Yvonne Buckley 12/04/19, added fix for ZG, na in plot

# Description: Code checks that transect and plot numbers are correctly formatted. 
##1. Check transect levels and trim white space from transect name, check for NA's and fix
##2. Check plot levels, check for NA's & fix
##3. Cross-check transects with lat & long start & stops (var2-5_coordinate_locations_RK_YB.R) - still to do this

##1. Trim white space from transect name, check for NA's & fix
#str(pdata)
#levels(pdata$transect)
#Error: Duplicate level of "T2" and "T2 "

pdata <- mydata
#Solution: Use trimsw to removes white space from character string
levels(pdata$transect) <- trimws(levels(pdata$transect))

#check result
levels(pdata$transect)
#[1] "1"  "t1" "T1" "T2" "T3" "T4" "T5" "T6" "T7"
#Clean up lower case t and transects without a T

pdata$transect <- gsub("t1", "T1", pdata$transect)
pdata$transect <- gsub("1", "T1", pdata$transect)
pdata$transect <- gsub("TT1", "T1", pdata$transect)
unique(pdata$transect)
#[1] "T1" "T2" NA   "T3" "T4" "T5" "T6" "T7"

#### check for NA's in transect names 
if(any(is.na(pdata$transect))) warning("NAs present in transect names")
pdata$unique_plot_year_id <- paste(pdata$site_code,pdata$transect,pdata$plot,pdata$s_year,sep = "_")
transect_na <- pdata$unique_plot_year_id[which(is.na(pdata$transect))]
print(transect_na)
print("values for site BG are potentially solved by running var_9 plant ID script")
## BG <- subset(pdata, pdata$site_code == "BG"), multiple fl. stems recorded - check fixed in flowering stems code


##2. Check plot levels, check for NA's & fix

levels(pdata$plot)


#Errors. One site (HV) uses a,b,c,d (this site has 1m plots divided in 4 to make 4 50cm plots)
#P1-P9 and P01-P09 
#lower case p & values with no letter P 
#numbers above 20 (should only be 20 plots per transect, but will ignore for now)


#Still some lower case p's and numbers only
pdata$plot <- gsub("p", "P", pdata$plot)
pdata$plot <- paste("P", pdata$plot, sep = "")
pdata$plot <- gsub("PP", "P", pdata$plot)
pdata$plot <- gsub("PNA", NA, pdata$plot)
#values P01-P09 changed to P1 to P9 for consistency
pdata$plot <- gsub("P0", "P", pdata$plot)

unique(pdata$plot)
pdata$plot <- as.factor(pdata$plot)





# #### Check for NA's in plot names 
if(any(is.na(pdata$plot))) warning("NAs present in plot names")
plot_na <- pdata$unique_plot_year_id[which(is.na(pdata$plot))]
print(plot_na)
print("values for site BG are potentially solved by running var_9 plant ID script")

## replacing NA in ZG with plot number of same plant id in previous year
######## THIS CHUNK NO LONGER REQUIRED BUT MAY BE USEFUL IN FUTURE - Alain 20/06/2019 ######
#ZG <- droplevels(subset(pdata,  pdata$site_code == "ZG")  )
#ZG2 <- droplevels(subset(ZG, ZG$plant_id == ZG$plant_id[is.na(ZG$plot)]))
#ZG3 <- droplevels(subset(ZG, is.na(ZG$plot)))
#ZG3$plot <- unique(ZG2$plot[!is.na(ZG2$plot)])
#pdata[pdata$site_code == "ZG" & is.na(pdata$plot),] <- ZG3


#return pdata to mydata 
mydata <- pdata

# check if number of unique transects are equal to number of unique transect lat long

# checking code added by Ruth Kelly
# 

 
# 
# #### code returns number of unique start and stop coordinates for each site and transect
check_table <- as.data.frame( pdata %>%
                                group_by(site_code, transect) %>%
                                 summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
                                           n_trans_lat_stop = n_distinct(transect_Lat_stop),
                                           n_trans_lon_start = n_distinct(transect_Lon_start),
                                           n_trans_lon_stop = n_distinct(transect_Lon_stop)))
check_table
# #### If everything is correct rows should sum to 4 
# #### (i.e. 1 coordinate of each type per transect and site)
# 

# 
# #### check which rows do not sum 4. 
# 
issues1 <- which(rowSums(check_table[,3:6]) != 4)
tocheck <- check_table[issues1,]

site_checks <- unique(tocheck$site_code)
# 
# ### return names of sites where there are potential issues
# 
# 
# 
# ## will return a warning if levels are not correct
 if(any(issues1 !=0)) {warning("Some sites have multiple GPS coordinates for the same Transect IDs, see list below")
}else{ 
     print("transect and plot are fixed. YAY!")}

 if(nrow(check_table) != 0) {
   print(site_checks)}

rm(pdata, site_checks, issues1, tocheck, check_table, transect_na, plot_na)
# 
# 
# 