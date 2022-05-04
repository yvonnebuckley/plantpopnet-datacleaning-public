# Transect and Plot
# Alain Finn
# Updated 08/04/2019

# Reviewed by Ruth Kelly - 11/03/2019, Yvonne Buckley 12/04/19

# Description: Code checks that transect and plot numbers are correctly formatted. 

#str(mydata)
#levels(mydata$transect)
#Error: Duplicate level of "T2" and "T2 "

#Solution: Use trimsw to removes white space from character string
levels(mydata$transect) <- trimws(levels(mydata$transect))

#check result
levels(mydata$transect)


levels(mydata$plot)
#Errors. One site (HV) uses a,b,c,d (this site has 1m plots divided in 4 to make 4 50cm plots)
#P1-P9 and P01-P09 
#numbers above 20 (should only be 20 plots per transect)

#Solution
#Leave HV as is with 1a,b,c,d for now, no further action required

#values P01-P09 changed to P1 to P9 for consistency
mydata$plot <- gsub("P0", "P", mydata$plot)
mydata$plot <- as.factor(mydata$plot)
#str(mydata$plot)
#levels(mydata$plot)

#### check for NA's in transect names 
if(any(is.na(mydata$transect))) warning("NAs present in transect names")
mydata$unique_plot_year_id <- paste(mydata$site_code,mydata$transect,mydata$plot,mydata$s_year,sep = "_")
transect_na <- mydata$unique_plot_year_id[which(is.na(mydata$transect))]
print(transect_na)

## BG <- subset(mydata, mydata$site_code == "BG"), multiple fl. stems recorded - check fixed in flowering stems code

# #### Check for NA's in plot names 
if(any(is.na(mydata$plot))) warning("NAs present in plot names")
plot_na <- mydata$unique_plot_year_id[which(is.na(mydata$plot))]
print(plot_na)
print("values for site BG are potentially solved by running var_9 plant ID script")

ZG <- subset(mydata, mydata$site_code == "ZG")

ZG[is.na(ZG$plot),]
x1 <- ZG[ZG$plant_id == ZG$plant_id[is.na(ZG$plot)],]
x2 <- rem_na_df(x1)
rows_rem(x1, x2)

  
#Values above p20, For EE as the plots are numbered by Transect (T1 P1-20, T2 P21-29) no further action required
#wrongPs <- mydata[mydata$plot %in% c("P21", "P22", "P23", "P24", "P25" ,"P26", "P27" ,"P28" ,"P29","P30" ,"P31", "P32", "P33", "P34" ,"P35","P36", "P37", "P39", "P40", "P41", "P42", "P43"),]
#Site EE: plot numbers in T2 range from P21-P29: no further action required
#Site ZM: plot numbers in T2 range from P21-P37 and T3 P39-P43: no further action required


                      

# check if number of unique transects are equal to number of unique transect lat long

# checking code added by Ruth Kelly
# 

 
# 
# #### code returns number of unique start and stop coordinates for each site and transect
# check_table <- as.data.frame( mydata %>%
#                                 group_by(site_code, transect) %>%
#                                 summarise(n_trans_lat_start = n_distinct(transect_Lat_start),
#                                           n_trans_lat_stop = n_distinct(transect_Lat_stop),
#                                           n_trans_lon_start = n_distinct(transect_Lon_start),
#                                           n_trans_lon_stop = n_distinct(transect_Lon_stop)))
# 
# #### If everything is correct rows should sum to 4 
# #### (i.e. 1 coordinate of each type per transect and site)
# 
# unique(mydata$transect[mydata$site_code == "BG"])
# 
# #### check which rows do not sum 4. 
# 
# issues1 <- which(rowSums(check_table[,3:6]) != 4)
# 
# ### return names of sites where there are potential issues
# 
# 
# 
# ## will return a warning if levels are not correct
# if(nrow(check_table) != 0)
#   warning("Some sites have multiple GPS coordinates for the same Transect IDs, see list below")
#     
# if(nrow(check_table) != 0) {
#   print(Site_Issues)}
# 
# 
# 