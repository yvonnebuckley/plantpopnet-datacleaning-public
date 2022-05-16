####### Data audit 
####### Nov. 2019 
####### Using the clean data  frame output from the compiled cleaner
####### Alain Finn


### Packages ####
library(dplyr)
library(compareDF)
library(htmlTable)

#audit steps
#1. Read in the cleaned version and the original from the PPN Master Google Drive. CSV versions need to be created and added to the site data folder in the Plantpopnet-Data-Cleaning project.
#2. Using compareDF - highlights any lines in the 2 dataframes that have had changes. Highlights the old version in Red and the new version in Green when printed as a HTML
#3. column names must match between the 2 files - requires removal of any new columns created during cleaning process eg plant_unique_id etc
#4. Those new columns will need to be checked separately Particularly plant_unique_id.
#5. compareDF has had a version update (5/01/20) so the syntax is slightly different for Y1 clean. Will need edits to Y0 code to run.

# using the mydataY0 object from the complier script or read in the draft dataset "PLANTPOPNET_Y0_draftV1.0_2019-11-01_.csv"
Y0 <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Master/PLANTPOPNET_Y0_draftV1.3_2020-11-18_.csv",header = T)
#Y0 <- mydataY0
#set.seed(4091)
#index<-sample(1:nrow(mydataY0), 500)
#index
#sample <- mydataY0[index,]


#Create a random sample of sites to cross check against the master data files.Random number generated online. 
set.seed(4091)
sample <- Y0 %>% filter(site_code %in% sample(levels(site_code), 10)) 
# [1] [1] BG    JSJ   KM    RUSC  STR   SW729 TJ    WIN   ZG    RO

###### BG ########
cleanBG <- Y0 %>% filter(site_code == "BG") 
cleanBG <- cleanBG %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

BG <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/BG_individual_plant_census_2015_Y0.csv",header = T)
BG <- BG %>% select(1:29)
BG <- rename(BG, rosette_ID = rosette_number) #column names must match 
BG <- rename(BG, suspected_clone_binary = suspected_clone)
BG$plant_survival <- BG$survival

check1 <- compare_df(cleanBG, BG, c("plant_id"))
check1$change_summary
check1$change_count
create_output_table(check1, limit = 400)
#audit notes: yes values entered for herbivory. additional flowering measurements removed. 

###### KM #########
cleanKM <- Y0 %>% filter(site_code == "KM") 
cleanKM <- cleanKM %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

KM <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/KM_individual_plant_census_2015_Y0.csv",header = T)
KM <- KM %>% select(1:29)
KM <- rename(KM, rosette_ID = rosette_number)
KM <- rename(KM, suspected_clone_binary = suspected_clone)
KM$plant_survival <- KM$survival

check2 <- compare_df(cleanKM, KM, c("plant_id"), change_markers = c("new", "old", "same"))
check2$change_summary
check2$change_count
create_output_table(check2, limit = 400)

#audit 1
print("values for fl.stem recorded as x/y have been converted to x,y in the master excel files. These will now be corrected by the fix in var16-21. Alain 06/11/2019 ")

###### STR ######## 
cleanSTR <- Y0 %>% filter(site_code == "STR") 
cleanSTR <- cleanSTR %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

STR <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/STR_individual_plant_census_2015_Y0.csv",header = T)
STR <- STR %>% select(1:29)
STR <- rename(STR, rosette_ID = rosette_number)
STR <- rename(STR, suspected_clone_binary = suspected_clone)
STR$plant_survival <- STR$survival

check3 <- compare_df(cleanSTR, STR, c("plant_id"), change_markers = c("new", "old", "same"))
check3$change_summary
check3$change_count
create_output_table(check3, limit = 400)

#audit 2
print("Code to pull fl. stem and inflor length fixed 13/11/2019. Edits made to line 72-75 in var16-21 outlier script") 
#audit notes: values for multiple flowering stems removed keeping the tallest stem and corresponding infloresence


#### TJ #####
cleanTJ <- Y0 %>% filter(site_code == "TJ") 
cleanTJ <- cleanTJ %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

TJ <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/TJ_individual_plant_census_2015_Y0.csv",header = T)
TJ <- TJ %>% select(1:29)
TJ <- rename(TJ, rosette_ID = rosette_number)
TJ <- rename(TJ, suspected_clone_binary = suspected_clone)
TJ$plant_survival <- TJ$survival

check4 <- compare_df(cleanTJ, TJ, c("plant_id"), change_markers = c("new", "old", "same"))
check4$change_summary
check4$change_count
create_output_table(check4, limit = 400)

##### ZG ######
cleanZG <- Y0 %>% filter(site_code == "ZG") 
cleanZG <- cleanZG %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

ZG <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/ZG_individual_plant_census_2016_Y0.csv",header = T)
ZG <- ZG %>% select(1:29)
ZG <- rename(ZG, rosette_ID = rosette_number)
ZG <- rename(ZG, suspected_clone_binary = suspected_clone)
ZG$plant_survival <- ZG$survival

check5 <- compare_df(cleanZG, ZG, c("plant_id"), change_markers = c("new", "old", "same"))
check5$change_summary
check5$change_count
create_output_table(check5, limit = 400)

#### JSJ #######
cleanJSJ <- Y0 %>% filter(site_code == "JSJ") 
cleanJSJ <- cleanJSJ %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

JSJ <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/JSJ_individual_plant_census_2015_Y0.csv",header = T)
JSJ <- JSJ %>% select(1:29)
JSJ <- rename(JSJ, rosette_ID = rosette_number)
JSJ <- rename(JSJ, suspected_clone_binary = suspected_clone)
JSJ$plant_survival <- JSJ$survival

check6 <- compare_df(cleanJSJ, JSJ, c("plant_id"), change_markers = c("new", "old", "same"))
check6$change_summary
check6$change_count
create_output_table(check6, limit = 400)

#Audit 3.
print("T3 P10 plant_id 71 has been removed but no indication of why. Fixed - edits to plant_id script. alain 8/11/2019")
Print("Fl_stem is picking the first value for stem not the largest - Fixed 13/11/2019")


#### RUSC #####
cleanRUSC <- Y0 %>% filter(site_code == "RUSC") 
cleanRUSC <- cleanRUSC %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

RUSC <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/RUSC_individual_plant_census_2015_Y0.csv",header = T)
RUSC <- RUSC %>% select(1:29)
RUSC <- rename(RUSC, rosette_ID = rosette_number)
RUSC <- rename(RUSC, suspected_clone_binary = suspected_clone)
RUSC$plant_survival <- RUSC$survival

check7 <- compare_df(cleanRUSC, RUSC, c("plant_id"),change_markers = c("new", "old", "same"))
check7$change_summary
check7$change_count
create_output_table(check7, limit = 400)

#### SW729 ####
cleanSW729 <- Y0 %>% filter(site_code == "SW729") 
cleanSW729 <- cleanSW729 %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,
                                    -x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

SW729 <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/SW729_individual_plant_census_2017_Y0.csv",header = T, stringsAsFactors = FALSE)
SW729 <- SW729 %>% select(1:29)
SW729 <- rename(SW729, rosette_ID = rosette_number)
SW729 <- rename(SW729, suspected_clone_binary = suspected_clone)
SW729$plant_survival <- SW729$survival

check8 <- compare_df(cleanSW729, SW729, c("plant_id"), change_markers = c("new", "old", "same"))
check8$change_summary
check8$change_count
create_output_table(check8, limit = 400)

#Audit 4
print("T1 P11 plant_id 30 has been removed but no indication of why. fixed by plant_id edits 8/11/2019 AF")
print("No_rosettes column is all 0s - fixed in master excel files - AF")

#### WIN ####
cleanWIN <- Y0 %>% filter(site_code == "WIN") 
cleanWIN <- cleanWIN %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,
                                -x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

WIN <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/WIN_individual_plant_census_2016_Y0.csv",header = T, stringsAsFactors = FALSE)
WIN <- WIN %>% select(1:29)
WIN <- rename(WIN, rosette_ID = rosette_number)
WIN <- rename(WIN, suspected_clone_binary = suspected_clone)
WIN$plant_survival <- WIN$survival

check9 <- compare_df(cleanWIN, WIN, c("plant_id"), change_markers = c("new", "old", "same"))
check9$change_summary
check9$change_count
create_output_table(check9, limit = 400)

#### RO ####
cleanRO <- Y0 %>% filter(site_code == "RO") 
cleanRO <- cleanRO %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,
                              -x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, -herbiv_orig)

RO <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/RO_individual_plant_census_2015_Y0.csv",header = T, stringsAsFactors = FALSE)
RO <- RO %>% select(1:29)
RO <- rename(RO, rosette_ID = rosette_number)
RO <- rename(RO, suspected_clone_binary = suspected_clone)
RO$plant_survival <- RO$survival

check10 <- compare_df(cleanRO, RO, c("plant_id"), change_markers = c("new", "old", "same"))
check10$change_summary
check10$change_count
create_output_table(check10, limit = 400)


