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


########################################### Year 1 Audit ###################################################################
Y1 <- read.csv(file.choose(), header = T, stringsAsFactors = T)
set.seed(7840)
sample <- Y1 %>% filter(site_code %in% sample(levels(site_code), 15)) 
### BI    EL    MN    OR_SS   SC    SW242 TUE   ZG   
### BI    CDF   EL    GB    GH    JSJ   MN    NRM   OR_SS  SC    SW242 TUE   ZG 

##### BI ########
cleanBI <- Y1 %>% filter(site_code == "BI") 
cleanBI <- cleanBI %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

BI <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/BI_individual_plant_census_2017_Y1.csv",header = T, stringsAsFactors = FALSE)
BI <- BI %>% select(1:29)
BI <- rename(BI, rosette_ID = rosette_number)
BI <- rename(BI, suspected_clone_binary = suspected_clone)
BI$plant_survival <- BI$survival  ## copying survival column so columns match exactly. required for compare_df


check11 <- compare_df(cleanBI, BI, c("plant_id"), change_markers = c("new", "old", "same"))
check11$change_summary
check11$change_count
create_output_table(check11, limit = 400)

print("multiple values for survival error corrected by fix to survival script - Feb 2020")

##### EL #######
cleanEL <- Y1 %>% filter(site_code == "EL") 
cleanEL <- cleanEL %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

EL <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/EL_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
EL <- EL %>% select(1:29)
EL <- rename(EL, rosette_ID = rosette_number)
EL <- rename(EL, suspected_clone_binary = suspected_clone)
EL$plant_survival <- EL$survival

check12 <- compare_df(cleanEL, EL, c("plant_id"), change_markers = c("new", "old", "same"))
check12$change_summary
check12$change_count
create_output_table(check12, limit = 400)


##### MN ######
cleanMN <- Y1 %>% filter(site_code == "MN") 
cleanMN <- cleanMN %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

MN <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/MN_individual_plant_census_2017_Y1.csv",header = T, stringsAsFactors = FALSE)
MN <- MN %>% select(1:29)
MN <- rename(MN, rosette_ID = rosette_number)
MN <- rename(MN, suspected_clone_binary = suspected_clone)
MN$plant_survival <- MN$survival

check13 <- compare_df(cleanMN, MN, c("plant_id"),change_markers = c("new", "old", "same"))
check13$change_summary
check13$change_count
create_output_table(check13, limit = 400)

print("multiple values for survival error corrected by fix to survival script - Feb 2020")


#### OR_SS ####
cleanOR_SS <- Y1 %>% filter(site_code == "OR_SS") 
cleanOR_SS <- cleanOR_SS %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                    -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

OR_SS <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/OR_SS_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
OR_SS <- OR_SS %>% select(1:29)
OR_SS <- rename(OR_SS, rosette_ID = rosette_number)
OR_SS <- rename(OR_SS, suspected_clone_binary = suspected_clone)
OR_SS$plant_survival <- OR_SS$survival

check14 <- compare_df(cleanOR_SS, OR_SS, c("plant_id"), change_markers = c("new", "old", "same"))
check14$change_summary
check14$change_count
create_output_table(check14, limit = 400)

print("X,Y coords for several plants have disappeared - solved in master data files - Formulas in cells")

##### SC ####
cleanSC <- Y1 %>% filter(site_code == "SC") 
cleanSC <- cleanSC %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

SC <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/SC_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
SC <- SC %>% select(1:29)
SC <- rename(SC, rosette_ID = rosette_number)
SC <- rename(SC, suspected_clone_binary = suspected_clone)
SC$plant_survival <- SC$survival

check15 <- compare_df(cleanSC, SC, c("plant_id"), change_markers = c("new", "old", "same"))
check15$change_summary
check15$change_count
create_output_table(check15, limit = 400)

print("NA in herbiv yes/no column but a comment in the herbiv_comments column. Script should have changed herbiv yes/no to yes- Fixed in script Feb 2020")

##### SW242 ####
cleanSW242 <- Y1 %>% filter(site_code == "SW242") 
cleanSW242 <- cleanSW242 %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                    -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

SW242 <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/SW242_individual_plant_census_2018_Y1.csv",header = T, stringsAsFactors = FALSE)
SW242 <- SW242 %>% select(1:29)
SW242 <- rename(SW242, rosette_ID = rosette_number)
SW242 <- rename(SW242, suspected_clone_binary = suspected_clone)
SW242$plant_survival <- SW242$survival

check16 <- compare_df(cleanSW242, SW242, c("plant_id"), change_markers = c("new", "old", "same"))
check16$change_summary
check16$change_count
create_output_table(check16, limit = 400)

print("Many 0s in dataset; flow stem height and inflorescence data - should be NAs for plantpopnet - Fixed in Master data files for SW sites")

##### TUE ####
cleanTUE <- Y1 %>% filter(site_code == "TUE") 
cleanTUE <- cleanTUE %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

TUE <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/TUE_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
TUE <- TUE %>% select(1:29)
TUE <- rename(TUE, rosette_ID = rosette_number)
TUE <- rename(TUE, suspected_clone_binary = suspected_clone)
TUE$plant_survival <- TUE$survival

check17 <- compare_df(cleanTUE, TUE, c("plant_id"), change_markers = c("new", "old", "same"))
check17$change_summary
check17$change_count
create_output_table(check17, limit = 400)



#### ZG ####

cleanZG <- Y1 %>% filter(site_code == "ZG") 
cleanZG <- cleanZG %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

ZG <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/ZG_individual_plant_census_2017_Y1.csv",header = T, stringsAsFactors = FALSE)
ZG <- ZG %>% select(1:29)
ZG <- rename(ZG, rosette_ID = rosette_number)
ZG <- rename(ZG, suspected_clone_binary = suspected_clone)
ZG$plant_survival <- ZG$survival

check18 <- compare_df(cleanZG, ZG, c("plant_id"))
check18$change_summary
check18$change_count
create_output_table(check18, limit = 400)

#### CDF ####
cleanCDF <- Y1 %>% filter(site_code == "CDF") 
cleanCDF <- cleanCDF %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

CDF <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/CDF_individual_plant_census_2017_Y1.csv",header = T, stringsAsFactors = FALSE)
CDF <- CDF %>% select(1:29)
CDF <- rename(CDF, rosette_ID = rosette_number)
CDF <- rename(CDF, suspected_clone_binary = suspected_clone)
CDF$plant_survival <-CDF$survival

check19 <- compare_df(cleanCDF, CDF, c("plant_id"))
check19$change_summary
check19$change_count
create_output_table(check19, limit = 400)

print("?no was changed to yes in herbiv yes/no for plant no. 42. check script. Fixed in herbiv script" )

#### GB ####
cleanGB <- Y1 %>% filter(site_code == "GB") 
cleanGB <- cleanGB %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

GB <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/GB_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
GB <- GB %>% select(1:29)
GB <- rename(GB, rosette_ID = rosette_number)
GB <- rename(GB, suspected_clone_binary = suspected_clone)
GB$plant_survival <-GB$survival

check20 <- compare_df(cleanGB, GB, c("plant_id"))
check20$change_summary
check20$change_count
create_output_table(check20, limit = 400)

#### GH ####
cleanGH <- Y1 %>% filter(site_code == "GH") 
cleanGH <- cleanGH %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                              -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

GH <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/GH_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
GH <- GH %>% select(1:29)
GH <- rename(GH, rosette_ID = rosette_number)
GH <- rename(GH, suspected_clone_binary = suspected_clone)
GH$plant_survival <-GH$survival

check21 <- compare_df(cleanGH, GH, c("plant_id"))
check21$change_summary
check21$change_count
create_output_table(check21, limit = 400)

print("losing flowering stem information when multiple comma seperated values recorded. good idea to check previous years too. Alain 09/02/21")

#### JSJ ####
cleanJSJ <- Y1 %>% filter(site_code == "JSJ") 
cleanJSJ <- cleanJSJ %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

JSJ <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/JSJ_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
JSJ <- JSJ %>% select(1:29)
JSJ <- rename(JSJ, rosette_ID = rosette_number)
JSJ <- rename(JSJ, suspected_clone_binary = suspected_clone)
JSJ$plant_survival <-JSJ$survival

check22 <- compare_df(cleanJSJ, JSJ, c("plant_id"))
check22$change_summary
check22$change_count
create_output_table(check22, limit = 800)

print("multi comma seperated flow stems not reduced to 1 value. Fixed - Var16-21 script needs to be run independently")
print("herbiv yes/no column not filled in when text is present in herbiv comments - fixed in script Feb 2020")

#### NRM ####
cleanNRM <- Y1 %>% filter(site_code == "NRM") 
cleanNRM <- cleanNRM %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,
                                -y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

NRM <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/NRM_individual_plant_census_2016_Y1.csv",header = T, stringsAsFactors = FALSE)
NRM <- NRM %>% select(1:29)
NRM <- rename(NRM, rosette_ID = rosette_number)
NRM <- rename(NRM, suspected_clone_binary = suspected_clone)
NRM$plant_survival <-NRM$survival

check23 <- compare_df(cleanNRM, NRM, c("plant_id"))
check23$change_summary
check23$change_count
create_output_table(check23, limit = 700)

print("multi comma seperated flow stems not reduced to 1 value. Fixed - Var16-21 script needs to be run independently ")
print("herbiv yes/no column not filled in when text is present in herbiv comments - Fixed in script Feb 2020")


########################################### Year 2 Audit ###################################################################
Y2 <- read.csv(file.choose(), header = T, stringsAsFactors = T)
str(Y2)
set.seed(4136)
sample <- Y2 %>% filter(site_code %in% sample(levels(site_code), 15)) 
unique(sample$site_code)
#[1] AL2  ARH  BHU  BI   DP   EL   HR   IO   LK1  PM   RUSC SBK  TUE  UC   ZG

#### AL2 ####
cleanAL2 <- Y2 %>% filter(site_code == "AL2") 
cleanAL2 <- cleanAL2 %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes,	-plant_survival2,	-herbiv_orig)

AL2 <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/AL2_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
AL2 <- AL2 %>% select(1:29)
AL2 <- rename(AL2, rosette_ID = rosette_number)
AL2 <- rename(AL2, suspected_clone_binary = suspected_clone)
AL2$plant_survival <- AL2$survival  ## copying survival column so columns match exactly. required for compare_df

checkAL2 <- compare_df(cleanAL2, AL2, c("plant_id"))
checkAL2$change_summary
checkAL2$change_count
create_output_table(checkAL2, limit = 400)

#### ARH ####
cleanARH <- Y2 %>% filter(site_code == "ARH") 
cleanARH <- cleanARH %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

ARH <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/ARH_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
ARH <- ARH %>% select(1:29)
ARH <- rename(ARH, rosette_ID = rosette_number)
ARH <- rename(ARH, suspected_clone_binary = suspected_clone)
ARH$plant_survival <- ARH$survival  ## copying survival column so columns match exactly. required for compare_df

checkARH <- compare_df(cleanARH, ARH, c("plant_id"))
checkARH$change_summary
checkARH$change_count
create_output_table(checkARH, limit = 400)

print("plant ID's not concatenated, don't include in audit")

#### BHU  ####
cleanBHU <- Y2 %>% filter(site_code == "BHU") 
cleanBHU <- cleanBHU %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

BHU <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/BHU_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
BHU <- BHU %>% select(1:29)
BHU <- rename(BHU, rosette_ID = rosette_number)
BHU <- rename(BHU, suspected_clone_binary = suspected_clone)
BHU$plant_survival <- BHU$survival  ## copying survival column so columns match exactly. required for compare_df

checkBHU <- compare_df(cleanBHU, BHU, c("plant_id"))
checkBHU$change_summary
checkBHU$change_count
create_output_table(checkBHU, limit = 400)

#### BI ####
cleanBI <- Y2 %>% filter(site_code == "BI") 
cleanBI <- cleanBI %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

BI <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/BI_individual_plant_census_2018_Y2.csv",header = T, stringsAsFactors = FALSE)
BI <- BI %>% select(1:29)
BI <- rename(BI, rosette_ID = rosette_number)
BI <- rename(BI, suspected_clone_binary = suspected_clone)
BI$plant_survival <- BI$survival  ## copying survival column so columns match exactly. required for compare_df

checkBI <- compare_df(cleanBI, BI, c("plant_id"))
checkBI$change_summary
checkBI$change_count
create_output_table(checkBI, limit = 400)
#### DP ####
cleanDP <- Y2 %>% filter(site_code == "DP") 
cleanDP <- cleanDP %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

DP <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/DP_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
DP <- DP %>% select(1:29)
DP <- rename(DP, rosette_ID = rosette_number)
DP <- rename(DP, suspected_clone_binary = suspected_clone)
DP$plant_survival <- DP$survival  ## copying survival column so columns match exactly. required for compare_df

checkDP <- compare_df(cleanDP, DP, c("plant_id"))
checkDP$change_summary
checkDP$change_count
create_output_table(checkDP, limit = 400)
### EL ####
cleanEL <- Y2 %>% filter(site_code == "EL") 
cleanEL <- cleanEL %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

EL <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/EL_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
EL <- EL %>% select(1:29)
EL <- rename(EL, rosette_ID = rosette_number)
EL <- rename(EL, suspected_clone_binary = suspected_clone)
EL$plant_survival <- EL$survival  ## copying survival column so columns match exactly. required for compare_df

checkEL <- compare_df(cleanEL, EL, c("plant_id"))
checkEL$change_summary
checkEL$change_count
create_output_table(checkEL, limit = 400)
### HR ####
cleanHR <- Y2 %>% filter(site_code == "HR") 
cleanHR <- cleanHR %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

HR <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/HR_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
HR <- HR %>% select(1:29)
HR <- rename(HR, rosette_ID = rosette_number)
HR <- rename(HR, suspected_clone_binary = suspected_clone)
HR$plant_survival <- HR$survival  ## copying survival column so columns match exactly. required for compare_df

checkHR <- compare_df(cleanHR, HR, c("plant_id"))
checkHR$change_summary
checkHR$change_count
create_output_table(checkHR, limit = 400)
### IO ####
cleanIO <- Y2 %>% filter(site_code == "IO") 
cleanIO <- cleanIO %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

IO <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/IO_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
IO <- IO %>% select(1:29)
IO <- rename(IO, rosette_ID = rosette_number)
IO <- rename(IO, suspected_clone_binary = suspected_clone)
IO$plant_survival <- IO$survival  ## copying survival column so columns match exactly. required for compare_df

checkIO <- compare_df(cleanIO, IO, c("plant_id"))
checkIO$change_summary
checkIO$change_count
create_output_table(checkIO, limit = 400)
### LK1 ####
cleanLK1 <- Y2 %>% filter(site_code == "LK1") 
cleanLK1 <- cleanLK1 %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

LK1 <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/LK1_individual_plant_census_2018_Y2.csv",header = T, stringsAsFactors = FALSE)
LK1 <- LK1 %>% select(1:29)
LK1 <- rename(LK1, rosette_ID = rosette_number)
LK1 <- rename(LK1, suspected_clone_binary = suspected_clone)
LK1$plant_survival <- LK1$survival  ## copying survival column so columns match exactly. required for compare_df

checkLK1 <- compare_df(cleanLK1, LK1, c("plant_id"))
checkLK1$change_summary
checkLK1$change_count
create_output_table(checkLK1, limit = 400)
### PM ####
cleanPM <- Y2 %>% filter(site_code == "PM") 
cleanPM <- cleanPM %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

PM <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/PM_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
PM <- PM %>% select(1:29)
PM <- rename(PM, rosette_ID = rosette_number)
PM <- rename(PM, suspected_clone_binary = suspected_clone)
PM$plant_survival <- PM$survival  ## copying survival column so columns match exactly. required for compare_df

checkPM <- compare_df(cleanPM, PM, c("plant_id"))
checkPM$change_summary
checkPM$change_count
create_output_table(checkPM, limit = 400)
### RUSC ####
cleanRUSC <- Y2 %>% filter(site_code == "RUSC") 
cleanRUSC <- cleanRUSC %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

RUSC <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/RUSC_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
RUSC <- RUSC %>% select(1:29)
RUSC <- rename(RUSC, rosette_ID = rosette_number)
RUSC <- rename(RUSC, suspected_clone_binary = suspected_clone)
RUSC$plant_survival <- RUSC$survival  ## copying survival column so columns match exactly. required for compare_df

checkRUSC <- compare_df(cleanRUSC, RUSC, c("plant_id"))
checkRUSC$change_summary
checkRUSC$change_count
create_output_table(checkRUSC, limit = 400)
### SBK ####
cleanSBK <- Y2 %>% filter(site_code == "SBK") 
cleanSBK <- cleanSBK %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

SBK <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/SBK_individual_plant_census_2018_Y2.csv",header = T, stringsAsFactors = FALSE)
SBK <- SBK %>% select(1:29)
SBK <- rename(SBK, rosette_ID = rosette_number)
SBK <- rename(SBK, suspected_clone_binary = suspected_clone)
SBK$plant_survival <- SBK$survival  ## copying survival column so columns match exactly. required for compare_df

checkSBK <- compare_df(cleanSBK, SBK, c("plant_id"))
checkSBK$change_summary
checkSBK$change_count
create_output_table(checkSBK, limit = 400)

print("x_coord were recorded in quadrants (a,b,c,d) but these have been removed by the cleaning script - should we keep?")
### TUE ####
cleanTUE <- Y2 %>% filter(site_code == "TUE") 
cleanTUE <- cleanTUE %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

TUE <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/TUE_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
TUE <- TUE %>% select(1:29)
TUE <- rename(TUE, rosette_ID = rosette_number)
TUE <- rename(TUE, suspected_clone_binary = suspected_clone)
TUE$plant_survival <- TUE$survival  ## copying survival column so columns match exactly. required for compare_df

checkTUE <- compare_df(cleanTUE, TUE, c("plant_id"))
checkTUE$change_summary
checkTUE$change_count
create_output_table(checkTUE, limit = 400)
### UC ####
cleanUC <- Y2 %>% filter(site_code == "UC") 
cleanUC <- cleanUC %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

UC <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/UC_individual_plant_census_2017_Y2.csv",header = T, stringsAsFactors = FALSE)
UC <- UC %>% select(1:29)
UC <- rename(UC, rosette_ID = rosette_number)
UC <- rename(UC, suspected_clone_binary = suspected_clone)
UC$plant_survival <- UC$survival  ## copying survival column so columns match exactly. required for compare_df

checkUC <- compare_df(cleanUC, UC, c("plant_id"))
checkUC$change_summary
checkUC$change_count
create_output_table(checkUC, limit = 400)
### ZG ####

cleanZG <- Y2 %>% filter(site_code == "ZG") 
cleanZG <- cleanZG %>% select(- unique_plot_year_id,	-plant_unique_id,	-unique_plot_id,	-x_coord_notes,	-y_coord_notes,	-suspected_clone_notes, 	-plant_survival2,	-herbiv_orig)

ZG <- read.csv("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Site data/Y2/ZG_individual_plant_census_2018_Y2.csv",header = T, stringsAsFactors = FALSE)
ZG <- ZG %>% select(1:29)
ZG <- rename(ZG, rosette_ID = rosette_number)
ZG <- rename(ZG, suspected_clone_binary = suspected_clone)
ZG$plant_survival <- ZG$survival  ## copying survival column so columns match exactly. required for compare_df

checkZG <- compare_df(cleanZG, ZG, c("plant_id"))
checkZG$change_summary
checkZG$change_count
create_output_table(checkZG, limit = 400)



