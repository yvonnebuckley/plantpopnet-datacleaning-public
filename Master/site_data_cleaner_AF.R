### Site Data Cleaner###
### Author: Alain Finn
### Latest Update March 2020 ###
### This script is to tidy up individual plant census data BEFORE it is added in to the database and compiled.


### Requirements for data to run properly in the compiler and proceeding cleaning scripts
### 29 variables with the correct headers
#### site_code,transect_Lat_start,transect_Lon_start,transect_Lat_stop,transect_Lon_stop,transect,plot,number_seedlings ,plant ID,x_coord,y_coord,
#### suspected_clone,survival,no_rosettes,rosette_number,no_leaves,leaf_length,leaf_width,no_fl_stems,fl_stem_height,inflor_length,inflor_phenology,
#### disease (yes/no),disease_comments,herbivory (yes/no),herbivory_comments,other_comments,c_year,s_year

### Ensure that data for columns 1:7 are present, people are inclined to leave this blank for new individuals in a plot

library(xlsx)
library(dplyr)

df <- read.xlsx2(file.choose(),sheetIndex = 1)
mydata <- df


#1. Remove empty columns after column 29
mydata <- mydata[1:29]

#### remove unnecessary columns 
mydata <- subset(mydata, select = -c(No., survival))

#2. Check correct column headers
#a comments/other_comments column needs renaming
mydata <- rename(mydata, other_comments = General.observations)
#or
mydata$other_comments <- NA

#b 
#c_year and s_year columns missing
# check if they are filled in and are correct format ie 2019 Y4 not 2019 5
mydata$c_year <- "2020"
mydata$s_year <- "Y5"

 


#d
names(mydata)
headers<- c("site_code","transect_Lat_start","transect_Lon_start","transect_Lat_stop","transect_Lon_stop","transect","plot","number_seedlings" ,"plant_id","x_coord","y_coord","suspected_clone",
            "survival","no_rosettes","rosette_number","no_leaves","leaf_length","leaf_width","no_fl_stems","fl_stem_height","inflor_length","inflor_phenology",
             "disease (yes/no)","disease_comments","herbivory (yes/no)","herbivory_comments","other_comments","c_year","s_year"
)
testdf <- data.frame(matrix(ncol = 29, nrow = 0))
colnames(testdf) <- headers

func <- function(x,y) {
  for (i in names(x)) {
    
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
      
      
    }  
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

func(mydata, testdf)

colnames(mydata) <- headers

func(mydata, testdf)
rm(testdf)




str(mydata)
#all columns are factors. Change to correct type. 

cols <- c(8,10,11,14,15,16,17,18,19,20,21,28)
mydata[,cols] = apply(mydata[,cols], 2, function(x) as.numeric(as.character(x)))
str(mydata)

#3. Fill in missing values - site_code which can be copied down from other cells
for (row in 2:length(mydata$site_code)){ # 2 so you don't affect column names
  if(mydata$site_code[row] == "") {    # if its empty...
    mydata$site_code[row] = mydata$site_code[row-1] # ...replace with previous row's value
  }
}

#4. Transect
#add T to transect column if missing 
mydata$transect <- sub("^", "T", mydata$transect)

for (row in 2:length(mydata$transect)){ # 2 so you don't affect column names
  if(mydata$transect[row] == "") {    # if its empty...
    mydata$transect[row] = mydata$transect[row-1] # ...replace with previous row's value
  }
}

# Plant_ID
mydata$plant_id <- gsub("^.{0,5}", "", mydata$plant_id)
mydata$plant_id <-gsub('[.]', '', mydata$plant_id)

#5. Plot
mydata$plot <- sub("^", "P", mydata$plot)

for (row in 2:length(mydata$plot)){ # 2 so you don't affect column names
  if(mydata$plot[row] == "") {    # if its empty...
    mydata$plot[row] = mydata$plot[row-1] # ...replace with previous row's value
  }
}


#6. Fill in missing values for lat lon coordinates. NOTE: 2 or more transects will require conditional statements
mydata$transect_Lat_start <- "53.19242"
mydata$transect_Lon_start <- "-1.762289"
mydata$transect_Lat_stop <- "53.19248"
mydata$transect_Lon_stop <- "-1.762431"


for (row in 2:length(mydata$transect_Lat_start)){ # 2 so you don't affect column names
  if(mydata$transect_Lat_start[row] == "") {    # if its empty...
    mydata$transect_Lat_start[row] = mydata$transect_Lat_start[row-1] # ...replace with previous row's value
  }
}

for (row in 2:length(mydata$transect_Lat_stop)){ # 2 so you don't affect column names
  if(mydata$transect_Lat_stop[row] == "") {    # if its empty...
    mydata$transect_Lat_stop[row] = mydata$transect_Lat_stop[row-1] # ...replace with previous row's value
  }
}

for (row in 2:length(mydata$transect_Lon_start)){ # 2 so you don't affect column names
  if(mydata$transect_Lon_start[row] == "") {    # if its empty...
    mydata$transect_Lon_start[row] = mydata$transect_Lon_start[row-1] # ...replace with previous row's value
  }
}

for (row in 2:length(mydata$transect_Lon_stop)){ # 2 so you don't affect column names
  if(mydata$transect_Lon_stop[row] == "") {    # if its empty...
    mydata$transect_Lon_stop[row] = mydata$transect_Lon_stop[row-1] # ...replace with previous row's value
  }
}

#7 removing N,S,E,W from lat/lon
mydata$transect_Lat_start <- gsub("[a-zA-Z]","",mydata$transect_Lat_start)
mydata$transect_Lon_start <- gsub("[a-zA-Z]","",mydata$transect_Lon_start)
mydata$transect_Lat_stop <- gsub("[a-zA-Z]","",mydata$transect_Lat_stop)
mydata$transect_Lon_stop <- gsub("[a-zA-Z]","",mydata$transect_Lon_stop)



#8 Checking measured variables
## look out for:
# variables measured in cm
# massive outliers 
sum <- mydata %>% 
  group_by(site_code) %>% 
  summarize(avgleaflen = mean(leaf_length, na.rm = TRUE), maxlen = max(leaf_length, na.rm = TRUE), minlen = min(leaf_length, na.rm = TRUE),
            avgleafwid = mean(leaf_width, na.rm = TRUE), maxwid = max(leaf_width, na.rm = TRUE), minwid = min(leaf_width, na.rm = TRUE),
            avgstemheight = mean(fl_stem_height, na.rm = TRUE), maxstem = max(fl_stem_height, na.rm = TRUE),
            avginflor = mean(inflor_length, na.rm = TRUE), maxinflor = max(inflor_length, na.rm = TRUE))

View(sum)

#multply values by 10 to covert to mm 
mydata$leaf_length <- mydata$leaf_length*10
mydata$leaf_width <- mydata$leaf_width*10
mydata$fl_stem_height <- mydata$fl_stem_height*10
mydata$inflor_length <- mydata$inflor_length*10

## select columns and rows
mydata <- mydata[1:29]
mydata <- slice_head(mydata, n = 125)

##write to excel file
write.xlsx(mydata, "C:/Users/alfinn/Desktop/Clean IPC/CPA_individual_plant_census_2020_Y0.xlsx", showNA = F)

write.csv(mydata,"C:/Users/alfinn/Desktop/clean_site.csv", row.names = F)
