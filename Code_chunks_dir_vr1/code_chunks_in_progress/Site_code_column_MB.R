###### Clean site_code column ######
#That script tries to solve the different issues met with the site_codes

#Maude Baudraz January 2019
#ppn<-read.csv("./Code_chunks_dir_vr1/code_chunks_in_progress/fullPPN_dataset_2019-01-28_.csv")
ppn<-mydata

#### Visualize data ####

table(ppn$site_code)

#One line has an empty site code: 
ppn[which(is.na(ppn$site_code)),] 

#Extract the coordinate of the transect that has no site code
Coord<-ppn$transect_Lat_start[which(is.na(ppn$site_code))] 

#Exctract the site code of the lines that have the same coordinate:
missing_site_code<-unique(ppn$site_code[which(ppn$transect_Lat_start==Coord)],na.omit=T)
if(length(missing_site_code)==2){#If there only is one site that has the same coordinates, plus the NA level you're looking to remove
  ppn$site_code[which(is.na(ppn$site_code))] <- missing_site_code[which(is.na(missing_site_code)==F)]
}#then attribute that code to the line with a NA. 

if(length(missing_site_code)!=2){#If there is more than two levels, Two populations have the same North coordinates
 print("error message; there is an unforseen case in your data. Two populations have the same North coordinates, although their site ID is different. Site_code_column_script line 47")}#


ppn[which(is.na(ppn$site_code)),] # NAs solved
ppn$site_code<-droplevels(ppn$site_code)
table(ppn$site_code) #looks ok now

####Checking the site codes match the Site description ones 
SiteDescriptionSheet<-read.csv("Coordinates_Feb2019_site_level.csv")
SiteDescriptionSheet$site_code

for(i in 1:length(levels(ppn$site_code))){#This loop will output a error message if a site code in the demographic data does not match any site code in the summary sheet. 
  if((levels(ppn$site_code)[i]%in%levels(SiteDescriptionSheet$site_code))==T){
    
  }
  if((levels(ppn$site_code)[i]%in%levels(SiteDescriptionSheet$site_code))==F){
    print(paste0("Site code ",levels(ppn$site_code)[i]," does not match the summary sheet"))
  }
}
levels(SiteDescriptionSheet$site_code)#Check the site codes in the description sheet to know where the difference stem from 
#On January 28th, the site codes C-DF and LK were not matching the site description (should read CDF and LK1). There is also an empty space after "TG" in the site description sheet. 
levels(ppn$site_code)<-c(levels(ppn$site_code),"CDF","LK1")
ppn$site_code[which(ppn$site_code=="C-DF")]<-"CDF"
ppn$site_code[which(ppn$site_code=="LK")]<-"LK1"


mydata<-ppn
