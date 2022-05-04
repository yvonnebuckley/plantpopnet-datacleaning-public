# Outlier detection
#Alain Finn
#Feb 2019

###Issues###
#data reads in as factors when they should be numerical, strings as factors corrects but may not work when added to compiler
#as script wont be reading in dataset
#columns contain text when they should contain only numbers
#some sites have multplie values for flow stems & inflorescences
#converting to as.numeric(as.character)(x) removes all of these. however it removes the multiple flow. stems also
#code below for pulling the longest stem and corresponding inflor is not working as expected. 




setwd("~/PLANTPOPNET/Plantpopnet-Data-Cleaning/Code_chunks_dir_vr1/code_chunks_in_progress")
# To detect outliers within sites on quantitative variables
library(dplyr)
library(stringr)
mydata <- read.csv("fullPPN_dataset_2019-03-06_.csv")


str(mydata)
#columns 16:21 should be numeric. If they are converted now, we would lose information from sites
#that have multple values in a cell (ie ACR flow stem height, inflor lengths). so we must deal with this first.


#shows values from cells in columns 14:21 that are not numbers
for(i in c(16:21)){
  print(names(mydata)[i])
  cha <- (str_extract(mydata[,i], "[aA-zZ]+"))
  print(cha[which(is.na(cha)==F)])
}
#Question - How to deal with these? move to new column and replace as NA's?




#Sites ACR, JSJ, HAS, STR, NRM  have multiple values for flow stem and inflor length in cells
st<-list()
sub<-subset(mydata,site_code=="ACR"| site_code=="JSJ"| site_code=="NRM"| site_code=="HAS"| site_code=="STR")

names(mydata)[20]
for(i in 1:nrow(mydata[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"),])){
  st[i]<-str_split(sub$fl_stem_height[i],pattern=",") #Separate the different values in cell
}

stem_height<-c()
stem_pos<-c()
for(i in 1:length(st)){
  print(i)
  if(is.na(st[[i]])){
    is.na(stem_height[i])<-c(1)
    is.na(stem_pos[i])<-c(1)
  }else{
    stem_height[i]<-max(st[[i]],na.rm=T)  #select only highest stem
    stem_pos[i]<-which(st[[i]]==max(st[[i]],na.rm=T)) #store it's position (to select corresponding inflorescence)
  }}

infl<-list()
names(mydata)[21]
for(i in 1:nrow(mydata[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"),])){
  infl[i]<-str_split(sub$inflor_length[i],pattern=",")
}
infl_height<-c()
for(i in 1:length(infl)){
  if(is.na(stem_pos[i])){
    is.na(infl_height[i])<-c(1)
  }else{
    infl_height[i]<-infl[[i]][stem_pos[i]] #keep only the inflorescence corresponding to the tallest stem
  }}

infl_height <- as.numeric(str_extract(infl_height, "[0-9]+?")) #Remove all the non numeric values

mydata$inflor_length[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR")]<-infl_height #insert in original data set (in my case "data")
mydata$fl_stem_height<-as.character(mydata$fl_stem_height)
mydata$fl_stem_height[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR")]<-stem_height

#sub<-subset(mydata,site_code=="ACR"| site_code=="JSJ"| site_code=="NRM"| site_code=="HAS"| site_code=="STR")

#now covert columns to numeric
cols = c(16:21)
mydata[,cols] = apply(mydata[,cols],2,function(x) as.numeric(as.character(x)))

str(mydata)


###Now recheck that there are only numbers: 
for(i in c(16:21)){
  print(names(mydata)[i])
  cha <- (str_extract(mydata[,i], "[aA-zZ]+"))
  print(cha[which(is.na(cha)==F)])
}#Better. Can go on now. 


outliers <- as.data.frame(mydata[c(1,17,18,20,21,29)])
#outliers$site_code_yr <- paste(outliers$site_code, outliers$s_year)

#subset data by study year 
Y0 <- outliers %>% filter (s_year == "Y0")
Y1 <- outliers %>% filter (s_year == "Y1")
Y2 <- outliers %>% filter (s_year == "Y2")


##########  Y0 ############

##### Leaf length #######
boxplot(leaf_length ~ site_code, data = Y0)

#call quantiles by site x leaf length
do.call(rbind, with(Y0, tapply(leaf_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#TNC has 2 possible typos
#EL + HV possibly in wrong units (cm instead of mm)


###### leaf width ######
boxplot(leaf_width ~ site_code, data = Y0)
do.call(rbind, with(Y0, tapply(leaf_width, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#HV looks to be in the wrong unit
#Sites with unusual values
#STR, BHU, UC, JSJ

#### fl_stem_height ######
boxplot(fl_stem_height ~ site_code, data = Y0)
do.call(rbind, with(Y0, tapply(fl_stem_height, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#STR max value


#### inflor_length #####
boxplot(inflor_length ~ site_code, data = Y0)
do.call(rbind, with(Y0, tapply(inflor_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#RUSC max value
#ROIS 0s
#TO 0s in lower quantiles
#ACR + PA showing NAs
#PC max value



############ Y1 ##########

##### Leaf length #######
boxplot(leaf_length ~ site_code, data = Y1)
do.call(rbind, with(Y1, tapply(leaf_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#Sites with obvious errors
#RUSC, GB(zeros), HV (check units)


###### leaf width #######
boxplot(leaf_width ~ site_code, data = Y1)
do.call(rbind, with(Y1, tapply(leaf_width, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#Check units for EE, HV
#GB has zero for leaf width
#NRM posisble typo
#RUSC typos and 0 for lowest value
#PM possible typo

#### fl_stem_height ######
boxplot(fl_stem_height ~ site_code, data = Y1)
do.call(rbind, with(Y1, tapply(fl_stem_height, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#### inflor_length #####
boxplot(inflor_length ~ site_code, data = Y1)
do.call(rbind, with(Y1, tapply(inflor_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
########### Y2 ############
##### Leaf length #######
boxplot(leaf_length ~ site_code, data = Y2)
do.call(rbind, with(Y2, tapply(leaf_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#GB 0 + max value 
#CH min value check


###### leaf width #######

boxplot(leaf_width ~ site_code, data = Y2)
do.call(rbind, with(Y2, tapply(leaf_width, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#GB 0 + max value 
#SC 0 
#GU max value
#RUSC max value

#### fl_stem_height ######
boxplot(fl_stem_height ~ site_code, data = Y2)
do.call(rbind, with(Y2, tapply(fl_stem_height, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))
#### inflor_length #####
boxplot(inflor_length ~ site_code, data = Y2)
do.call(rbind, with(Y2, tapply(inflor_length, 
                               interaction(site_code = site_code),
                               quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))














