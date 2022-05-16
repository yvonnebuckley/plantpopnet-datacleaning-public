# Outlier detection
#Alain Finn
#Feb 2019
#Review; Maude Baudraz, 02.04.2019

###Issues###
#1. columns contain text when they should contain only numbers
#2. some sites have multplie values for flow stems & inflorescences
#3. check for errors and outliers.




#1. non numeric values
#str(mydata)
#columns 16:21 should be numeric. If they are converted now, we would lose information from sites
#that have multple values in a cell (ie ACR flow stem height, inflor lengths). so we must deal with this first.




str(mydata)
#shows values from cells in columns 16:21 that are not numbers 
x <- for(i in c(16:21)){
  print(names(mydata)[i])
  cha <- (str_extract(mydata[,i], "[aA-zZ]+"))
  print(cha[which(is.na(cha)==F)])
} 
if(any(x = TRUE)) warning ("Non numeric values present")

## remove text from no_leaves column
## "leaves"
no_lvs <- mydata$no_leaves
no_lvs[grep("leaves", no_lvs, ignore.case = TRUE)] <- NA
mydata$no_leaves <- no_lvs

## remove text from leaf_length column
##"eaten" "RUSC"
lf_ln <- mydata$leaf_length
lf_ln[grep("eaten", lf_ln, ignore.case = TRUE)] <- NA
lf_ln[grep("RUSC", lf_ln, ignore.case = TRUE)] <- NA
mydata$leaf_length <- lf_ln
### Remove text from no_fl_stems column
##"but" "no"  "no"  "no"
no_st <- mydata$no_fl_stems
no_st[grep("but", no_st, ignore.case = TRUE)] <- NA
no_st[grep("no", no_st, ignore.case = TRUE)] <- NA
mydata$no_fl_stems <- no_st

#### Remove text from fl_stem_height column and add to other_comments
# "grazed" , "eaten"  , "broken" , "damaged", "Damaged", "see")
fl_st <- mydata$fl_stem_height
fl_st[grep("grazed", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("eaten", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("broken", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("damaged", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("Damaged", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("see", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("MISSING", fl_st, ignore.case = TRUE)] <- NA
fl_st[grep("Broken", fl_st, ignore.case = TRUE)] <- NA
mydata$fl_stem_height <- fl_st

infl <- mydata$inflor_length
infl[grep("inflorescence", infl, ignore.case = TRUE)] <- NA
infl[grep("dried", infl, ignore.case = TRUE)] <- NA
infl[grep("dispersed", infl, ignore.case = TRUE)] <- NA
infl[grep("mature", infl, ignore.case = TRUE)] <- NA
infl[grep("MISSING", infl, ignore.case = TRUE)] <- NA
infl[grep("n", infl, ignore.case = TRUE)] <- NA
infl[grep("damaged", infl, ignore.case = TRUE)] <- NA
infl[grep("Damaged", infl, ignore.case = TRUE)] <- NA
infl[grep("see", infl, ignore.case = TRUE)] <- NA
mydata$inflor_length <- infl


x <- for(i in c(16:21)){
  print(names(mydata)[i])
  cha <- (str_extract(mydata[,i], "[aA-zZ]+"))
  print(cha[which(is.na(cha)==F)])
} 
# fl_stem_height has no alpha characters remaining! proceed to step 2
# alphas in inflo_height will be dealt with later on

mydata_prefix <- mydata #storing another data object incase the following section creates an error
#2. Multiple values recorded for flowering stem columns
#Sites ACR, JSJ, HAS, STR, NRM, KM  have multiple values for flow stem and inflor length in cells
is.na(mydata$fl_stem_height) <- ifelse(mydata$fl_stem_height == "ERROR", 1, 0)

st<-list()
sub<-subset(mydata,site_code=="ACR"| site_code=="JSJ"| site_code=="NRM"| site_code=="HAS"| site_code=="STR"| site_code== "KM"| site_code == "GH")

names(mydata)[20]
for(i in 1:nrow(mydata[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"| mydata$site_code=="KM"| mydata$site_code == "GH"),])){
  st[i]<-str_split(sub$fl_stem_height[i],pattern=",") #Separate the different values in cell
}

stem_height<-c()
stem_pos <- c()
for(i in 1:length(st)){
  #print(i)
  if(all(is.na(st[[i]]))){
    is.na(stem_height[i])<-c(1)
    is.na(stem_pos[i])<-c(1)
  } else {
    stem_height[i] <- max(as.numeric(st[[i]]),na.rm=T)  #select only highest stem
    stem_pos[i] <- which(as.numeric(st[[i]]) == max(as.numeric(st[[i]]),na.rm=T)) #store it's position (to select corresponding inflorescence)
  }}

infl<-list()
names(mydata)[21]
for(i in 1:nrow(mydata[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"| mydata$site_code=="KM"| mydata$site_code == "GH"),])){
  infl[i]<-str_split(sub$inflor_length[i],pattern=",")
}
infl_height<-c()
for(i in 1:length(infl)){
  if(is.na(stem_pos[i])){
    is.na(infl_height[i])<-c(1)
  }else{
    infl_height[i]<-infl[[i]][stem_pos[i]] #keep only the inflorescence corresponding to the tallest stem
  }}

infl_height <- as.numeric(str_extract(infl_height, "[0-9]+")) #Remove all the non numeric values
    
mydata$inflor_length[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"| mydata$site_code=="KM"| mydata$site_code == "GH")]<-infl_height #insert in original data set (in my case "data")
mydata$fl_stem_height<-as.character(mydata$fl_stem_height)
mydata$fl_stem_height[which(mydata$site_code=="ACR"| mydata$site_code=="JSJ"| mydata$site_code=="NRM"| mydata$site_code=="HAS"| mydata$site_code=="STR"| mydata$site_code=="KM"| mydata$site_code == "GH")]<-stem_height

#sub<-subset(mydata,site_code=="ACR"| site_code=="JSJ"| site_code=="NRM"| site_code=="HAS"| site_code=="STR")

#now covert columns to numeric
cols = c(16:21) 
mydata[,cols] = apply(mydata[,cols],2,function(x) as.numeric(as.character(x)))

str(mydata)

### remove 0s from flow. stem height and inflo. length as they affect means etc
stems <- mydata$fl_stem_height
stems[stems == 0] <- NA
inflor <- mydata$inflor_length
inflor[inflor == 0] <- NA

mydata$fl_stem_height <- stems
mydata$inflor_length <- inflor



###Now recheck that there are only numbers: 
y <- for(i in c(16:21)){
  print(names(mydata)[i])
  cha <- (str_extract(mydata[,i], "[aA-zZ]+?"))
  print(cha[which(is.na(cha)==F)]) 
}



outliers <- mydata %>% 
  select(site_code, plant_unique_id, no_leaves, leaf_length, leaf_width, no_fl_stems, fl_stem_height, inflor_length, s_year)

#subset data by study year 
Y0 <- outliers %>% filter (s_year == "Y0")


sum0 <- Y0 %>% 
    group_by(site_code) %>% 
    summarize(avgleaflen = mean(leaf_length, na.rm = TRUE), maxlen = max(leaf_length, na.rm = TRUE),
              avgleafwid = mean(leaf_width, na.rm = TRUE), maxwid = max(leaf_width, na.rm = TRUE),
              avgstemheight = mean(fl_stem_height, na.rm = TRUE), maxstem = max(fl_stem_height, na.rm = TRUE),
              avginflor = mean(inflor_length, na.rm = TRUE), maxinflor = max(inflor_length, na.rm = TRUE))

View(sum0)



##########  Y0 ############

####### no. of leaves ########
#par(mar=c(5,5,3,3))
#boxplot(no_leaves ~ site_code, data = Y0,las=2, main= "number of leaves", ylab="no. leaves",xlab=" ")
#title(xlab="Site", line=4)
#print ("check for typos in sites, UC, BG, GB")

#call quantiles by site x leaf length
#do.call(rbind, with(Y0, tapply(leaf_length, 
#                               interaction(site_code = site_code),
#                                quantile, probs = c(0, 0.1, 0.5, 0.9, 1), na.rm = T)))

x1 <- Y0 %>% group_by(site_code) %>% mutate(z_score = scale(no_leaves))
x1 <- x1 %>% filter(z_score >= -5&z_score >=5)


##### Leaf length #######
x2<- Y0 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_length))
x2 <- x2 %>% filter(z_score >= -5&z_score >=5)

###### leaf width ######
x3 <- Y0 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_width))
x3 <- x3 %>% filter(z_score >= -5&z_score >=5)


#### fl_stem_height ######
x4 <- Y0 %>% group_by(site_code) %>% mutate(z_score = scale(fl_stem_height))
x4 <- x4%>% filter(z_score >= -5&z_score >=5)

#### inflor_length #####
x5 <- Y0 %>% group_by(site_code) %>% mutate(z_score = scale(inflor_length))
x5 <- x5%>% filter(z_score >= -5&z_score >=5)


# ############ Y1 ##########
# 
# ####### no. of leaves ########
# x1 <- Y1 %>% group_by(site_code) %>% mutate(z_score = scale(no_leaves))
# x1 <- x1 %>% filter(z_score >= -5&z_score >=5)
# 
# 
# ##### Leaf length #######
# x2<- Y1 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_length))
# x2 <- x2 %>% filter(z_score >= -5&z_score >=5)
# 
# ###### leaf width ######
# x3 <- Y1 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_width))
# x3 <- x3 %>% filter(z_score >= -5&z_score >=5)
# 
# 
# #### fl_stem_height ######
# x4 <- Y1 %>% group_by(site_code) %>% mutate(z_score = scale(fl_stem_height))
# x4 <- x4%>% filter(z_score >= -5&z_score >=5)
# 
# #### inflor_length #####
# x5 <- Y1 %>% group_by(site_code) %>% mutate(z_score = scale(inflor_length))
# x5 <- x5%>% filter(z_score >= -5&z_score >=5)
# 
# 
# ############ Y2 ##########
# 
# ####### no. of leaves ########
# x1 <- Y2 %>% group_by(site_code) %>% mutate(z_score = scale(no_leaves))
# x1 <- x1 %>% filter(z_score >= -5&z_score >=5)
# 
# 
# ##### Leaf length #######
# x2<- Y2 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_length))
# x2 <- x2 %>% filter(z_score >= -5&z_score >=5)
# 
# ###### leaf width ######
# x3 <- Y2 %>% group_by(site_code) %>% mutate(z_score = scale(leaf_width))
# x3 <- x3 %>% filter(z_score >= -5&z_score >=5)
# 
# 
# #### fl_stem_height ######
# x4 <- Y2 %>% group_by(site_code) %>% mutate(z_score = scale(fl_stem_height))
# x4 <- x4%>% filter(z_score >= -5&z_score >=5)
# 
# #### inflor_length #####
# x5 <- Y2 %>% group_by(site_code) %>% mutate(z_score = scale(inflor_length))
# x5 <- x5%>% filter(z_score >= -5&z_score >=5)


rm(x1,x2,x3,x4,x5,Y0,Y1,Y2,outliers,x,y,infl,infl_height,stem_height,st,cols,stem_pos,cha,sub)


