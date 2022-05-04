install.packages("rworldmap")
library(rworldmap) 
plot(coastsCoarse)


##Hi Ruth! Here my attempts to get the transect coordinates to work :

#Load the data: 
pops_2<-read.csv("/Users/maudeb/Documents/PlantPopNet/Plantpopnet-Data-Cleaning/fullPPN_dataset_2018-06-12_.csv",sep=",",header=T)

names(pops_2)
unique(pops_2$transect_Lon_start)

#Create a sub- dataframe with only one line per site (to be able to spot general formattign mistakes)
df<-c()
for(sc in 1:length(unique(pops_2$site_code))){
  df<-rbind(df,pops_2[which(pops_2$site_code==unique(pops_2$site_code)[sc])[1],])
}

#Data frame to see one lat and lon coordinates
loc<-data.frame(df$transect_Lat_start,df$transect_Lon_start)
#Data frame to see starta and end lat and lon coordinates
loc2<-data.frame(df$transect_Lat_start,df$transect_Lon_start,df$transect_Lat_stop,df$transect_Lon_stop)

##Troubleshooting: 
loc2

#Three problems I could see: 
#1: some have the N and E or N and W on.
#2. What format is 10 S 0607724               4263981 in ?
#3. What happened with this bit? \xa037.95865           -78.4732166


#The problem number three makes the coordinates unreadable in R, so fix first: 

#3. What happened with this bit? \xa037.95865           -78.4732166
#Brought it forward as it seems to make things but

grepl("\xa", pops_2$transect_Lat_start)

#The error message returns a number of lines: 
pops_2[18394,]
pops_2[which(pops_2$site_code=="VA"),c(2:5)]
levels(pops_2$transect_Lat_start)<-c(levels(pops_2$transect_Lat_start),"37.95865")
pops_2[which(pops_2$site_code=="VA"),2]<-"37.95865"


#2. What format is 10 S 0607724               4263981 in ?
#A convertor online (http://www.earthpoint.us/Convert.aspx) says it may be in California, at 38.5177007°, -121.7643222: 
pops_2[which(pops_2$transect_Lon_start=="4263981"),]
#Indeed, the river next to the plot is Putah Creek and the plot ID is PC
levels(pops_2$transect_Lat_start)<-c(levels(pops_2$transect_Lat_start),"38.5177007")
levels(pops_2$transect_Lon_start)<-c(levels(pops_2$transect_Lon_start),"-121.7643222")
levels(pops_2$transect_Lat_stop)<-c(levels(pops_2$transect_Lat_stop),"38.5176821")
levels(pops_2$transect_Lon_stop)<-c(levels(pops_2$transect_Lon_stop),"-121.7642651")

numbs_1<-which(pops_2$transect_Lon_start=="4263981")
pops_2[numbs_1,2]<-"38.5177007"
pops_2[numbs_1,3]<-"-121.7643222"
pops_2[numbs_1,4]<-"38.5176821"
pops_2[numbs_1,5]<-"-121.7642651"

#pops_2$transect_Lat_start[which(pops_2$site_code=="VA")]<-"37.95865"
#pops_2$transect_Lat_start[c(18394:18398)]
#levels(pops_2$transect_Lat_start)<-c(levels(pops_2$transect_Lat_start),"37.95865")
#pops_2$transect_Lat_start[c(18394:18398)]<-"37.95865"
#levels(pops_2$transect_Lat_start)

#grepl("\xa", pops_2$transect_Lat_start)

#pops_2$transect_Lat_start[c(18399:18403)]
#levels(pops_2$transect_Lat_start)<-c(levels(pops_2$transect_Lat_start),"37.95865")
#pops_2$transect_Lat_start[c(18399:18403)]<-"37.95865"
#levels(pops_2$transect_Lat_start)

#grepl("\xa", pops_2$transect_Lat_start)

#pops_2$transect_Lat_start[c(18404:18408)]
#levels(pops_2$transect_Lat_start)<-c(levels(pops_2$transect_Lat_start),"37.95865")
#pops_2$transect_Lat_start[c(18404:18408)]<-"37.95865"
#levels(pops_2$transect_Lat_start)

#grepl("\xa", pops_2$transect_Lat_start)




#1: some have the N and E or N and W on.
#52.144308 N            -8.948419W is in Cork, just suppress the N and W
#36.37866 N          -121.56724 W is in estonia
#53.05706 N           009.51672 W is in germany... 
#46.74930            17.23734 is in Romania, in a city?
#All seem likely options; suppress the Ns and Ws. BUT WE REALLY NEED TO HAVE ACCESS TO THE SITE DESCRIPTION FILE

pops_2$transect_Lat_start<-as.character(pops_2$transect_Lat_start)

pops_2$transect_Lat_start[which(grepl("N", pops_2$transect_Lat_start)==T)]
numbs<-which(grepl("N", pops_2$transect_Lat_start)==T)
for(l in 1:length(which(grepl("N", pops_2$transect_Lat_start)==T))){
  pops_2$transect_Lat_start[numbs[l]]<-gsub('N','',pops_2$transect_Lat_start[numbs[l]])
}

pops_2$transect_Lat_start[which(grepl("S", pops_2$transect_Lat_start)==T)]
numbs<-which(grepl("S", pops_2$transect_Lat_start)==T)
for(l in 1:length(which(grepl("S", pops_2$transect_Lat_start)==T))){
  pops_2$transect_Lat_start[numbs[l]]<-gsub('S','',pops_2$transect_Lat_start[numbs[l]])
}

pops_2$transect_Lat_stop<-as.character(pops_2$transect_Lat_stop)

pops_2$transect_Lat_stop[which(grepl("N", pops_2$transect_Lat_stop)==T)]
numbs<-which(grepl("N", pops_2$transect_Lat_stop)==T)
for(l in 1:length(which(grepl("N", pops_2$transect_Lat_stop)==T))){
  pops_2$transect_Lat_stop[numbs[l]]<-gsub('N','',pops_2$transect_Lat_stop[numbs[l]])
}

pops_2$transect_Lat_stop[which(grepl("S", pops_2$transect_Lat_stop)==T)]
numbs<-which(grepl("S", pops_2$transect_Lat_stop)==T)
for(l in 1:length(which(grepl("S", pops_2$transect_Lat_stop)==T))){
  pops_2$transect_Lat_stop[numbs[l]]<-gsub('S','',pops_2$transect_Lat_stop[numbs[l]])
}

pops_2$transect_Lon_start<-as.character(pops_2$transect_Lon_start)

pops_2$transect_Lon_start[which(grepl("W", pops_2$transect_Lon_start)==T)]
numbs<-which(grepl("W", pops_2$transect_Lon_start)==T)
for(l in 1:length(which(grepl("W", pops_2$transect_Lon_start)==T))){
  pops_2$transect_Lon_start[numbs[l]]<-gsub('W','',pops_2$transect_Lon_start[numbs[l]])
}
pops_2$transect_Lon_start[which(grepl("E", pops_2$transect_Lon_start)==T)]
numbs<-which(grepl("E", pops_2$transect_Lon_start)==T)
for(l in 1:length(which(grepl("E", pops_2$transect_Lon_start)==T))){
  pops_2$transect_Lon_start[numbs[l]]<-gsub('E','',pops_2$transect_Lon_start[numbs[l]])
}

pops_2$transect_Lon_stop<-as.character(pops_2$transect_Lon_stop)

pops_2$transect_Lon_stop[which(grepl("E", pops_2$transect_Lon_stop)==T)]
numbs<-which(grepl("E", pops_2$transect_Lon_stop)==T)
for(l in 1:length(which(grepl("E", pops_2$transect_Lon_stop)==T))){
  pops_2$transect_Lon_stop[numbs[l]]<-gsub('E','',pops_2$transect_Lon_stop[numbs[l]])
}

pops_2$transect_Lon_stop[which(grepl("W", pops_2$transect_Lon_stop)==T)]
numbs<-which(grepl("W", pops_2$transect_Lon_stop)==T)
for(l in 1:length(which(grepl("W", pops_2$transect_Lon_stop)==T))){
  pops_2$transect_Lon_stop[numbs[l]]<-gsub('W','',pops_2$transect_Lon_stop[numbs[l]])
}

#Verification: 

df<-c()
for(sc in 1:length(unique(pops_2$site_code))){
  df<-rbind(df,pops_2[which(pops_2$site_code==unique(pops_2$site_code)[sc])[1],])
}

loc<-data.frame(df$transect_Lat_start,df$transect_Lon_start)
loc2<-data.frame(df$transect_Lat_start,df$transect_Lon_start,df$transect_Lat_stop,df$transect_Lon_stop)

##Checking: 
loc2


##Plotting: 
#Select only one line per site: 
head(df)

quartz()
plot(coastsCoarse)
points(df[,c(3,2)],col="red")
text(df[,c(3,2)],labels=df$site_code,cex=0.6)

plot(coastsCoarse)
points(df[,c(3,2)],col="chartreuse4",pch=16)

#points(df[-which(df$site_code%in%c("RO","TNC","HV","HR")),c(3,2)],col="chartreuse4",pch=16)




#####Previous attempts to fix things (I think, but check in case something doesn't make sense later on)

for(l in 1:length(which(grepl("N", pops_2$transect_Lat_start)==T))){
  pops_2$transect_Lat_start[which(grepl("N", pops_2$transect_Lat_start)==T)[l]]<-
    line_of_interest<- as.character(pops_2$transect_Lat_start[which(grepl("N", pops_2$transect_Lat_start)==T)[l]])
    
    A<-strsplit(line_of_interest,split=NULL)
    B<-strsplit(line_of_interest, split = "")
    B<-B[[1]]
    B<-c(B)
    paste0(A[[1]][1:(length(A[[1]])-1)])
}

pops_2
lapply(grep(pops_2$transect_Lat_start,"N"))


loc$df.transect_Lat_start[grep("N",loc$df.transect_Lat_start)]

strsplit(as.character(loc$df.transect_Lat_start[1]),split="")

grep("N",strsplit(as.character(loc$df.transect_Lat_start[22]),split=""))

gsub("([0-9]+).*$",loc$df.transect_Lat_start)

coords<-cbind(as.character(pops_2$transect_Lat_start),as.character(pops_2$transect_Lon_start),as.character(pops_2$transect_Lat_stop),as.character(pops_2$transect_Lon_stop))
Pops_3<-lapply(coords, type.convert)

pops_3[] <- lapply(coords, gsub, pattern='N', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_start, gsub, pattern='S', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_start, gsub, pattern='W', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_start, gsub, pattern='E', replacement='')

pops_3 <- lapply(pops_3$transect_Lon_start, gsub, pattern='N', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_start, gsub, pattern='S', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_start, gsub, pattern='W', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_start, gsub, pattern='E', replacement='')

pops_3 <- lapply(pops_3$transect_Lat_stop, gsub, pattern='N', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_stop, gsub, pattern='S', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_stop, gsub, pattern='W', replacement='')
pops_3 <- lapply(pops_3$transect_Lat_stop, gsub, pattern='E', replacement='')

pops_3 <- lapply(pops_3$transect_Lon_stop, gsub, pattern='N', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_stop, gsub, pattern='S', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_stop, gsub, pattern='W', replacement='')
pops_3 <- lapply(pops_3$transect_Lon_stop, gsub, pattern='E', replacement='')
#names(pops_2)
df<-c()
for(sc in 1:length(unique(pops_3$site_code))){
  df<-rbind(df,pops_3[which(pops_3$site_code==unique(pops_3$site_code)[sc])[1],])
}

loc_2<-data.frame(df$transect_Lat_start,df$transect_Lon_start)



