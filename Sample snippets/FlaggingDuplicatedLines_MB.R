#####Flagging duplicated line #####
#Maude Baudraz
#April 2019

#Input and dependencies
#This script takes in a dataset of the shape of the PPN dataset (data), respecting its headers order and spelling. 
#It prints out any set of rows that are likely to be complete duplicates (number of rosettes different from the number of lines).

#To check for duplicated lines. y0: 
for(sc in 1:length(unique(data$site_code))){ #For each population in your subset
  sub<-subset(data,data$site_code==unique(data$site_code)[sc])
  
  #List and length of list of plant_ids for that popluation: 
  id_list<-unique(sub$plant_id)
  id_length<-length(unique(sub$plant_id))
  
  
  for(id in 1:id_length){#For each plant in your population
    #Select them one by one:     
    cond0<-which(sub$plant_id==id_list[id]&sub$s_year=="Y0")
   if(length(cond0)>0){
     Row<-sub[cond0,]
      if(length(Row$leaf_length)!=length(unique(Row$leaf_length))){
        if(length(unique(Row$rosette_number))!=length(Row$rosette_number)){
          print(Row)
      }}
    }}}

#To check for duplicated lines. y1: 
for(sc in 1:length(unique(data$site_code))){ #For each population in your subset
  sub<-subset(data,data$site_code==unique(data$site_code)[sc])
  
  #List and length of list of plant_ids for that popluation: 
  id_list<-unique(sub$plant_id)
  id_length<-length(unique(sub$plant_id))
  for(id in 1:id_length){#For each plant in your population
    #Select them one by one:     
    cond0<-which(sub$plant_id==id_list[id]&sub$s_year=="Y1")
    if(length(cond0)>0){
      Row<-sub[cond0,]
      if(length(Row$leaf_length)!=length(unique(Row$leaf_length))){
        if(length(unique(Row$rosette_number))!=length(Row$rosette_number)){
          print(Row)
        }}
    }
    }}