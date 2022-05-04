#####Genet computation tool #####
#Maude Baudraz
#April 2019

#Input and dependencies
#This script takes in a dataset of the shape of the PPN dataset (data), respecting its headers order and spelling. 
#It makes use of the plant_unique_id computed during the data cleaning steps, and generally assumes the data to be clean of errors. 

#The script produces the output dataset dat2, in which each line is one genet in one year. 
#As individual rosettes are not marked in the PPN protocol, this script pools all rosettes attributed to the same individual each year, and pools them together to compute the size of the individual (genet). 
#The same logic as on the field is respected; the leaves are added up, only the width and length of the longest accross all rosettes is recorded. If the widht of the longest leaf is missing, the biggest width is recorded. If a number of leaves in y0 is missing, the number of leaves of the genet is set to 0. In y1, the code checks and only adds up the number of leaves of the rosettes that survived. Nummber of leaves is set to NA if a rosette that is marked as survival= yes has no number of leaves recorded. 
#Similarly, the longest stem accross all rosettes is recorded, and the inflorescnece data corresponding to this stem is recorded. When inflorescence data for the longest stem is missing, the longest inflorescene for which data is knwon is selected. 


dat2<-data.frame()
for(sc in 1:length(unique(data$site_code))){ #For each population in your subset
  sub<-subset(data,data$site_code==unique(data$site_code)[sc])
  
  #List and length of list of plant_ids for that popluation: 
  id_list<-unique(sub$plant_id)
  id_length<-length(unique(sub$plant_id))
  
  #Pool individuals in y0: 
  for(id in 1:id_length){#For each plant in your population
    print(id_list[id])
    #Select the plants one by one:     
    cond0<-which(sub$plant_id==id_list[id]&sub$s_year=="Y0")
    
    #Check that the plant was measured this year: 
    if(length(cond0)==0){#if it wasn't, do not run the script for this year. 
      print(paste(id_list[id],"was not visited in Y0"))
    }else{
      
      
      
      if(length(cond0)>0){    #if the plant is not there in Y1, don't do anything
        row<-sub[cond0[1],]#Take first row in output dataset to have coordinates (herbivory comments then only true for the first rosette)
        
        row$no_leaves<-sum(sub$no_leaves[cond0],na.rm=F)#Compute the total number of leaves. If any rosette is missing, the size is set to NA by na.rm=F. Ok for year zero. 
        
        #check that the number of leaves for each rosette is known
        if(anyNA(sub$no_leaves[cond0])){#Print warning if n° of leaves for some plant are missing
          print(paste("pop",unique(data$site_code)[sc],"plant_id",id_list[id],"has got missing n° of leaves"))
        }
        
        
        #Select longest leaf
        #first if there are none (either because all eaten, or because genet died): 
        if(all(is.na(sub$leaf_length[cond0]))){
          row$leaf_length<-NA #then everything should be NA
          #row$leaf_width<-NA
          print(paste("pop",sc,"plant",id_list[id],"has no leaf length"))
        }else{
          row$leaf_length<-max(as.numeric(as.character(sub$leaf_length[cond0])),na.rm=T)
        }
        
        #Select the width corresponding the the longest leaf: 
        if(all(is.na(sub$leaf_length[cond0]))==TRUE){
          a<-sample(c(cond0),1) #just take a random rosette within the ones of that genet and keep going (should just avoid an error message)
        }else{
          a<-which(sub$leaf_length[cond0]==max(sub$leaf_length[cond0],na.rm=T)) #if there is leaf-length info, then take the longest
        }
        
        if(length(a)>1){
          a<-a[1] #if several ex-aequo leaves, only take one (the first)
        }
        
        if(length(a)==0){#In case there's only unknown width: 
          row$leaf_width<-NA
          print(paste(unique(sub$site_code)[sc],id_list[id],"leaf width is na, no replacement"))
        }else{ if(length(cond0)>1&is.na(sub$leaf_width[cond0][a])){
          #If the leaf width of the longest leaf is unknown and there are others 
          if(all(is.na(sub$leaf_width[cond0]))==FALSE){#if there are known width, select the widest
            row$leaf_width<-max(as.numeric(as.character(sub$leaf_width[cond0])),na.rm=T)
            ##print(paste(unique(sub$site_code)[sc],id,"Has got missing leaf width (corresponding to longest)"))   #Error message silenced as it produces too many warnings
          }
          if(all(is.na(sub$leaf_width[cond0]))==TRUE){#if there are no other known values, then NA
            row$leaf_width<-NA
          }
        }else{
          row$leaf_width<-sub$leaf_width[cond0][a]
          #If no NA, then leaf width should just be that of the longest leaf
          #print(paste(id_list[id],"all good leaves :)")) #that message is not the best, as it is actually just for the width
        }}
        
      }
      
      # }
      
      
      #Select the floral stems 
      
      #first if there are none: 
      if(sum(as.numeric(as.character(sub$no_fl_stems[cond0])),na.rm=T)==0){
        row$fl_stem_height<-NA #then everything should be NA
        row$inflor_length<-NA
        row$inflor_phenology<-NA
      }else{
        row$no_fl_stems<-sum(as.numeric(as.character(sub$no_fl_stems[cond0])),na.rm=T) #Add up each rosette's stems for a genet total 
      }
      
      if(all(is.na(sub$fl_stem_height[cond0]))==TRUE){
        row$fl_stem_height<-NA
        if(length(cond0)==1){#this has to be checked, as if unfortuna
          a<-cond0
        }
        if(length(cond0)>1){
          a<-a[1]
        }}
      if(all(is.na(sub$fl_stem_height[cond0]))==FALSE){
        row$fl_stem_height<-max(as.numeric(as.character(sub$fl_stem_height[cond0])),na.rm=T) 
        a<-which(as.numeric(as.character(sub$fl_stem_height[cond0]))==max(as.numeric(as.character(sub$fl_stem_height[cond0])),na.rm=T))
      }
      
      if(length(a)>1){
        a<-a[1]#Some stems might be ex-aequo; in which case just pick the first one
      }
      if(all(is.na(sub$inflor_length[cond0]))==TRUE){  #replacement
        row$inflor_length<-NA
      }else{
        if(length(cond0)>1&is.na(sub$inflor_length[cond0][a])){ #If the inflorescence of the longest stem is missing
          row$inflor_length<-max(as.numeric(as.character(sub$inflor_length[cond0])),na.rm=T)#replace by that of another stem on another ramet
          a<-which(sub$inflor_length[cond0]==max(as.numeric(as.character(sub$inflor_length[cond0])),na.rm=T))
          if(length(a)>1){
            a<-a[1]
          }
          #print(paste(unique(sub$site_code)[sc],id)) #not very informative error message
          print(paste(sub$plant_id[cond0][a],"has complemented inflorescence length")) 
        }else{
          row$inflor_length<-sub$inflor_length[a]
        }
      }
      if(length(a)==0){
        row$inflor_phenology<-NA
      }else{
        row$inflor_phenology<-sub$inflor_phenology[a] #attribute the phenology of the longest stem, or the one which inflorescence length was selected (a was changed)
      }}
    
    if(is.na(row$no_rosettes)){
      row$no_rosettes<-length(row$no_rosettes[cond0])
    }else{
      if(row$no_rosettes<length(row$no_rosettes[cond0])){#requires a number (cannot be NA)
        row$no_rosettes<-length(row$no_rosettes[cond0])
      }}#To make sure that even if the PI did not enter it/put several lines, then number of rosettes corresponds to how many rosettes were recorded. 
    
    dat2<-rbind(dat2,row)
  }
  #Now start again for y1: 
  for(id in 1:id_length){#For each plant in your population
    print(id_list[id])
    #Select the plants one by one:     
    cond0<-which(sub$plant_id==id_list[id]&sub$s_year=="Y1")
    
    #Check that the plant was measured this year: 
    if(length(cond0)==0){
      print(paste(id_list[id],"was not visited in Y1"))
    }else{
      
      if(length(cond0)>0){    #if the plant is not there in Y1, don't do anything
        row<-sub[cond0[1],]#Take first row in output dataset to have coordinates (herbivory comments then only true for the first rosette)
        
        if(all(is.na(sub$no_leaves[cond0]))==F){
          row$no_leaves<-sum(sub$no_leaves[cond0],na.rm=F)#Compute the total number of leaves as in Y0; no rosette died, so ok
        }
        
        if(anyNA(sub$no_leaves[cond0])){#If some number of leaves are missing, they can be missing because a rosette died (add up as 0), or because they couldn't be countet or where forgotten (account as NA)
          cond2<-which(sub$survival!="no")
          cond1<-cond0[-which(cond0%in%cond2)]#Takes the rows that come from that genet and did not die
          row$no_leaves<-sum(sub$no_leaves[cond1],na.rm=F)#Compute the total number of leaves for the one which lived
        }
        
        if(anyNA(sub$no_leaves[cond0])){#Print warning if n° of leaves for some plant are missing
          print(paste("pop",unique(data$site_code)[sc],"plant_id",id_list[id],"has got missing n° of leaves"))
        }
        
        #Select longest leaf
        #first if there are none (either because all eaten, or because genet died): 
        if(all(is.na(sub$leaf_length[cond0]))){
          row$leaf_length<-NA #then everything should be NA
          #row$leaf_width<-NA
          print(paste("pop",sc,"plant",id_list[id],"has no leaf length"))
        }else{
          row$leaf_length<-max(as.numeric(as.character(sub$leaf_length[cond0])),na.rm=T)
        }
        
        #Select the width corresponding the the longest leaf: 
        if(all(is.na(sub$leaf_length[cond0]))==TRUE){
          a<-sample(c(cond0),1) #just take a random rosette within the ones of that genet and keep going (should just avoid an error message)
        }else{
          a<-which(sub$leaf_length[cond0]==max(sub$leaf_length[cond0],na.rm=T)) #if there is leaf-length info, then take the longest
        }
        
        if(length(a)>1){
          a<-a[1] #if several ex-aequo leaves, only take one (the first)
        }
        
        if(length(a)==0){#In case there's only unknown width: 
          row$leaf_width<-NA
          print(paste(unique(sub$site_code)[sc],id_list[id],"leaf width is na, no replacement"))
        }else{ if(length(cond0)>1&is.na(sub$leaf_width[cond0][a])){
          #If the leaf width of the longest leaf is unknown and there are others 
          if(all(is.na(sub$leaf_width[cond0]))==FALSE){#if there are known width, select the widest
            row$leaf_width<-max(as.numeric(as.character(sub$leaf_width[cond0])),na.rm=T)
            ##print(paste(unique(sub$site_code)[sc],id,"Has got missing leaf width (corresponding to longest)"))   #Error message silenced as it produces too many warnings
          }
          if(all(is.na(sub$leaf_width[cond0]))==TRUE){#if there are no other known values, then NA
            row$leaf_width<-NA
          }
        }else{
          row$leaf_width<-sub$leaf_width[cond0][a]
          #If no NA, then leaf width should just be that of the longest leaf
          #print(paste(id_list[id],"all good leaves :)")) #that message is not the best, as it is actually just for the width
        }}
      }
      
      # }
      
      #Select the floral stems 
      
      #first if there are none: 
      if(sum(as.numeric(as.character(sub$no_fl_stems[cond0])),na.rm=T)==0){
        row$fl_stem_height<-NA #then everything should be NA
        row$inflor_length<-NA
        row$inflor_phenology<-NA
      }else{
        row$no_fl_stems<-sum(as.numeric(as.character(sub$no_fl_stems[cond0])),na.rm=T) #Add up each rosette's stems for a genet total 
      }
      
      if(all(is.na(sub$fl_stem_height[cond0]))==TRUE){
        row$fl_stem_height<-NA
        if(length(cond0)==1){#this has to be checked, as if unfortuna
          a<-cond0
        }
        if(length(cond0)>1){
          a<-a[1]
        }}
      if(all(is.na(sub$fl_stem_height[cond0]))==FALSE){
        row$fl_stem_height<-max(as.numeric(as.character(sub$fl_stem_height[cond0])),na.rm=T) 
        a<-which(as.numeric(as.character(sub$fl_stem_height[cond0]))==max(as.numeric(as.character(sub$fl_stem_height[cond0])),na.rm=T))
      }
      
      if(length(a)>1){
        a<-a[1]#Some stems might be ex-aequo; in which case just pick the first one
      }
      if(all(is.na(sub$inflor_length[cond0]))==TRUE){  #replacement
        row$inflor_length<-NA
      }else{
        if(length(cond0)>1&is.na(sub$inflor_length[cond0][a])){ #If the inflorescence of the longest stem is missing
          row$inflor_length<-max(as.numeric(as.character(sub$inflor_length[cond0])),na.rm=T)#replace by that of another stem on another ramet
          a<-which(sub$inflor_length[cond0]==max(as.numeric(as.character(sub$inflor_length[cond0])),na.rm=T))
          if(length(a)>1){
            a<-a[1]
          }
          #print(paste(unique(sub$site_code)[sc],id)) #not very informative error message
          print(paste(sub$plant_id[cond0][a],"has complemented inflorescence length")) 
        }else{
          row$inflor_length<-sub$inflor_length[a]
        }
      }
      if(length(a)==0){
        row$inflor_phenology<-NA
      }else{
        row$inflor_phenology<-sub$inflor_phenology[a] #attribute the phenology of the longest stem, or the one which inflorescence length was selected (a was changed)
      }}
    
    if(is.na(row$no_rosettes)){
      row$no_rosettes<-length(row$no_rosettes[cond0])
    }else{
      if(row$no_rosettes<length(row$no_rosettes[cond0])){#requires a number (cannot be NA)
        row$no_rosettes<-length(row$no_rosettes[cond0])
      }}#To make sure that even if the PI did not enter it/put several lines, then number of rosettes corresponds to how many rosettes were recorded. 
    
    dat2<-rbind(dat2,row)
  }
  print(paste("population",unique(data$site_code)[sc],"finished"))
}