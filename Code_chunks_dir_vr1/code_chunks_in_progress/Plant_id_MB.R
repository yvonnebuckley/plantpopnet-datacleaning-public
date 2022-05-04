## Maude Baudraz
## January 2019


###### Fixing plant ID column ######
#Issues: 
#It is known that some authors reused dead plant's ideas in subsequent years. 
#Also, some plants by mistake got the same ID in different plots. We need a plant ID that is truly unique. 
#Solution: 
#Concatenate the Site Code, the transect, the plot and the plant ids, which will give a unique id per plant,


#### PPN demographic data version: 
#My data is the fullPPN_dataset_2019-01-28_.csv having undergone the Site_code_column and the seedling number correction
#ppn<-mydata
#View(table(ppn$plant_id))

#List of issues: 
#some have letters (117a or 117b), some are much above 100, which is totally ok but unexpected
#a few are in the thousands
#Some plant_ids were already concatenated in the processs of putting together the data, but following another logic 

#### Concatenate the plant_ids ####
#Concatenate the IDS that are not yet concatenated
Concatenated<-grep("T",ppn$plant_id)
ToConca<-ppn[-Concatenated,] #take onyl the individuals that weren't concatenated
ToConca$plant_unique_id<-paste(ToConca$site_code,ToConca$transect,ToConca$plot,ToConca$plant_id,sep = "_")   #concatenate the unique IDs and store them in a new column
### rosette_number Needs to be corrected to the new name ####

#The previously concatenated ones have no site code info
#Create a new plant_id including the site_code
Conca<-ppn[Concatenated,] #Keep only the one that were concatenated
ids<-lapply(strsplit(as.character(ppn$plant_id[Concatenated]),split="-"),"[",2) #unconcatenate them
Conca$plant_unique_id<-paste(Conca$site_code,Conca$transect,Conca$plot,ids,sep = "_") #Concatenate unique ID following new logic (store them in new column)

ppn<-rbind(ToConca,Conca)
ppn$plant_unique_id


#Trouble shooting
#A: Some names have nas concatenated; why? 
#B: Check that the plant_IDs are not present more than the amount of time they should. 

#A: Check NAs: 
lines_with_na<-grep("NA",ppn$plant_unique_id)
#View(ppn[lines_with_na,])
dim(ppn[lines_with_na,])
print(paste("Plant IDs that have NAs in them ",nrow(ppn[lines_with_na,])))

#A.1: Some have a line per empty plot --> delete, unless there is a number of seedling
#A.2: Some have a bunch of rows for additionnal stems ---> delete

#A.1: Some have a line per empty plot --> delete, unless there is a number of seedling

#Create an id that's unique per plot and per year (ie with year concatenated)
unique_plot_id<-paste(ppn$site_code,ppn$transect,ppn$plot,ppn$s_year,sep = "_")
ppn$unique_plot_id<-unique_plot_id
class(unique_plot_id)
unique_plot_id<-as.factor(unique_plot_id)

#Supress those plots which have only one line, with no indidual id in it, and no seedling number. 
levels(unique_plot_id)[which(table(unique_plot_id)<2)]#Checks which ones have only one line

have_seedlings<-ppn$unique_plot_id[which(ppn$number_seedlings>0)] #Take the id of the plots that have seedlings
have_one_single_row_in_data_set<-levels(unique_plot_id)[which(table(unique_plot_id)<2)]#Takes the id of the plots that have only one line
ppn<-ppn[-which(ppn$unique_plot_id%in%have_one_single_row_in_data_set & (ppn$unique_plot_id%in%have_seedlings==F)),]#Suppresses the rows that do have only one line and no seedlings
ppn<-droplevels(ppn)

#A.2: Some have a bunch of rows for additionnal stems ---> delete
ppn[which(is.na(ppn$plant_id)),]#most seem to be empty of any information, but a few lines have some data entered
#suppress those rows that do not record any seedling and also no individual (no plant_id, no no_leaves), as we agreed that additionall stems alone should not be kept
ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&(ppn$number_seedlings==0)),] 

#Watch carefully the special cases that remain
View(ppn[which(is.na(ppn$plant_id)),])

#For these that do carry some sort of information (mainly only extra stems, but also a seedling number), 
#I have to make sure I have at least one line per plot left in my dataset with the number of seedling, 
#but else suppress those lines which have no individual info (they apparently contain additionnal flowering stems)
un<-unique(ppn$unique_plot_id[which(is.na(ppn$plant_id))])
for(u in 1:length(un)){
  suby<-ppn[which(ppn$unique_plot_id==un[u]),]
  if(nrow(suby)>nrow(ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),])){#If I have some rows in the entire ppn dataset that have information from that plot (they will then contain the number of seedling anyway)
    #Suprress the extra lines with only stem data
    print(paste0(un[u],"ok"))
    ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),]
  }
  if(nrow(suby)==nrow(ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),])){ #If the only rows in the whole dataset are these that I am about to suppress as they have no individual information 
    #Then I have to worry about keeping the number of seedling information
    
    print(paste0("Plot ",un[u], " may have some weird things happening "))
    
    #Take the first line that has that plot code:
    line<-ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[1],]
    #Supress all the information but for the plot and seedling number, and the year: 
    line[,c(9:26)]<-NA
    line[,c(33:34)]<-NA
    if(length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]))>1){
    ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[2:length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]))],]
     }
    ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[1],]<-line
    }
}

View(ppn[which(is.na(ppn$plant_id)),])#This should now only show rows that one would wish to see in the dataset for one reason or the other (mainly information about the seedling number)

#as of Feb 1 2019, there is only one question that remains open; what to do with the individual in pop BG that is being recorder but has no ID? 
  #Check with Alain in original sheets
#Didn't check that the number of rosettes per plant_id does match the number of rows, as this will be done to check the number of rosettes, and would become circular. 
#Also didn't check that no rosettes might have been registered as being on the neighboring plot to the main plant?

#Potentially move the bits that move the rows with only seedling information and the additionnal stems to another section of the compiler.  

#Peer review the scripts

##Clean columns after script
