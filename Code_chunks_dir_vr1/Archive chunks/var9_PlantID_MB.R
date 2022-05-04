## Maude Baudraz
## January 2019

## RK: Reviewed 08/04/2019 by Ruth Kelly

###### Fixing plant ID column ######
#Issues: 
#It is known that some authors reused dead plant's ideas in subsequent years. 
#Also, some plants by mistake got the same ID in different plots. We need a plant ID that is truly unique. 
#Solution: 
#Concatenate the Site Code, the transect, the plot and the plant ids, which will give a unique id per plant,


#### PPN demographic data version: 
#My data is the fullPPN_dataset_2019-01-28_.csv having undergone the Site_code_column and the seedling number correction
ppn<-mydata
#View(table(ppn$plant_id))

#List of issues: 
#some have letters (117a or 117b), 
#(AF 04/02/2019): Original comment suggested a problem with Ids over 100 or in the thousands. This is absolutely fine and does not need to be fixed 
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

#Problem here
# Previouosly concatedated IDs have different formats eg T1P1_1, T1P1-1

ids1<-(lapply(strsplit(as.character(ppn$plant_id[Concatenated]),split="_"),"[",2)) #unconcatenate the ones with _
ids2 <-(lapply(strsplit(as.character(ppn$plant_id[Concatenated]),split="-"),"[",2)) #unconcatenate the ones with -


a <- names(ids1)%in%names(ids2)&is.na(ids1)
ids3 <-  replace(ids1, a, ids2[names(which(a))])


Conca$plant_unique_id<-paste(Conca$site_code,Conca$transect,Conca$plot,ids3,sep = "_") #Concatenate unique ID following new logic (store them in new column)

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

#A.1
#Create an id that's unique per plot and per year (ie with year concatenated)
unique_plot_id<-paste(ppn$site_code,ppn$transect,ppn$plot,ppn$s_year,sep = "_")  ####review: Alain. this variable was already made in VAr 8 - seedlings, and called unique_plot_year_id. suggest i change the one in var 8 to the same, so we dont have a duplicate variable in the final product. END ########
ppn$unique_plot_id<-unique_plot_id
class(unique_plot_id)
unique_plot_id<-as.factor(unique_plot_id)

#Supress those plots which have only one line, with no indivdual id in it, and no seedling number. 
levels(unique_plot_id)[which(table(unique_plot_id)<2)]#Checks which ones have only one line

have_seedlings<-ppn$unique_plot_id[which(ppn$number_seedlings>0)] #Take the id of the plots that have seedlings
have_one_single_row_in_data_set<-levels(unique_plot_id)[which(table(unique_plot_id)<2)]#Takes the id of the plots that have only one line
ppn<-ppn[-which(ppn$unique_plot_id%in%have_one_single_row_in_data_set & (ppn$unique_plot_id%in%have_seedlings==F)),]#Suppresses the rows that do have only one line and no seedlings
ppn<-droplevels(ppn)

#A.2: Some have a bunch of rows for additionnal stems ---> delete
ppn[which(is.na(ppn$plant_id)),]#most seem to be empty of any information, but a few lines have some data entered
#suppress those rows that do not record any seedling and also no individual (no plant_id, no no_leaves), as we agreed that additionall stems alone should not be kept

#11.2.2019: original line that bugs: ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&(ppn$number_seedlings==0)),] 
#Attempts to fix it: 

if(length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&(ppn$number_seedlings==0)))>0){
  ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&(ppn$number_seedlings==0)),] 
}

if(length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&is.na(ppn$number_seedlings)))>0){
  ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&is.na(ppn$number_seedlings)),] 
}

#Watch carefully the special cases that remain
#View(ppn[which(is.na(ppn$plant_id)),])

#For these that do carry some sort of information (mainly only extra stems, but also a seedling number), 
#I have to make sure I have at least one line per plot left in my dataset with the number of seedling, 
#but else suppress those lines which have no individual info (they apparently contain additionnal flowering stems)

#11.2.2019: Original line that doesn't work un<-unique(ppn$unique_plot_id[which(is.na(ppn$plant_id))])
#ppn<-cbind(ppn,unique_plot_id)
un<-unique(ppn$unique_plot_id[which(is.na(ppn$plant_id))])

#### RK: there are warning messages coming from this loop 

#### eg.  8: In `[<-.data.frame`(`*tmp*`, which(is.na(ppn$plant_id) & is.na(ppn$no_leaves) &  :
#### provided 33 variables to replace 32 variables


for(u in 1:length(un)){
  suby<-ppn[which(ppn$unique_plot_id==un[u]),]
  if(nrow(suby)>nrow(ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),])){#If I have some rows in the entire ppn dataset that have information from that plot (they will then contain the number of seedling anyway)
    #Suprress the extra lines which only record stem data
    print(paste("In",un[u],"lines were deleted as they only record extra floral stems."))
    ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),]
  }
  if(nrow(suby)==nrow(ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]),])){ #If the only rows in the whole dataset are these that I am about to suppress as they have no individual information 
    
    #Then I have to worry about keeping the number of seedling information
    print(paste("Plot ",un[u], ", an empty line is the only record of the number of seedlings"))

#### RK: a message like "These plots have no plant IDs and only contain seedlings. And replicate instances of these rows have been removed where they exist" 
        
    #Take the first line that has that plot code:
    line<-ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[1],]
    #Supress all the information but for the plot and seedling number, and the year: 

#### RK: please check the two lines below. I think your object 'line' and the ppn dataset
    #### have only 32 columns. Here you add an NA at position 33, which induces a warning
    #### further down at line 135 when you try to add line back into the ppn dataset. 
    
    line[,c(9:26)]<-NA
    #### i.e. I think this next line should be c(31:32)] <- NA
    line[,c(31:32)]<-NA
    if(length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]))>1){#if there are more than one lines for that plot_id, 
    ppn<-ppn[-which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[2:length(which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u]))],] #keep only one
     }
    ppn[which(is.na(ppn$plant_id)&is.na(ppn$no_leaves)&ppn$unique_plot_id==un[u])[1],]<-line #and then attribute the cleaned line to that plot
    }
}

print("The lines above were dealt with, only printed for transparancy.")
print("If a new set of years is added, please check that the following lines only contain information that you are happy to see in the final data set (no stems with no plant id, for instance). if they are ok, do nothing. Else, worry")
print(ppn[which(is.na(ppn$plant_id)),])#This should now only show rows that one would wish to see in the dataset for one reason or the other (mainly information about the seedling number)

#ppn<-ppn[,-c(30,32)]
ppn$plant_unique_id<-as.factor(ppn$plant_unique_id)

mydata <- ppn
#as of Feb 1 2019, there is only one question that remains open; what to do with the individual in pop BG that is being recorder but has no ID? 
#Check with Alain in original sheets
#AF has fixed this in the original data. It was a second rosette of a plant which had been mistakenly left blank

#Didn't check that the number of rosettes per plant_id does match the number of rows, as this will be done to check the number of rosettes, and would become circular. 
#Also didn't check that no rosettes might have been registered as being on the neighboring plot to the main plant?

#Potentially move the bits that move the rows with only seedling information and the additionnal stems to another section of the compiler.  

#Peer review the scripts

##Clean columns after script
