## Maude Baudraz
## January 2019

## RK: Reviewed 08/04/2019 by Ruth Kelly
## YB: Reviewed 02/09/2019 by Yvonne Buckley
## YB: Reviewed & edited 28/10/19 by Yvonne Buckley

###### Fixing plant ID column ######
#Issues: 
#It is known that some authors reused dead plant's ids in subsequent years. 
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
Concatenated <- grep("T", ppn$plant_id)
ToConca<-ppn[-Concatenated,] #take onyl the individuals that weren't concatenated
ToConca$plant_unique_id <- paste(ToConca$site_code,ToConca$transect,ToConca$plot,ToConca$plant_id,sep = "_")   #concatenate the unique IDs and store them in a new column


#The previously concatenated ones have no site code info
#Create a new plant_id including the site_code
Conca<-ppn[Concatenated,] #Keep only the one that were concatenated

#Problem here
# Previouosly concatedated IDs have different formats eg T1P1_1, T1P1-1

ids1 <- (lapply(strsplit(as.character(ppn$plant_id[Concatenated]),split="_"),"[",2)) #unconcatenate the ones with _
ids2 <-(lapply(strsplit(as.character(ppn$plant_id[Concatenated]),split="-"),"[",2)) #unconcatenate the ones with -


a <- names(ids1) %in% names(ids2) & is.na(ids1)
ids3 <-  replace(ids1, a, ids2[names(which(a))])


Conca$plant_unique_id<-paste(Conca$site_code,Conca$transect,Conca$plot,ids3,sep = "_") #Concatenate unique ID following new logic (store them in new column)

ppn<-rbind(ToConca,Conca)
ppn$plant_unique_id


#Trouble shooting
#A: Some names have nas concatenated; why? 
#B: Check that the plant_IDs are not present more than the amount of time they should. 

#A: Check NAs: 
lines_with_na <- grep("NA",ppn$plant_unique_id)
#View(ppn[lines_with_na,])
dim(ppn[lines_with_na,])
print(paste("Number of Plant IDs that have NAs in them: ",nrow(ppn[lines_with_na,])))
droplevels(unique(mydata$site_code[lines_with_na]))

#A.1: Some have a line per empty plot --> delete, unless there is a number of seedling
#A.2: Some have a bunch of rows for additionnal stems ---> delete

#A.1
#Create an id that's unique per plot and per year (ie with year concatenated)
unique_plot_id<-paste(ppn$site_code,ppn$transect,ppn$plot,ppn$s_year,sep = "_")  ####review: Alain. this variable was already made in VAr 8 - seedlings, and called unique_plot_year_id. suggest i change the one in var 8 to the same, so we dont have a duplicate variable in the final product. END ########
ppn$unique_plot_id<-unique_plot_id
class(unique_plot_id)
unique_plot_id<-as.factor(unique_plot_id)


#Supress those plots which have only one line, with no indivdual id in it, and no seedling number. 
##### The below chunk of code is a fix for Audit 3 & 4. It's a bit 'agricultural' but works for now. Alain 08/11/2019 

na_ppn <- ppn
na_ppn <- ppn[is.na(ppn$plant_id),]
unique_plot_id<-paste(na_ppn$site_code,na_ppn$transect,na_ppn$plot,na_ppn$s_year,sep = "_")
na_ppn$unique_plot_id<-unique_plot_id
class(unique_plot_id)
unique_plot_id<-as.factor(unique_plot_id)
levels(unique_plot_id)[which(table(unique_plot_id)<2)]


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
  suby <- ppn[which(ppn$unique_plot_id == un[u]),] ## subset of problematic records where plant_id is NA
  test_noleaves <- which(is.na(ppn$plant_id) & is.na(ppn$no_leaves) & ppn$unique_plot_id==un[u]) ##YB: create a test condition
  tt <-  nrow(ppn[test_noleaves,]) ## YB: apply test condition
  if(tt > 0 & nrow(suby) > tt) {
    # If tt is 0 then missing id's have leaf data & need to be corrected separately. If no. rows in subset of NA plant IDs(suby) is greater than tt then there are some records in suby that have no leaves (& therefore are likely multiple fl. stem recores & can be discarded)
    # If I have some rows in the entire ppn dataset that have information from that plot (they will then contain the number of seedling anyway)
    # Suprress the extra lines which only record stem data
    ppn2 <- ppn[-which(is.na(ppn$plant_id) & is.na(ppn$no_leaves) & ppn$unique_plot_id==un[u]),] 
    print(paste("In",un[u],"lines were deleted as they only record extra floral stems."))
  }
  
  if(tt >0 & nrow(suby) == nrow(ppn[test_noleaves,])){
    #If the only rows in the whole dataset are these that I am about to suppress as they have no individual information 
    #Then I have to worry about keeping the number of seedling information
    print(paste("Plot ",un[u], ", These plots have no plant IDs and only contain seedlings. And replicate instances of these rows have been removed where they exist"))
        
    #Take the first line that has that plot code:
    line <- ppn[test_noleaves[1],]
    line_na <- line
    is.na(line_na) <- TRUE # create a corrected line that has NA's everywhere
    #Supress all the information but for the plot and seedling number, and the year: 
    line_na <- line[names(line)[1:8]] #keep site, T, P and no. seedlings info #dont want to refer to columns by number
    line_na <- line[names(line)[31:32]] # keep newly generated unique id's

    #for any columns contained in this list transfer values over
    
    if(length(test_noleaves) > 1){
      #if there are more than one lines for that plot_id, 
    ppn<-ppn[-test_noleaves[2:length(test_noleaves),]] 
    #keep only one
     }
    ppn[test_noleaves[1],] <- line #and then attribute the cleaned line to that plot
    }
}

print("The lines above were dealt with, only printed for transparancy.")
print("If a new set of years is added, please check that the following lines only contain information that you are happy to see in the final data set (no stems with no plant id, for instance). if they are ok, do nothing. Else, worry")
print(ppn[which(is.na(ppn$plant_id)),])#This should now only show rows that one would wish to see in the dataset for one reason or the other (mainly information about the seedling number)

#recheck number of lines with NA
lines_with_na <- grep("NA",ppn$plant_unique_id)
#View(ppn[lines_with_na,])
dim(ppn[lines_with_na,])
print(paste("Number of Plant IDs that have NAs in them: ",nrow(ppn[lines_with_na,])))
droplevels(unique(mydata$site_code[lines_with_na]))

## sort out copy/fill error as some rosettes don't end up with the same plant.id
## AF to go back to raw data and fix up copy/fill errors 

#ppn<-ppn[,-c(30,32)]
ppn$plant_unique_id<-as.factor(ppn$plant_unique_id)

mydata <- ppn
#as of Feb 1 2019, there is only one question that remains open; what to do with the individual in pop BG that is being recorder but has no ID? 
#Check with Alain in original sheets
#AF has fixed this in the original data. It was a second rosette of a plant which had been mistakenly left blank

#Didn't check that the number of rosettes per plant_id does match the number of rows, as this will be done to check the number of rosettes, and would become circular. 
#Also didn't check that no rosettes might have been registered as being on the neighboring plot to the main plant?

#Potentially move the bits that move the rows with only seedling information and the additionnal stems to another section of the compiler.  



##Clean columns after script
