## data cleaning number_seedlings
## Yvonne Buckley 19/11/18
## ready to source into master file 29/11/2018
## outputs: mydata$number_seedlings 


#mydata = output from the compiler on 11.02.2019

## ERROR: factor with text entry for na
# str(mydata$number_seedlings)
# levels(mydata$number_seedlings)

## SOLUTION: change to numeric and coerce text to NA - will give warnings - ignore
mydata$fix_number_seedlings <- as.numeric(as.character(mydata$number_seedlings))

## CHECK: check fix
# str(mydata$fix_number_seedlings)
# mydata$fix_number_seedlings[1:500]
# mydata$number_seedlings[1:500]
# summary(mydata$number_seedlings)
# summary(mydata$fix_number_seedlings)

## tidy up and replace number_seedlings with fixed version
mydata$number_seedlings <- mydata$fix_number_seedlings
mydata <- subset(mydata, select = -fix_number_seedlings)
mydata <- droplevels(mydata)

#### Seedling number
#Goes into more details than the previous one by YB, mainly checking that the number of seedling is always the same in any plot.
#Maude Baudraz January 2019

seeds<-mydata# #My data is the fullPPN_dataset_2019-01-28_.csv having undergone the Site_code_column correction
seeds$number_seedlings

#Seedling number shouldn't be na, but zeros if empty, or the number of seedsling in any other mention of the same plot in the same year: 
#Reattribute to those plots which have an NA the number of seedlings of any other mention of the same plot in the same year
seeds[which(is.na(seeds$number_seedlings)),]
seeds$unique_plot_year_id<-paste(seeds$site_code,seeds$transect,seeds$plot,seeds$s_year,sep = "_")

#for all the rows in seeds, if any one plot has a value somewhere, attribute. Else, attribute zero. 
Plots_that_have_an_na<-seeds$unique_plot_year_id[which(is.na(seeds$number_seedlings))]
Plots_that_have_an_na<-unique(Plots_that_have_an_na)

problematic_plots<-c()
for(i in 1:length(Plots_that_have_an_na)){
  all_occurences_of_plot_i<-seeds[which(seeds$unique_plot_year_id==Plots_that_have_an_na[i]),]
  #If all the number of seedlings for that plot are nas
  if(length(which(is.na(all_occurences_of_plot_i$number_seedlings)))==length(all_occurences_of_plot_i$number_seedlings)){
    seeds$number_seedlings[which(seeds$unique_plot_year_id==Plots_that_have_an_na[i])]<-0 #then all the number of seedlings should be turned into 0s
  }else{
    #If some of the occurences of the plot have a value, the other nas: 
    if(length(which(is.na(all_occurences_of_plot_i$number_seedlings)))!=length(all_occurences_of_plot_i$number_seedlings)){
      #If all plots have the same value recorded (and only some lines for the same plot forget to repeat it)
      if(length(unique(all_occurences_of_plot_i$number_seedlings[which(is.na(all_occurences_of_plot_i$number_seedlings)==F)]))==1){
        seeds$number_seedlings[which(seeds$unique_plot_year_id==Plots_that_have_an_na[i])]<-unique(all_occurences_of_plot_i$number_seedlings[which(is.na(all_occurences_of_plot_i$number_seedlings)==F)])
        #Then set the number of seed to that unique value
      }
      if(length(unique(all_occurences_of_plot_i$number_seedlings[which(is.na(all_occurences_of_plot_i$number_seedlings)==F)]))>1){
        print(paste("plot",all_occurences_of_plot_i$unique_plot_year_id,"has several number of seedlings written down"))
        problematic_plots<-append(problematic_plots,Plots_that_have_an_na[i])
        seeds$number_seedlings[which(seeds$unique_plot_year_id==Plots_that_have_an_na[i])]<-max(unique(all_occurences_of_plot_i$number_seedlings[which(is.na(all_occurences_of_plot_i$number_seedlings)==F)]),na.rm=T)
      }
    }}}

problematic_plots<-unique( problematic_plots)
problematic_plots
print("The number of seedling for the following plots was set to the maximal annotated value for... ")
print(problematic_plots)


####Now do other plots, that do not have NA values, have various numbers of seedlings written down?
plots_names<-unique(seeds$unique_plot_year_id)#Produces a list of unique identifier for each plot in PPN, repeated only once
for(p in 1:length(p)){
  sld_numb<-unique(seeds$number_seedlings[which(seeds$unique_plot_year_id==plots_names[p])]) #If all the rows about this one plot have the same value, this should have a length of one
  if(length(sld_numb)>1){#If not, then produce an error message
    print(paste("plot ",plots_names[p],"has different seedling numbers written down"))
    ##No case for this in January 2019. Else I would suggest to select the maximum value, and hence apply the following line of code: 
    #seeds$number_seedlings[which(seeds$unique_plot_year_id==plots_names[p])]<-max(sld_num)
  }
}


#####
seeds$number_seedlings
hist(seeds$number_seedlings)
summary(seeds$number_seedlings)#The extreme values are over 60 (up to 127). Are they trustworthy?
seeds[which(seeds$number_seedlings=="127"),]
#I do not wish to change anything to the field sheets of the PPN collaborators, so will stick to this


##Put back into my data: 
mydata<-seeds

