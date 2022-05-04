
## Annabel Smith, October 2018

## Snippets of code for error-checking PlantPopNet demographic data. 

# --- *** DATA PROBLEMS *** --- #

# --- *** FIXED DATA PROBLEMS *** --- #

# FIXED. Some people, like "AC" or "HV" in Y0 didn't record every rosette. Most did. But plant_id can be used anyway as the unique individual to get the number of plants per plot

# HV has weird plot names:
hv_dat<-dem[which(dem$site_code=="HV"),c("site_code","c_year","s_year","transect","plot","plant_id","survival","no_rosettes","rosette_number","no_leaves","leaf_length")]
hv_dat<-tidy.df(hv_dat)
head(hv_dat)

xnew<-as.character(unlist(read.table("xx.txt",header=F)))
x<-as.character(hv_dat$plot)
pattern<-levels(hv_dat$plot)
repl<-paste("P",1:length(levels(hv_dat$plot)),sep="")

Reduce(function(x,i)gsub(pattern[i],repl[i],x),seq_along(pattern),xnew)

# write(Reduce(function(x,i) gsub(pattern[i],repl[i],x),seq_along(pattern),xnew),file="new.txt",sep="\t")
# write(x,"newx.txt",sep="\t")

# FIXED. Calculating density, where the number of plants is length(levels(plant_id)) assumes that plant_id is never concatenated with rosette number (or anything else); this assumption seems to be true in the following check:

# Check how people labelled their plant IDs
dem[which(dem$plant_id==sample(levels(dem$plant_id),1)),c("site_code","c_year","s_year","transect","plot","plant_id","survival","no_rosettes","rosette_number","no_leaves","leaf_length")]

# FIXED. Anna re-used plant_ids across plots at IO (and possibly elsewhere), e.g. plant_id==5 appears in P1 and P4; did anyone else do this?
io_dat<-dem[which(dem$site_code=="IO"),c("site_code","c_year","s_year","transect","plot","plant_id","survival","no_rosettes","rosette_number","no_leaves","leaf_length")]
io_dat[which(io_dat$plant_id==sample(io_dat$plant_id,1)),]

# This means that plant_id absolutely has to be concatenated with transect and plot. 

# Note that some sites have already concatenated transect_plot_plant_id where the ID was re-used across plots:
# ARH, PA, RO
unique(dem$site_code[grep("T",dem$plant_id)])
	
# FIXED. The best way to deal with this problem will be to remove the T1_P* from the plant_id (it doesn't matter that the plant_id will be duplicated, because it is at other sites too; it has to be fixed anyway), then re-concatenate in the script.



# If a plant is alive, the no_fl_stems should never be NA, it should always be 0:

alive<-dem[which(dem$survival=="yes"),]
alive<-tidy.df(alive)
head(alive)

# CHECK that where no_fl_stems==0 or NA the fl_stem_height and inflor_length cols are ALWAYS NA

table(is.na(dem$no_fl_stems[c(which(dem$no_fl_stems==0),which(is.na(dem$no_fl_stems)))]))

no_stems<-dem[-which(dem$no_fl_stems>0),]
no_stems<-tidy.df(no_stems)
head(no_stems)
table(no_stems$no_fl_stems)
length(which(is.na(no_stems$no_fl_stems)))
dim(no_stems)
which(!is.na(no_stems$fl_stem_height))
which(!is.na(no_stems$inflor_length))

# plot histograms of fecundity cols

fec_cols<-c("no_fl_stems","fl_stem_height","inflor_length")
fecund<-dem[,c(1:which(colnames(dem)=="plant_id"),which(colnames(dem) %in% fec_cols))]
head(fecund)

quartz("",12,3,pointsize=20,dpi=80)
par(mfrow=c(1,4), mar=c(1,4,1,1), mgp=c(2.5,1,0))
boxplot(fecund$no_fl_stems[fecund$no_fl_stems>0], ylab="Number of flowering stems [>0]")
boxplot(fecund$fl_stem_height[fecund$fl_stem_height>0], ylab="Flowering stem height [>0]")
boxplot(fecund$inflor_length[fecund$inflor_length>0], ylab="Inflorescence length [>0]")
boxplot(dem$reprod[dem$reprod>0], ylab="Reproductive effort [>0]")

xx$out
plot(xx)
hist(xx$out)

fecund[which(fecund$no_fl_stems>50),]

# FIXED (nothing to fix, they were all NA); Check that survival always == NA in Y0:
demy0<-dem[which(dem$s_year=="Y0"),]
demy0<-tidy.df(demy0)
which(!is.na(demy0$survival))
table(demy0$survival)

# FIXED: 
# check that where survival == "no" the number of flowering stems is always NA:

deaddat<-dem[which(dem$survival=="no"),]
deaddat<-tidy.df(deaddat)
head(deaddat)

deaddat[which(!is.na(deaddat$no_fl_stems)),c(which(colnames(deaddat)=="site_code"):which(colnames(deaddat)=="plant_id"),which(colnames(deaddat) %in% c("survival","no_fl_stems","pid","site_yr","flowering")))]
table(deaddat$no_fl_stem)



# FIXED. ROSETTE PROBLEM:
# This code is updated below to pick out a wider range of errors. 

# where survival=="no" for a single individual (pid), within year, within site, the number rows in the data should be 1. All mismatches will be lines to remove:
head(dem,2)

# first get index of year x site combinations which have at least one "no" (there's no need to do this on Y0 data for example, since there are no "no"s):
survno<-dem[which(dem$survival=="no"),]
survno<-tidy.df(survno)
head(survno)
survno$pid<-as.factor(survno$pid)

sy2<-levels(survno$site_yr)
dup.out<-list()

# then find instances where there is a duplicated pid; for those that have > 1 row, find where one of the rows is "no": 

for (i in 1:length(sy2)){

sy.thisrun<-sy2[i]

full.dat<-dem[dem$site_yr==sy.thisrun,c("site_code","c_year","s_year","transect","plot","plant_id","survival","no_rosettes","pid","site_yr")]
full.dat<-tidy.df(full.dat)
head(full.dat)

if(length(which(duplicated(full.dat$pid)))==0) next

dup.pid<-full.dat$pid[which(duplicated(full.dat$pid))]
red.dat<-full.dat[which(full.dat$pid %in% dup.pid),]
red.dat<-tidy.df(red.dat)
head(red.dat)

if(length(which(red.dat$survival=="no"))==0) next 

rd2<-red.dat[which(red.dat$survival=="no"),]
rd2<-tidy.df(rd2)
rd2<-rd2[,c("site_yr","pid")]

dup.out[[i]]<-rd2

} # close for

dr<-do.call(rbind, dup.out)
dr$dc<-paste(dr$site_yr,dr$pid,sep="")
dr<-dr[-which(duplicated(dr$dc)),]
dr<-tidy.df(dr)
dr$dc<-NULL
head(dr)

tpl<-tapply(dr$pid,dr$site_yr,paste,collapse=", ")
tpl[19]

# write.table(data.frame(site_yr=dimnames(tpl), rows=tpl),file="dup.txt",sep="\t",row.names=F, quote=F)

# write.table(dupres,"dupres.txt",row.names=F,quote=F,sep="\t")







# FIXED. DUPLICATE ROWS & plant_id

# Some people, e.g. EE have included the data twice where there were two flowering stems... 

# Find instances of repeated plant_ids within plots within years, where there is only one rosette:

dem$site_yr<-as.factor(paste(dem$site_code, dem$s_year, sep="_"))
head(dem)

sy<-levels(dem$site_yr)

# the original code only detected duplicates where the number of rosettes was 1. Have updated to detect where the duplicated id is within the SAME number of rosettes (not just where rosettes==1). 

offenders<-data.frame(site_yr=sy,mismatches=NA)

mismatch<-list()

for (i in 1:length(sy)){

sy.thisrun<-sy[i]
d.thisrun<-dem[dem$site_yr==sy.thisrun,]
d.thisrun<-tidy.df(d.thisrun)
head(d.thisrun)

# get dup ids
dup.id<-d.thisrun$pid[which(duplicated(d.thisrun$pid))]

if(length(dup.id)==0) mismatch[[i]]<-data.frame(site_yr=sy.thisrun,mismatches="none")
if(length(dup.id)==0) next

# subset data to only all rows with dups:
d.thisrun<-d.thisrun[which(d.thisrun$pid %in% dup.id),]

head(d.thisrun[,c("site_yr","c_year","s_year","pid","no_rosettes","rosette_number","no_leaves")])

# What you want to know is: is the length of each ID == to the number of rosettes for each (the assumption here is that the number of rosettes has been entered correctly, which is mostly but not always true):

# Don't know if this will always be in order, so make two dfs and combine:
x_df<-data.frame(pid_fromtab=names(table(d.thisrun$pid)),no_records=as.numeric(table(d.thisrun$pid)))

y_df<-data.frame(pid_fromdf=d.thisrun$pid[which(!duplicated(d.thisrun$pid))],no_rosettes_fromdf=d.thisrun$no_rosettes[which(!duplicated(d.thisrun$pid))])

new_df<-merge(x_df,y_df,by.x="pid_fromtab",by.y="pid_fromdf")
new_df$same<-ifelse(new_df$no_records==new_df$no_rosettes_fromdf,1,0)

mismatches<-as.character(new_df$pid_fromtab[new_df$same==0])

if(length(mismatches)>0) mismatch[[i]]<-data.frame(site_yr=rep(sy.thisrun,length(mismatches)),mismatches=mismatches)
if(length(mismatches)==0) mismatch[[i]]<-data.frame(site_yr=sy.thisrun,mismatches="none")

} # close for

mism.res<-do.call(rbind,mismatch)
head(mism.res)

# These are the ones you're gonna have to deal with, even the NAs

mism2<-mism.res[-which(mism.res$mismatches=="none"),]
mism2<-tidy.df(mism2)

mism2$ex<-paste(mism2$site_yr, mism2$mismatches, sep="_")
mism2<-mism2[-which(duplicated(mism2$ex)),]
mism2<-tidy.df(mism2)
mism2$ex<-NULL
# write.table(mism2,"mism2.txt",quote=F,sep="\t",row.names=F)

dem[which(dem$site_code=="TUE" & dem$plant_id=="57"),]
dem[which(dem$site_code=="ARH" & dem$pid=="T2_P3_1"),]
dem[which(dem$site_code=="ARH" & dem$x_coord==4.5),]

off2<-offenders[which(offenders$how_many>0),]

dat.now<-dem[which(dem$site_yr=="TUE_Y0"),c("site_yr","c_year","s_year","pid","no_rosettes","rosette_number","no_leaves")]

dat.now[dat.now$pid==dat.now[which(dat.now$no_rosettes=="1"),][which(duplicated(dat.now[which(dat.now$no_rosettes=="1"),]$pid)),]$pid,]

unique(dat.now[which(duplicated(dat.now$pid) & dat.now$no_rosettes==1),]$pid)

dem[which(dem$site_code=="PC" & dem$plant_id=="47"),]

for (i in 1:nrow(dens_sum)){

site.thisrun<-as.character(dens_sum$site_code[i])
dat.thisrun<-dens[which(dens$site_code==site.thisrun),]
dat.thisrun<-tidy.df(dat.thisrun)
head(dat.thisrun)

years.thisrun<-levels(dat.thisrun$s_year)

for (j in 1:length(years.thisrun)){

year.now<-years.thisrun[j]
dat.now<-dat.thisrun[dat.thisrun$s_year== year.now,]
dat.now<-tidy.df(dat.now)
head(dat.now)

head(dem)

n_plants<-length(levels(dat.now$plant_id))
n_plots<-length(levels(dat.now$plot))

# Plants per m2:
dens_sum[which(dens_sum==site.thisrun),grep(paste(year.now,"plant",sep="_"),colnames(dens_sum))]<-(n_plants/n_plots)*4

} # close j years

} # close i sites




# FIXED: # RE-CHECK the number of rosettes is == to the number of rows for each alive plant:
sy<-levels(dem$site_yr)

# the original code only detected duplicates where the number of rosettes was 1. Have updated to detect where the duplicated id is within the SAME number of rosettes (not just where rosettes==1). 

mismatch<-list()

for (i in 1:length(sy)){

sy.thisrun<-sy[i]
d.thisrun<-dem[dem$site_yr==sy.thisrun,]
d.thisrun<-tidy.df(d.thisrun)
head(d.thisrun)

# get dup ids
dup.id<-d.thisrun$pid[which(duplicated(d.thisrun$pid))]

if(length(dup.id)==0) mismatch[[i]]<-data.frame(site_yr=sy.thisrun,mismatches="none")
if(length(dup.id)==0) next

# subset data to only all rows with dups:
d.thisrun<-d.thisrun[which(d.thisrun$pid %in% dup.id),]

head(d.thisrun[,c("site_yr","c_year","s_year","pid","no_rosettes","rosette_number","no_leaves")])

# What you want to know is: is the length of each ID == to the number of rosettes for each (the assumption here is that the number of rosettes has been entered correctly, which is mostly but not always true):

# Don't know if this will always be in order, so make two dfs and combine:
x_df<-data.frame(pid_fromtab=names(table(d.thisrun$pid)),no_records=as.numeric(table(d.thisrun$pid)))

y_df<-data.frame(pid_fromdf=d.thisrun$pid[which(!duplicated(d.thisrun$pid))],no_rosettes_fromdf=d.thisrun$no_rosettes[which(!duplicated(d.thisrun$pid))])

new_df<-merge(x_df,y_df,by.x="pid_fromtab",by.y="pid_fromdf")
new_df$same<-ifelse(new_df$no_records==new_df$no_rosettes_fromdf,1,0)

mismatches<-as.character(new_df$pid_fromtab[new_df$same==0])

if(length(mismatches)>0) mismatch[[i]]<-data.frame(site_yr=rep(sy.thisrun,length(mismatches)),mismatches=mismatches)
if(length(mismatches)==0) mismatch[[i]]<-data.frame(site_yr=sy.thisrun,mismatches="none")

} # close for

mism.res<-do.call(rbind,mismatch)
head(mism.res)







### stem height problem

dir()
dd<-read.table("stem_heights.txt",header=T)
inf<-read.table("infl.txt",header=T)

head(inf)

# This gets the biggest stem
xx<-apply(dd[,which(colnames(dd)=="s1"):length(dd)],1,function(x) max(as.numeric(x),na.rm=T))

# This gets the place for the biggest stem:
place<-apply(dd[,which(colnames(dd)=="s1"):length(dd)],1,function(x) which(as.numeric(x)==max(as.numeric(x),na.rm=T))[1])

# This gets the biggest infl:
biginf<-apply(inf[,which(colnames(inf)=="in1"):length(inf)],1,function(x) max(as.numeric(x),na.rm=T))

# This gets the infl length for the biggest stem (i.e. the infl length for the longest stem):

inf$place<-place
inf$inf_length<-NA
head(inf)

# can't believe I'm doing this but...
# try Reduce() next time! Reduce(function(x,i) gsub(pattern[i],repl[i],x),seq_along(pattern),x)

for (i in 1:nrow(inf)){
row.thisrun<-inf[i,which(colnames(inf)=="in1"):which(colnames(inf)=="in12")]
if(is.na(inf$place[i])) next else inf$inf_length[i]<-row.thisrun[inf$place[i]]
}
inf$inf_length<-unlist(inf$inf_length)
inf$biggest_inf<-biginf

# There are quite a few NAs in the inf_length, so put the biggest in these places:
inf$inf_length[which(is.na(inf$inf_length))]<-inf$biggest_inf[which(is.na(inf$inf_length))]
inf$inf_length[which(inf$inf_length=="-Inf")]<-NA
inf$biggest_inf[which(inf$biggest_inf=="-Inf")]<-NA
head(inf)

dd$biggest_stem<-xx

dd2<-dd[,c("site_code","plant_id","biggest_stem")]
head(dd2)

inf2<-inf[,c("site_code","plant_id","inf_length","biggest_inf")]
head(inf2)

# write.table(inf2,"inf2.txt",quote=F,row.names=F,sep="\t")




















