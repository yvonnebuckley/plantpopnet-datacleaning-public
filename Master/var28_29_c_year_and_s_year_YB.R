## c_year and s_year
## YB 07/01/2019
## warning messages implemented 21/01/19

## reivewed by Caroline Mckeon 13/03/19

## Diagnostic only - no script changes to source

#mydata <- read.csv("fullPPN_dataset_2019-07-01_.csv", header = T)

##  s_year should be a factor
x <- is.factor(mydata$s_year) ## should be TRUE
if(x != TRUE) warning('check that s_year is a factor e.g. Y0, Y1 etc')
levels_s_year <- levels(mydata$s_year)  ## Y0, Y1, Y2, ...

x <- any(is.na(mydata$s_year)) ## should be FALSE
if(x == TRUE) warning('NA detected in s_year: check correct survey year is input')


## c_year should be numeric values only
x <- is.numeric(mydata$c_year)  ## should be TRUE
if(x != TRUE) warning('c_year should be numeric')

x <- any(is.na(mydata$c_year)) ## should be FALSE
if(x == TRUE) warning('NA detected in c_year: check correct census year is input')

## for each site c_year is the actual calendar year of first census
## for each site s_year is the survey year (Y0, Y1, ...)
## for each site there should be sequential c_years and the same sequence of s_years

t1 <- table(mydata$c_year)
t2 <- table(mydata$s_year)

tc <- table(mydata$site_code, mydata$c_year)
#tc[1,]
ts <- table(mydata$site_code, mydata$s_year)
#ts[1,]

#x <- rep(NA, dim(tc)[1])

for(i in 1:dim(tc)[1]) {
  ## tests for each site whether same number of observations are in tc and ts
  test_years <- ts[i,ts[i,]>0] == tc[i,tc[i,]>0]
  x[i] <- any(test_years == FALSE) 
}
if(any(x == TRUE)) {
  warning('No. obs. in tc != no. obs in ts: check s_year and c_year correspond for the site returned here')
  row.names(tc)[which(x == TRUE)]
}
## WIN has an error (year coded as 2016 should be 2017 - Fixed in raw data files by alain 01/05/20)

