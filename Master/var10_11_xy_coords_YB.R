## Yvonne Buckley 19/11/18
##checking within plot x,y coordinates

## reviewed by Caroline Mckeon 13/03/19

#mydata <- read.csv("fullPPN_dataset_2018-06-12_.csv", header = T)

## ERROR: checking levels of factor x_coord to see if they can be coerced to NA
# levels(mydata$x_coord)
# levels(mydata$y_coord)
  
## ERROR: Manually checking records with text rather than numbers - text from the levels function
# print(subset(mydata, mydata$x_coord == "ERROR"))
# print(subset(mydata, mydata$y_coord == "ERROR"))
# print(subset(mydata, mydata$x_coord == "see notes"))

## SOLUTION: create new variable to contain "fixed" values leaving original variable unchanged for comparison
## you will get warnings as NAs introduced by coercion - this should be ok - you can suppress in the markdown code chunk if necessary
mydata$fix_x_coord <- mydata$x_coord
mydata$fix_y_coord <- mydata$y_coord

mydata$fix_x_coord <- as.numeric(as.character(mydata$fix_x_coord))
mydata$fix_y_coord <- as.numeric(as.character(mydata$fix_y_coord))
  
## CHECK FIX
## print(summary(mydata$fix_x_coord))
## print(summary(mydata$fix_y_coord)) 

## TIDYING UP
## create new variables called x_coord_notes and y_coord_notes which contain the old versions of x & y_coord including useful notes
mydata$x_coord_notes <- mydata$x_coord
mydata$y_coord_notes <- mydata$y_coord

## x_coord and y_coord now contain numeric values and NAs only
mydata$x_coord <- mydata$fix_x_coord
mydata$y_coord <- mydata$fix_y_coord

## remove fix_ variables as no longer needed & drop unused levels
mydata <- mydata[, -which(names(mydata) %in% c("fix_x_coord", "fix_y_coord"))]
mydata <- droplevels(mydata)





  


