#Multivariate Environmental Similarity Surface (MESS) analysis
##for the Bluntface Shiner Project Chapter 1 using modEvA package.
#ModEva package contains a command for MESS analysis and unlike other MESS packages,
##modEvA is able to analyze river datasets, rather than only raster-type data.
#Our objective is to compare similarity between environmental covarites in the east
##and west ranges of BFS. Lets get started!

#set working directory
setwd("C:/Users/ponyf/Documents/Thesis/R coding")

#install necessary packages and load them in
install.packages("modEva")


#Lets load in the data. This dataframe was created for Species Distribution Modelling
#with Maxent-in-R. It will work here too. Load it in and take a gander.

AllBFS <- read.csv("BfsAllpopv11.csv", header = T, na.strings = "-9999")
AllBFS <- AllBFS[complete.cases(AllBFS),]

#streams of the East and West are already divided. With each distribution is all of the 
##covariates we explored. 
#We need to make subsets of East and West separately so that we can compare later.

WEST <- subset(AllBFS, Region == "West")
WEST.WO.ID <- subset(WEST, select = c(7:14))
WEST.W.ID <- subset(WEST, select = c(COMID, 7:14))

EAST <- subset(AllBFS, Region == "East")
EAST.WO.ID <- subset(EAST, select = c(7:14))
EAST.W.ID<- subset(EAST, select = c(COMID, 7:14))

#Run this MESS for West to East
mess.with.ID <- MESS(V = WEST.WO.ID, P = EAST.W.ID, id.col = 1)

head(mess.with.ID)
range(mess.with.ID[ , "TOTAL"])

write.csv(mess.with.ID, "MESSWesttoEast.csv")

#Run again for East to West

mess.with.ID.2 <- MESS(V = EAST.WO.ID, P = WEST.W.ID, id.col = 1)

head(mess.with.ID.2)
range(mess.with.ID.2[ , "TOTAL"])

write.csv(mess.with.ID.2, "MESSEasttoWest.csv")


