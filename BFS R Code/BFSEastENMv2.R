####Welcome to my Maxent in R crash test, I am the crash dummy. I will base this code off an example from the venerable Dr. Andrew Taylor. Here we go...

setwd("C:/Users/ponyf/Desktop/Maxent in R/Final/")

install.packages("ENMeval")
install.packages("dplyr")


library(ENMeval)
library(dplyr)

############################WEST ANALYSIS####################################
bfssdmeast<- read.csv("BFSEastpopv9.csv", header = T, na.strings = "-9999")


bfssdmeast<- data.frame(select(bfssdmeast, "BFS_East", "COMID", "ElevCat", "ClayCat", "SandCat", "RckDepCat", "Divergence", "Slope", "Confinement", "TotDASqKM", "HUC8"))
bfssdmeast <- bfssdmeast[!duplicated(bfssdmeast),]
bfssdmeast <- bfssdmeast[complete.cases(bfssdmeast),]

Envdatasummary <- summary(bfssdmeast)
Envdatasummary
write.csv(Envdatasummary, "bfssdmeastenvdatasummary.csv")

bfssdmeast$longitude <- bfssdmeast$COMID
bfssdmeast$latitude <- bfssdmeast$COMID
bfssdmeast <- bfssdmeast %>% select(longitude, latitude, everything())


occs.EAST <- subset(bfssdmeast, BFS_East == "1")
occs.EAST <- subset(occs.EAST, select = -c(BFS_East, COMID))


bg.EAST <- subset(bfssdmeast, BFS_East == "0")
bg.EAST <- subset(bg.EAST, select = -c(BFS_East, COMID))

occs.EAST <- subset(occs.EAST, subset = HUC8 %in% c(3180002, 3180003, 6040005, 8010201, 8010202, 8010203, 8010204, 8010205, 8010207, 8010208, 8010209, 8010210,
                                                    8030201, 8030202, 8030203, 8030204, 8030205, 8030206, 8060100, 8060201, 8060202, 8060203, 8060204, 8060205, 
                                                    8060206, 8070201))

bg.EAST <- subset(bg.EAST, subset = HUC8 %in% c(3180001, 3180002, 3180003, 3180004, 3180005, 6010101, 6010102, 6010103, 6010104, 6010105, 6010106, 6010107, 6010108, 
                                                6010201, 6010202, 6010203, 6010204, 6010205, 6010206, 6010207, 6010208, 6020001, 6020002, 6020003, 6020004, 6030001, 
                                                6030002, 6030003, 6030004, 6030005, 6030006, 6040001, 6040002, 6040003, 6040004, 6040005, 6040006, 8010100, 8010201,
                                                8010202, 8010203, 8010204, 8010205, 8010206, 8010207, 8010208, 8010209, 8010210, 8010211, 8020100, 8020201, 8020202, 
                                                8020203, 8020204, 8020205, 8020301, 8020302, 8020303, 8020304, 8020401, 8020402, 8030100, 8030201, 8030202, 8030203, 
                                                8030204, 8030205, 8030206, 8030207, 8030208, 8030209, 8040101, 8040102, 8040103, 8040201, 8040202, 8040203, 8040204, 
                                                8040205, 8040206, 8040207, 8040301, 8040302, 8040303, 8040304, 8040305, 8040306, 8050001, 8050002, 8050003, 8060100,
                                                8060201, 8060202, 8060203, 8060204, 8060205, 8060206, 8070100, 8070201, 8070202, 8070203, 8070204, 8070205, 8070300,
                                                8080101, 8080102, 8080103, 8080201, 8080202, 8080203, 8080204, 8080205, 8080206, 8090100, 8090201, 8090202, 8090203, 
                                                8090301, 8090302, 22163556))

occs.EAST["HUC8"][occs.EAST["HUC8"] == "3180002"] <- "1"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "3180003"] <- "2"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "6040005"] <- "3"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010201"] <- "4"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010202"] <- "5"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010203"] <- "6"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010204"] <- "7"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010205"] <- "8"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010207"] <- "9"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010208"] <- "10"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010209"] <- "11"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8010210"] <- "12"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030201"] <- "13"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030202"] <- "14"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030203"] <- "15"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030204"] <- "16"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030205"] <- "17"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8030206"] <- "18"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060100"] <- "19"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060201"] <- "20"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060202"] <- "21"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060203"] <- "22"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060204"] <- "23"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060205"] <- "24"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8060206"] <- "25"
occs.EAST["HUC8"][occs.EAST["HUC8"] == "8070201"] <- "26"



bg.EAST["HUC8"][bg.EAST["HUC8"] == "3180001"] <- "1"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "3180002"] <- "2"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "3180003"] <- "3"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "3180004"] <- "4"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "3180005"] <- "5"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010101"] <- "6"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010102"] <- "7"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010103"] <- "8"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010104"] <- "9"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010105"] <- "10"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010106"] <- "11"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010107"] <- "12"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010108"] <- "13"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010201"] <- "14"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010202"] <- "15"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010203"] <- "16"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010204"] <- "17"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010205"] <- "18"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010206"] <- "19"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010207"] <- "20"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6010208"] <- "21"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6020001"] <- "22"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6020002"] <- "23"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6020003"] <- "24"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6020004"] <- "25"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030001"] <- "26"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030002"] <- "27"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030003"] <- "28"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030004"] <- "29"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030005"] <- "30"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6030006"] <- "31"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040001"] <- "32"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040002"] <- "33"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040003"] <- "34"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040004"] <- "35"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040005"] <- "36"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "6040006"] <- "37"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010100"] <- "38"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010201"] <- "39"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010202"] <- "40"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010203"] <- "41"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010204"] <- "42"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010205"] <- "43"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010206"] <- "44"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010207"] <- "45"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010208"] <- "46"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010209"] <- "47"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010210"] <- "48"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8010211"] <- "49"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020100"] <- "50"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020201"] <- "51"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020202"] <- "52"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020203"] <- "53"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020204"] <- "54"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020205"] <- "55"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020301"] <- "56"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020302"] <- "57"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020303"] <- "58"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020304"] <- "59"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020401"] <- "60"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8020402"] <- "61"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030100"] <- "62"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030201"] <- "63"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030202"] <- "64"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030203"] <- "65"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030204"] <- "66"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030205"] <- "67"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030206"] <- "68"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030207"] <- "69"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030208"] <- "70"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8030209"] <- "71"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040101"] <- "72"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040102"] <- "73"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040103"] <- "74"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040201"] <- "75"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040202"] <- "76"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040203"] <- "77"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040204"] <- "78"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040205"] <- "79"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040206"] <- "80"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040207"] <- "81"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040301"] <- "82"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040302"] <- "83"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040303"] <- "84"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040304"] <- "85"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040305"] <- "86"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8040306"] <- "87"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8050001"] <- "88"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8050002"] <- "89"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8050003"] <- "90"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060100"] <- "91"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060201"] <- "92"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060202"] <- "93"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060203"] <- "94"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060204"] <- "95"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060205"] <- "96"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8060206"] <- "97"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070100"] <- "98"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070201"] <- "99"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070202"] <- "100"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070203"] <- "101"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070204"] <- "102"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070205"] <- "103"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8070300"] <- "104"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080101"] <- "105"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080102"] <- "106"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080103"] <- "107"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080201"] <- "108"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080202"] <- "109"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080203"] <- "110"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080204"] <- "111"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080205"] <- "112"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8080206"] <- "113"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090100"] <- "114"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090201"] <- "115"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090202"] <- "116"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090203"] <- "117"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090301"] <- "118"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "8090302"] <- "119"
bg.EAST["HUC8"][bg.EAST["HUC8"] == "22163556"] <- "120"

occs.EAST$HUC8_dubnum <- as.double(occs.EAST$HUC8)
bg.EAST$HUC8_dubnum <- as.double(bg.EAST$HUC8)


huc8.EAST <- list(occs.grp = occs.EAST$HUC8_dubnum,
                  bg.grp = bg.EAST$HUC8_dubnum)

occs.EAST <- subset(occs.EAST, select = -c(HUC8, HUC8_dubnum))

bg.EAST <- subset(bg.EAST, select = -c(HUC8, HUC8_dubnum))


tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5)

BFSeast <- ENMevaluate(occs.EAST, bg = bg.EAST, algorithm = "maxent.jar", tune.args = tune.args,
                       partitions = "user", user.grp = huc8.EAST, categoricals = c("Divergence", "Confinement"))


#Save the ENMeval file to pull later
save(BFSeast, file = "BFSeastENMresults")

res.EAST <- eval.results(BFSeast)
res.EAST
write.table(res.EAST, file = "BFSeast_models.csv", sep = ",", quote = FALSE, row.names = F)

bestmodeast <- which(BFSeast@results$AICc==min(BFSeast@results$AICc))
BFSeast@models[bestmodeast]
#best model is fc.LQH_rm.2

BFSeast_topmod <- eval.models(BFSeast)[["fc.LQH_rm.2"]]
# the enframe function from the tibble package makes this named vector into a 
# more readable table.
install.packages("tibble")
library(tibble)
BFSeastBetas <- enframe(BFSeast_topmod@results)
write.table(BFSeastBetas, file = "BFSeast_topmodel_betas.csv", sep = ",", quote = FALSE, row.names = F)


#Paste Maxent.jar (as downloaded online) into the java folder of the dismo package at this location & code checks to make sure it will run
install.packages("dismo")
library(dismo)
install.packages("rJava")
Sys.setenv(JAVA_HOME="C:Program Files/Java/jre1.8.0_321/")
library(rJava)
system.file("java", package="dismo")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava))

#Creating argument list for Maxent in dismo, including RM and feature class specifics from top model in ENMeval
#Briant, use this list pasted below to match to our best model and the settings that ENMeval would otherwise default to
#https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ/m/Fj8Two0lmHIJ
#CHECK AND DOUBLE CHECK RMargs BEFORE RUNNING
#This is the best model fc.LQH_rm.2
RMargs <-c(paste0("betamultiplier=2"))
RMargs <- c(RMargs, paste0("writeplotdata=TRUE"))
RMargs <- c(RMargs, paste0("removeDuplicates=TRUE"))
RMargs <- c(RMargs, paste0("responsecurves=TRUE"))
RMargs <- c(RMargs, paste0("writebackgroundpredictions=TRUE"))

#We want to only turn on the feature class types of the top ENMeval model
RMargs <- c(RMargs, paste0("linear=TRUE"))
RMargs <- c(RMargs, paste0("quadratic=TRUE"))
RMargs <- c(RMargs, paste0("product=FALSE"))
RMargs <- c(RMargs, paste0("threshold=FALSE"))
RMargs <- c(RMargs, paste0("hinge=TRUE"))

####We also want to do jackknifing for variable importance, so adding this
RMargs <- c(RMargs, paste0("jackknife=TRUE"))

#A few lines of code needed here to where you split input data into two frames.
#ORDER OF THE ROWS MATTERS
#dataframe "p" is 0's for background and 1's for occurrence, in same order as...(see next line)
#dataframe "XAll_var"is just the lat, long, and environmental covariates for each row (but no pres/abs)
#make sure inputs are otherwise same as those entered into ENMeval previously
p2 <- as.data.frame(bfssdmeast$BFS_East)
env_E <- subset(bfssdmeast, select = -c(BFS_East, COMID, longitude, latitude, HUC8))


env_ECOMID <- subset(bfssdmeast, select = COMID)


#Running Maxent model with dismo with our arguments and increase the max number of background points to include all background
#Make sure categorical variables are treated as factors on these lines
#for nbg set it to above 200,000
BFSbesteast <- maxent (env_E, p2, args = RMargs, nbg= 250000,
                       path = "BFSbesteast", 
                       silent = FALSE)


save(BFSbesteast, file = "BFSbesteastmaxent")
plot(BFSbesteast)
response(BFSbesteast)



#Can look at specific response individually
#partial response (avg prediction for all data points used to fit the model)
#response(BFSbestwest, var="Q0001C_Yr")
#response(BFSbestwest, var="Q0001C_Yr", at=mean)


####Projecting model back onto input data to save predictions to segments
###https://rdrr.io/github/johnbaums/rmaxent/f/README.md
#####################################

install.packages("devtools")
library(devtools)
install_github('johnbaums/rmaxent')
library(rmaxent)

#Obtain cloglog for east
predictEAST2EAST <- project(BFSbesteast, env_E)
predictEAST2EAST <-as.data.frame(predictEAST2EAST)
#Append Maxent model predictions to model input data
env_E$pred_raw <-predictEAST2EAST$prediction_raw
env_E$pred_log <- predictEAST2EAST$prediction_logistic
env_E$pred_cloglog <- predictEAST2EAST$prediction_cloglog
env_E <- cbind(env_E, env_ECOMID)
#Export the resulting projection with final model predictions back to appropriate model folder
write.csv(env_E, "predictEAST2EAST.csv")


bfssdmwest<- read.csv("BFSWestpopv2.csv", header = T, na.strings = "-9999")
bfssdmwest <- bfssdmwest[!duplicated(bfssdmwest),]
bfssdmwest <- bfssdmwest[complete.cases(bfssdmwest),]
bfssdmwest <- data.frame(select(bfssdmwest, "BFS_West", "COMID", "ElevCat", "ClayCat", "SandCat", "RckDepCat", "Divergence", "Slope", "Confinement", "TotDASqKM", "HUC8"))
bfssdmwest$longitude <- bfssdmwest$COMID
bfssdmwest$latitude <- bfssdmwest$COMID
bfssdmwest <- bfssdmwest %>% select(longitude, latitude, everything())
env_WCOMID <- subset(bfssdmwest, select = COMID)
env_forE2W <- subset(bfssdmwest, select = -c(BFS_West, COMID, longitude, latitude, HUC8))


#obtain cloglog for east onto west
predictEAST2WEST <-project(BFSbesteast, env_forE2W)
predictEAST2WEST <-as.data.frame(predictEAST2WEST)

#Append Maxent model predictions to model input data
env_forE2W$pred_raw <- predictEAST2WEST$prediction_raw
env_forE2W$pred_log <-predictEAST2WEST $prediction_logistic
env_forE2W$pred_cloglog <- predictEAST2WEST $prediction_cloglog
env_forE2W <- cbind(env_forE2W, env_WCOMID)
#Export the resulting projection with final model predictions back to appropriate model folder
write.csv(env_forE2W, "predictEAST2WEST.csv")


####calculate MESS (multivariate environmental similarity surfaces between two regions)
####See function and example in link below and use full EAST (e.g., bg and occs) vs full WEST 
####https://rdrr.io/cran/modEvA/man/MESS.html



#####THEN, REPEAT for the other side of the distribution!!!   :)
