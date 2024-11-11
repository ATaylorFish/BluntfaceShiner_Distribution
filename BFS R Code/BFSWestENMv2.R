####Welcome to my Maxent in R crash test, I am the crash dummy. I will base this code off an example from the venerable Dr. Andrew Taylor.
####Begin with the Western range (below), followed by East, and then All. Here we go...

setwd("C:/Users/ponyf/Desktop/Maxent in R/Final")


install.packages("ENMeval")
install.packages("dplyr")
install.packages("dismo")


library(ENMeval)
library(dplyr)


############################WEST ANALYSIS####################################
#Load in data. We must remove duplicates and incomplete rows.
bfssdmwest<- read.csv("BFSWestpopv2.csv", header = T, na.strings = "-9999")


#Note: -9999 is for NA in the dataset
#lets load this data in

####Lets look at some summary stats of the environmental dataset. Create a .csv file to reference the
####stats in the future
Envdatasummary <- summary(bfssdmwest)
Envdatasummary
write.csv(Envdatasummary, "bfssdmwestenvdatasummary.csv")

####################Check for correlation between environmental variables#########
####the NA = -9999 is being problematic. the 'use' code will fix this######
cor(bfssdmwest[,12:22], use = "complete.obs", method ='spearman')


####since I am fancy, ill graph this for the banter
install.packages('corrplot')
library(corrplot)

envvarcor <- cor(bfssdmwest[,12:22], use = "complete.obs", method ='spearman')
envvarcor
corrplot(envvarcor)

#Nice, now lets see which variables are least correlated.
#Remove the highly correlated variables (e.g., r > |0.7|)
#seems like bif class, total drainage area, and streamorder are highly correlated so we will remove one. I see that gradient class and slope are highly correlated as well, so I will need to choose one...Ill go with total drainage and slope
#ElevCat, ClayCat, SandCat, RckDepCat, Divergence, Slope, Confinement, TotDASqKM



#We will select least correlated variables
#also, I messed up with "ecoregions" so remove that too..
bfssdmwest <- data.frame(select(bfssdmwest, "BFS_West", "COMID", "ElevCat", "ClayCat", "SandCat", "RckDepCat", "Divergence", "Slope", "Confinement", "TotDASqKM", "HUC8"))
bfssdmwest <- bfssdmwest[!duplicated(bfssdmwest),]
bfssdmwest <- bfssdmwest[complete.cases(bfssdmwest),]

Envdatasummary <- summary(bfssdmwest)
Envdatasummary
write.csv(Envdatasummary, "bfssdmwestenvdatasummary.csv")

#We need lat and long which we substitute with COMID
bfssdmwest$longitude <- bfssdmwest$COMID
bfssdmwest$latitude <- bfssdmwest$COMID
bfssdmwest <- bfssdmwest %>% select(longitude, latitude, everything())

#We need to separate our occurrence records and our background records
occs.WEST <- subset(bfssdmwest, BFS_West == "1")
occs.WEST <- subset(occs.WEST, select = -c(BFS_West, COMID))


bg.WEST <- subset(bfssdmwest, BFS_West == "0")
bg.WEST <- subset(bg.WEST, select = -c(BFS_West, COMID))

#we also need to set the partition groups. These are based on HUC8. 
occs.WEST <- subset(occs.WEST, subset = HUC8 %in% c(11060005, 11070101, 11070102, 11070103, 11070104, 11070106, 11070203, 11070207, 11070208, 11070209, 11110102, 11110103, 11030013,
                                                    11060004, 11060006, 11070107, 11070201, 11070202, 11070204, 11070205, 11070206, 11110202))

bg.WEST <- subset(bg.WEST, subset = HUC8 %in% c(11060005, 11070101, 11070102, 11070103, 11070104, 11070106, 11070203, 11070207, 11070208, 11070209, 11110102, 11110103, 11010001, 
                                                    11010002, 11010003, 11010004, 11010005, 11010006, 11010007, 11010008, 11010009, 11010010, 11010011, 11010012, 11010013, 11010014, 
                                                    11020001, 11020002, 11020003, 11020004, 11020005, 11020006, 11020007, 11020008, 11020009, 11020010, 11020011, 11020012, 11140208,
                                                    11020013, 11030001, 11030002, 11030003, 11030004, 11030005, 11030006, 11030007, 11030008, 11030009, 11030010, 11030011, 11030012, 
                                                    11030014, 11030015, 11030016, 11030017, 11030018, 11040001, 11040002, 11040003, 11040004, 11040005, 11040006, 11040007, 11040008, 
                                                    11050001, 11050002, 11050003, 11060001, 11060002, 11060003, 11070105, 11070203, 11080001, 11080002, 11080003, 11080004, 11080005, 
                                                    11080006, 11080007, 11080008, 11090101, 11090102, 11090103, 11090104, 11090105, 11090106, 11090201, 11090202, 11090203, 11090204, 
                                                    11100101, 11100102, 11100103, 11100104, 11100201, 11100202, 11100203, 11100301, 11100302, 11100303, 11110101, 11110104, 11110105,
                                                    11110201, 11110203, 11110204, 11110205, 11110206, 11110207, 11120101, 11120102, 11120103, 11120104, 11120105, 11120201, 11120202, 
                                                    11120301, 11120302, 11120303, 11120304, 11130101, 11130102, 11130103, 11130104, 11130105, 11130201, 11130202, 11130203, 11130204, 
                                                    11130205, 11130206, 11130207, 11130208, 11130209, 11130210, 11130301, 11130302, 11130303, 11130304, 11140101, 11140102, 11140103,
                                                    11140104, 11140105, 11140106, 11140107, 11140108, 11140109, 11140201, 11140202, 11140203, 11140204, 11140205, 11140206, 11140207,
                                                    11140209, 11140301, 11140302, 11140303, 11140304, 11140305, 11140306, 11140307, 11030013, 11060004, 11060006, 11070107, 11070201, 
                                                    11070202, 11070204, 11070205, 11070206, 11110202))
#Make the partitions as a list
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11060005"] <- "1"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070101"] <- "2"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070102"] <- "3"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070103"] <- "4"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070104"] <- "5"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070106"] <- "6"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070203"] <- "7"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070207"] <- "8"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070208"] <- "9"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070209"] <- "10"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11110102"] <- "11"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11110103"] <- "12"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11030013"] <- "13"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11060004"] <- "14"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11060006"] <- "15"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070107"] <- "16"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070201"] <- "17"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070202"] <- "18"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070204"] <- "19"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070205"] <- "20"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11070206"] <- "21"
occs.WEST["HUC8"][occs.WEST["HUC8"] == "11110202"] <- "22"

bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060005"] <- "1"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070101"] <- "2"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070102"] <- "3"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070103"] <- "4"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070104"] <- "5"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070106"] <- "6"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070203"] <- "7"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070207"] <- "8"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070208"] <- "9"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070209"] <- "10"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110102"] <- "11"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110103"] <- "12"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010001"] <- "13"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010002"] <- "14"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010003"] <- "15"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010004"] <- "16"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010005"] <- "17"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010006"] <- "18"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010007"] <- "19"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010008"] <- "20"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010009"] <- "21"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010010"] <- "22"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010011"] <- "23"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010012"] <- "24"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010013"] <- "25"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11010014"] <- "26"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020001"] <- "27"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020002"] <- "28"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020003"] <- "29"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020004"] <- "30"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020005"] <- "31"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020006"] <- "32"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020007"] <- "33"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020008"] <- "34"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020009"] <- "35"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020010"] <- "36"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020011"] <- "37"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020012"] <- "38"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11020013"] <- "39"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030001"] <- "40"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030002"] <- "41"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030003"] <- "42"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030004"] <- "43"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030005"] <- "44"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030006"] <- "45"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030007"] <- "46"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030008"] <- "47"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030009"] <- "48"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030010"] <- "49"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030011"] <- "50"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030012"] <- "51"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030014"] <- "52"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030015"] <- "53"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030016"] <- "54"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030017"] <- "55"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030018"] <- "56"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040001"] <- "57"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040002"] <- "58"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040003"] <- "59"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040004"] <- "60"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040005"] <- "61"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040006"] <- "62"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040007"] <- "63"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11040008"] <- "64"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11050001"] <- "65"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11050002"] <- "66"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11050003"] <- "67"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060001"] <- "68"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060002"] <- "69"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060003"] <- "70"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070105"] <- "71"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070203"] <- "72"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080001"] <- "73"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080002"] <- "74"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080003"] <- "75"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080004"] <- "76"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080005"] <- "77"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080006"] <- "78"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080007"] <- "79"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11080008"] <- "80"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090101"] <- "81"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090102"] <- "82"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090103"] <- "83"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090104"] <- "84"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090105"] <- "85"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090106"] <- "86"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090201"] <- "87"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090202"] <- "88"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090203"] <- "89"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11090204"] <- "90"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100101"] <- "91"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100102"] <- "92"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100103"] <- "93"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100104"] <- "94"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100201"] <- "95"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100202"] <- "96"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100203"] <- "97"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100301"] <- "98"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100302"] <- "99"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11100303"] <- "100"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110104"] <- "101"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110105"] <- "102"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110201"] <- "103"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110203"] <- "104"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110204"] <- "105"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110205"] <- "106"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110206"] <- "107"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110207"] <- "108"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120101"] <- "109"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120102"] <- "110"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120103"] <- "111"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070203"] <- "112"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120104"] <- "113"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120105"] <- "114"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120201"] <- "115"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120202"] <- "116"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120301"] <- "117"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120302"] <- "118"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120303"] <- "119"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11120304"] <- "120"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130101"] <- "121"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130102"] <- "122"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130103"] <- "123"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130104"] <- "124"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130105"] <- "125"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130201"] <- "126"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130202"] <- "127"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130203"] <- "128"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130204"] <- "129"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130205"] <- "130"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130206"] <- "131"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130207"] <- "132"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130208"] <- "133"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130209"] <- "134"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130210"] <- "135"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130301"] <- "136"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130302"] <- "137"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130303"] <- "138"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11130304"] <- "139"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140101"] <- "140"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140102"] <- "141"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140103"] <- "142"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140104"] <- "143"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140105"] <- "144"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140106"] <- "145"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140107"] <- "146"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140108"] <- "147"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140109"] <- "148"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140201"] <- "149"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140202"] <- "150"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140203"] <- "151"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140204"] <- "152"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140205"] <- "153"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140206"] <- "154"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140207"] <- "155"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140208"] <- "156"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140209"] <- "157"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140301"] <- "158"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140302"] <- "159"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140303"] <- "160"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140304"] <- "161"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140305"] <- "162"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140306"] <- "163"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11140307"] <- "164"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11030013"] <- "165"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060004"] <- "166"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11060006"] <- "167"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070107"] <- "168"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070201"] <- "169"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070202"] <- "170"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070204"] <- "171"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070205"] <- "172"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11070206"] <- "173"
bg.WEST["HUC8"][bg.WEST["HUC8"] == "11110202"] <- "174"

#######REMOVE LATER: set limit on number of data to test#######
set.seed(706)
bg.WEST <- sample_n(bg.WEST, 1200)
###############################################################

#Make double column
occs.WEST$HUC8_dubnum <- as.double(occs.WEST$HUC8)
bg.WEST$HUC8_dubnum <- as.double(bg.WEST$HUC8)

#Create the user.grp using our occs and bg groups we just made
huc8.WEST <- list(occs.grp = occs.WEST$HUC8_dubnum,
                  bg.grp = bg.WEST$HUC8_dubnum)

#remove the unnecessary columns
occs.WEST <- subset(occs.WEST, select = -c(HUC8, HUC8_dubnum))

bg.WEST <- subset(bg.WEST, select = -c(HUC8, HUC8_dubnum))

#set our model settings
tune.args = list(fc = c("L","LQ","LQH", "H"), rm = 1:5)

#run ENMeval model

BFSwest <- ENMevaluate(occs.WEST, bg = bg.WEST, algorithm = "maxent.jar", tune.args = tune.args, 
                       partitions = "user", user.grp = huc8.WEST, categoricals = c("Divergence", "Confinement"))


#Save the ENMeval file to pull later
save(BFSwest, file = "BFSwestENMresults")

#Model delta AIC values less than 2 are the best ones. We can select all models with delta AIC > 2 and plot it. 
res.WEST <- eval.results(BFSwest)
res.WEST
write.table(res.WEST, file = "BFSwest_models.csv", sep = ",", quote = FALSE, row.names = F)

bestmodwest <- which(BFSwest@results$AICc==min(BFSwest@results$AICc))
BFSwest@models[bestmodwest]
# the top model is "fc.LQH_rm.3"

# The "betas" slot in a maxnet model is a named vector of the variable 
# coefficients and what kind they are (in R formula notation).
# Note that the html file that is created when maxent.jar is run is **not** kept.
BFSwest_topmod <- eval.models(BFSwest)[["fc.LQH_rm.3"]]
# the enframe function from the tibble package makes this named vector into a 
# more readable table.
install.packages("tibble")
library(tibble)
BFSwestBetas <- enframe(BFSwest_topmod@results)
write.table(BFSwestBetas, file = "BFSwest_topmodel_betas.csv", sep = ",", quote = FALSE, row.names = F)


#Second, we focus on the best-fit model
#SO, we can run this best model in dismo package, using all their handy (and less buggy) helper functions
#We want the following:  1)variable importance, 2) the variable response curves
#Then, 3)we want to save the best-fit model's cloglog predictions into a dataframe or csv so we can map predictions

library(dismo)
#tutorial for installing rJava on windows 10: https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
install.packages("rJava")
#Paste Maxent.jar (as downloaded online) into the java folder of the dismo package at this location & code checks to make sure it will run
Sys.setenv(JAVA_HOME="C:Program Files/Java/jre1.8.0_321/")
library(rJava)
system.file("java", package="dismo")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava))

#Creating argument list for Maxent in dismo, including RM and feature class specifics from top model in ENMeval
#Briant, use this list pasted below to match to our best model and the settings that ENMeval would otherwise default to
#https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ/m/Fj8Two0lmHIJ
#CHECK AND DOUBLE CHECK RMargs BEFORE RUNNING
#This is the best model fc.LQH_rm.3
RMargs <-c(paste0("betamultiplier=3"))
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
p <- as.data.frame(bfssdmwest$BFS_West)
env_W <- subset(bfssdmwest, select = -c(BFS_West, COMID, longitude, latitude, HUC8))

#this will come in handy when obtaining cloglog for the west in later steps
env_WCOMID <- subset(bfssdmwest, select = COMID)


#Running Maxent model with dismo with our arguments and increase the max number of background points to include all background
#Make sure categorical variables are treated as factors on these lines
#for nbg set it to above 200,000
BFSbestwest <- maxent (env_W, p, args = RMargs, nbg= 250000,
                                  path = "BFSbestwest", 
                                  silent = FALSE)
#Note when looking at response curves of very complex models, the marginal cURVES are most important
#Variable contributions and response curves
save(BFSbestwest, file = "BFSbestwestmaxent")
plot(BFSbestwest)
response(BFSbestwest)
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
predictWEST2WEST <- project(BFSbestwest, env_W)
predictWEST2WEST <-as.data.frame(predictWEST2WEST)

#Append Maxent model predictions to model input data
env_W$pred_raw <-predictWEST2WEST$prediction_raw
env_W$pred_log <- predictWEST2WEST$prediction_logistic
env_W$pred_cloglog <- predictWEST2WEST$prediction_cloglog
env_W <- cbind(env_W, env_WCOMID)
#Export the resulting projection with final model predictions back to appropriate model folder
write.csv(env_W, "predictWEST2WEST.csv")






bfssdmeast<- read.csv("BFSEastpopv8.csv", header = T, na.strings = "-9999")
bfssdmeast <- bfssdmeast[!duplicated(bfssdmeast),]
bfssdmeast <- bfssdmeast[complete.cases(bfssdmeast),]
bfssdmeast<- data.frame(select(bfssdmeast, "BFS_East", "COMID", "ElevCat", "ClayCat", "SandCat", "RckDepCat", "Divergence", "Slope", "Confinement", "TotDASqKM", "HUC8"))
bfssdmeast$longitude <- bfssdmeast$COMID
bfssdmeast$latitude <- bfssdmeast$COMID
bfssdmeast <- bfssdmeast %>% select(longitude, latitude, everything())
env_ECOMID <- subset(bfssdmeast, select = COMID)
env_forW2E <- subset(bfssdmeast, select = -c(BFS_East, COMID, longitude, latitude, HUC8))


####Projecting dismo model of WEST to EAST 
####need to bring in environment of the opposing region
####opposing region dataframe must match the columns of the dismo model build, and have no occs/bg designation
predictWEST2EAST <-project(BFSbestwest, env_forW2E)
predictWEST2EAST <-as.data.frame(predictWEST2EAST)

#Append Maxent model predictions to model input data
env_forW2E$pred_raw <- predictWEST2EAST$prediction_raw
env_forW2E$pred_log <- predictWEST2EAST$prediction_logistic
env_forW2E$pred_cloglog <- predictWEST2EAST$prediction_cloglog
env_forW2E <- cbind(env_forW2E, env_ECOMID)
#Export the resulting projection with final model predictions back to appropriate model folder
write.csv(env_forW2E, "predictWEST2EAST.csv")

####calculate MESS (multivariate environmental similarity surfaces between two regions)
####See function and example in link below and use full EAST (e.g., bg and occs) vs full WEST 
####https://rdrr.io/cran/modEvA/man/MESS.html



#####THEN, REPEAT for the other side of the distribution!!!   :)
