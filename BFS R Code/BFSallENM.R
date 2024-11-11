setwd("C:/Users/ponyf/Desktop/Maxent in R/Final")
install.packages("ENMeval")
install.packages("dplyr")



library(ENMeval)
library(dplyr)

############################WEST ANALYSIS####################################
bfssdmall<- read.csv("BFSAllpopv11.csv", header = T, na.strings = "-9999")



bfssdmall<- data.frame(select(bfssdmall, "BFS_All", "COMID", "ElevCat", "ClayCat", "SandCat", "RckDepCat", "Divergence", "Slope", "Confinement", "TotDASqKM", "HUC8"))
bfssdmall <- bfssdmall[!duplicated(bfssdmall),]
bfssdmall <- bfssdmall[complete.cases(bfssdmall),]

Envdatasummary <- summary(bfssdmall)
Envdatasummary
write.csv(Envdatasummary, "bfssdmallenvdatasummary.csv")


bfssdmall$longitude <- bfssdmall$COMID
bfssdmall$latitude <- bfssdmall$COMID
bfssdmall <- bfssdmall %>% select(longitude, latitude, everything())


occs.ALL<- subset(bfssdmall, BFS_All == "1")
occs.ALL <- subset(occs.ALL, select = -c(BFS_All, COMID))


bg.ALL <- subset(bfssdmall, BFS_All == "0")
bg.ALL <- subset(bg.ALL, select = -c(BFS_All, COMID))

occs.ALL <- subset(occs.ALL, subset = HUC8 %in% c(3180002, 3180003, 6040005, 8010201, 8010202, 8010203, 8010204, 8010205, 8010207, 8010208, 8010209, 8010210,
                                                  8030201, 8030202, 8030203, 8030204, 8030205, 8030206, 8060100, 8060201, 8060202, 8060203, 8060204, 8060205, 
                                                  8060206, 8070201, 11060005, 11070101, 11070102, 11070103, 11070104, 11070106, 11070203, 11070207, 11070208, 
                                                  11070209, 11110102, 11110103, 11030013, 11060004, 11060006, 11070107, 11070201, 11070202, 11070204, 11070205,
                                                  11070206, 11110202))

bg.ALL <- subset(bg.ALL, subset = HUC8 %in% c(3180001, 3180002, 3180003, 3180004, 3180005, 6010101, 6010102, 6010103, 6010104, 6010105, 6010106, 6010107, 6010108, 
                                              6010201, 6010202, 6010203, 6010204, 6010205, 6010206, 6010207, 6010208, 6020001, 6020002, 6020003, 6020004, 6030001, 
                                              6030002, 6030003, 6030004, 6030005, 6030006, 6040001, 6040002, 6040003, 6040004, 6040005, 6040006, 8010100, 8010201,
                                              8010202, 8010203, 8010204, 8010205, 8010206, 8010207, 8010208, 8010209, 8010210, 8010211, 8020100, 8020201, 8020202, 
                                              8020203, 8020204, 8020205, 8020301, 8020302, 8020303, 8020304, 8020401, 8020402, 8030100, 8030201, 8030202, 8030203, 
                                              8030204, 8030205, 8030206, 8030207, 8030208, 8030209, 8040101, 8040102, 8040103, 8040201, 8040202, 8040203, 8040204, 
                                              8040205, 8040206, 8040207, 8040301, 8040302, 8040303, 8040304, 8040305, 8040306, 8050001, 8050002, 8050003, 8060100,
                                              8060201, 8060202, 8060203, 8060204, 8060205, 8060206, 8070100, 8070201, 8070202, 8070203, 8070204, 8070205, 8070300,
                                              8080101, 8080102, 8080103, 8080201, 8080202, 8080203, 8080204, 8080205, 8080206, 8090100, 8090201, 8090202, 8090203, 
                                              8090301, 8090302, 22163556, 11060005, 11070101, 11070102, 11070103, 11070104, 11070106, 11070203, 11070207, 11070208, 11070209, 11110102, 11110103, 11010001, 
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

occs.ALL["HUC8"][occs.ALL["HUC8"] == "3180002"] <- "1"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "3180003"] <- "2"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "6040005"] <- "3"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010201"] <- "4"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010202"] <- "5"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010203"] <- "6"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010204"] <- "7"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010205"] <- "8"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010207"] <- "9"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010208"] <- "10"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010209"] <- "11"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8010210"] <- "12"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030201"] <- "13"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030202"] <- "14"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030203"] <- "15"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030204"] <- "16"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030205"] <- "17"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8030206"] <- "18"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060100"] <- "19"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060201"] <- "20"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060202"] <- "21"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060203"] <- "22"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060204"] <- "23"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060205"] <- "24"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8060206"] <- "25"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "8070201"] <- "26"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11060005"] <- "27"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070101"] <- "28"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070102"] <- "29"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070103"] <- "30"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070104"] <- "31"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070106"] <- "32"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070203"] <- "33"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070207"] <- "34"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070208"] <- "35"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070209"] <- "36"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11110102"] <- "37"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11110103"] <- "38"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11030013"] <- "39"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11060004"] <- "40"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11060006"] <- "41"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070107"] <- "42"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070201"] <- "43"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070202"] <- "44"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070204"] <- "45"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070205"] <- "46"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11070206"] <- "47"
occs.ALL["HUC8"][occs.ALL["HUC8"] == "11110202"] <- "48"


bg.ALL["HUC8"][bg.ALL["HUC8"] == "3180001"] <- "1"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "3180002"] <- "2"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "3180003"] <- "3"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "3180004"] <- "4"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "3180005"] <- "5"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010101"] <- "6"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010102"] <- "7"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010103"] <- "8"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010104"] <- "9"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010105"] <- "10"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010106"] <- "11"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010107"] <- "12"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010108"] <- "13"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010201"] <- "14"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010202"] <- "15"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010203"] <- "16"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010204"] <- "17"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010205"] <- "18"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010206"] <- "19"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010207"] <- "20"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6010208"] <- "21"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6020001"] <- "22"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6020002"] <- "23"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6020003"] <- "24"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6020004"] <- "25"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030001"] <- "26"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030002"] <- "27"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030003"] <- "28"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030004"] <- "29"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030005"] <- "30"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6030006"] <- "31"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040001"] <- "32"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040002"] <- "33"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040003"] <- "34"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040004"] <- "35"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040005"] <- "36"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "6040006"] <- "37"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010100"] <- "38"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010201"] <- "39"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010202"] <- "40"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010203"] <- "41"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010204"] <- "42"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010205"] <- "43"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010206"] <- "44"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010207"] <- "45"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010208"] <- "46"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010209"] <- "47"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010210"] <- "48"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8010211"] <- "49"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020100"] <- "50"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020201"] <- "51"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020202"] <- "52"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020203"] <- "53"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020204"] <- "54"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020205"] <- "55"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020301"] <- "56"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020302"] <- "57"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020303"] <- "58"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020304"] <- "59"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020401"] <- "60"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8020402"] <- "61"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030100"] <- "62"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030201"] <- "63"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030202"] <- "64"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030203"] <- "65"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030204"] <- "66"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030205"] <- "67"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030206"] <- "68"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030207"] <- "69"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030208"] <- "70"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8030209"] <- "71"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040101"] <- "72"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040102"] <- "73"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040103"] <- "74"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040201"] <- "75"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040202"] <- "76"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040203"] <- "77"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040204"] <- "78"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040205"] <- "79"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040206"] <- "80"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040207"] <- "81"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040301"] <- "82"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040302"] <- "83"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040303"] <- "84"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040304"] <- "85"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040305"] <- "86"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8040306"] <- "87"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8050001"] <- "88"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8050002"] <- "89"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8050003"] <- "90"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060100"] <- "91"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060201"] <- "92"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060202"] <- "93"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060203"] <- "94"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060204"] <- "95"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060205"] <- "96"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8060206"] <- "97"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070100"] <- "98"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070201"] <- "99"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070202"] <- "100"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070203"] <- "101"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070204"] <- "102"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070205"] <- "103"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8070300"] <- "104"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080101"] <- "105"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080102"] <- "106"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080103"] <- "107"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080201"] <- "108"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080202"] <- "109"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080203"] <- "110"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080204"] <- "111"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080205"] <- "112"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8080206"] <- "113"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090100"] <- "114"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090201"] <- "115"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090202"] <- "116"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090203"] <- "117"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090301"] <- "118"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "8090302"] <- "119"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "22163556"] <- "120"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060005"] <- "121"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070101"] <- "122"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070102"] <- "123"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070103"] <- "124"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070104"] <- "125"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070106"] <- "126"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070203"] <- "127"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070207"] <- "128"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070208"] <- "129"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070209"] <- "130"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110102"] <- "131"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110103"] <- "132"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010001"] <- "133"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010002"] <- "134"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010003"] <- "135"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010004"] <- "136"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010005"] <- "137"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010006"] <- "138"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010007"] <- "139"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010008"] <- "140"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010009"] <- "141"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010010"] <- "142"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010011"] <- "143"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010012"] <- "144"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010013"] <- "145"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11010014"] <- "146"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020001"] <- "147"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020002"] <- "148"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020003"] <- "149"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020004"] <- "150"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020005"] <- "151"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020006"] <- "152"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020007"] <- "153"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020008"] <- "154"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020009"] <- "155"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020010"] <- "156"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020011"] <- "157"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020012"] <- "158"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11020013"] <- "159"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030001"] <- "160"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030002"] <- "161"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030003"] <- "162"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030004"] <- "163"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030005"] <- "164"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030006"] <- "165"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030007"] <- "166"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030008"] <- "167"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030009"] <- "168"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030010"] <- "169"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030011"] <- "170"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030012"] <- "171"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030014"] <- "172"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030015"] <- "173"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030016"] <- "174"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030017"] <- "175"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030018"] <- "176"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040001"] <- "177"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040002"] <- "178"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040003"] <- "179"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040004"] <- "180"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040005"] <- "181"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040006"] <- "182"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040007"] <- "183"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11040008"] <- "184"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11050001"] <- "185"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11050002"] <- "186"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11050003"] <- "187"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060001"] <- "188"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060002"] <- "189"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060003"] <- "190"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070105"] <- "191"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070203"] <- "192"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080001"] <- "193"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080002"] <- "194"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080003"] <- "195"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080004"] <- "196"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080005"] <- "197"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080006"] <- "198"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080007"] <- "199"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11080008"] <- "200"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090101"] <- "201"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090102"] <- "202"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090103"] <- "203"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090104"] <- "204"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090105"] <- "205"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090106"] <- "206"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090201"] <- "207"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090202"] <- "208"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090203"] <- "209"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11090204"] <- "210"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100101"] <- "211"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100102"] <- "212"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100103"] <- "213"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100104"] <- "214"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100201"] <- "215"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100202"] <- "216"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100203"] <- "217"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100301"] <- "218"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100302"] <- "219"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11100303"] <- "220"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110104"] <- "221"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110105"] <- "222"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110201"] <- "223"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110203"] <- "224"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110204"] <- "225"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110205"] <- "226"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110206"] <- "227"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110207"] <- "228"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120101"] <- "229"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120102"] <- "230"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120103"] <- "231"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070203"] <- "232"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120104"] <- "233"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120105"] <- "234"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120201"] <- "235"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120202"] <- "236"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120301"] <- "237"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120302"] <- "238"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120303"] <- "239"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11120304"] <- "240"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130101"] <- "241"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130102"] <- "242"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130103"] <- "243"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130104"] <- "244"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130105"] <- "245"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130201"] <- "246"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130202"] <- "247"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130203"] <- "248"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130204"] <- "249"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130205"] <- "250"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130206"] <- "251"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130207"] <- "252"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130208"] <- "253"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130209"] <- "254"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130210"] <- "255"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130301"] <- "256"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130302"] <- "257"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130303"] <- "258"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11130304"] <- "259"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140101"] <- "260"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140102"] <- "261"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140103"] <- "262"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140104"] <- "263"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140105"] <- "264"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140106"] <- "265"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140107"] <- "266"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140108"] <- "267"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140109"] <- "268"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140201"] <- "269"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140202"] <- "270"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140203"] <- "271"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140204"] <- "272"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140205"] <- "273"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140206"] <- "274"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140207"] <- "275"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140208"] <- "276"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140209"] <- "277"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140301"] <- "278"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140302"] <- "279"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140303"] <- "280"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140304"] <- "281"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140305"] <- "282"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140306"] <- "283"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11140307"] <- "284"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11030013"] <- "285"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060004"] <- "286"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11060006"] <- "287"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070107"] <- "288"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070201"] <- "289"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070202"] <- "290"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070204"] <- "291"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070205"] <- "292"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11070206"] <- "293"
bg.ALL["HUC8"][bg.ALL["HUC8"] == "11110202"] <- "294"

occs.ALL$HUC8_dubnum <- as.double(occs.ALL$HUC8)
bg.ALL$HUC8_dubnum <- as.double(bg.ALL$HUC8)


huc8.ALL <- list(occs.grp = occs.ALL$HUC8_dubnum,
                 bg.grp = bg.ALL$HUC8_dubnum)

occs.ALL <- subset(occs.ALL, select = -c(HUC8, HUC8_dubnum))

bg.ALL <- subset(bg.ALL, select = -c(HUC8, HUC8_dubnum))


tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5)

BFSALL <- ENMevaluate(occs.ALL, bg = bg.ALL, algorithm = "maxent.jar", tune.args = tune.args,
                       partitions = "user", user.grp = huc8.ALL, categoricals = c("Divergence", "Confinement"))


save(BFSALL, file = "BFSallENMresults")

#Model delta AIC values less than 2 are the best ones. We can select all models with delta AIC > 2 and plot it. 
res.ALL <- eval.results(BFSALL)
res.ALL
write.table(res.ALL, file = "BFSall_models.csv", sep = ",", quote = FALSE, row.names = F)

bestmodall <- which(BFSALL@results$AICc==min(BFSALL@results$AICc))
BFSALL@models[bestmodall]
# the top model is "fc.LQH_rm.4"

# The "betas" slot in a maxnet model is a named vector of the variable 
# coefficients and what kind they are (in R formula notation).
# Note that the html file that is created when maxent.jar is run is **not** kept.
BFSall_topmod <- eval.models(BFSALL)[["fc.H_rm.4"]]
# the enframe function from the tibble package makes this named vector into a 
# more readable table.
install.packages("tibble")
library(tibble)
BFSallBetas <- enframe(BFSall_topmod@results)
write.table(BFSallBetas, file = "BFSall_topmodel_betas.csv", sep = ",", quote = FALSE, row.names = F)


#Second, we focus on the best-fit model
#SO, we can run this best model in dismo package, using all their handy (and less buggy) helper functions
#We want the following:  1)variable importance, 2) the variable response curves
#Then, 3)we want to save the best-fit model's cloglog predictions into a dataframe or csv so we can map predictions


library(dismo)
#tutorial for installing rJava on windows 10: https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
install.packages("rJava")
#Paste Maxent.jar (as downloaded online) into the java folder of the dismo package at this location & code checks to make sure it will run
options(java.parameters = "-Xmx1g" )
Sys.setenv(JAVA_HOME="C:Program Files/Java/jre1.8.0_321/")
library(rJava)
system.file("java", package="dismo")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava))
  
#Creating argument list for Maxent in dismo, including RM and feature class specifics from top model in ENMeval
#Briant, use this list pasted below to match to our best model and the settings that ENMeval would otherwise default to
#https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ/m/Fj8Two0lmHIJ
#CHECK AND DOUBLE CHECK RMargs BEFORE RUNNING
#This is the best model fc.LQH_rm.4
RMargs <-c(paste0("betamultiplier=4"))
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
p <- as.data.frame(bfssdmall$BFS_All)
env_A <- subset(bfssdmall, select = -c(BFS_All, COMID, longitude, latitude, HUC8))

#this will come in handy when obtaining cloglog for the west in later steps
env_ACOMID <- subset(bfssdmall, select = COMID)


#Running Maxent model with dismo with our arguments and increase the max number of background points to include all background
#Make sure categorical variables are treated as factors on these lines
#for nbg set it to above 200,000
BFSbestall <- maxent (env_A, p, args = RMargs, nbg= 250000,
                       path = "BestAll", 
                       silent = FALSE)
#Note when looking at response curves of very complex models, the marginal cURVES are most important
#Variable contributions and response curves
save(BFSbestall, file = "BFSbestallmaxent")
plot(BFSbestall)
response(BFSbestall)
#Can look at specific response individually
#partial response (avg prediction for all data points used to fit the model)
#response(BFSbestall, var="Q0001C_Yr")
#response(BFSbestall, var="Q0001C_Yr", at=mean)


####Projecting model back onto input data to save predictions to segments
###https://rdrr.io/github/johnbaums/rmaxent/f/README.md
#####################################

install.packages("devtools")
library(devtools)
install_github('johnbaums/rmaxent')
library(rmaxent)


#Obtain cloglog for east
predictall <- project(BFSbestall, env_A)
predictall <-as.data.frame(predictall)

#Append Maxent model predictions to model input data
env_A$pred_raw <-predictall$prediction_raw
env_A$pred_log <- predictall$prediction_logistic
env_A$pred_cloglog <- predictall$prediction_cloglog
env_A <- cbind(env_A, env_ACOMID)
#Export the resulting projection with final model predictions back to appropriate model folder
write.csv(env_A, "predictall.csv")


