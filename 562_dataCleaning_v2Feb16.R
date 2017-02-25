# Clean NA

setwd("E:/downloadsJan132017/")

library(readxl)
library(foreign)
library(data.table)
library(dplyr)
library(DMwR)
library(scrime)
library(rpart)

datao = read_excel('NY property data.xlsx')


# 1.  Read Data--------------------------------------



# note

# 1. BBLE represent the location of property
# Borough, block and lot is a unique parcel identifier. The  following are the valid codes for each of the five boroughs:
# 1 = Manhattan
# 2 = Bronx
# 3 = Brooklyn
# 4 = Queens
# 5 = Staten Island
# The block is a subset of a borough. The lot is a subset of a  block unique within a borough and block.

# 2. EASEMENT : for class E there's a lot 0 value.

# An easement is the right to use land owned by another person.  The following easement codes are used in New York City:
# A     = indicates the portion of the lot that has an air easement
# B     = indicates non-air rights
# E     =	indicates the portion of the lot that has a land easement
# F-M = are duplicates of the E easement
# N     = non-transit
# P     = piers
# R     = railroads
# S     = streets
# U     = U.S. Government

# 3. OWNer : delete

# 4. BLDGCL : Building class : Keep
# 5. TAXCLASS : keep
# 6. LF / LD : keep
# 7 Stories : number of stories in building Level
# 8. Value : keep
# 9. EXCD1/2 : Code keep as char
# 10. EXMPTCL : exempt class,Keep 

# 11. STADDR : delete
# 12. Period/year/Valtype : delet


data = datao[,-c(4,6,18,28,29,30)]
names(data)
class(datao[,10])


# 2. Change 0 to Missing Value --------------------------------------------


data =as.data.frame(data)

data[,c(7:14,18:23)] = apply(data[,c(7:14,18:23)],2,as.numeric)

Change0toNA = function(s){
  s[which(s == 0 | s == "0")] = NA
  s
}

data[,c(7:14,18:23)] = apply(data[,c(7:14,18:23)],2,Change0toNA)


findNA = function(x) {
  sum(is.na(x))
}

data = data %>% mutate(Area = paste(substring(BBLE,1,1),BLOCK,spe = ""))

head(data$Area)

names(data)

data = data[,-c(2,3)]

apply(data,2,findNA)

backup_beforeChange = data

# RECORD     BBLE    BLOCK EASEMENT   BLDGCL TAXCLASS  LTFRONT  LTDEPTH  STORIES  FULLVAL   AVLAND    AVTOT   EXLAND 
# 0           0        0   1044532        0        0        0        0    52142        0        0        0        0 
# EXTOT    EXCD1      ZIP  EXMPTCL    BLDFRONT BLDDEPTH  AVLAND2   AVTOT2  EXLAND2   EXTOT2    EXCD2 
# 0        425933    26356  1033583        0        0   767609   767603   961900   918642   957634 



# 3. Change Missing value ---------------------------------------

# 3.1 to median & rf

names(data)

ChangetoMedian = function(s){
  s[is.na(s)] = median(s,na.rm = T)
  s
}

data$EASEMENT[is.na(data$EASEMENT)] = "NoValue"
data$EXMPTCL[is.na(data$EXMPTCL)] = "NoValue"
data[,c(5:12,16:21)] = apply(data[,c(5:12,16:21)],2,ChangetoMedian)

# apply(data,2,findNA)

write.dta(data,"BeforeRF.dta")

# library(readstata13)
# 
# setwd("E:/downloadsJan132017/")
# 
# data = read.dta13("BeforeRF.dta")
# # 
# library(DMwR)
# #
# a = as.data.frame(data[,c(5:14,16:21)])
# 
# a$EXCD1 = as.factor(a$EXCD1)
# a$ZIP = as.factor(a$ZIP)
# #
# #
# a$ZIP[which(a$ZIP == "NA")] = NA
# a$EXCD1[which(a$EXCD1 == "NA")] = NA
# #
# sum(is.na(a$ZIP))
# sum(is.na(a$EXCD1))
# 
# knnOutput <- knnImputation(a)  # perform knn imputation.
# anyNA(knnOutput)
# 
# sum(is.na(data$EXCD1))
# 
# names(data)
# 
# class_mod = rpart(EXCD1 ~ .-ZIP, data=data[,c(5:14,16:21)], method="class", na.action=na.omit)
# 
# excd1_pred <- predict(class_mod, data=data[,c(5:14,16:21)])
# 
# library(mice)
# 
# data$EXCD1 = as.factor(data$EXCD1)
# miceMod <- mice(data[,c(5:12,14,16:21)], method="rf")  # perform mice imputation, based on random forests.
# 
# a = as.data.frame(data[,c(5:13,16:21)])
# 
# a$EXCD1 = as.factor(a$EXCD1)
# a$ZIP = as.factor(a$ZIP)

# 
# a$ZIP[which(a$ZIP == "NA")] = NA
# a$EXCD1[which(a$EXCD1 == "NA")] = NA
# 
# sum(is.na(a$ZIP))
# sum(is.na(a$EXCD1))
# 
# library(data.table)
# 
# a= as.data.table(a)
# 
# miceMod <- mice(a, method="rf")
# # 
# 
# class(a$ZIP)
# 
# ?mice
# 
# miceOutput <- complete(miceMod)  # generate the completed data.
# anyNA(miceOutput)
# 
# miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf")
# 
# ?rpart

data$EXCD1[is.na(data$EXCD1)] = "NoValue"
data$ZIP[is.na(data$ZIP)] = "NoValue"




# knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])  # perform knn imputation.
# anyNA(knnOutput)

# randomforest

# library(DMwR)
# knnOutput <- knnImputation(data[,c(5:14,16:21)])  # perform knn imputation.
# anyNA(knnOutput)
# 
# str(unique(data$ZIP))
# str(unique(data$EXCD1))
# 
# data2 = as.matrix(data)
# try <- knncatimputeLarge(data2[,c(5:14,16:21)])
# 
# 
# library(mice)
# names(data)
# miceMod <- mice(data[,c(5:12,14,16:21)], method="rf")  # perform mice imputation, based on random forests.
# miceOutput <- complete(miceMod)  # generate the completed data.
# anyNA(miceOutput)
# 
# 
# 
# apply(data,2,findNA)



# data = data %>% mutate(LotVol = LTFRONT * LTDEPTH, BuLVol = BLDFRONT * BLDDEPTH)

# x = function(s){as.numerics}
# 
# apply(c(data$LTDEPTH),2,x)

# Group and new variables

data_EASEMENT=data %>%  group_by(EASEMENT) %>%  
  summarise(EASEMENTmeanFULLVAL=mean(FULLVAL),
            EASEMENTmeanAVLAND=mean(AVLAND),
            EASEMENTmeanAVTOT=mean(AVTOT),
            EASEMENTmeanEXLAND=mean(EXLAND),
            EASEMENTmeanEXTOT=mean(EXTOT),
            EASEMENTmeanAVLAND2=mean(AVLAND2),
            EASEMENTmeanAVTOT2=mean(AVTOT2),
            EASEMENTmeanEXLAND2=mean(EXLAND2),
            EASEMENTmeanEXTOT2=mean(EXTOT2))

data_TAXCLASS=data %>%  group_by(TAXCLASS) %>%  
  summarise(TAXCLASSmeanFULLVAL=mean(FULLVAL),
            TAXCLASSmeanAVLAND=mean(AVLAND),
            TAXCLASSmeanAVTOT=mean(AVTOT),
            TAXCLASSmeanEXLAND=mean(EXLAND),
            TAXCLASSmeanEXTOT=mean(EXTOT),
            TAXCLASSmeanAVLAND2=mean(AVLAND2),
            TAXCLASSmeanAVTOT2=mean(AVTOT2),
            TAXCLASSmeanEXLAND2=mean(EXLAND2),
            TAXCLASSmeanEXTOT2=mean(EXTOT2),
            TAXCLASSmeanAVtoFULL=mean(AVTOT/FULLVAL))

data_STORIES=data %>%  group_by(STORIES) %>%  
  summarise(STORIESmeanFULLVAL=mean(FULLVAL),
            STORIESmeanAVLAND=mean(AVLAND),
            STORIESmeanAVTOT=mean(AVTOT),
            STORIESmeanEXLAND=mean(EXLAND),
            STORIESmeanEXTOT=mean(EXTOT),
            STORIESmeanAVLAND2=mean(AVLAND2),
            STORIESmeanAVTOT2=mean(AVTOT2),
            STORIESmeanEXLAND2=mean(EXLAND2),
            STORIESmeanEXTOT2=mean(EXTOT2),
            STORIESmeanAVtoFULL=mean(AVTOT/FULLVAL))

data_EXCD1=data %>%  group_by(EXCD1) %>%  
  summarise(EXCD1meanFULLVAL=mean(FULLVAL),
            EXCD1meanAVLAND=mean(AVLAND),
            EXCD1meanAVTOT=mean(AVTOT),
            EXCD1meanEXLAND=mean(EXLAND),
            EXCD1meanEXTOT=mean(EXTOT),
            EXCD1meanAVLAND2=mean(AVLAND2),
            EXCD1meanAVTOT2=mean(AVTOT2),
            EXCD1meanEXLAND2=mean(EXLAND2),
            EXCD1meanEXTOT2=mean(EXTOT2))

data_EXMPTCL=data %>%  group_by(EXMPTCL) %>%  
  summarise(EXMPTCLmeanFULLVAL=mean(FULLVAL),
            EXMPTCLmeanAVLAND=mean(AVLAND),
            EXMPTCLmeanAVTOT=mean(AVTOT),
            EXMPTCLmeanEXLAND=mean(EXLAND),
            EXMPTCLmeanEXTOT=mean(EXTOT),
            EXMPTCLmeanAVLAND2=mean(AVLAND2),
            EXMPTCLmeanAVTOT2=mean(AVTOT2),
            EXMPTCLmeanEXLAND2=mean(EXLAND2),
            EXMPTCLmeanEXTOT2=mean(EXTOT2))

data_EXCD2=data %>%  group_by(EXCD2) %>%  
  summarise(EXCD2meanFULLVAL=mean(FULLVAL),
            EXCD2meanAVLAND=mean(AVLAND),
            EXCD2meanAVTOT=mean(AVTOT),
            EXCD2meanEXLAND=mean(EXLAND),
            EXCD2meanEXTOT=mean(EXTOT),
            EXCD2meanAVLAND2=mean(AVLAND2),
            EXCD2meanAVTOT2=mean(AVTOT2),
            EXCD2meanEXLAND2=mean(EXLAND2),
            EXCD2meanEXTOT2=mean(EXTOT2))


data_Area=data %>%  group_by(Area) %>%  
  summarise(AREAmeanFULLVAL=mean(FULLVAL),
            AREAmeanAVLAND=mean(AVLAND),
            AREAmeanAVTOT=mean(AVTOT),
            AREAmeanEXLAND=mean(EXLAND),
            AREAmeanEXTOT=mean(EXTOT),
            AREAmeanAVLAND2=mean(AVLAND2),
            AREAmeanAVTOT2=mean(AVTOT2),
            AREAmeanEXLAND2=mean(EXLAND2),
            AREAmeanEXTOT2=mean(EXTOT2),
            AREAmeanAVtoFULL=mean(AVTOT/FULLVAL))

data_BLDGCL=data %>%  group_by(BLDGCL) %>%  
  summarise(BLDGCLmeanFULLVAL=mean(FULLVAL),
            BLDGCLmeanAVLAND=mean(AVLAND),
            BLDGCLmeanAVTOT=mean(AVTOT),
            BLDGCLmeanEXLAND=mean(EXLAND),
            BLDGCLmeanEXTOT=mean(EXTOT),
            BLDGCLmeanAVLAND2=mean(AVLAND2),
            BLDGCLmeanAVTOT2=mean(AVTOT2),
            BLDGCLmeanEXLAND2=mean(EXLAND2),
            BLDGCLmeanEXTOT2=mean(EXTOT2),
            BLDGCLmeanAVtoFULL=mean(AVTOT/FULLVAL))


dataMerge = merge(x = data, y = data_EASEMENT, by = "EASEMENT",all.x = T)
dataMerge = merge(x = dataMerge, y = data_TAXCLASS, by = "TAXCLASS",all.x = T)
dataMerge = merge(x = dataMerge, y = data_STORIES, by = "STORIES",all.x = T)
dataMerge = merge(x = dataMerge, y = data_EXCD1, by = "EXCD1",all.x = T)
dataMerge = merge(x = dataMerge, y = data_EXMPTCL, by = "EXMPTCL",all.x = T)
dataMerge = merge(x = dataMerge, y = data_EXCD2, by = "EXCD2",all.x = T)
dataMerge = merge(x = dataMerge, y = data_Area, by = "Area",all.x = T)
dataMerge = merge(x = dataMerge, y = data_BLDGCL, by = "BLDGCL",all.x = T)

dataMerge = dataMerge %>%
  mutate(AVtoFULL = AVTOT/FULLVAL, BlV = BLDFRONT*BLDDEPTH , LOTV = LTFRONT*LTDEPTH)

head(dataMerge)

apply(dataMerge,2,findNA)

write.dta(dataMerge,"Cleaned_data_V4.dta")
getwd()

setwd("E:/downloadsJan132017/")

# find NA

a = read.dta13("Cleaned_data_V3.dta")

names(a)

selection = c(628469,
              110590,
              435044,
              392193,
              269656,
              808580,
              157201,
              1024619,
              722995,
              108523)

b = subset(a, RECORD %in% selection)

write.csv(b,"finalSelection.csv")
getwd()

apply(a,2,findNA)
