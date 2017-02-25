library(foreign)
library(biotools)
library(readstata13)
library(data.table)
library(ggplot2)
library(dtplyr)
library(bit64)
library(dplyr)
library(h2o)
library(stats)
library(biotools)

setwd("C:/Users/peipe/Desktop/562/Project1")
ny = fread('NY_property_data.csv', header = T, sep = ',')

ny=read.dta13("Cleaned_data_V4.dta")

EXMPTCL,EXCD2,EXCD1

names(ny)
###Adding new  variables: Amongst which taking AVLAND/EXLAND, AVTOT/EXTOT, FULLVAL/BuildVolume, AVLAND/LotArea as 4 expert variables

ny<-ny%>%
  mutate(LotArea=LTFRONT*LTDEPTH)%>%
  mutate(BuildVolume=BLDFRONT*BLDDEPTH*STORIES)%>%
  mutate(AVLAND/EXLAND)%>%
  mutate(AVTOT/EXTOT)%>%
  mutate(FULLVAL/BuildVolume)%>%
  mutate(AVLAND/LotArea)

#names(ny)
####data group By zip
data_ZIP<-ny%>%
  group_by(ZIP)%>%
  summarise(ZIPmeanFULLVAL=mean(FULLVAL),
            ZIPmeanAVLAND=mean(AVLAND),
            ZIPmeanAVTOT=mean(AVTOT),
            ZIPmeanEXLAND=mean(EXLAND),
            ZIPmeanEXTOT=mean(EXTOT),
            ZIPmeanAVLANDtoEXLAND=mean(`AVLAND/EXLAND`),
            ZIPmeanAVTOTtoEXTOT=mean(`AVTOT/EXTOT`),
            ZIPmeanFVtoBLDVOL=mean(`FULLVAL/BuildVolume`),
            ZIPmeanAVtoFULL=mean(AVtoFULL),
            ZIPmeanAVLANDtoLotArea=mean(`AVLAND/LotArea`))

ny = merge(x = ny, y = data_ZIP, by = "ZIP",all.x = T)

###?????Double check EXMPTCL;group LANDtoTOT by EXMP
data_EXMP<-ny%>%
  group_by(EXMPTCL)%>%
  summarise(EXMPmeanAVLANDtoEXLAND=mean(`AVLAND/EXLAND`),
            EXMPmeanAVTOTtoEXTOT=mean(`AVTOT/EXTOT`))

ny=merge(x=ny,y=data_EXMP,by="EXMPTCL",all.x=T)
#head(ny,10)

###group AVLANDtoAVTOT,EXLANDtoEXTOT,AVtoFULL, FVtoBLDVOL by BLDC
data_BLDC<-ny%>%
  group_by(BLDGCL)%>%
  summarise(BLDCmeanAVLANDtoEXLAND=mean(`AVLAND/EXLAND`),
            BLDCmeanAVTOTtoEXTOT=mean(`AVTOT/EXTOT`),
            BLDCmeanFVtoBLDVOL=mean(`FULLVAL/BuildVolume`),
            BLDCmeanAVtoFULL=mean(AVtoFULL),
            BLDCmeanAVLANDtoLotArea=mean(`AVLAND/LotArea`),
            BLDCmeanSTORIES=mean(STORIES),
            BLDCmeanLotArea=mean(LotArea),
            BLDCmeanBLDVOL=mean(BuildVolume))
           
          
ny=merge(x=ny,y=data_BLDC,by="BLDGCL",all.x=T)

####group AVLANDtoAVTOT,EXLANDtoEXTOT,AVtoFULL, FVtoBLDVOL by TXC
data_TXC<-ny%>%
  group_by(TAXCLASS)%>%
  summarise(TXCmeanAVLANDtoEXLAND=mean(`AVLAND/EXLAND`),
            TXCmeanAVTOTtoEXTOT=mean(`AVTOT/EXTOT`),
            TXCmeanFVtoBLDVOL=mean(`FULLVAL/BuildVolume`),
            TXCmeanAVtoFULL=mean(AVtoFULL),
            TXCmeanAVLANDtoLotArea=mean(`AVLAND/LotArea`),
            TXCmeanSTORIES=mean(STORIES),
            TXCmeanAVLAND=mean(AVLAND),
            TXCmeanLotArea=mean(LotArea),
            TXCmeanBLDVOL=mean(BuildVolume))
ny=merge(x=ny,y=data_TXC,by="TAXCLASS",all.x=T)

###experiment:1000 samples
#ny_smp = sample_n(ny, 1000, replace = FALSE)
ny_smp = ny 
# check which columns have NA or "" or 0
# sapply(ny_smp, function(x) sum(is.na(x) | x=="" | x == 0))
# sapply(ny, function(x) sum( x == ""))
# sapply(ny, function(x) sum(is.na(x)))
# sapply(ny, function(x) sum( x == 0))

# fill NA with mean of the entire column
#ny_smp_xna = ny_smp0[,c("STORIES","EXCD1","ZIP","AVLAND2","AVTOT2","EXLAND2","EXCD2","EXTOT2")]
#ny_smp_xna[] <- lapply(ny_smp_xna, function(x) { x[is.na(x)] <- median(x, na.rm=TRUE); x })
#ny_smp = cbind(ny_smp0[,-c("STORIES","EXCD1","ZIP","AVLAND2","AVTOT2","EXLAND2","EXCD2","EXTOT2")],ny_smp_xna)

# data manipulation
# head(ny_smp)
# str(ny_smp)
# names(ny_smp)


myfxn <- function(var1,var2){var1/var2}
###creating 6 expert variables: based on grouping of fullval, avtot by different categorical variables
ny.TXC.AVTOT = data.frame(ny_smp[,c("AVTOT")])
ny.TXC.AVTOT1 = data.frame(sapply(ny.TXC.AVTOT ,myfxn, var2 = ny_smp$TAXCLASSmeanAVTOT))
colnames(ny.TXC.AVTOT1)<-c("AVTOT_TXC.AVTOT")
head(ny.TXC.AVTOT1)


ny.Area.AVTOT = data.frame(ny_smp[,c("AVTOT")])
ny.Area.AVTOT1 =data.frame(sapply(ny.Area.AVTOT ,myfxn, var2 = ny_smp$AREAmeanAVTOT))
colnames(ny.Area.AVTOT1)<-c("AVTOT_Area.AVTOT")
head(ny.Area.AVTOT1)

ny.BLDGCL.AVTOT=data.frame(ny_smp[,c("AVTOT")])
ny.BLDGCL.AVTOT1=data.frame(sapply(ny.BLDGCL.AVTOT,myfxn,var2=ny_smp$BLDGCLmeanAVTOT))
colnames(ny.BLDGCL.AVTOT1)<-c("AVTOT_BLD.AVTOT")
head(ny.BLDGCL.AVTOT1)


ny.TXC.FV = ny_smp[,c("AVLAND","AVTOT","FULLVAL")]
ny.TXC.FV1 =data.frame(sapply(ny.TXC.FV ,myfxn, var2 = ny_smp$TAXCLASSmeanFULLVAL))
colnames(ny.TXC.FV1)<-c("AVLAND_TXC.FV","AVTOT_TXC.FV","FULLVAL_TXC.FV")
head(ny.TXC.FV1)


###creating 6 expert variables:EXLAND,EXTOT respectively group by BLDC,EXMP,TAX
ny_smp1=ny_smp%>%
  mutate(EXLAND_BLDC=EXLAND/BLDGCLmeanEXLAND)%>%
  mutate(EXLAND_EXMP=EXLAND/EXMPTCLmeanEXLAND)%>%
  mutate(EXLAND_TAX=EXLAND/TAXCLASSmeanEXLAND)%>%
  mutate(EXTOT_BLDC=EXTOT/ZIPmeanEXTOT)%>%
  mutate(EXTOT_EXMP=EXLAND/EXMPTCLmeanEXLAND)%>%
  mutate(EXTOT_TAX=EXTOT/TAXCLASSmeanEXTOT)

names(ny_smp1)

ny_BLDC_EXM_TXC.EXLAND_EXTOT1=ny_smp1[,c(138:143)]  
names(ny_BLDC_EXM_TXC.EXLAND_EXTOT1)


###Creating 3 expert variables: (AVLAND/EXLAND) group by EXMP,TXC,BLD
ny.EXM_TXC_BLDC.LAND1=data.frame(lapply(data.frame(ny_smp[,c("AVLAND/EXLAND")]),myfxn,
                                   var2=ny_smp[,c("EXMPmeanAVLANDtoEXLAND",
                                                  "TXCmeanAVLANDtoEXLAND",
                                                 "BLDCmeanAVLANDtoEXLAND")]))
                                                 
colnames(ny.EXM_TXC_BLDC.LAND1)<-c("AVLANDtoEXLAND_EXMP","AVLANDtoEXLAND_TXC","AVLANDtoEXLAND_BLDC")

###Creating 3 expert variables: (AVTOT/EXTOTD) group by EXMP,TXC,BLD

ny.EXM_TXC_BLDC.TOT1=data.frame(lapply(data.frame(ny_smp[,c("AVTOT/EXTOT")]),myfxn,
                                var2=ny_smp[,c("EXMPmeanAVTOTtoEXTOT",
                                               "TXCmeanAVTOTtoEXTOT",
                                               "BLDCmeanAVTOTtoEXTOT")]))
                                          
colnames(ny.EXM_TXC_BLDC.TOT1)<-c("AVTOTtoEXTOT_EXMP","AVTOTtoEXTOT_TXC","AVTOTtoEXTOT_BLDC")


##creating 2 expert variables: AVtoFULL group by BLD, TXC,ZIP

ny.BLD_TXC_ZIP.AVtoFULL1=data.frame(lapply(data.frame(ny_smp[,c("AVtoFULL")]),myfxn,
                                var2=ny_smp[,c("BLDCmeanAVtoFULL",
                                               "TXCmeanAVtoFULL",
                                               "ZIPmeanAVtoFULL")]))
colnames(ny.BLD_TXC_ZIP.AVtoFULL1)<-c("AVtoFULL_BLD","AVtoFULL_TXC","AVtoFULL_ZIP")

###creating 3 expert variables: FVtoBLDVOL group by  ZIP,BLD,TXC
ny.AREA_ZIP_BLD_TXC.FVtoBLDVOL=data.frame(ny_smp[,c("FULLVAL/BuildVolume")])
ny.AREA_ZIP_BLD_TXC.FVtoBLDVOL1=data.frame(lapply(ny.AREA_ZIP_BLD_TXC.FVtoBLDVOL,myfxn,
                                    var2=ny_smp[,c("ZIPmeanFVtoBLDVOL",
                                                   "BLDCmeanFVtoBLDVOL",
                                                   "TXCmeanFVtoBLDVOL")]))
colnames(ny.AREA_ZIP_BLD_TXC.FVtoBLDVOL1)<-c("FVtoBLDVOL_ZIP","FVtoBLDVOL_BLD","FVtoBLDVOL_TXC")


###creating 3 expert variables: AVLAND/LotArea group by ZIP,BLD,TXC
ny.AREA_ZIP_BLD_TXC1=data.frame(lapply(data.frame(ny_smp[,c("AVLAND/LotArea")]),myfxn,
                          var2=ny_smp[,c("ZIPmeanAVLANDtoLotArea",
                                         "BLDCmeanAVLANDtoLotArea",
                                         "TXCmeanAVLANDtoLotArea")]))
colnames(ny.AREA_ZIP_BLD_TXC1)=c("AVLANDtoLotArea_ZIP",
                                 "AVLANDtoLotArea_BLD","AVLANDtoLotArea_TXC")


##creating 3 expert variables: LotArea group by BLDC, TXC

ny.BLD_TXC.LotArea1=data.frame(lapply(data.frame(ny_smp[,c("LotArea")]),myfxn,
                            var2=ny_smp[,c("BLDCmeanLotArea",
                                          "TXCmeanLotArea")]))
colnames(ny.BLD_TXC.LotArea1)=c("LotArea_BLDC","LotArea_TXC")

###creating 2 expert variables: BLDVOL groupby BLDC, TXC
ny.BLD_TXC.BLDVOL1=data.frame(lapply(data.frame(ny_smp[,c("BuildVolume")]),myfxn,
                            var2=ny_smp[,c("BLDCmeanBLDVOL",
                                           "TXCmeanBLDVOL")]))
colnames(ny.BLD_TXC.BLDVOL1)=c("BLDVOL_BLDC","BLDVOL_TXC")


####creating 2 expert variables: STORIES group by BLDC, TXC
ny.BLDC_TXC.STORIES1=data.frame(lapply(data.frame(ny_smp[,c("STORIES")]),myfxn,
                                var2=ny_smp[,c("BLDCmeanSTORIES",
                                               "TXCmeanSTORIES")]))
colnames(ny.BLDC_TXC.STORIES1)=c("STORIES_BLDC","STORIES_TXC")

###creating 4 expert variables: AVLAND group by ZIP, AREA,BLDC,TXC
ny.ZIP_AREA_BLDC_TXC.AVLAND1=data.frame(lapply(data.frame(ny_smp[,c("AVLAND")]),myfxn,
                                var2=ny_smp[,c("ZIPmeanAVLAND",
                                              "AREAmeanAVLAND",
                                               "BLDGCLmeanAVLAND",
                                               "TXCmeanAVLAND")]))
colnames(ny.ZIP_AREA_BLDC_TXC.AVLAND1)=c("AVLAND_ZIP","AVLAND_AREA","AVLAND_BLDC","AVLAND_TXC")



####Creating 1 expert variable: AssessDiffRatio
ny.AssessDiff=ny_smp %>%
  mutate(assess_ratio= ifelse(ny_smp$TAXCLASS %in% c("1","1A","1B","1C","1D"),0.06,0.45))%>%
  mutate(AssessDiffRatio=(AVTOT-FULLVAL*assess_ratio)/AVTOT)%>%
  select(AssessDiffRatio)

head(ny.AssessDiff)


#####Combine the expert variables into a new dataframe
names(ny_smp)
ny1_smp=ny_smp[,c(1:19,100,103:108)]

names(ny1_smp)
ny1_smp=cbind(ny1_smp,ny.TXC.AVTOT1,ny.Area.AVTOT1,ny.BLDGCL.AVTOT1,
              ny.TXC.FV1,ny_BLDC_EXM_TXC.EXLAND_EXTOT1,ny.EXM_TXC_BLDC.LAND1,
              ny.EXM_TXC_BLDC.TOT1,ny.BLD_TXC_ZIP.AVtoFULL1,ny.AREA_ZIP_BLD_TXC.FVtoBLDVOL1,
              ny.AREA_ZIP_BLD_TXC1,ny.BLD_TXC.LotArea1,ny.BLD_TXC.BLDVOL1,
              ny.BLDC_TXC.STORIES1,ny.ZIP_AREA_BLDC_TXC.AVLAND1,ny.AssessDiff)

names(ny1_smp)
####z scale:excluding categorical variables
ny2_smp=ny1_smp[,-c(1:10,34,37)]
names(ny2_smp)

Z_ny2_smp=data.frame(scale(ny2_smp))
head(Z_ny2_smp)
## check the scale result whether there is NA or not after zscale
# sapply(Z_ny2_smp, function(x) sum(is.na(x) | x==""))

# fraud score method 1
#  heuristic algorithm

###PCA
PCA1.Z_smp<-prcomp(Z_ny2_smp,center =TRUE,scale. = FALSE) 

summary(PCA1.Z_smp)

#####Optional:CalculatingEigenvalues of PC's
# ev <- data.frame(PCA1.Z_smp$sdev^2)
# View(ev)
####Plotting Percentage of Variance Explained by each PC????
#plot(PCA1.Z_smp, type="l")
####selecting the top 14 PCs, Variance explained by 80.6%
pca = PCA1.Z_smp$x[,c(1:14)]

### get the top 30 pca proportaion of variance from summary
PCA_var = summary(PCA1.Z_smp)$importance[2,1:30]
PCA_var = data.frame(cbind(seq(1,30,1),PCA_var))
colnames(PCA_var) = c("pcx","variance")

####Plotting Percentage of Variance Explained by each PC????
#plot(PCA1.Z_smp, type="l")

# plot PCA variance chart
ggplot(PCA_var, aes(x = as.factor(pcx), y = (variance) ))+
  geom_bar(stat='identity',color='black',fill='dodgerblue3') +
  geom_line(group = 1, col = "red",size = 0.6)+
  geom_point(col = "red",size=1) +
  ylab("Variance")+
  xlab("PCA factors")+
  ggtitle("PCA variance Chart")+
  theme_classic()

#plot chart to show relation between PCA and original x
rot = PCA1.Z_smp$rotation
pc1 = cbind(row.names(rot),rot[,1])
colnames(pc1) = c("org","pc")
pc1 = data.frame(pc1)
pc1$pc = as.numeric(as.character(pc1$pc))
# str(pc1)
pc1 %>%
  arrange(pc) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(org,pc), y = pc))+
  geom_bar(stat = "identity",fill = "dodgerblue3" , colour = "black",size = .2,bins = 30)+
  coord_flip() +
  xlab("PC1")+
  ylab("Original variables")+
  ggtitle("PC1 vs Original Variables")+
  theme_classic()



####Heuristic Function
Score.Heur = apply(abs(scale(pca)),1,sum)^(1/3)
## scale the score to 0-1
Score.Heur = (Score.Heur-min(Score.Heur))/(max(Score.Heur)-min(Score.Heur))
Score.Heur = data.frame(Score.Heur)
# summary(Score.Heur)

ggplot(Score.Heur, aes(x = Score.Heur, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "grey60",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Heuristic Fraud Score")+
  ylab("") +
  ggtitle("Heuristic Algorithm Fraud Score Histogram")+
  theme_classic()

## get the row number of top 1% heuristic fraud score
ix1 = order(Score.Heur,decreasing=T)[1:10000]
#### Autoencoder
ae_pca = h2o.init()
ae_train = as.h2o(pca,destination_frame = 'autoencoder_train')
feature_names = colnames(pca)
ae_dl = h2o.deeplearning(x=feature_names,training_frame = ae_train,
                               autoencoder=TRUE, reproducible=TRUE, seed=1234,
                               hidden=c(14),epochs=50)
ae_anon = h2o.anomaly(ae_dl, ae_train, per_feature = FALSE)
## get reconstruction error
err = as.data.frame(ae_anon)
Score.AE = err$Reconstruction.MSE^(1/6)
## scale ae fraud score to 0-1
Score.AE = (Score.AE-min(Score.AE))/(max(Score.AE)-min(Score.AE))
Score.AE = data.frame(Score.AE)
# summary(Score.AE)
ggplot(Score.AE, aes(x = Score.AE, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "grey60",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Autoencoder Fraud Score")+
  ylab("") +
  ggtitle("Autoencoder Algorithm Fraud Score Histogram")+
  theme_classic()

## get the row number of top1% ae fraud score
ix2 = order(Score.AE,decreasing=T)[1:10000]

## calculate the overlapping rownumber of the two score, it's 52.84%
overlap = ifelse(ix1 %in% ix2, 1, 0)

sum(overlap)/length(overlap)

#### final score
## give 70% weight to ae score and plot the distribution
Score.Combined = 0.7*Score.AE+0.3*Score.Heur
Score.Combined = data.frame(Score.Combined)
colnames(Score.Combined)='Score.Combined'
ggplot(Score.Combined, aes(x = Score.Combined, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "grey60",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Combined Fraud Score")+
  ylab("") +
  ggtitle("Combined Fraud Score Histogram")+
  theme_classic()

## get the top1% score and add the three scores back to original data before pca
#str(Z_ny2_smp),23
ix3 = order(Score.Combined,decreasing=T)[1:10000]

#### still go with records with top Autoencoder score because it's more intepratable
#ny_score = cbind(ny[,1:23],Score.Heur,Score.AE,Score.Combined)[ix3,]
ny_score = cbind(ny2_smp[,1:23],Score.Heur,Score.AE,Score.Combined)[ix2,]
#head(ny_score,10)
head(ny_score,10)


#################################
# fraud score method 2
# Mahalanobis Distance
# http://stackoverflow.com/questions/18658961/mahalanobis-distance-in-r
# http://stackoverflow.com/questions/22134398/mahalonobis-distance-in-r-error-system-is-computationally-singular

#Z_ny2_smp.cov = cov(Z_ny2_smp)
#dim(Z_ny2_smp.cov)

####Mahalanobis_Distance <- D2.dist(Z_ny2_smp, Z_ny2_smp.Cov)
# Mah_Dist =mahalanobis(Z_ny2_smp, center = colMeans(Z_ny2_smp) , Z_ny2_smp.Cov ,tol=1e-100)

#Mah_Dist =mahalanobis(Z_ny2_smp, center = FALSE , Z_ny2_smp.cov ,tol=1e-100)
#Mah_Dist = data.frame(Mah_Dist)
#summary(Mah_Dist)

#plot
#ggplot(Mah_Dist, aes(x = Mah_Dist, y = ..density.. ))+ 
  #geom_histogram(fill = "cornsilk" , colour = "grey60",size = .2, bins = 15)+
  #geom_density(adjust = 2) +
 # xlim(0,200) +
  #xlab("Fraud Score")+
  #ylab("") +
  #ggtitle("Mahalanobis Distance Fraud Score Histogram")

# fraud score method 3
# neural network algorithm autoencoder 
# https://cran.r-project.org/web/packages/autoencoder/autoencoder.pdf
# library(autoencoder)
# 
# nl=3                          ## number of layers (default is 3: input, hidden, output)
# unit.type = "logistic"        ## specify the network unit type, i.e., the unit's 
# 
# ## activation function ("logistic" or "tanh")
# Nx.patch=10                   ## width of training image patches, in pixels
# Ny.patch=10                   ## height of training image patches, in pixels
# N.input = Nx.patch*Ny.patch   ## number of units (neurons) in the input layer (one unit per pixel)
# N.hidden = 10*10                ## number of units in the hidden layer
# lambda = 0.0002               ## weight decay parameter     
# beta = 6                      ## weight of sparsity penalty term 
# rho = 0.01                    ## desired sparsity parameter
# epsilon <- 0.001              ## a small parameter for initialization of weights 
# ## as small gaussian random numbers sampled from N(0,epsilon^2)
# max.iterations = 2000         ## number of iterations in optimizer

# sample 70% data as trainning dataset from pca
#smp_n = 0.7*dim(pca)[1];
#pca = as.data.frame(pca)
#pca_train = sample_n(pca,700,replace = FALSE)

# pca = data.matrix(pca)
# 
# autoencoder.object <- autoencode(X.train = pca ,nl=nl,N.hidden=N.hidden,
#                                  unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
#                                  optim.method="BFGS",max.iterations=max.iterations,
#                                  rescale.flag=TRUE,rescaling.offset=0.001)



#cat("autoencode(): mean squared error for training set: ",
#  round(autoencoder.object$mean.error.training.set,3),"\n")


# pca.output <- predict(autoencoder.object, X.input=pca, hidden.output=FALSE)$X.output
# 
# pca.AE.dist=(pca.output - pca)^2
# Score.AE = apply(pca.AE.dist,1,sum)
# Score.AE = data.frame(Score.AE)
# summary(Score.AE)
# head(Score.AE)

### To analysis the reason why some records get high fraud score.
###Looking at records scoring highest in autoencoder analysis

# summary(Score.AE1)
# Score.AE1 = data.frame(cbind(id=row.names(Score.AE),Score.AE))
# Score.AE2 = Score.AE1 %>%
#   arrange(-Score.AE) # %>%
# #filter(Score.AE > 1000)
# 
# 
# ny1_smp_id=data.frame(cbind(id=row.names(ny1_smp),ny1_smp))
# 
# ###looking for high scored records in ny1_smp
# ny_smp.AE = ny1_smp_id%>%
#   filter(id %in% Score.AE2$id)
# 
# ny_smp.highAEscore= merge(x =ny_smp.AE , y = Score.AE2, by = "id",all.x = T)
# ny_smp.highAEscore=ny_smp.highAEscore%>%
#   arrange(-Score.AE)
# ny_smp.highAEscore=ny_smp.highAEscore[,c(1,67,2:66)]
# head(ny_smp.highAEscore)
# 
# 
# 
# #summary(ny_smp[ny_smp$EXCD1 == 1017,])
# #summary(ny_smp[ny_smp$TAXCLASS == 1,])
# 
# 
# ####Looking at some records scoring highest in Heur 
# Score.Heur = cbind(id=row.names(Score.Heur),Score.Heur)
# 
# Score.Heur1 = Score.Heur %>%
#   arrange(-Score.Heur)# %>%
# # filter(Score.Heur > 1000)
# 
# ####ny_smp.highHEURscore = ny_smp[Score.Heur1$`row.names(Score.Heur)`,]###does not match
# ny_smp.HE = ny1_smp_id #%>%
# #  filter(id %in% Score.Heur1$id)
# 
# ny_smp.highHEscore= merge(x =ny_smp.HE , y = Score.Heur1, by = "id",all.x = T)
# ny_smp.highHEscore=ny_smp.highHEscore%>%
#   arrange(-Score.Heur)
# 
# ny_smp.highHEscore=ny_smp.highHEscore[,c(1,67,2:66)]
# head(ny_smp.highHEscore)
# #View(Score.Heur1)
# install.packages('h2o')
# # merge AE and HE into one dataset
# ny_smp.FraudScore= merge(x =ny_smp.highHEscore , y = ny_smp.highAEscore[,c(1,2)], by = "id",all.x = T)
# ny_smp.FraudScore=ny_smp.FraudScore[,c(1,68,2:67)]
# ny_smp.FraudScore=ny_smp.FraudScore%>%
#   arrange(-Score.AE)
# 
# ny_smp.HighFraudScore = head(ny_smp.FraudScore,10000)
# 
# write.csv(ny_smp.HighFraudScore, file = "ny_smp.HighFraudScore.csv")
