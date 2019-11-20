rm(list=ls())

library(plyr)
library("XML")

path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
outPath       <- file.path(".","output")
functionPath  <- file.path(".","functions")

surveyYearMat    <- c(2018)

surveyYear <- surveyYearMat[1]

StoXDataPath  <- file.path(".","data",'StoX',surveyYear)
figurePath    <- file.path(".","figures",'StoX',surveyYear)

# build directory list
dataDirs <- list.dirs(path = StoXDataPath, full.names = TRUE, recursive = FALSE)

idxDataDir <- 1

projectPath   <- dataDirs[idxDataDir]

#Extract data from xml
xml.file  <- file.path(projectPath,"/process/","project.xml")
data      <- xmlParse(xml.file, useInternalNodes = TRUE)
xml_data  <- xmlToList(data)

# Generate Transect file with unique ID
edsu <- ldply(xml_data$processdata$edsupsu , data.frame,stringsAsFactors = F)
colnames(edsu) <- c("Unit","Transect","SampleUnit")
medsu <- strsplit(edsu$SampleUnit,"/")
edsu<-cbind(as.data.frame(do.call(rbind,medsu),stringsAsFactors = F),edsu$Transect)
names(edsu) <- c("Cruise","LOG","Date","Time","Transect") 
edsu$ID <- paste(edsu$Cruise,"_",edsu$LOG,sep="")

# process the data for that survey
spp_data_int <- read.table(file.path(projectPath,'output','baseline','data','4_NASC_NASC.txt'),
                           header=TRUE,stringsAsFactors = F)
spp_data_int <- spp_data_int[c("SampleUnit","NASC")]
log <- strsplit(spp_data_int$SampleUnit,"/")
nascinfo <-cbind(as.data.frame( do.call(rbind,log),stringsAsFactors = F),spp_data_int$NASC)

names(nascinfo) <- c("Cruise","LOG","Date","Time","SA")
nascinfo$SA<-as.numeric(nascinfo$SA)
nascinfo$ID <- paste(nascinfo$Cruise,"_",nascinfo$LOG,sep="")
#Read Log file
loginfo <- read.table(file.path(projectPath,'output','baseline','data','3_FilterAcoustic_AcousticData_DistanceFrequency.txt'),
                      sep="\t",header=TRUE,stringsAsFactors = F)
names(loginfo)
loginfo <- loginfo[c("cruise","log_start","start_time","lon_start","lat_start","integrator_dist")]
loginfo$log_start<- sprintf("%.1f",loginfo$log_start)
names(loginfo) <- c("Cruise","LOG","Time","ACLON","ACLAT","Dist")
loginfo <- loginfo[!duplicated(loginfo),]
loginfo$ID <- paste(loginfo$Cruise,"_",loginfo$LOG,sep="")
#Merge data frames for: log, SA and transect no

str(loginfo)
str(nascinfo)
str(edsu)

#which(loginfo$ID %in% nascinfo$ID==FALSE)

spp_datap1  <- merge(loginfo, nascinfo, by="ID",all.x=TRUE)
spp_datap1 <- spp_datap1[-which(names(spp_datap1)%in% c("Cruise.y", "LOG.y"  ,  "Date"    , "Time.y"))]

colnames(spp_datap1)[3]<- "LOG"
colnames(spp_datap1)[2] <- "Cruise"
colnames(spp_datap1)[4]<- "Time"
colnames(spp_datap1 )

spp_datap2  <- merge(spp_datap1, edsu, by="ID",all.x=TRUE)
spp_data_int <- Reduce(function(x, y) merge(x, y, by="ID",all.x=TRUE), list(loginfo,nascinfo,edsu))
spp_data_int <- spp_datap2  [,-c(9,10,11,12)]


spp_data_int$SA[is.na(spp_data_int$SA)] <- 0
spp_data_int <- spp_data_int[order(spp_data_int$Cruise.x, as.numeric(spp_data_int$LOG.x)),]
# spp_data_int<-spp_data_int[,c(2,3,4,5,6,9,10,11,16)]
Stratum <- ldply(xml_data$processdata$psustratum,data.frame,stringsAsFactors = F)
colnames(Stratum) <- c("Id", "Stratum", "Transect")

write.csv(spp_data_int,"C:/Users/sakin001/Google Drive/HERAS/mergedNASCS14.csv",row.names = F)

############################################################
# resample data into 5 nmi intervals within each transect
############################################################

only.transect <- spp_data_int[!is.na(spp_data_int$Transect), ] # This bit selects the lines that has associated transects
only.transect$TransectNo <- as.numeric(gsub("T", "", only.transect$Transect))
only.transect <- (merge(only.transect,Stratum, by="Transect"))

mlist <- list()
for (i in unique(only.transect$TransectNo))
{
  mTransects <- only.transect[only.transect$TransectNo == i, ]
  mTransects$inid <- floor((as.numeric(mTransects$LOG.x)) / 5)
  mTransects$inid_one <- floor((as.numeric(mTransects$LOG.x)) )
  
  d1 <- as.numeric(duplicated(mTransects$inid_one,fromLast=FALSE ))#deal with 0.1 nmi EDSU
  d2 <- as.numeric(duplicated(mTransects$inid_one,fromLast=TRUE ))#deal with 0.1 nmi EDSU
  
  mTransects$duplicates <- apply(as.data.frame(cbind(d1,d2)),1,max)#deal with 0.1 nmi EDSU
  mTransects$FID <- paste0(mTransects$Transect, "_", mTransects$inid)
  mTransects$FONEID <- paste0(mTransects$Transect, "_", mTransects$inid_one)
  mlist[[i]] <- mTransects
}

resample.all.transects <- as.data.frame(do.call(rbind, mlist))
resample.transects <- as.data.frame(do.call(rbind, mlist))

short.transects <- resample.transects[resample.transects$duplicates==1,]#deal with 0.1 nmi EDSU

resample.transects <- resample.transects[resample.transects$duplicates!=1,]

mSHORTdist <- aggregate(cbind(Dist, SA,Dist*SA) ~ FONEID, short.transects, sum)
mlondist <- aggregate(cbind(Dist, SA,Dist*SA) ~ FONEID, resample.transects, sum)
malldist <- rbind(mSHORTdist,mlondist)
tt <- NULL
resample.transects.ID <- resample.all.transects[which(colnames(resample.all.transects) %in% c("FID","FONEID"))]
tt <-  merge(resample.transects.ID,malldist,by = "FONEID",incomparables = F)
malldist <-  tt[!duplicated(tt$FONEID),]

mdist <- aggregate(cbind(Dist, SA,Dist*SA) ~ FID, malldist, mean)# get the mean SA of 5 nmi distances
mdist$mSA <- (mdist$V3) # mSa is the mean SA of 5 nmi distances
check.table<- mdist[which(mdist$SA - mdist$V3>500),] # check  where differences between the two way of calculating mean is large >500
# mLog <- resample.transects[, c(1,2,3,4,6,7,9,12,14)]
mLog <- resample.all.transects[!duplicated(resample.all.transects$FID), c(1,2,3,4,6,7,9,12,16)]
mLog.check <- resample.transects[duplicated(resample.transects$FID), c(1,2,3,4,6,7,9,12,16)]
str(mdist)
mLog$FID%in%mdist$FID
transects.resampled <- merge(mdist, mLog,by = "FID",all=T)
str(transects.resampled )

write.csv(transects.resampled ,"C:/Users/sakin001/Google Drive/HERAS/HERAS_FEB19.csv",row.names = F)

###############################################
#calculate the transect length per stratum
###############################################
spp_data_int$Transect<- as.character(spp_data_int$Transect)
tDistances <- aggregate(Dist ~ Transect, spp_data_int, sum)

colnames(Stratum) <- c("Unit", "Stratum", "Transect")
mStratum <- merge(tDistances, Stratum, by = "Transect")

tDistances <- aggregate(Dist ~ Stratum, data = mStratum, sum) 
tDistances<- tDistances[order(as.numeric(tDistances$Stratum)),] #distances per stratum

###################################
# Get bio assignment 
###################################
assFun<- function(x,y){x[[y]]}
bioas <- xml_data$processdata$bioticassignment
Samp.Unit.Assign <- xml_data$processdata$suassignment
mbioas<-as.data.frame(do.call(rbind,lapply(bioas,assFun,2)),row.names = F)
mSamp.Unit.Assign<-cbind(as.data.frame(do.call(rbind,lapply(Samp.Unit.Assign,assFun,1)),row.names = F),
                         as.data.frame(do.call(rbind,lapply(Samp.Unit.Assign,assFun,2)),row.names = F))
colnames(mSamp.Unit.Assign) <- c("assignmentid","Transect" ,"estlayer" )
mSamp.Unit.Assign<- data.frame(lapply(mSamp.Unit.Assign, as.character), stringsAsFactors=FALSE)
bioassign.step1<- merge(mSamp.Unit.Assign,mStratum,by="Transect")
bioassign.step2<- merge(mbioas,bioassign.step1,by="assignmentid")# Get bio assignment
bioassign.step2<- data.frame(lapply(bioassign.step2, as.character), stringsAsFactors=FALSE)

indBiotic <- read.table(file.path(projectPath,'output','baseline','data','6_FilterBiotic_BioticData_Individual.txt'),
                        sep="\t",header=TRUE,stringsAsFactors = F)
indBiotic<- indBiotic[!is.na(as.numeric(indBiotic$age))&indBiotic$aphia==126417,]
indBiotic$station <- paste0(indBiotic$cruise,"/",indBiotic$serialno)

###############################################################
# Read final Stox output file "1_FillMissingData_SuperIndividuals.txt"
###############################################################
SuperInd <- read.table(file.path(projectPath,'output','baseline','report','1_FillMissingData_SuperIndividuals.txt'),
                       sep="\t",header=TRUE,stringsAsFactors = F)
SuperInd$station<- paste0(SuperInd$cruise,"/",SuperInd$serialno,collapes="")

bioassign.step3 <-  merge(SuperInd,bioassign.step2,by= "station")
bioassign.step3$TS <- 10^((20*log10(bioassign.step3$LenGrp)-71.2)/10) # get sigma bs for each length group
TRlist <- data.frame(matrix(nrow=0,ncol=5,byrow = T))
colnames(TRlist)<- c("specialstage","TS","Transect","MAT","IMM")
bioassign.check<- bioassign.step3[bioassign.step3$specialstage=="-" ,] # eliminate unknown stage fish

bioassign.step3<- bioassign.step3[bioassign.step3$specialstage!="-" ,] # eliminate unknown stage fish
zz=1

###############################################################
# Get maturity propotons per transet. This gets data from combined trawls that are assigned to a transect
###############################################################
for(tr in 1:length(unique(bioassign.step3$Transect)))
{
  
  TRassign <- bioassign.step3[bioassign.step3$Transect==unique(bioassign.step3$Transect)[tr],] # take a transect at each loop
  
  #This is a step for equal weighting the contribution each individual trawl assigned to this specific transect
  for(k in unique(TRassign$serialno))
  {
    maxw <- max(table(TRassign$serialno))
    TR.tab <- data.frame(table(TRassign$serialno))
    coeff<- maxw/TR.tab[TR.tab==k,]$Freq
    TRassign[TRassign$serialno==k,]$TS <- TRassign[TRassign$serialno==k,]$TS*coeff # TS is multiplied by the weighting factor
  }
  
  
  
  TRprop<- aggregate(TS~specialstage,data=TRassign,sum) # sum the sigma bs per maturity category.
  #Number of fish and their target stregth at each maturity category will be used to split the NASC
  # in that specific transect.
  scat <- c("IMM","MAT")
  
  if(!("IMM"%in%TRprop$specialstage)){IMMAT<-c("IMM",0)}
  if(!("MAT"%in%TRprop$specialstage)){MMAT<-c("MAT",0)}
  
  madd<-  which(c(exists("IMMAT"),exists("MMAT")))
  
  if(length(madd)==1 ) {TRprop[2,]<-c(scat[madd],0)}
  if(length(madd)==2 ) {TRprop[1,]<-c(scat[1],0); TRprop[2,]<-c(scat[2],0)}
  
  TRprop$TS <- as.numeric(TRprop$TS )
  TRprop$MAT <- (TRprop[TRprop$specialstage%in%"MAT",]$TS)/sum(TRprop$TS )
  TRprop$IMM <- (TRprop[TRprop$specialstage%in%"IMM",]$TS)/sum(TRprop$TS )
  
  TRprop$Transect <- unique(bioassign.step3$Transect)[tr]
  TRlist <- rbind(TRlist,TRprop)
  
  zz=zz+1
  mvar<- c("IMMAT","MMAT")
  rm(list=mvar)
}

##############################################
#Maturity proportion data frame
##############################################
TRlist<- TRlist[,c(3,4,5)]
TRlist <- TRlist[!duplicated(TRlist),]

##############################################
# Get number of aged fish per Stratum
##############################################

agesum <- aggregate(age~station,indBiotic,length) 
nfish<- merge(bioassign.step2,agesum,by="station")
nfish$uniqueID <- as.character(paste0(nfish$station,"/",nfish$Stratum))
nfish<- nfish[!duplicated(nfish$uniqueID ),]
nfishStratum<- aggregate(age~Stratum,nfish,sum)

###############################################
# Get final table
###############################################
final <- merge(tDistances,nfishStratum,by="Stratum")
final<- final[order(as.numeric(final$Stratum)),]

###############################################
# Split Nasc by Maturity as percentages in transetcs
###############################################

transects.resampled$Transect <- as.character(transects.resampled$Transect)
nasc.maturity<- merge(transects.resampled, TRlist, by="Transect")
colnames(nasc.maturity)

data_abu <- read.table(file.path(projectPath,'output','baseline','data','19_AbundanceByLength_Abundance.txt'),
                       header=TRUE,stringsAsFactors = F,sep="\t")

strata_abu <- aggregate(Abundance~SampleUnit,data=data_abu,sum)
colnames(strata_abu) <- c("Stratum" ,"Abundance")
strata_nasc <-aggregate(mSA~Stratum,nasc.maturity,sum)
nasc2abu <- merge(strata_abu,strata_nasc,by="Stratum")

nasc2abu$multiplier <- (nasc2abu$Abundance / nasc2abu$mSA)
nasc2abu <- nasc2abu[c(1,4)]
nasc.maturity <- merge(nasc.maturity,nasc2abu ,by="Stratum")
nasc.maturity$maturperc <- nasc.maturity$MAT*nasc.maturity$mSA*nasc.maturity$multiplier*0.000001
nasc.maturity$immaturperc <- nasc.maturity$IMM*nasc.maturity$mSA*nasc.maturity$multiplier*0.000001
nasc.maturity<- nasc.maturity[,c("Stratum","Transect","FID","mSA","ID","Cruise.x",	"LOG.x","ACLON","ACLAT","MAT","IMM","multiplier","maturperc", "immaturperc")]


colnames(nasc.maturity) <-c("Stratum","Transect","FID","SA","ID","Cruise",	"LOG","longitude","latitude","Mat_Perc","Imm_Perc","multiplier","MAT", "IMM")

write.csv(nasc.maturity,"nasc.maturity.Europe_T2.csv", row.names = F)