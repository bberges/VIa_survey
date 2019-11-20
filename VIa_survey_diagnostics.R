rm(list = ls())

#### Packages ####
library(ggplot2)
library(moments)


path <- 'C:/git/VIa_survey/'

try(setwd(path),silent=TRUE)

surveyYear    <- 2019
mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
functionPath  <- file.path(".","functions")
reportPath    <- file.path(".","reports",surveyYear)
rawDataPath   <- file.path(".","data",surveyYear)
figurePath    <- file.path(".","figures",surveyYear)

# load species list
fileName <- 'species_codes_201911.csv'

speciesList <- read.csv(file.path(dataPath,fileName), fill = TRUE, header = TRUE)

# input parameters
edgesLength  <- seq(from = 0, to = 1000, by = 10)
#centerLength <- seq(from = 0, to = 1000, by = 10)

species <- 'HER'
uniqueSpeciesName   <- as.character(speciesList$SPECIESNAME[match(species,speciesList$SPECIESID)])
uniqueSpeciesWORMS  <- as.numeric(speciesList$WORMS[match(species,speciesList$SPECIESID)])

# compute ratio
load(file.path(rawDataPath,paste0(surveyYear,"_HERAS",".Rdata")))

platformUnique <- as.character(unique(BiologyAll$CruiseLocalID))

for(idxPlatform in 1:length(platformUnique)){
  BiologyPlatform <- BiologyAll[BiologyAll$CruiseLocalID %in% platformUnique[idxPlatform] & BiologyAll$CatchSpeciesCode == uniqueSpeciesWORMS,]
  haulUnique <- as.numeric(as.character(unique(BiologyPlatform$HaulNumber)))
  country <- as.character(CruiseAll$CruiseCountry[CruiseAll$CruiseLocalID %in% platformUnique[idxPlatform]])
  
  outMat <- array(  NA,
                    dim=c(length(haulUnique),
                          4))
    
  for(idxHaul in 1:length(haulUnique)){
    BiologyPlatformFilt <- BiologyPlatform[BiologyPlatform$HaulNumber %in% haulUnique[idxHaul],]
    
    lengtFish <- as.numeric(as.character(BiologyPlatformFilt$BiologyLengthClass))
    
    histInfo <- hist(lengtFish,
                     edgesLength,
                     plot=FALSE)
    
    idxShift <- 50-which(histInfo$count==max(histInfo$count), arr.ind=TRUE)
    
    outMat[idxHaul,1] <- length(which(histInfo$counts!=0))
    outMat[idxHaul,2] <- length(lengtFish)
    outMat[idxHaul,3] <- kurtosis(histInfo$counts[which(histInfo$counts!=0)])#kurtosis(tempVar)#/length(which(histInfo$counts!=0))
    outMat[idxHaul,4] <- (outMat[idxHaul,1])/outMat[idxHaul,2]
    #outMat[,idxHaul] <- histInfo$counts
    # kurtosis((histInfo$count-mean(histInfo$count))/sd(histInfo$count))*length(which(histInfo$counts!=0))
  }
  
  if(idxPlatform == 1){
    df.out <- as.data.frame(cbind(outMat,rep(country, dim(outMat)[1])))
    colnames(df.out) <- c('N_classes','N_fish','kurt','ratio','country')
  }else{
    new.df <- as.data.frame(cbind(outMat,rep(country, dim(outMat)[1])))
    colnames(new.df) <- c('N_classes','N_fish','kurt','ratio','country')
    df.out <- rbind(df.out,new.df)
  }
}


df.out$N_classes  <- as.numeric(as.character(df.out$N_classes))
df.out$N_fish     <- as.numeric(as.character(df.out$N_fish))
df.out$kurt       <- as.numeric(as.character(df.out$kurt))
df.out$ratio      <- as.numeric(as.character(df.out$ratio))
df.out <- df.out[df.out$N_fish < 100,]
ggplot(df.out, aes(x=N_fish, y=ratio,color=country)) + geom_point() + ggtitle(surveyYear)