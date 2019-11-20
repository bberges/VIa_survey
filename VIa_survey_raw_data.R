rm(list = ls())

#### Packages ####
library(ggplot2)
library(mapplots) # draw.pie
data("coast") # coastlines
library(maps)
library(mapdata)
library(gdata)  # drop.levels
library(reshape) # melt and cast
library(gstat)
library(sp)

path <- 'C:/git/VIa_survey/'

try(setwd(path),silent=TRUE)

mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
functionPath  <- file.path(".","functions")

source(file.path(functionPath,"load_ICESdB.R"))
source(file.path(functionPath,"plot_biotic.R"))
source(file.path(functionPath,"plot_acoustic.R"))
source(file.path(functionPath,"mk_report_ICESdB.R"))
source(file.path(functionPath,"mk_length_matrix.R"))

mkReport  <- TRUE
mkPlot    <- TRUE

# load species list
fileName <- 'species_codes_201911.csv'

speciesList <- read.csv(file.path(dataPath,fileName), fill = TRUE, header = TRUE)

#surveyYearMat    <- c(2019,2018,2017)
surveyYearMat    <- c(2019)

for(surveyYear in surveyYearMat){

reportPath    <- file.path(".","reports",surveyYear)
rawDataPath   <- file.path(".","data",'raw_data',surveyYear)
figurePath    <- file.path(".","figures",'raw_data',surveyYear)

# build directory list
dataDirs <- list.dirs(path = rawDataPath, full.names = TRUE, recursive = TRUE)
dataDirs <- dataDirs[2:length(dataDirs)]

count <- 0
# loop on directories (i.e. each country)
for(idxDir in dataDirs){
  count <- count + 1
  print(idxDir)
  fileMat <- list.files(path = idxDir,pattern = "\\.csv$")
  
  ### load data
  for(idxFile in fileMat){
    A <- load_ICESdb(idxDir,idxFile)
    
    header <- A[[1]]
    for(idxHeader in 2:length(A)){
      eval(parse(text = paste0(header[idxHeader-1],
                               '=A[[',
                               as.character(idxHeader),']]')))
    }
  }

  if(mkPlot){
    ### plot Biotic data
    plot_biotic(Cruise,
                Biology,
                Catch,
                Haul,
                speciesList,
                figurePath)
      
    ### plot Acoustic data
    plot_acoustic(Data,
                  Cruise,
                  Haul,
                  figurePath)
  }
  
  if(mkReport){
    # make report
    mk_report_ICESdB(Cruise,
                     Biology,
                     Catch,
                     Haul,
                     Data,
                     speciesList,
                     reportPath)
  }
  
  if(count == 1){
    CatchAll    <- Catch
    HaulAll    <- Haul
    BiologyAll  <- Biology
    CruiseAll   <- Cruise
    DataAll     <- Data
  }else{
    CatchAll    <- rbind(CatchAll,Catch)
    HaulAll     <- rbind(HaulAll,Haul)
    BiologyAll  <- rbind(BiologyAll,Biology)
    CruiseAll   <- rbind(CruiseAll,Cruise)
    DataAll     <- rbind(DataAll,Data)
  }
}


save(CatchAll, HaulAll, BiologyAll, CruiseAll, DataAll, file = file.path(rawDataPath,paste0(surveyYear,"_VIa_survey",".Rdata")))
}