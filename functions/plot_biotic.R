plot_biotic <- function(Cruise,Biology,Catch,Haul,speciesList,figurePath){
  
  haulsPerGraph <- 10
  
  idxMatch <- match(c('HER', 'SPR'),speciesList$SPECIESID)
  
  idxFiltBio <- as.numeric(as.character(Biology$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Biology <- Biology[idxFiltBio,]
  
  idxFiltCatch <- as.numeric(as.character(Catch$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Catch <- Catch[idxFiltCatch,]

  country       <- as.character(Cruise$CruiseCountry)
  cruiselocalID <- as.character(Cruise$CruiseLocalID)
  
  uniqueSpecies <- as.numeric(as.character(unique(Biology$CatchSpeciesCode)))
  nSpecies  <- length(uniqueSpecies)
  
  uniqueSpeciesName <- as.character(speciesList$SPECIESNAME[match(uniqueSpecies,speciesList$WORMS)])
  uniqueSpeciesIDStr <- as.character(speciesList$SPECIESID[match(uniqueSpecies,speciesList$WORMS)])
  
  #### Step 1.1 overall Length frequencies ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    lengthInd    <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))
    lengthInd    <- lengthInd[which(!is.na(lengthInd))]
    
    if(length(lengthInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_length_all_',uniqueSpeciesIDStr[idxSpecies],'_',cruiselocalID,'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      hist(lengthInd,
           breaks = seq(from = -5, to = 405, by = 10),
           ylab = 'Count (#)',
           xlab = 'age (wr)',
           main = uniqueSpeciesName[idxSpecies])
      
      dev.off()
    }
  }
  
  #### Step 1.2 Length frequencies for each haul ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    LF_mat <- mk_length_matrix(BiologyTemp)
    
    nPlots <- ceiling(dim(LF_mat)[2]/haulsPerGraph)
    
    startPlots <- 1
    for(idxPlot in 1:nPlots){
      if((startPlots+haulsPerGraph) <= dim(LF_mat)[2]){
        currentPlots <- startPlots:(startPlots+haulsPerGraph-1)
      }else{
        currentPlots <- startPlots:dim(LF_mat)[2]
      }
      
      png(file.path(figurePath,paste0('biotic_',
                                      country,
                                      '_lengthfrequency_haul_',
                                      uniqueSpeciesIDStr[idxSpecies],
                                      '_',
                                      as.character(idxPlot),'_',
                                      cruiselocalID,'.png')), 
          width = 1500, 
          height = 900)
      
      par(mfrow=c(1,haulsPerGraph),mar=c(5,2.5,4,1)+0.1)
      
      barplot(LF_mat[,currentPlots[1]],
              horiz=T,
              names.arg=row.names(LF_mat),
              main=names(LF_mat[currentPlots[1]]),
              cex.names = 1.5)
      title(colnames(LF_mat)[currentPlots[1]],cex.main =2.5)
      legend("topleft", legend = paste0(uniqueSpeciesIDStr[idxSpecies],' N=',sum(LF_mat[,currentPlots[1]])),bty = "n",cex=2.5) 
      
      if(length(currentPlots) > 1){
        for (i in 2:length(currentPlots)){
          barplot(LF_mat[,currentPlots[i]],
                  horiz=T,main=names(LF_mat[currentPlots[i]]),
                  cex.names = 1.5)
          title(colnames(LF_mat)[currentPlots[i]],cex.main=2.5)
          legend("topleft", legend = paste0('N=',sum(LF_mat[,currentPlots[i]])),bty = "n",cex=2.5) 
        }
      }
      
      dev.off()
      
      startPlots <- startPlots + haulsPerGraph
    }
  }
  
  #### Step 1.3 - length-weight relationship ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    lengthInd <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))*1e-1
    weightInd <- as.numeric(as.character(BiologyTemp$BiologyIndividualWeight))
    
    lengthInd <- lengthInd[which(!is.na(weightInd))]
    weightInd <- weightInd[which(!is.na(weightInd))]
    
    if(length(weightInd) != 0 && length(lengthInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_weight_length_',uniqueSpeciesIDStr[idxSpecies],'_',cruiselocalID,'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      
      plot(x = lengthInd,
           y = weightInd,
           xlab = "length (cm)",
           ylab = "weight (gr)", 
           main = paste0("Length vs weight - ",uniqueSpeciesName[idxSpecies]))
      
      dev.off()
    }
  }
  
  #### Step 1.4 age distribution for each haul ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    haulUnique      <- as.numeric(as.character(unique(BiologyTemp$HaulNumber)))
    haulUnique      <- haulUnique[order(haulUnique)]
    nHauls          <- length(haulUnique)
    
    png(file.path(figurePath,paste0('biotic_',country,'_age_distribution_haul_',uniqueSpeciesIDStr[idxSpecies],'cruiselocalID','.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
    
    opar <- par(mfrow = n2mfrow(nHauls), mar = c(2,2,1,1), oma = c(2,2,3,0))
    
    for(idxHaul in 1:nHauls){
      BiologyTempHaul <- BiologyTemp[BiologyTemp$HaulNumber == haulUnique[idxHaul],]
      
      ageInd <- as.numeric(as.character(BiologyTempHaul$BiologyIndividualAge))
      
      ageInd    <- ageInd[which(!is.na(ageInd))]
      
      ageInd[ageInd > 10] <- 10
      
      hist(ageInd,
           breaks = (0-0.5):(10+0.5),
           ylab = 'Count (#)',
           xlab = 'age',
           main = paste0('Haul ', haulUnique[idxHaul]%%1000))# -min(haulUnique)+1,'-',uniqueSpeciesIDStr[idxSpecies]
      legend("topleft", legend = paste0('N=',length(BiologyTempHaul$BiologyLengthClass)),bty = "n") 
    }
    dev.off()
  }
  
  #### Step 1.5 - length - age relationship ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    lengthInd <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))*1e-1
    ageInd <- as.numeric(as.character(BiologyTemp$BiologyIndividualAge))
    
    lengthInd <- lengthInd[which(!is.na(ageInd))]
    ageInd    <- ageInd[which(!is.na(ageInd))]
    
    if(length(lengthInd) != 0 && length(ageInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_age_length_',uniqueSpeciesIDStr[idxSpecies],'_',cruiselocalID,'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      
      plot(x = lengthInd,
           y = ageInd,
           xlab = "length (cm)",
           ylab = "age (wr)", 
           main = paste0("age vs length -",uniqueSpeciesName[idxSpecies]))
      
      dev.off()
    }
  }
  
  #### Step 1.6 - Age distribution  ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    ageInd    <- as.numeric(as.character(BiologyTemp$BiologyIndividualAge))
    ageInd    <- ageInd[which(!is.na(ageInd))]
    
    if(length(ageInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_age_all_',uniqueSpeciesIDStr[idxSpecies],'cruiselocalID','.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      hist(ageInd,
           breaks = (min(ageInd)-0.5):(max(ageInd)+0.5),
           ylab = 'Count (#)',
           xlab = 'age (wr)',
           main = uniqueSpeciesName[idxSpecies])
      
      dev.off()
    }
  }
  
  #### Step 1.7 - Catch proportions SPR and HER  ####
  haulNumbers <- as.numeric(as.character(Catch$HaulNumber))
  
  dfCatch <- data.frame(  station=character(length = dim(Catch)[1]), 
                          catch=double(length = dim(Catch)[1]), 
                          lon=double(length = dim(Catch)[1]),
                          lat=double(length = dim(Catch)[1]),
                          species=character(length = dim(Catch)[1]),
                          stringsAsFactors = FALSE)
  
  for(idxHaul in 1:dim(dfCatch)[1]){
    currentSpecies <- as.numeric(as.character(Catch$CatchSpeciesCode[idxHaul]))
    b <- speciesList$WORMS == currentSpecies
    b[is.na(b)] <- FALSE
    boolHaul <- Haul$HaulNumber == Catch$HaulNumber[idxHaul]
    
    dfCatch$station[idxHaul]  <- haulNumbers[idxHaul]-min(haulNumbers)+1
    dfCatch$catch[idxHaul]    <- as.numeric(as.character(Catch$CatchSpeciesCategoryWeight[idxHaul]))/1e-3
    dfCatch$lon[idxHaul]      <- as.numeric(as.character(Haul$HaulStartLongitude[boolHaul]))
    dfCatch$lat[idxHaul]      <- as.numeric(as.character(Haul$HaulStartLatitude[boolHaul]))
    dfCatch$species[idxHaul]  <- as.character(speciesList$SPECIESID[b])
  }
  
  #dfCatch <- drop.levels(dfCatch[dfCatch$species %in% c('HER', 'SPR'),])
  
  hh <- make.xyz(x = dfCatch$lon, y = dfCatch$lat, z = dfCatch$catch, group = dfCatch$species)
  
  #opar <- par(mfrow = c(1,1), mar = c(2,2,1,1), oma = c(2,2,3,0))
  png(file.path(figurePath,paste0('biotic_',country,'_catch_prop_',cruiselocalID,'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
  
  map('worldHires', 
      col = "green", 
      fill = TRUE, 
      xlim = c(min(dfCatch$lon)-2, 
               max(dfCatch$lon)+2), 
      ylim = c(min(dfCatch$lat)-2, 
               max(dfCatch$lat)+2))
  box()
  axis(side = 2, las = 2)
  draw.pie(x = hh$x, y = hh$y, z = hh$z, col = rainbow(10),radius=0.3)
  legend.pie(min(hh$x)-1, max(hh$y)+.25, labels = dimnames(hh$z)[[2]], col = rainbow(10), radius = 0.2, bty = "n")
  
  dev.off()
}