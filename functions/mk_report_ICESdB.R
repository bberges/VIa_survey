mk_report_ICESdB <- function(Cruise,
                             Biology,
                             Catch,
                             Haul,
                             Data,
                             speciesList,
                             reportPath){
  
  haulsPerGraph <- 10

  
  country       <- as.character(Cruise$CruiseCountry)
  cruiselocalID <- as.character(Cruise$CruiseLocalID)
  
  pdf(file.path(reportPath,paste('HERAS_raw_data_report_',country,'_',cruiselocalID,".pdf",sep="")))

  # filter bio data to Herring and Sprat  
  idxMatch <- match(c('HER', 'SPR'),speciesList$SPECIESID)
  
  idxFiltBio <- as.numeric(as.character(Biology$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Biology <- Biology[idxFiltBio,]
  
  idxFiltCatch <- as.numeric(as.character(Catch$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Catch <- Catch[idxFiltCatch,]
  
  uniqueSpecies <- as.numeric(as.character(unique(Biology$CatchSpeciesCode)))
  nSpecies  <- length(uniqueSpecies)
  
  uniqueSpeciesName <- as.character(speciesList$SPECIESNAME[match(uniqueSpecies,speciesList$WORMS)])
  uniqueSpeciesIDStr <- as.character(speciesList$SPECIESID[match(uniqueSpecies,speciesList$WORMS)])
  
  # get species in acoustic file, not necessarily HER and SPR due to MIX and CLU categories
  uniqueSpeciesAc <- as.character(unique(Data$DataSaCategory))
  nSpeciesAc      <- length(uniqueSpeciesAc)
  
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
      
      par(mfrow=c(1,haulsPerGraph),mar=c(5,2.5,4,1)+0.1)
      
      barplot(LF_mat[,currentPlots[1]],
              horiz=T,
              names.arg=row.names(LF_mat))

      
      title(main = list(paste0( 'H=',as.numeric(colnames(LF_mat)[currentPlots[1]])%%100,
                                '\nN=',sum(LF_mat[,currentPlots[1]]),
                                '\n',
                                uniqueSpeciesIDStr[idxSpecies]), cex = 1,
                        col = "red", font = 3))
      #legend("topleft", legend = paste0(uniqueSpeciesIDStr[idxSpecies],' N=',sum(LF_mat[,currentPlots[1]])),bty = "n",cex=2.5) 
      
      if(length(currentPlots) > 1){
        for (i in 2:length(currentPlots)){
          barplot(LF_mat[,currentPlots[i]],
                  horiz=T)
          title(main = list(paste0('H=',as.numeric(colnames(LF_mat)[currentPlots[i]])%%100,
                                   '\nN=',sum(LF_mat[,currentPlots[i]])), cex = 1,
                            col = "red", font = 3))
        }
      }
      
      startPlots <- startPlots + haulsPerGraph
    }
  }
  
  #### Build cruisetracks #### 
  # setup the cruite tracks
  cruiseTracks              <- Data
  
  cruiseTracks <- as.data.frame(cbind(  as.numeric(as.character(cruiseTracks$LogLatitude)),
                                        as.numeric(as.character(cruiseTracks$LogLongitude)),
                                        as.numeric(as.character(cruiseTracks$LogDistance))))
  
  colnames(cruiseTracks) <- c('LogLatitude','LogLongitude','LogDistance')
  
  agg <- aggregate(cruiseTracks,
                   by = list(cruiseTracks$LogDistance),
                   FUN = max)
  
  #### sA-values on cruisetrack #### 
  
  # loop on each species
  for(idxSpecies in 1:nSpeciesAc){ ## 
    currentSpecies  <- uniqueSpeciesAc[idxSpecies]
    DataFilt        <- Data[Data$DataSaCategory == currentSpecies,]
    
    cruiseTracksFilt <- as.data.frame(cbind(  as.numeric(as.character(DataFilt$LogLatitude)),
                                              as.numeric(as.character(DataFilt$LogLongitude)),
                                              as.numeric(as.character(DataFilt$LogDistance))))
    
    colnames(cruiseTracksFilt) <- c('LogLatitude','LogLongitude','LogDistance')
    
    saVec <- as.data.frame(as.numeric(as.character(DataFilt$DataValue)))
    
    colnames(saVec) <- c('SA')
    
    agg_1 <- aggregate(saVec,
                       by = list(cruiseTracksFilt$LogDistance),
                       FUN = sum)
    
    agg <- aggregate(cruiseTracksFilt,
                     by = list(cruiseTracksFilt$LogDistance),
                     FUN = max)
    
    SA.df <- as.data.frame(cbind(agg_1$SA,agg$LogLatitude,agg$LogLongitude))
    colnames(SA.df) <- c('SA','LogLatitude','LogLongitude')
    SA.df <- SA.df[which(SA.df$SA!=0),]
    
    # create df for hauls
    haulLon <- as.numeric(as.character(Haul$HaulStartLongitude))
    haulLat <- as.numeric(as.character(Haul$HaulStartLatitude))
    haulLab <- as.numeric(as.character(Haul$HaulNumber))%%100
    
    df.hauls <- as.data.frame(cbind(haulLat,haulLon,haulLab))
    
    # plotting
    xlim <- c(min(cruiseTracks$LogLongitude)-1, 
              max(cruiseTracks$LogLongitude)+1)
    ylim <- c(min(cruiseTracks$LogLatitude)-0.5,
              max(cruiseTracks$LogLatitude)+0.5)
    myMap <- map_data("worldHires", xlim = xlim, ylim = ylim)
    
    p <- ggplot(myMap, aes(long, lat, group = group)) +
      geom_polygon() +
      geom_point( data=SA.df, aes(x=LogLongitude, y=LogLatitude,size=SA),alpha=0.3,shape=1,colour = "blue",inherit.aes = F)+
      geom_path(data=cruiseTracks,aes(x=LogLongitude, y=LogLatitude),colour = "red",inherit.aes = F) +
      geom_text(data=df.hauls, aes( x=haulLon, y=haulLat, label=haulLab),
                angle=45, fontface="bold",inherit.aes = F) +
      ggtitle(uniqueSpeciesAc[idxSpecies]) +
      theme(plot.title = element_text(color="red", size=14, face="bold.italic")) +
      coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
    print(p)
  }
  
    dev.off()
}