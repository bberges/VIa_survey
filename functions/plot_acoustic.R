plot_acoustic <- function(Data,Cruise,Haul,figurePath){
  
  country       <- as.character(Cruise$CruiseCountry)
  cruiselocalID <- as.character(Cruise$CruiseLocalID)
  
  #idxFiltData <- as.character(Data$DataSaCategory) %in% c('HER', 'SPR', 'CLU','MIX') # ,'CLU','NOP','MAC','SAN'
  #Data <- Data[idxFiltData,]
  
  uniqueSpecies <- as.character(unique(Data$DataSaCategory))
  nSpecies      <- length(uniqueSpecies)
  
  #### Step 2.1 - cruisetracks #### 
  # setup the cruite tracks
  cruiseTracks              <- Data
  
  cruiseTracks <- as.data.frame(cbind(  as.numeric(as.character(cruiseTracks$LogLatitude)),
                                        as.numeric(as.character(cruiseTracks$LogLongitude)),
                                        as.numeric(as.character(cruiseTracks$LogDistance))))
  
  colnames(cruiseTracks) <- c('LogLatitude','LogLongitude','LogDistance')
  
  agg <- aggregate(cruiseTracks,
                  by = list(cruiseTracks$LogDistance),
                  FUN = max)
  
  png(file.path(figurePath,paste0('acoustic_',country,'_cruisetracks_map_',cruiselocalID,'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
  
  map('worldHires', 
      col = "green", 
      fill = TRUE, 
      xlim = c(min(agg$LogLongitude)-2, 
               max(agg$LogLongitude)+2), 
      ylim = c(min(agg$LogLatitude)-2, 
               max(agg$LogLatitude)+2))
  box()
  axis(side = 2, las = 2)
  points(agg$LogLongitude, 
         agg$LogLatitude, 
         pch = 46, col = "red")
  legend("topleft", legend = 'cruise tracks realised', bty = "n")
  
  dev.off()
  
  #### Step 2.2 - sA-values on cruisetrack #### 
  
  # loop on each species
  for(idxSpecies in 1:nSpecies){ ## set relevant species here e.g. c('WHB', 'HER',...)
    currentSpecies  <- uniqueSpecies[idxSpecies]
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
    
    png(file.path(figurePath,paste0('acoustic_',country,'_SA_map_',currentSpecies,'_',cruiselocalID,'.png')), width = 30, height = 30, units = "cm", res = 300, pointsize = 10)
    
    map('worldHires', 
        col = "green", 
        fill = TRUE, 
        xlim = c(min(cruiseTracks$LogLongitude)-0.5, 
                 max(cruiseTracks$LogLongitude)+0.5), 
        ylim = c(min(cruiseTracks$LogLatitude)-0.5, 
                 max(cruiseTracks$LogLatitude)+0.5))
    box()
    axis(side = 2, las = 2)
    points(SA.df$LogLongitude, SA.df$LogLatitude, cex = log(SA.df$SA)/2, col = "darkgrey")
    points(cruiseTracks$LogLongitude, cruiseTracks$LogLatitude, pch = 46, col = "red")
    haulLon <- as.numeric(as.character(Haul$HaulStartLongitude))
    haulLat <- as.numeric(as.character(Haul$HaulStartLatitude))
    haulLab <- as.character(Haul$HaulNumber)
    points(haulLon, haulLat, pch = 21,col = "black")
    text(x = haulLon, y = haulLat, haulLab, pos = 1)
    #draw.shape(coast, col = "darkgreen")
    legend("topleft", legend = uniqueSpecies[idxSpecies], bty = "n")
    
    dev.off()
  }
}