load_ICESdb <- function(dataPath,fileName){
  ncol <- max(count.fields(file.path(dataPath,fileName), sep = ","))
  
  myTab <- read.csv(file.path(dataPath,fileName), fill = TRUE, header = FALSE,
                    col.names = paste0("V", seq_len(ncol)))
  
  # find headers
  myHeaders <- myTab[myTab[,2] == 'Header',]
  #myHeaders <- myHeaders[myHeaders[,1] == 'Catch',]
  
  outputStr <- 'return(list(as.character(myHeaders[,1])'
  
  for(i in myHeaders[,1]){
    tempTab <- myTab[myTab[,1] == i,]
    
    tempTabNew <- list()
    # filter columns
    count <- 0
    for(idx in 1:length(tempTab[1,])){
      if(is.na(nchar(as.character(tempTab[1,idx]))) == FALSE){
        if(nchar(as.character(tempTab[1,idx])) != 0 ){
          count <- count + 1
          tempTabNew[count] <- as.character(tempTab[1,idx])
        }
      }
    }
    
    tempFrame <- as.data.frame(tempTab[2:dim(tempTab)[1],1:length(tempTabNew)])#,col.names=
    
    for(idx in 1:length(tempTabNew)){
      colnames(tempFrame)[idx] <- tempTabNew[idx]
    }
    tempFrame <- as.data.frame(tempFrame)
    eval(parse(text = paste0(i,'<-tempFrame')))
    
    outputStr <- paste0(outputStr,',',i)
  }
  
  outputStr <- paste0(outputStr,'))')
  
  eval(parse(text = outputStr))
}