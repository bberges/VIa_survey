mk_length_matrix <- function(Biology){
  
  haulNumber    <- as.numeric(as.character(unique(Biology$HaulNumber)))
  haulNumber    <- haulNumber[order(haulNumber)]
  
  edgesLength  <- seq(from = -5, to = 405, by = 10)
  centerLength <- seq(from = 0, to = 400, by = 10)
  
  outMat <- array(  NA,
                    dim=c(length(edgesLength)-1,
                          length(haulNumber)))
  
  for(idxHaul in 1:length(haulNumber)){
    BiologyTemp <- Biology[Biology$HaulNumber == haulNumber[idxHaul],]
    length <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))
    
    histInfo <- hist(length,
                     edgesLength,
                     plot=FALSE)
    outMat[,idxHaul] <- histInfo$counts
  }
  
  outMat <- as.data.frame(outMat)
  colnames(outMat) <- haulNumber
  rownames(outMat) <- centerLength
  
  return(outMat)
}