createMatrix <- function (row){
  numX <- matrix(row, 28, byrow = T)
  
  colSum <- rep(0, 12)
  colSum <- numX[,1]
  
  rowSum <- rep(0, 12) 
  rowSum <-  numX[1,]
  
  for (i in 10:19){
    rowTemp <- numX[i,]
    colTemp <- numX[,i]
    
    rowTemp <-  rowTemp %% 8.5
    colTemp <-  colTemp %% 8.5
    
    #adj <-  1 / i
    #rowTemp <- adj + rowTemp
    #colTemp <- adj + colTemp
    
    rowSum <- cbind(rowTemp , rowSum)
    colSum <- cbind(colTemp , colSum)
  }
  return(as.vector(cbind(rowSum,colSum)))
}