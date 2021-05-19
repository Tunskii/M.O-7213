parseData <- function(data, firstcolumn, noRuns){
    col <- firstcolumn
    
    allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
    cols <- seq(col,noRuns*allstats, by=allstats)
    subdata <- data[,cols]
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      pdata[i,2] = mean(subdata[i,])
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
    }
  
    return (pdata)
}


t1 = parseData(res1,2,10)
t2 = parseData(res2,2,10)
t3 = parseData(res3,2,10)