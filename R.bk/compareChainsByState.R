compareChainsByState<-function(query1, query2){
  library(plyr)
  df1 <- getCountbyUSState(query1)
  df2 <- getCountbyUSState(query2)
  
  names(df1)  <-  toupper(names(df1))
  names(df2)  <-  toupper(names(df2))
  
  df1 <- df1[,order(names(df1))]
  df2 <- df2[,order(names(df2))]
  
  
  #rbind the two results.. If any state is missing for one, rbind.fill still allows us to rbind column, filling NA for the missing values
  compare <- rbind.fill(df1,df2)
  
  #Replace NA by 0 
  compare[is.na(compare)] <- 0
  
  #Transpose
  t(ifelse(compare[1,] > compare[2,],query1, query2))
}

