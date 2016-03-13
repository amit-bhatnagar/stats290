initializeFactual <- function(key){
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  URL = paste0(baseURL,"&KEY=",key)
  
  out<- tryCatch(
    {
    fromJSON(URL)
    #If no error, set factualAPIKey that other functions can use  
    factualAPIKey <<- key
    },
    error=function(cond) {
    message(cond)
    message("\n Invalid Factual key")  
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
 )  
}

