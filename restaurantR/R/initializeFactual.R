#' Initialize factual and set factualAPIKey as a global variable
#' where it can be accessed by other functions
#' @param key a restaurant-related query like "Subway" or "Indian"
#' @import jsonlite
#' @export
#' @examples
#' initializeFactual("Ov7qkrDDdAqLwVneSnZZssSwT8nttVb9urqugaDn")#mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo

initializeFactual <- function(key){
  #factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  factualAPIKey = "Ov7qkrDDdAqLwVneSnZZssSwT8nttVb9urqugaDn "
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  URL = paste0(baseURL,"&KEY=",key)

  out<- tryCatch(
    {
    jsonlite::fromJSON(URL)
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