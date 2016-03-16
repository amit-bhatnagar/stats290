##
## Global variable for factualAPIKey. Setting it to a default value
##
# factualAPIKey <- "Ov7qkrDDdAqLwVneSnZZssSwT8nttVb9urqugaDn"
factualAPIKey <- "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

#' Initialize factual and set factualAPIKey as a global variable
#' where it can be accessed by other functions
#' @param key a restaurant-related query like "Subway" or "Indian"
#' @import jsonlite
#' @export
#' @examples
#' initializeFactual("Ov7qkrDDdAqLwVneSnZZssSwT8nttVb9urqugaDn")

initializeFactual <- function(key){


  out<- tryCatch(
    {
      baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
      URL = paste0(baseURL,"&KEY=",key)
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
