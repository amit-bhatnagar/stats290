#' Given a city name, a cusisine type and a diet type, provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param dietType One of diet types: "vegan","vegetarian","gluttenfree","organic","healthy"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantByDietType("Bellevue","Vegetarian","Thai")

getRestaurantByDietType<-function(city, dietType, cuisine, full = FALSE){
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  limit=20
  offset=0

  if(missing(cuisine)){
    cuisine = ""
  }

  out <- tryCatch(
    {
      city = gsub(" ","+",city)
      dietType = tolower(dietType)
      supportedDietTypes =c("vegan","vegetarian","gluttenfree","organic","healthy")

      isDietTypeSupported = any(supportedDietTypes == dietType )

      if(!isDietTypeSupported){
        cat(paste(dietType,"is not a supported diet type \n"))
        cat(paste("Diet type should be one of these:\n"))
        cat(paste(supportedDietTypes,"\n"))

        return(FALSE)
      }

      dietType =paste0("options_",dietType)

      USfilter="{\"country\":\"US\"}"

      cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
      dietFilter = paste0("{\"",dietType,"\":{\"$eq\":\"TRUE\"}}")

      cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
      allFilters=paste(cityFilter,dietFilter,cuisineFilter,sep = ",")

      filters=paste0("{\"$and\":[",allFilters,"]}")

      URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
      getData <- jsonlite::fromJSON(URL, flatten = TRUE)
      fullFactualResponse = as.data.frame(getData$response)

      #Make names more easily understandable by dropping "data." that factual attaches
      names(fullFactualResponse) <- sub("data.", "\\2", names(fullFactualResponse))

      nameLatLong = data.frame(name=fullFactualResponse$name
                               ,longitude=as.double(fullFactualResponse$longitude)
                               ,latitude=as.double(fullFactualResponse$latitude))

      if(full)
        return(fullFactualResponse)
      else
        return(nameLatLong)
    },
    error=function(cond) {
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )#tryCatch
  return(out)
}
