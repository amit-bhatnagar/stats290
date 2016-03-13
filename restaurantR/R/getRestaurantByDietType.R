#' Given a city name, a cusisine type and a diet type, provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param dietType One of diet types: "vegan","vegetarian","gluttenfree","organic","healthy"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getKidsFriendlyRestaurants("New York","Thai")

getRestaurantByDietType<-function(city, dietType, cuisine, full){

  if(missing(cuisine)){
    cuisine = ""
  }
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

  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"

  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  dietFilter = paste0("{\"",dietType,"\":{\"$eq\":\"TRUE\"}}")

  allFilters=paste(cityFilter,dietFilter,sep = ",")

  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
  offset=0

  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=fromJSON(URL, flatten = TRUE)
  fullFactualResponse = as.data.frame(getData$response)

  #Make names more easily understandable by dropping "data." that factual attaches
  names(fullFactualResponse) <- sub("data.", "\\2", names(fullFactualResponse))

  nameLatLong = data.frame(name=fullFactualResponse$name
                           ,longitude=as.double(fullFactualResponse$longitude)
                           ,latitude=as.double(fullFactualResponse$latitude))

  if(full)
    fullFactualResponse
  else
    nameLatLong

}
