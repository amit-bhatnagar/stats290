#' Given a city name, a cusisine type and a meal type, provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param mealType One of these: "lunch","dinner","breakfast"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantsByMealType("Bellevue","Lunch","Thai")

getRestaurantsByMealType<-function(city, mealType, cuisine, full = FALSE){
  if(missing(cuisine)){
    cuisine = ""
  }
  city = gsub(" ","+",city)

  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  mealType = tolower(mealType)
  supportedMealTypes =c("lunch","dinner","breakfast")

  isMealTypeSupported = any(supportedMealTypes == mealType )

  if(!isMealTypeSupported){
    cat(paste(mealType,"is not a supported meal type \n"))
    cat(paste("Meal should be one of these:\n"))
    cat(paste(supportedMealTypes,"\n"))

    return()
  }

  mealType=paste0("meal_",mealType)

  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"

  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")

  mealTypeFilter = paste0("{\"",mealType,"\":{\"$eq\":\"TRUE\"}}")


  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
  allFilters=paste(cityFilter,mealTypeFilter,cuisineFilter,sep = ",")

  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
  offset=0

  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=jsonlite::fromJSON(URL, flatten = TRUE)

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
