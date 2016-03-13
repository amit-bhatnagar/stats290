#'  Given a city name and a cuisine type, provide a list of restaurants that offer catering
#' @param city A city name
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getCateringRestaurants("San Jose","Indian")


getCateringRestaurants<-function(city, cuisine, full = FALSE){

  if(missing(cuisine)){
    cuisine = ""
  }

  city = gsub(" ","+",city)

  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"

  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  cateringFilter = "{\"meal_cater\":{\"$eq\":\"TRUE\"}}"

  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")

  allFilters=paste(cityFilter,cateringFilter,cuisineFilter,sep = ",")

  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
  offset=0

  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=fromJSON(URL, flatten = TRUE)

  if(length(getData$response$data)!=0){
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
  else{
    warning("No restaurants that provide catering with selected cuisine in this location")
  }

}
