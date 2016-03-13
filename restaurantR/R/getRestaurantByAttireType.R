#' Given a city name, a cusisine type and an attire type, provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param attireType One of these: "streetwear","casual","business casual","smart casual","formal"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantByAttireType("San Francisco","business casual","Asian")

getRestaurantByAttireType<-function(city, attireType, cuisine, full = FALSE){

  if(missing(cuisine)){
    cuisine = ""
  }

  city = gsub(" ","+",city)
  attireType = tolower(attireType)
  supportedAttireTypes =c("streetwear","casual","business casual","smart casual","formal")


  isAttireTypeSupported = any(supportedAttireTypes == attireType )

  if(!isAttireTypeSupported){
    warning(paste(attireType,"is not a supported attire type \n"))
    message(paste("Attire should be one of these:"))
    message(paste(supportedAttireTypes,"\n"))
    return(NA)
  }

  attireType = gsub(" ","+",attireType)

  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"

  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  attireFilter = paste0("{\"attire\":{\"$eq\":\"",attireType,"\"}}")

  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
  allFilters=paste(cityFilter,attireFilter,cuisineFilter,sep = ",")

  filters=paste0("{\"$and\":[",allFilters,"]}")

  offset=0

  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData <- jsonlite::fromJSON(URL, flatten = TRUE)

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
  }  else{
  warning("No results with the requested attire type")
  }
}
