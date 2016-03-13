#' Given a city name, a cusisine type and a price-range type, provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param priceRange One of these: "very inexpensive","inexpensive","moderate","expensive","very expensive"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantByPriceRange("San Francisco","moderate","Asian")

getRestaurantByPriceRange<-function(city, priceRange, cuisine, full = FALSE){

  if(missing(cuisine)){
    cuisine = ""
  }

  city = gsub(" ","+",city)
  priceRange = tolower(priceRange)
  supportedPriceRanges =c("very inexpensive","inexpensive","moderate","expensive","very expensive")

  isPriceRangeSupported = any(supportedPriceRanges == priceRange )

  if(!isPriceRangeSupported){
    cat(paste(priceRange,"is not a supported price range \n"))
    cat(paste("Price range should be one of these:\n"))
    cat(paste(supportedPriceRanges,"\n"))

    return()
  }

  #Factual stores price-ranges as numeric values 1: Less than $15 2: $15-30 3: $30-50 4: $50-75 5: $75+
  # We now convert the user-supplied price-range to one of these

  priceRange =  which.max(supportedPriceRanges == priceRange)

  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"

  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  priceFilter = paste0("{\"price\":{\"$eq\":\"",priceRange,"\"}}")

  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
  allFilters=paste(cityFilter,priceFilter,cuisineFilter,sep = ",")

  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
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
    warning("No results for the requested price-range")
  }
}
