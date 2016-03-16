#' Given a city name, a cusisine type and an order type (takeout or delivery),
#' provide a list of restaurants that meet these conditions
#' @param city A city name
#' @param orderType "delivery","takeout"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantsWithDeliveryOrTakeOut("San Francisco","delivery","Indian")

getRestaurantsWithDeliveryOrTakeOut<-function(city, orderType, cuisine, full = FALSE){

  factualAPIKey  <-  factualAPIKey
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  limit <- 20
  offset <- 0

  if(missing(cuisine)){
    cuisine <- ""
  }
  out <- tryCatch(
    {
      city <- gsub(" ","+",city)

      orderType <- tolower(orderType)
      supportedOrderTypes <- c("delivery","takeout")

      isOrderTypeSupported <- any(supportedOrderTypes == orderType )

      if(!isOrderTypeSupported){
        warning(paste(orderType,"is not a supported order type \n"))
        warning(paste("Meal should be one of these:\n"))
        warning(paste(supportedOrderTypes,"\n"))
        #return()
      }

      if(orderType == "delivery")
        orderType="meal_deliver"
      else orderType="meal_takeout"

      USfilter <- "{\"country\":\"US\"}"

      cityFilter <- paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")

      orderTypeFilter <- paste0("{\"",orderType,"\":{\"$eq\":\"TRUE\"}}")

      cuisineFilter <- paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")

      allFilters <- paste(cityFilter,orderTypeFilter,cuisineFilter,sep = ",")

      filters <- paste0("{\"$and\":[",allFilters,"]}")

      URL <- paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)

      getData <- jsonlite::fromJSON(URL, flatten = TRUE)

      if(length(getData$response$data)!=0){

        fullFactualResponse <- as.data.frame(getData$response)

        #Make names more easily understandable by dropping "data." that factual attaches
        names(fullFactualResponse) <- sub("data.", "\\2", names(fullFactualResponse))

        nameLatLong <- data.frame(name=fullFactualResponse$name,
                                  longitude=as.double(fullFactualResponse$longitude),
                                  latitude=as.double(fullFactualResponse$latitude))

        if(full){
          return(fullFactualResponse)
        } else{
          return(nameLatLong)
        }
      }
      else{
        warning("No restaurants with selected cuisine that offer ", orderType)
      }
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
