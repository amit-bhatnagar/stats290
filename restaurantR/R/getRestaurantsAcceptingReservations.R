#'  Given a city name and a cusisine type, provide a list of restaurants that accept reservations
#' @param city A city name
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @param full if TRUE returns full factual response. Default is FALSE,
#'        which results in a smaller data frame with just restaurant name, latitude and longitude
#' @import jsonlite
#' @export
#' @examples
#' getRestaurantsAcceptingReservations("San Francisco","Asian")


getRestaurantsAcceptingReservations<-function(city, cuisine, full = FALSE){
  # factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  factualAPIKey <- "Ov7qkrDDdAqLwVneSnZZssSwT8nttVb9urqugaDn"
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  limit <- 20
  offset <- 0

  if(missing(cuisine)){
    cuisine <- ""
  }

  out <- tryCatch(
    {
      city <- gsub(" ","+",city)

      USfilter <- "{\"country\":\"US\"}"

      cityFilter <- paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
      reservationFilter <- "{\"reservations\":{\"$eq\":\"TRUE\"}}"


      cuisineFilter <- paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")

      allFilters <- paste(cityFilter,reservationFilter,cuisineFilter,sep = ",")

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
        warning("No restaurants with selected cuisine that accept reservations in this location")
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
