#'  Given a point ofinterest, provide a list of nearby restaurants
#' @param pointName A point of interest like "Golden Gate Bridge" or "Empire State Building"
#' @param category Category name
#' @import jsonlite ggmap
#' @export
#' @examples
#' getRestaurantsNearLocation("Space Needle","Asian")

getRestaurantsNearLocation<-function(pointName, category){
  limit=20
  offset=0
  factualAPIKey = "mKxC6I9ldeTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  out <- tryCatch(
    {
      pointLatLong = ggmap::geocode(pointName)
      pointLatLong = paste(pointLatLong[2],pointLatLong[1],sep = ",")

      locURL <- paste0("http://api.v3.factual.com/t/places?geo={\"$circle\":{\"$center\":[",pointLatLong,"],\"$meters\": 5000}}")

      category = gsub(" ","+",category)
      categoryFilter = paste0("{\"category_labels\":{\"$includes\":\"",category,"\"}}")

      filters=paste0("{\"$and\":[",categoryFilter,"]}")

      URL=paste0(locURL,"&filters=",filters,"&limit=",limit,"&offset=",offset,"&KEY=",factualAPIKey)

      # #    getData <- jsonlite::fromJSON(URL, flatten = TRUE)
      #   df1 = as.data.frame(getData$response)
      #
      #   df2 = cbind(name=df1$data.name,longitude=df1$data.longitude,latitude=df1$data.latitude)
      #   df2 = as.data.frame(df2)

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