#'  Given a point ofinterest, provide a list of nearby restaurants
#' @param pointName A point of interest like "Golden Gate Bridge" or "Empire State Building"
#' @import jsonlite ggmap
#' @export
#' @examples
#' plotRestaurantsNearLocation("Space Needle")


plotRestaurantsNearLocation<-function(pointName){
  limit=20
  offset=0
  out <- tryCatch(
    {
      pointLatLong = geocode(pointName)
      pointLatLong = paste(pointLatLong[2],pointLatLong[1],sep = ",")

      factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

      locURL <- paste0("http://api.v3.factual.com/t/places?geo={\"$point\":[",pointLatLong,"]}")
      # 34.06021,-118.41828]}

      URL=paste0(locURL,"&limit=",limit,"&offset=",offset,"&KEY=",factualAPIKey)

      #    getData <- jsonlite::fromJSON(URL, flatten = TRUE)
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
