#'  Given a point ofinterest, provide a list of nearby restaurants
#' @param pointName A point of interest like "Golden Gate Bridge" or "Empire State Building"
#' @param cuisine Cuisine name like "Indian", "Thai" etc.
#' @import jsonlite ggmap
#' @export
#' @examples
#' plotRestaurantsNearLocation("Stanford University", "Pizza")


plotRestaurantsNearLocation<-function(pointName,cuisine){
  limit=20
  offset=0
  out <- tryCatch(
    {
      if(missing(cuisine)){
        cuisine = ""
      }

      pointLatLong = ggmap::geocode(pointName)
      pointLatLong = paste(pointLatLong[2],pointLatLong[1],sep = ",")

      factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

      baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
      geoFilters <- paste0("geo={\"$circle\":{\"$center\":[",pointLatLong,"],\"$meters\": 5000}}")

      locURL <- paste0(baseURL,geoFilters)

      cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
      allFilters=paste(cuisineFilter,sep = ",")

      filters=paste0("{\"$and\":[",allFilters,"]}")

      URL=paste0(locURL,"&filters=",filters, "&limit=",limit,"&offset=",offset,"&KEY=",factualAPIKey)

      getData <- jsonlite::fromJSON(URL, flatten = TRUE)

        if(length(getData$response$data)!=0){

          fullFactualResponse = as.data.frame(getData$response)

          #Make names more easily understandable by dropping "data." that factual attaches
          names(fullFactualResponse) <- sub("data.", "\\2", names(fullFactualResponse))

          nameLatLong = data.frame(name=fullFactualResponse$name
                                   ,longitude=as.double(fullFactualResponse$longitude)
                                   ,latitude=as.double(fullFactualResponse$latitude))

          bbox <- ggmap::make_bbox(nameLatLong$longitude, nameLatLong$latitude, f = 0.1)

          citymap = get_map(location = bbox , maptype = "hybrid" , source = "osm")

          citymap = ggmap(citymap)

          citymap = citymap + ggplot2::geom_point(data=nameLatLong, ggplot2::aes(x=longitude, y=latitude),
                                                  color = 'blue',
                                                  shape = 18,
                                                  size = 8, alpha = 0.6)

          #Renaming as a work-around to a known ggplot2 issue
          names(nameLatLong) = c("name","lon","lat")
          citymap = citymap  +    ggplot2::geom_text(ggplot2::aes(label=name), data=nameLatLong, hjust= - 0.05,
                                                     fontface = 'bold',color = "blue",
                                                     size = 5, check_overlap = TRUE)
        }  else{
          warning("No results found near ", pointName)
        }

    },
    error=function(cond) {
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
#     ,
#     warning=function(cond) {
#       message(cond)
#       # Choose a return value in case of warning
#       return(NULL)
#     }
  )#tryCatch
  return(out)
}
