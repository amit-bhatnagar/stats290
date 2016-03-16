#' Takes two queries from the user and returns the query with more results by state
#' @param query  query a restaurant-related query like "Subway" or "Indian"
#' @export
#' @import jsonlite plyr ggmap
#' @examples
#' mapQueryCountByUSState ("Chipotle")

mapQueryCountByUSState<-function(query){

  out <- tryCatch(
    {
      factualAPIKey <- factualAPIKey
      baseURL <- "http://api.v3.factual.com/t/restaurants-us/facets?select=region&filters={\"country\":\"US\"}&limit=60"

      queryURLEncoded <- URLencode(query)

      URL <- paste0(baseURL,"&q=",queryURLEncoded,"&KEY=",factualAPIKey)

      getData <- jsonlite::fromJSON(URL, flatten = TRUE)
      dataByRegion <- as.data.frame(getData$response$data$region)

      #Capitalizing State name abbreviations
      names(dataByRegion) <- toupper(names(dataByRegion))

      #Factual names Indiana as "IN.". Renaming this to "IN" for consistency
      colnames(dataByRegion)[which(names(dataByRegion)=="IN.")] <- "IN"

      #Drop DC as it does not appear in the list of 50 states in R
      dataByRegion=dataByRegion[names(dataByRegion) != "DC" ]

      #Not all states may be present in the code,
      #following code gets full list of US states filling 0 for the ones missing in original code

      tempDF <- data.frame(t((tempVal=rep(0,50))))
      names(tempDF) <- state.abb

      dataByRegion <- plyr::rbind.fill(dataByRegion,tempDF)
      dataByRegion <- dataByRegion[,order(names(dataByRegion))]

      #Replace NA by 0
      dataByRegion[is.na(dataByRegion)] <- 0

      dataByRegion <- t(dataByRegion[1,])

      group <- long <- lat <- longitude <- latitude <- NULL

      states <- data.frame(region = tolower(state.name),
                           latitude = state.center$y,
                           longitude = state.center$x)
      rownames(states) <- state.abb
      chainDataWithLatLong <- cbind(states,dataByRegion)
      us_state_map <- map_data('state')

      finalData <- merge(chainDataWithLatLong, us_state_map, by = 'region')

      displayMap <- ggplot(data = finalData, ggplot2::aes(x = long, y = lat, group = group))
      # displayMap <- displayMap + geom_polygon(aes(fill = cut_number(as.numeric(dataByRegion[,1]),5)), show.legend = FALSE)
      displayMap <- displayMap + ggplot2::geom_polygon (show.legend = FALSE)

      displayMap <- displayMap + ggplot2::geom_path(colour = 'goldenrod1', linemitre  = 2)
      displayMap <- displayMap + ggplot2::coord_map()
      displayMap <- displayMap + ggplot2::geom_text(data = chainDataWithLatLong, ggplot2::aes(x = longitude, y = latitude, label = dataByRegion, group=NULL, fontface = "bold"))
      displayMap <- displayMap + ggplot2::theme_bw()
      return(displayMap)
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
