#' Takes two queries from the user and returns the query with more results by state
#' @param query1 First restaurant-related query
#' @param query2 First restaurant-related query
#' @export
#' @import ggmap
#' @examples
#' plotQueryComparisonByState("Pizza Hut","Dominoe's")
#' plotQueryComparisonByState("Dosa","Naan")

plotQueryComparisonByState<-function(query1, query2){
  #Setting variables to NULL to avoid NOTES in devtools::check
  lat = long = latitude = longitude = group = NULL

  winner = compareQueriesByState(query1, query2)
  color <- ifelse(winner==query1,"red","green")
  winnerDF = as.data.frame(winner)
  winnerDF$color=color

  states <- data.frame(region = tolower(state.name),latitude=state.center$y,longitude=state.center$x)
  rownames(states) <-state.abb
  chainDataWithLatLong = cbind(states,winnerDF)
  us_state_map <- ggplot2::map_data('state')

  finalData <- merge(chainDataWithLatLong, us_state_map, by = 'region')
  displayMap <- ggplot2::ggplot(data = finalData, ggplot2::aes(x = long, y = lat, group = group))
  displayMap <- displayMap + ggplot2::geom_polygon(ggplot2::aes(fill = color), show.legend = FALSE)
  displayMap <- displayMap + ggplot2::geom_path(colour = 'goldenrod1', linemitre  = 2)
  displayMap <- displayMap + ggplot2::coord_map()
  displayMap <- displayMap + ggplot2::geom_text(data = chainDataWithLatLong,
                                       ggplot2::aes(x = longitude, y = latitude, label = winner, group=NULL, fontface = "bold"),
                                       check_overlap = TRUE)
  displayMap <- displayMap + ggplot2::theme_dark()
  displayMap
}
