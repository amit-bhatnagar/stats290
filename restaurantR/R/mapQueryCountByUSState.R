mapQueryCountByUSState<-function(chainCountByState){

  out <- tryCatch(
    {
      group = long = lat = longitude = latitude = NULL

      names(chainCountByState) = toupper(names(chainCountByState))

      #Factual names Indian as "IN.". Renaming this to "IN" for consistency
      colnames(chainCountByState)[which(names(chainCountByState)=="IN.")] = "IN"


      #Drop DC as it does not appear in the list of 50 states in R
      chainCountByState=chainCountByState[names(chainCountByState) != "DC" ]

      tempDF = data.frame(t((tempVal=rep(0,50))))
      names(tempDF)=state.abb


      chainCountByState <- rbind.fill(chainCountByState,tempDF)
      chainCountByState=chainCountByState[,order(names(chainCountByState))]

      #Replace NA by 0
      chainCountByState[is.na(chainCountByState)] <- 0

      chainCountByState=t(chainCountByState[1,])

      states <- data.frame(region = tolower(state.name),latitude=state.center$y,longitude=state.center$x)
      rownames(states) <-state.abb
      chainDataWithLatLong = cbind(states,chainCountByState)
      us_state_map <- map_data('state')

      finalData <- merge(chainDataWithLatLong, us_state_map, by = 'region')

      displayMap <- ggplot(data = finalData, ggplot2::aes(x = long, y = lat, group = group))
      # displayMap <- displayMap + geom_polygon(aes(fill = cut_number(as.numeric(chainCountByState[,1]),5)), show.legend = FALSE)
      displayMap <- displayMap + ggplot2::geom_polygon (show.legend = FALSE)

      displayMap <- displayMap + ggplot2::geom_path(colour = 'goldenrod1', linemitre  = 2)
      displayMap <- displayMap + ggplot2::coord_map()
      displayMap <- displayMap + ggplot2::geom_text(data = chainDataWithLatLong, ggplot2::aes(x = longitude, y = latitude, label = chainCountByState, group=NULL, fontface = "bold"))
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

