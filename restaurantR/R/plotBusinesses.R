#' Plot business on a map from a city name and a date-frame of business name, longitude and latitude
#' @param nameLatLong a date-frame of business name, longitude and latitude
#' @import ggplot2 ggmap
#' @export
#' @examples
#' plotBusinesses(MountainView_Indian_Alcohol_Friendly)

plotBusinesses <-function(nameLatLong){
  out <- tryCatch(
    {
      longitude <- latitude <- NULL

      bbox <- ggmap::make_bbox(nameLatLong$longitude, nameLatLong$latitude)

      citymap <- get_map(location = bbox , maptype = "roadmap" , source = "osm")

      citymap <- ggmap(citymap)

      citymap <- citymap + ggplot2::geom_point(data=nameLatLong, ggplot2::aes(x=longitude, y=latitude),
                                              color = 'blue',
                                              shape = 18,
                                              size = 8, alpha = 0.6)


      #Renaming as a work-around to a known ggplot2 issue

      names(nameLatLong) <- c("name","lon","lat")
      citymap <- citymap  +    ggplot2::geom_text(ggplot2::aes(label=name), data=nameLatLong, hjust= - 0.05,
                                                 fontface = 'bold',color = "blue",
                                                 size = 5, check_overlap = TRUE)

      citymap
    },
    error=function(cond) {
      message(cond)

      # Choose a return value in case of error
      return(NA)
    }

  )#tryCatch
  return(out)
}












