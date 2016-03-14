#' Plot business on a map from a city name and a date-frame of business name, longitude and latitude
#' @param city A city name
#' @param businessDF a date-frame of business name, longitude and latitude
#' @import ggplot2 ggmap
#' @export
#' @examples
#' plotBusinesses("Mountain View",MountainView_Indian_Alcohol_Friendly)


plotBusinesses <-function(city, businessDF){
  # library(ggmap)

  longitude = latitude = NULL

  citymap = get_map(location = city , zoom = 15, maptype = "roadmap" , source = "google")

  citymap = ggmap(citymap)

  citymap = citymap + ggplot2::geom_point(data=businessDF, ggplot2::aes(x=longitude, y=latitude),
                       color = 'blue',
                       size = 8, alpha = .6)
  #citymap = citymap  +    ggplot2::geom_text(ggplot2::aes(label=name), data=businessDF, hjust=-1,
              # fontface = 'bold',check_overlap = TRUE)
  citymap
}












