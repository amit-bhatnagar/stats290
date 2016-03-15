#' Plot business on a map from a city name and a date-frame of business name, longitude and latitude
#' @param businessDF a date-frame of business name, longitude and latitude
#' @import ggplot2 ggmap
#' @export
#' @examples
#' plotBusinesses(MountainView_Indian_Alcohol_Friendly)

plotBusinesses <-function(businessDF){
  out <- tryCatch(
    {
      longitude = latitude = NULL

      bbox <- ggmap::make_bbox(businessDF$longitude, businessDF$latitude, f = 0.1)

      citymap = get_map(location = bbox , maptype = "roadmap" , source = "google")

      citymap = ggmap(citymap)

      citymap = citymap + ggplot2::geom_point(data=businessDF, ggplot2::aes(x=longitude, y=latitude),
                                              color = 'red',
                                              size = 4, alpha = .6)
      #citymap = citymap  +    ggplot2::geom_text(ggplot2::aes(label=name), data=businessDF, hjust=-1,
      # fontface = 'bold',check_overlap = TRUE)

#https://rstudio-pubs-static.s3.amazonaws.com/10840_89eda656b4494a609dbba609cda0830e.html
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












