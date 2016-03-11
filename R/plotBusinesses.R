plotBusinesses <-function(city, businessDF){
  library(ggmap)
  citymap = get_map(location = city , zoom = 15, maptype = "roadmap" , source = "google")
  
  citymap = ggmap(citymap)
  
  citymap + geom_point(data=businessDF, aes(x=longitude, y=latitude),
                       color = 'blue',
                       size = 8, alpha = .6) +
    geom_text(aes(label=name), data=businessDF, hjust=-1,
              fontface = 'bold',check_overlap = TRUE)
  
}
