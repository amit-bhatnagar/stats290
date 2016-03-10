
testComparisonByState<-function(){

library(ggmap)

query1 = "Subway"
query2 = "Chipotle"

testRand=runif(50)
Winner = ifelse(testRand<0.3, query1,query2)
names(Winner) = state.name
color <- ifelse(Winner==query1,"red","green")
mydf = as.data.frame(Winner)
mydf$color=color


states <- data.frame(region = tolower(state.name),latitude=state.center$y,longitude=state.center$x)
rownames(states) <-state.abb
chainDataWithLatLong = cbind(states,mydf)
us_state_map <- map_data('state')


finalData <- merge(chainDataWithLatLong, us_state_map, by = 'region')
p1 <- ggplot(data = finalData, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon(aes(fill = color), show.legend = FALSE)
p1 <- p1 + geom_path(colour = 'goldenrod1', linemitre  = 2)
p1 <- p1 + coord_map()
p1 <- p1 + geom_text(data = chainDataWithLatLong, aes(x = longitude, y = latitude, label = Winner, group=NULL, fontface = "bold"))
p1 <- p1 + theme_bw()
p1
}
