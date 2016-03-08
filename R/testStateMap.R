library(XML)
library(ggplot2)
library(plyr)
library(maps)

unemp <-
  readHTMLTable('http://www.bls.gov/web/laus/laumstrk.htm',
                colClasses = c('character', 'character', 'numeric'))[[2]]

names(unemp) <- c('rank', 'region', 'rate')
unemp$region <- tolower(unemp$region)

us_state_map <- map_data('state')
map_data <- merge(unemp, us_state_map, by = 'region')

map_data <- arrange(map_data, order)

states <- data.frame(state.center, state.abb)

p1 <- ggplot(data = map_data, aes(x = long, y = lat, group = group))
p1 <- ggplot(data = map_data, aes(x = long, y = lat))
p1 <- p1 + geom_polygon(aes(fill ="red"))
p1 <- p1 + geom_path(colour = 'gray', linemitre  = 2)
# p1 <- p1 + scale_fill_brewer('Unemployment Rate (Jan 2011)', palette  = 'PuRd')
p1 <- p1 + coord_map()
p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2)
p1 <- p1 + theme_bw()
p1