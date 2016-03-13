
getQuerybyRegion<-function(City, Cuisine){
library(jsonlite)
require(devtools)
factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"


baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

query=paste(City,Cuisine)
URL=paste0(baseURL,"&q=",URLencode(query),"&KEY=",factualAPIKey)


getData=fromJSON(URL, flatten = TRUE)
class(getData$response)
df = as.data.frame(getData$response)

library(ggmap)
City = get_map(City, maptype = "roadmap")
City_map = ggmap(City) 
ggsave(City_map, file = "map8.png", width = 5, height = 5, type = "cairo-png")

City_map + geom_point(data=d, aes(df$data.longitude, df$data.latitude), color="red", size=4, alpha=0.5) 

# +geom_text(aes(label=df$data.name, size=3,vjust=0)))
}