

#http://developer.factual.com/api-docs/#Read
rm(list = ls())




baseURL = "http://api.v3.factual.com/data/t/restaurants-us#"
# filters="{\"$and\":[{\"region\":{\"$eq\":\"WA\"}},{\"locality\":{\"$eq\":\"Atlanta\"}},{\"cuisine\":{\"$includes\":\"Indian\"}}]}"
filters="{\"$and\":[{\"region\":{\"$eq\":\"WA\"}},{\"locality\":{\"$eq\":\"Atlanta\"}},{\"cuisine\":{\"$includes\":\"Indian\"}}]}"

#filters="{%22$and%22:[{%22region%22:{%22$eq%22:%22WA%22}},{%22locality%22:{%22$eq%22:%22Atlanta%22}},{%22cuisine%22:{%22$includes%22:%22Indian%22}}]}"
filters = URLencode(filters)
URL=paste0(baseURL,"&filters=",filters,"&KEY=",factualAPIKey)
 URL=cat(URL)
getData=fromJSON(URL, flatten = TRUE)
# class(getData$response)

baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Indian,\"Atlanta\"%22offset=20%22limit=30"

baseURL = URLencode(baseURL)
print(baseURL)


URL=paste0(baseURL,"&KEY=",factualAPIKey)


#Mapping code starts here
library(jsonlite)
require(devtools)
factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"


baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

query = "Indian Bellevue"
limit=20
offset=20
URL=paste0(baseURL,"&q=",URLencode(query),"&KEY=",factualAPIKey,"&limit=",limit,"&offset=",offset)
getData=fromJSON(URL, flatten = TRUE)
class(getData$response)
df1 = as.data.frame(getData$response)

library(ggmap)
Atlanta = get_map("Bellevue, WA", maptype =x "roadmap")
Atlanta_map = ggmap(Atlanta) 
ggsave(Atlanta_map, file = "map8.png", width = 5, height = 5, type = "cairo-png")

Atlanta_map + geom_point(data=d, aes(df$data.longitude, df$data.latitude), color="red", size=4, alpha=0.5) 

# +geom_text(aes(label=df$data.name, size=3,vjust=0)))
