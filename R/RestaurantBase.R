#http://developer.factual.com/api-docs/#Read
rm(list = ls())

library(jsonlite)
require(devtools)
factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
#works baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Indian,seattle"
#Works baseURL <- "http://api.v3.factual.com/t/restaurants-us?filters={\"name\":{\"$bw\":\"star\"}}&include_count=true"
#Does not work baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,\"Los Angeles\""
#Do a full-text search of the restaurant database for rows that match the word coffee and the phrase los angeles:
baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,\"California\""
#To support paging in your app, return rows 20-40 of the full-text search result above:
#Works baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,\"Seattle\"&offset=20&limit=20"


#http://api.v3.factual.com/t/restaurants-us?q=Coffee,"Los Angeles"
#query = "Coffee, Seattle"
#URL=paste0(baseURL,"&?q=",query,"&KEY=",factualAPIKey)
#URL=paste0(baseURL,query,"&KEY=",factualAPIKey)
URL=paste0(baseURL,"&KEY=",factualAPIKey)


getData=fromJSON(URL, flatten = TRUE)
class(getData$response)
df = as.data.frame(getData$response)



