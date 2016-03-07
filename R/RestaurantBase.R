#http://developer.factual.com/api-docs/#Read
rm(list = ls())

library(jsonlite)
require(devtools)
factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"


#works baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Indian,seattle"
#Works baseURL <- "http://api.v3.factual.com/t/restaurants-us?filters={\"name\":{\"$bw\":\"star\"}}&include_count=true"
#Do a full-text search of the restaurant database for rows that match the word coffee and the phrase los angeles:
#Works baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,\"Los Angeles\""

#To support paging in your app, return rows 20-40 of the full-text search result above:


#http://api.v3.factual.com/t/restaurants-us?q=Coffee,"Los Angeles"
#query = "Coffee, Seattle"
#URL=paste0(baseURL,"&?q=",query,"&KEY=",factualAPIKey)
#URL=paste0(baseURL,query,"&KEY=",factualAPIKey)

#Works but returns only 20 results
#baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,%22California%22%26offset=0%26limit=30"
#Works but returns only 60 results max
baseURL <- "http://api.v3.factual.com/t/restaurants-us?q=Coffee,\"Seattle\"%22offset=20%22limit=30"

baseURL = URLencode(baseURL)
print(baseURL)

URL=paste0(baseURL,"&KEY=",factualAPIKey)


getData=fromJSON(URL, flatten = TRUE)
class(getData$response)
df = as.data.frame(getData$response)

getQuerybyRegion<-function(query){
  library(jsonlite)
  query = URLencode(query)
  
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us"
  URL=paste0(baseURL,"&q=",query,"&KEY=",factualAPIKey)
  
  getData=fromJSON(URL, flatten = TRUE)
  
  dataByRegion = as.data.frame(getData$response$data$region)
  
  print(dataByRegion)
}

getQuerybyRegion("Seattle") #Does not work HTTP 401

