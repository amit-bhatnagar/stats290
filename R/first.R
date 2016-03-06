# OAuth Key mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo
# OAuth Secret RYEq7PEtNw4DJsIpguRszJbik3tnYc8mKKk79UX3

getQuerybyRegion<-function(query){
library(jsonlite)
query = URLencode(query)

factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

baseURL <- "http://api.v3.factual.com/t/places/facets?select=region"
URL=paste0(baseURL,"&q=",query,"&KEY=",factualAPIKey)

getData=fromJSON(URL, flatten = TRUE)

dataByRegion = as.data.frame(getData$response$data$region)

 print(dataByRegion)
}


getQuerybyLocality<-function(query){
  library(jsonlite)
  query = URLencode(query)
  
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/places/facets?select=locality"
  
  URL=paste0(baseURL,"&q=",query,"&KEY=",factualAPIKey)
  
  getData=fromJSON(URL, flatten = TRUE)
  dataByLocality = as.data.frame(getData$response$data$locality)
  
  print(dataByLocality)
}
