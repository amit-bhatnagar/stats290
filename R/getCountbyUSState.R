getCountbyUSState<-function(query){
  library(jsonlite)
  query = URLencode(query)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/places/facets?select=region&filters={\"country\":\"US\"}&limit=60"
  URL=paste0(baseURL,"&q=",query,"&KEY=",factualAPIKey)
  
  getData=fromJSON(URL, flatten = TRUE)
  
  dataByRegion = as.data.frame(getData$response$data$region)

}


