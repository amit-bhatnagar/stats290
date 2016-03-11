getSmokingFriendlyRestaurants<-function(city){
  
  city = gsub(" ","+",city)
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  SmokingFilter = "{\"smoking\":{\"$eq\":\"TRUE\"}}"
  
  allFilters=paste(cityFilter,SmokingFilter,sep = ",")
  
  filters=paste0("{\"$and\":[",allFilters,"]}")
  
  limit=20
  offset=0
  
  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=fromJSON(URL, flatten = TRUE)
  
  df1 = as.data.frame(getData$response)
  
  df2 = data.frame(name=df1$data.name,longitude=as.double(df1$data.longitude),latitude=as.double(df1$data.latitude))
  
}
