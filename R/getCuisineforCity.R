getCuisineforCity<-function(cuisine, city){
  
  cuisine = gsub(" ","+",cuisine)
  city = gsub(" ","+",city)
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")
  
  allFilters=paste(USfilter,cityFilter,cuisineFilter,sep = ",")
  
  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
  offset=0
  URL=paste0(baseURL,"&filters=",filters,"&limit=",limit,"&offset=",offset,"&KEY=",factualAPIKey)

  getData=fromJSON(URL, flatten = TRUE)
  df1 = as.data.frame(getData$response)
     
  df2 = cbind(name=df1$data.name,longitude=df1$data.longitude,latitude=df1$data.latitude)
  df2 = as.data.frame(df2)
  
}






