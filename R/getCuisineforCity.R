getCuisineforCity<-function(cuisine, City){

  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"

  USfilter="{\"country\":\"US\"}"
  
  # {"locality":{"$eq":"NEW+YORK"}}
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",City,"\"}}")
  cuisineFilter = paste0("{\"cuisine\":{\"$includes\":\"",cuisine,"\"}}")

  
    allFilters=paste(USfilter,cityFilter,cuisineFilter,sep = ",")
  
  filters=paste0("{\"$and\":[",allFilters,"]}")

  limit=20
  offset=0
  URL=paste0(baseURL,"&filters=",filters,"&limit=",limit,"&offset=",offset,"&KEY=",factualAPIKey)
  cat(URL)
  
  
  
 getData=fromJSON(URL, flatten = TRUE)
 df1 = as.data.frame(getData$response)
   
 df2 = cbind(df1$data.name,df1$data.longitude,df1$data.latitude)
}






