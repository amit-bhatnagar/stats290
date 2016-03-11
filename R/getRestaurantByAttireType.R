getRestaurantByAttireType<-function(city, attireType){
  
  city = gsub(" ","+",city)
  attireType = tolower(attireType)
  supportedAttireTypes =c("streetwear","casual","business casual","smart casual","formal")
  
 
  isAttireTypeSupported = any(supportedAttireTypes == attireType )
  
  if(!isAttireTypeSupported){
    cat(paste(attireType,"is not a supported attire type \n"))
    return()
  }
  
  attireType = gsub(" ","+",attireType)
  
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  attireFilter = paste0("{\"attire\":{\"$eq\":\"",attireType,"\"}}")
  
  allFilters=paste(cityFilter,attireFilter,sep = ",")
  
  filters=paste0("{\"$and\":[",allFilters,"]}")
  
  limit=20
  offset=0 
  
  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=fromJSON(URL, flatten = TRUE)
  
  
  if(length(getData$response$data)!=0){
  df1 = as.data.frame(getData$response)
  df2 = data.frame(name=df1$data.name,longitude=as.double(df1$data.longitude),latitude=as.double(df1$data.latitude))
  df2
  }  else{
  cat("No results with the requested attire type")
  }
}
