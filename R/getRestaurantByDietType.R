getRestaurantByDietType<-function(city, dietType){
  
  city = gsub(" ","+",city)
  dietType = tolower(dietType)
  supportedDietTypes =c("vegan","vegetarian","gluttenfree","organic","healthy")
  
  isDietTypeSupported = any(supportedDietTypes == dietType )
  
  if(!isDietTypeSupported){
    cat(paste(dietType,"is not a supported diet type \n"))
    return()
  }

    dietType =paste0("options_",dietType)
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  dietFilter = paste0("{\"",dietType,"\":{\"$eq\":\"TRUE\"}}")
  
  allFilters=paste(cityFilter,dietFilter,sep = ",")
  
  filters=paste0("{\"$and\":[",allFilters,"]}")
  
  limit=20
  offset=0
  
  URL = paste0(baseURL,"filters=",filters,"&KEY=",factualAPIKey)
  getData=fromJSON(URL, flatten = TRUE)
  
  df1 = as.data.frame(getData$response)
  
  df2 = data.frame(name=df1$data.name,longitude=as.double(df1$data.longitude),latitude=as.double(df1$data.latitude))
  df2
}
