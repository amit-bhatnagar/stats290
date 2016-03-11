getRestaurantsByMealType<-function(city, mealType){
  
  city = gsub(" ","+",city)
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  mealType = tolower(mealType)
  supportedMealTypes =c("lunch","dinner","breakfast")
  
  isMealTypeSupported = any(supportedMealTypes == mealType )
  
  if(!isMealTypeSupported){
    cat(paste(mealType,"is not a supported meal type \n"))
    cat(paste("Meal should be one of these:\n"))
    cat(paste(supportedMealTypes,"\n"))
    
    return()
  }
  
  mealType=paste0("meal_",mealType)
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  
  mealTypeFilter = paste0("{\"",mealType,"\":{\"$eq\":\"TRUE\"}}")
  
  
  allFilters=paste(cityFilter,mealTypeFilter,sep = ",")
  
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
    cat("No restaurants found that provide the selected meal type")
  }
  
}
