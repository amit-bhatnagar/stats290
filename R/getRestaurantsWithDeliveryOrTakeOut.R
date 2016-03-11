getRestaurantsWithDeliveryOrTakeOut<-function(city, orderType){
  
  city = gsub(" ","+",city)
  
  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  orderType = tolower(orderType)
  supportedOrderTypes =c("delivery","takeout")
  
  isOrderTypeSupported = any(supportedOrderTypes == orderType )
  
  if(!isOrderTypeSupported){
    cat(paste(orderType,"is not a supported order type \n"))
    cat(paste("Meal should be one of these:\n"))
    cat(paste(supportedOrderTypes,"\n"))
    
    return()
  }
  
  if(orderType == "delivery")
     orderType="meal_deliver"
  else orderType="meal_takeout"

  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  
  orderTypeFilter = paste0("{\"",orderType,"\":{\"$eq\":\"TRUE\"}}")
  
  
  allFilters=paste(cityFilter,orderTypeFilter,sep = ",")
  
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
