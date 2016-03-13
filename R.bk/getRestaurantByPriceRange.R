getRestaurantByPriceRange<-function(city, priceRange){
  
  city = gsub(" ","+",city)
  priceRange = tolower(priceRange)
  supportedPriceRanges =c("very inexpensive","inexpensive","moderate","expensive","very expensive")
  
  
  isPriceRangeSupported = any(supportedPriceRanges == priceRange )
  
  if(!isPriceRangeSupported){
    cat(paste(priceRange,"is not a supported price range \n"))
    cat(paste("Price range should be one of these:\n"))
    cat(paste(supportedPriceRanges,"\n"))
    
    return()
  }
  
  #Factual stores price-ranges as numeric values 1: Less than $15 2: $15-30 3: $30-50 4: $50-75 5: $75+ 
  # We now convert the user-supplied price-range to one of these 
  
  priceRange =  which.max(supportedPriceRanges == priceRange)

  library(jsonlite)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"
  
  baseURL <- "http://api.v3.factual.com/t/restaurants-us?"
  
  USfilter="{\"country\":\"US\"}"
  
  cityFilter = paste0("{\"locality\":{\"$eq\":\"",city,"\"}}")
  priceFilter = paste0("{\"price\":{\"$eq\":\"",priceRange,"\"}}")
  
  allFilters=paste(cityFilter,priceFilter,sep = ",")
  
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
    cat("No results for the requested price-range")
  }
}
