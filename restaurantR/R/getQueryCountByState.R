#' Gets count of restaurant results matching a query by US state
#' @param query a restaurant-related query like "Subway" or "Indian"
#' @import jsonlite plyr
#' @export
#' @examples
#' getQueryCountByUSState("Chipotle")

getQueryCountByUSState<-function(query){
  queryURLEncoded = URLencode(query)
  factualAPIKey = "mKxC6I9lTWnKNTSNF12e3keaWblCXqoaZ1qROdVo"

  baseURL <- "http://api.v3.factual.com/t/places/facets?select=region&filters={\"country\":\"US\"}&limit=60"
  URL=paste0(baseURL,"&q=",queryURLEncoded,"&KEY=",factualAPIKey)

  getData=jsonlite::fromJSON(URL, flatten = TRUE)
  dataByRegion = as.data.frame(getData$response$data$region)

  #Capitalizing State name abbreviations
  names(dataByRegion) = toupper(names(dataByRegion))

  #Factual names Indiana as "IN.". Renaming this to "IN" for consistency
  colnames(dataByRegion)[which(names(dataByRegion)=="IN.")] = "IN"

  #Drop DC as it does not appear in the list of 50 states in R
  dataByRegion=dataByRegion[names(dataByRegion) != "DC" ]

  #Not all states may be present in the code,
  #following code gets full list of US states filling 0 for the ones missing in original code

  tempDF = data.frame(t((tempVal=rep(0,50))))
  names(tempDF)=state.abb


  dataByRegion <- plyr::rbind.fill(dataByRegion,tempDF)
  dataByRegion=dataByRegion[,order(names(dataByRegion))]

  #Replace NA by 0
  dataByRegion[is.na(dataByRegion)] <- 0

  dataByRegion=t(dataByRegion[1,])

  colnames(dataByRegion) = paste0("Count of restaurants matching \"",query,"\" by state")
  dataByRegion
}


