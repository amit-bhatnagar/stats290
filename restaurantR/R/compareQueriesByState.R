#' Takes two queries from the user and returns the query with more results by state
#' @param query1 First restaurant-related query
#' @param query2 First restaurant-related query
#' @export
#' @examples
#' compareQueriesByState("Pizza Hut","Dominoe's")

compareQueriesByState<-function(query1, query2){
  df1 <- getQueryCountByUSState(query1)
  df2 <- getQueryCountByUSState(query2)

  compare = cbind(df1, df2)

  ifelse(compare[,1] > compare[,2],query1, query2)
}

