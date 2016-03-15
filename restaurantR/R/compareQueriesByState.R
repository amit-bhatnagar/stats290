#' Takes two queries from the user and returns the query with more results by state
#' @param query1 First restaurant-related query
#' @param query2 First restaurant-related query
#' @export
#' @examples
#' compareQueriesByState("Pizza Hut","Dominoe's")

compareQueriesByState<-function(query1, query2){
  out <- tryCatch(
    {
      df1 <- getQueryCountByUSState(query1)
      df2 <- getQueryCountByUSState(query2)

      compare <- cbind(df1, df2)

      return(ifelse(compare[,1] > compare[,2],query1, query2))
    },
    error=function(cond) {
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )#tryCatch
  return(out)
}

