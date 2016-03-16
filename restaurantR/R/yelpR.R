#rm(list = ls())
#require(httr)
#require(jsonlite)
#require(plyr)
require(R6)
#' Create a \code{yelpR} object
#'
#' @description \code{yelpR} is a restaurant object that encapsulates the data returned from yelp.
#' A yelp restaurant data object consisists of 2 main components, connection settings and data settings. connection settings
#' include the keys that are needed to authenticate with yelp. The data object stores the restaurant results that match a
#' specific criteria.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import httr jsonlite plyr
#' @field consumerKey consumerKey used to establish a yelp connection
#' @field consumerSecret consumerSecret used to establish a yelp c
#' onnection
#' @field token token used to establish a yelp connection
#' @field token_secret token_secret used to establish a yelp connection
#' @field data The most recent restaurant data set that queried by the user displaying top restaurants to analyze at the top. Two type of queries are supported - restaurant data
#' location and cusine type or
#' location, cusine and deals avaialble
#' We apply a sorting algorithm to sort the results based on both the review and rating of each restaurant
#' taking into account both the review and rating of each restaurant. Thereby the data from there by the user. Typically the data set contains 27
#' columns defined by yelp that describe information about the restaurant viz.
#' "region.span.latitude_delta"      "region.span.longitude_delta"     "region.center.latitude"
#' "region.center.longitude"         "total"                           "businesses.is_claimed"
#' "businesses.rating"               "businesses.mobile_url"           "businesses.rating_img_url"
#' "businesses.review_count"         "businesses.name"                 "businesses.rating_img_url_small"
#' "businesses.url"                  "businesses.categories"           "businesses.phone"
#' "businesses.snippet_text"         "businesses.image_url"            "businesses.snippet_image_url"
#' "businesses.display_phone"        "businesses.rating_img_url_large" "businesses.id"
#' "businesses.is_closed"            "businesses.menu_date_updated"    "businesses.menu_provider"
#' "dup.row.names"                   "businesses.gift_certificates"    "businesses.deals"
#' The user then queries this data frame for further results depending on the nature of the task
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{yelpR$new()}}{Creates a empty \code{yelpR}
#'         object which is then initialized}
#'   \item{\code{InitializeYelp(consumerKey,consumerSecret,token,token_secret)}}{Authenticates the user to establish a yelp connection.
#'          Returns true if the conntection was successfully established. otherwise displays an error with return value NA. Warning
#'          messages are displayed with error value NULL.}
#'   \item{\code{getData()}}{Returns the restaurant data of the last issued query.}
#'   \item{\code{queryData(city, state, cusine)}}{Returns restaurant data as a data frame based on the filter criteria specified.
#'                InitializeYelp should return true for this method to be successful.}
#'    \item{\code{shortlistRestaurants(city, state, cusine)}}{Returns 6 shortlisted restaurants as a data frame based on the filter criteria specified.
#'                InitializeYelp should return true for this method to be successful. Basic information about the restaurant such as name, ph, url,
#'                rating and review are avaialble for the user to analyze.}
#'    \item{\code{getRestaurantWithDeals(city, state, cusine)}}{Returns restaurants that offer deals based on the filter criteria specified.
#'                InitializeYelp should return true for this method to be successful. All information about the restaurant is returned for the usser
#'                to analyze }
#'
#' }
#'
#' @examples
#' restaurantObj <- yelpR$new()
#' restaurantObj$InitializeYelp()
#' restaurantDF = restaurantObj$queryData("Stanford","CA","Indian")
#' head(restaurantDF)$businesses.name
#'
#' restaurantDF = restaurantObj$queryData("San Francisco","CA","Spanish")
#' head(restaurantDF)$businesses.name
#'
#' restaurantDF = restaurantObj$shortlistRestaurants("Napa Valley","CA","Italian")
#' print(restaurantDF)
#' print("Take your significant other, goto these places and Enjoy !")
#'
#' restaurantDF = restaurantObj$getRestaurantWithDeals("San Francisco","CA","Indian")
#' head(restaurantDF)$businesses.name
#'
#'
#' @export
#' @format An \code{\link{R6Class}} generator object

yelpR <- R6Class("yelpR",
                 private = list(
                                data = NA,
                                consumerKey = NA,
                                consumerSecret = NA,
                                token = NA,
                                token_secret = NA,
                                flatten = function(col)
                                {
                                  address<-c()
                                  for (j in 1:length(col))
                                  {
                                    l<-length(col[[j]])
                                    str <- ""
                                    for (k in 1:l)
                                    {
                                      str <- paste0(str,col[[j]][k],sep=", ")
                                    }
                                    address[j] <- str
                                  }
                                  #Remove trailing ", "
                                  address <- substr(address, 1, nchar(address)-2)
                                  address
                                },
                                NormalizeColumns = function(df)
                                {
                                  df[1:nrow(df),"display_address"] = private$flatten(df[,"businesses.location"]$display_address)
                                  df[1:nrow(df),"city"] = unlist(df[,"businesses.location"]$city)
                                  df[1:nrow(df),"neighborhoods"] = private$flatten(df[,"businesses.location"]$neighborhoods)
                                  df[1:nrow(df),"cross_streets"] = private$flatten(df[,"businesses.location"]$cross_streets)
                                  drops <- drops <- c("businesses.location")#"businesses.gift_certificates","businesses.deals",
                                  df <- df[ , !(names(df) %in% drops)]
                                }
                                ),
                 public = list(
                   InitializeYelp = function(consumerKey="ASkXlev4NL0jpvX2RuUMhw", consumerSecret="F1W_k2qrZRRfKcMXIltlDG1fdEY", token = "vynHw9DBQqeR4y-VEqby0eFce2LiFUM3", token_secret="uflhgBEYKMHe0KXA9POlzgEs9EM")
                   {
                     print("Choose oauthtype, ")
                     out <- tryCatch(
                       {
                         #Establish connection to yelp
                         print("Connecting to yelp.  If you are establishing yelp connection for first time,at the prompt to choose an auth type, select option 1. Otherwise you are good to go.")
                         myapp <- oauth_app("YELP", key=consumerKey, secret=consumerSecret)
                         sig <- sign_oauth1.0(myapp, token=token,token_secret=token_secret)
                         private$consumerKey <- consumerKey
                         private$consumerSecret <- consumerSecret
                         private$token <- token
                         private$token_secret <- token_secret
                         return(TRUE)
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
                       },
                       finally={
                         # NOTE:
                         # Here goes everything that should be executed at the end,
                         # regardless of success or error.
                         # If you want more than one expression to be executed, then you
                         # need to wrap them in curly brackets ({...}); otherwise you could
                         # just have written 'finally=<expression>'
                         message("Processed yelp connection")
                       }
                     )
                     return(out)
                   },

                   getData = function() private$data,
                   #This method search for restaurants pertaining to a specific cusine in a given location
                   #If city or state is not specified, ressults default to United States
                   #If cusine is not specified, all restaurants that serve food as returned by yelp will be retrieved
                   queryData = function(city = "any", state ="any", cusine="any")
                   {
                     restaurantDF <- list()
                     out <- tryCatch(
                       {
                         #Handle numeric input
                         if(is.numeric(city) || is.numeric(state) || is.numeric(cusine))
                         {
                           stop("Error: Input parameters needs to be string, You have specified numeric input")
                         }
                         #NULL handle
                         if(is.null(city) || is.na(city) || is.nan(city))
                           city = "any"
                         if(is.null(state) || is.na(state) || is.nan(state))
                           state = "any"
                         if(is.null(cusine) || is.na(cusine) || is.nan(cusine))
                           cusine = "any"

                         for(i in 0:9)
                         {
                           limit <- 20
                           offset <- i*limit
                           city = URLencode(city)
                           if(city =="any" || state == "any")
                           {
                             if(cusine != "any")
                             {
                               #Cusine specified.
                               if(city != "any") #Use city as the location if it is specified
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"&term=",cusine)
                               else if(state != "any") #use state as the location if it is specified
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",state,"&term=",cusine)
                               else #Default to US as the location since neither city nor state is specified
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=US&term=",cusine)
                             }
                             else
                             {
                               #Cusine is not specified. Default to food as the search term
                               if(city != "any") #Use city as the location if it is specified
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"&term=food")
                               else if(state != "any") #use state as the location if it is specified
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",state,"&term=food")
                               else
                                 yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=US&term=food")
                             }
                           }
                           else
                           {
                             yelpURL <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"%20",state,"&term=",cusine)
                           }
                           yelpURL <- URLencode(yelpURL)
                           #Trace print(yelpURL)
                           yelpData <- GET(yelpURL, sig)
                           yelpDataContent <- content(yelpData)
                           yelpDataList <- jsonlite::fromJSON(toJSON(yelpDataContent))
                           #TRACE head(data.frame(yelpDataList))
                           restaurantDF[[i+1]] <-  (data.frame(yelpDataList,dup.row.names=FALSE))
                           row.names(restaurantDF[[i+1]]) <- seq(offset+1,offset+limit,1)
                         }
                         #Trace a = sapply(restaurantDF, function(x) print(dim(x)))
                         #Trace print(a)
                         #Trace sapply(restaurantDF, print(dim))
                         restaurantDF <- lapply(restaurantDF,private$NormalizeColumns)
                         #Trace a = sapply(restaurantDF, function(x) print(dim(x)))
                         #Trace print(a)
                         restaurantDF <- rbind.fill(restaurantDF[[1]],restaurantDF[[2]],restaurantDF[[3]],restaurantDF[[4]],restaurantDF[[5]],restaurantDF[[6]],restaurantDF[[7]],restaurantDF[[8]],restaurantDF[[9]],restaurantDF[[10]])
                         restaurantDF[,"businesses.aggregaterating"] <- as.numeric(restaurantDF[,"businesses.rating"])* as.numeric(restaurantDF[,"businesses.review_count"])

                         restaurantDF_sorted <- restaurantDF[order(-restaurantDF[,"businesses.aggregaterating"]), ]

                         private$data <- restaurantDF_sorted
                         return(private$data)
                         #return(restaurantDF_sorted)
                         #return(restaurantDF)
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
                   },
                   #This method shortlists restaurants pertaining to a specific cusine in a given location
                   #It retrieves 6 restaurants that match the specified criteria displaying necessary information such as the name, ph, url, rating
                   # and review of the restaurant
                   #If city or state is not specified, ressults default to United States
                   #If cusine is not specified, all restaurants that serve food as returned by yelp will be retrieved
                   shortlistRestaurants = function(city = "any", state ="any", cusine="any")
                   {
                     out <- tryCatch(
                       {
                         city = URLencode(city)
                         state = URLencode(state)
                         cusine = URLencode(cusine)

                         restaurantDF = self$queryData(city, state, cusine)
                         restaurantDF = head(restaurantDF)
                         shortlistColumns <- c("businesses.name", "display_address", "businesses.display_phone", "businesses.url", "businesses.rating","businesses.review_count")
                         restaurantDF <- restaurantDF[shortlistColumns]
                         private$data <- restaurantDF
                         return(private$data)
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
                   },
                   getRestaurantWithDeals = function(city = "any", state ="any", cusine="any")
                   {
                     out <- tryCatch(
                       {
                         city = URLencode(city)
                         state = URLencode(state)
                         cusine = URLencode(cusine)

                         restaurantDF = self$queryData(city, state, cusine)
                         restaurantDF = restaurantDF[!(is.null(restaurantDF$businesses.deals) | restaurantDF$businesses.deals == 'NULL'), ]
                         private$data <- restaurantDF
                         return(private$data)
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
                   )
)





