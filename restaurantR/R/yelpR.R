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
#' }
#'
#' @examples
#' restaurantObj <- yelpR$new()
#' restaurantObj$InitializeYelp()
#' restaurantDF = restaurantObj$queryData("Bellevue","WA","Chinese")
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
                                NormalizeColumns = function(df)
                                {
                                  drops <- drops <- c("businesses.location")#"businesses.gift_certificates","businesses.deals",
                                  df = df[ , !(names(df) %in% drops)]
                                }

                                ),
                 public = list(
                   InitializeYelp = function(consumerKey="ASkXlev4NL0jpvX2RuUMhw", consumerSecret="F1W_k2qrZRRfKcMXIltlDG1fdEY", token = "vynHw9DBQqeR4y-VEqby0eFce2LiFUM3", token_secret="uflhgBEYKMHe0KXA9POlzgEs9EM")
                   {
                     print("Choose oauthtype")
                     out <- tryCatch(
                       {
                         #private$consumerKey = consumerKey
                         #private$consumerSecret = consumerSecret
                         #private$token = token
                         #private$token_secret = token_secret
                         #Establish connection to yelp
                         print("Connecting to yelp. Choose any of the auth type")
                         myapp <<- oauth_app("YELP", key=consumerKey, secret=consumerSecret)
                         sig <<- sign_oauth1.0(myapp, token=token,token_secret=token_secret)
                         private$consumerKey = consumerKey
                         private$consumerSecret = consumerSecret
                         private$token = token
                         private$token_secret = token_secret
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
                     dflist <- list()
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

                         result.df = as.data.frame(matrix(ncol=26, nrow=200),dup.row.names=TRUE)
                         for(i in 0:9)
                         {
                           limit <- 20
                           offset = i*limit
                           if(city =="any" || state == "any")
                           {
                             if(cusine != "any")
                             {
                               #Cusine specified.
                               if(city != "any") #Use city as the location if it is specified
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"&term=",cusine)
                               else if(state != "any") #use state as the location if it is specified
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",state,"&term=",cusine)
                               else #Default to US as the location since neither city nor state is specified
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=US&term=",cusine)
                             }
                             else
                             {
                               #Cusine is not specified. Default to food as the search term
                               if(city != "any") #Use city as the location if it is specified
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"&term=food")
                               else if(state != "any") #use state as the location if it is specified
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",state,"&term=food")
                               else
                                 yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=US&term=food")
                             }
                           }
                           else
                             yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=",city,"%20",state,"&term=",cusine)
                           yelpurl = URLencode(yelpurl)
                           #Trace print(yelpurl)
                           locationdata=GET(yelpurl, sig)
                           locationdataContent = content(locationdata)
                           locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
                           head(data.frame(locationdataList))
                           dflist[[i+1]] =  (data.frame(locationdataList,dup.row.names=FALSE))
                           row.names(dflist[[i+1]]) = seq(offset+1,offset+limit,1)
                         }
                         #Trace a = sapply(dflist, function(x) print(dim(x)))
                         #Trace print(a)
                         #Trace sapply(dflist, print(dim))
                         dflist = lapply(dflist,private$NormalizeColumns)
                         #Trace a = sapply(dflist, function(x) print(dim(x)))
                         #Trace print(a)
                         dflist = rbind.fill(dflist[[1]],dflist[[2]],dflist[[3]],dflist[[4]],dflist[[5]],dflist[[6]],dflist[[7]],dflist[[8]],dflist[[9]],dflist[[10]])
                         dflist[,"businesses.aggregaterating"] = as.numeric(dflist[,"businesses.rating"])* as.numeric(dflist[,"businesses.review_count"])

                         dflist_sorted = dflist[order(-dflist[,"businesses.aggregaterating"]), ]

                         private$data = dflist_sorted
                         return(private$data)
                         #return(dflist_sorted)
                         #return(dflist)
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





