rm(list = ls())
require(httr)
require(jsonlite)
require(plyr)
require(R6)

YelpRestaurantData <- R6Class("YelpRestaurantData",
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
                   setData = function(data) {
                     #if (!private$valid(private$symbol, data)) {
                     #   stop("Bad data!")
                     #}
                     private$data <- data
                   },
                   GetYelpData = function()
                   {
                     dflist <- list()
                     result.df = as.data.frame(matrix(ncol=26, nrow=200),dup.row.names=TRUE)
                     for(i in 0:9)
                     {
                       limit <- 20
                       offset = i*limit
                       yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&offset=",offset,"&location=Chicago%20IL&term=bar")
                       locationdata=GET(yelpurl, sig)
                       locationdataContent = content(locationdata)
                       locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
                       head(data.frame(locationdataList))
                       dflist[[i+1]] =  (data.frame(locationdataList,dup.row.names=FALSE))
                       row.names(dflist[[i+1]]) = seq(offset+1,offset+limit,1)
                       #if(i == 0)
                       #  result.df = (data.frame(locationdataList))
                       #else
                       #rbind(result.df,(data.frame(locationdataList)))
                     }
                     a = sapply(dflist, function(x) print(dim(x)))
                     print(a)
                     sapply(dflist, print(dim))
                     dflist = lapply(dflist,private$NormalizeColumns)
                     a = sapply(dflist, function(x) print(dim(x)))
                     print(a)
                     #dflist = lapply(dflist,rbind.fill)
                     #do.call(rbind.fill, dflist)
                     dflist = rbind.fill(dflist[[1]],dflist[[2]],dflist[[3]],dflist[[4]],dflist[[5]],dflist[[6]],dflist[[7]],dflist[[8]],dflist[[9]],dflist[[10]])
                     
                     #return(dflist)
                     private$data = dflist
                     return(private$data)
                   }
                   )
)
YelpObj <- YelpRestaurantData$new()
YelpObj
YelpObj$InitializeYelp()
dflist = YelpObj$GetYelpData()
dflist = YelpObj$GetYelpData()
