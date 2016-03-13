# yelp
consumerKey = "ASkXlev4NL0jpvX2RuUMhw"
consumerSecret = "F1W_k2qrZRRfKcMXIltlDG1fdEY"
token = "vynHw9DBQqeR4y-VEqby0eFce2LiFUM3"
token_secret = "uflhgBEYKMHe0KXA9POlzgEs9EM"

require(httr)
#require(httpuv)
require(jsonlite)
# authorization
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

limit <- 10

# 10 bars in Chicago
yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&location=Chicago%20IL&term=bar")
# or 10 bars by geo-coordinates
#yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&ll=37.788022,-122.399797&term=bar")

locationdata=GET(yelpurl, sig)
locationdataContent = content(locationdata)
locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
head(data.frame(locationdataList))
result.df = (data.frame(locationdataList))

#http://stackoverflow.com/questions/17748566/how-can-i-turn-an-r-data-frame-into-a-simple-unstyled-html-table
# install.packages("xtable")
library("xtable")
sample_table <- mtcars[1:3,1:3]
print(xtable(sample_table), type="html", file="C:\\Kavitha\\UW\\Stanford\\STATS290\\ProjectProposal\\example.html")
print(xtable(sample_table, align="llll"), 
      type="html", html.table.attributes="",file="C:\\Kavitha\\UW\\Stanford\\STATS290\\ProjectProposal\\example2.html")
#library(jsonlite)
#all.equal(mtcars, fromJSON(toJSON(mtcars)))
