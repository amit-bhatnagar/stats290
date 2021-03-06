#' R6Test: A Test package for R6
#'
#' \code{yelpRTest} provides a \code{yelpR} object that stores restaurant data based on the users preference.
#'
#' @section Introduction:
#'
#' RestaurantR is a restaurant research package that provides restaurant information based on the users search criteria.
#' It provides a sorted restaurant list based on review and rating with best restaurants at the top
#' it provides capability for obtainining narrowed down restaurant list based on basic restaurant preferences
#' It provides an ability to plot the restaurants retrieved on a map
#' This package will typically be used by food blogs or restaurant research magazines to analyze restaurant data and develop
#' articles based on the results retreived.
#'
#' @section Detail:
#'
#' \code{yelpRTest} allows you to query for reVstaurant data and plot it on a map
#'
#'
#' @examples
#' restaurantObj <- yelpR$new()
#' restaurantObj$InitializeYelp()
#' restaurantDF = restaurantObj$queryData("Bellevue","WA","Chinese")
#' restaurantDF = restaurantObj$queryData(NULL,NULL,NULL)
#' restaurantDF = restaurantObj$queryData(1,NULL,NULL) #Error
#' restaurantDF = restaurantObj$queryData(NA,NA,NULL) #Top restaurants in the US
#' restaurantDF = restaurantObj$queryData("Redmond",NA,NULL) #Top restaurants in city of Redmond
#' restaurantDF = restaurantObj$queryData("San Jose",NA,NULL) #Top restaurants in city of San Jose
#' restaurantDF = restaurantObj$queryData(NULL,"Colorado",NULL) #Top restaurants in Colorado
#' @docType package
#' @name yelpRTest
NULL


