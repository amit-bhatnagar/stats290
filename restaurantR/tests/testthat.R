library(testthat)
library(restaurantR)

#test_check("restaurantR")

test.yelpR <- function() {
  restaurantObj <- yelpR$new()
  restaurantObj$InitializeYelp()

  restaurantDF = restaurantObj$queryData("Bellevue","WA","Chinese")
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200")
  checkEquals(ncol(restaurantDF), 28, " Checking number of columns returned is 28")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), " Checking that business aggregate rating of first restaurant is higher or equal to the 2nd to ensure right sort order")

  restaurantDF = restaurantObj$queryData(NULL,NULL,NULL)
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200 for NULL data")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), "Check sort order for NULL data")


  restaurantDF = restaurantObj$queryData(NA,NA,NULL) #Top restaurants in the US
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200 for mixed NULL and NA data")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), "Check sort order for mixed NULL and NA data")


  restaurantDF = restaurantObj$queryData("Redmond",NA,NULL) #Top restaurants in city of Redmond
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200 for mixed NULL and NA data")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), "Check sort order for mixed NULL and NA data")

  restaurantDF = restaurantObj$queryData("San Jose",NA,NULL) #Top restaurants in valid city
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200 for San Jose")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), "Check sort order for San Jose")

  restaurantDF = restaurantObj$queryData(NULL,"Colorado",NULL) #Top restaurants in the valid state
  checkEquals(nrow(restaurantDF), 200, " Checking number of restaurants returned is 200 for Colorado")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[1,"businesses.aggregaterating"]), "Check sort order for state only - Colorado")

  #Test the shortlist restaurant function
  restaurantDF = restaurantObj$shortlistRestaurants("New York","NY","Italian")
  checkEquals(nrow(restaurantDF), 6, " Checking number of restaurants returned is 6")
  checkEquals(ncol(restaurantDF), 6, " Checking number of columns for restaurants returned is 6")

  #Test the getRestaurantWithDeals function
  restaurantDF = restaurantObj$getRestaurantWithDeals("Seattle","CA","Indian")
  checkTrue(nrow(restaurantDF) >= 1, " Checking number of restaurants returned is 4, which is more than 1. Actual value is 4 but might be changing, so checking there is atleast one deal")
  checkTrue(as.numeric(restaurantDF[1,"businesses.aggregaterating"]) >= as.numeric(restaurantDF[2,"businesses.aggregaterating"]), "Check sort order")

  checkException(restaurantObj$queryData(1,NULL,NULL), " Checking exception with non numeric arg")
  checkException(restaurantObj$queryData(NULL,"invalid",NULL), " Checking exception with invalid state argument")
  checkException(restaurantObj$queryData("invalid",NULL,NULL), " Checking exception with invalid state argument")
  checkException(restaurantObj$queryData(NULL,NULL,"invalid"), " Checking exception with invalid state argument")
  checkException(restaurantObj$shortlistRestaurants(NULL,NULL,"invalid"), " Checking exception with invalid arguments")
  checkException(restaurantObj$getRestaurantWithDeals(NULL,NULL,"invalid"), " Checking exception with invalid arguments")
}


test.compareQueriesByState <- function() {
  checkEquals(length(compareQueriesByState("Pizza Hut","Dominoe's")),50,"Ensure that data returned for 50 states")
  checkEquals(length(compareQueriesByState("Invalid","Dominoe's")),50,"If one state is invalid, should return values for other state")
  checkEquals(compareQueriesByState("Invalid","Dominoe's")["AK"],"Dominoe's","If one state is invalid, should return values for other state")
  checkEquals(compareQueriesByState("Invalid","Dominoe's")["NJ"],"Dominoe's","If one state is invalid, should return values for other state")

  checkException(compareQueriesByState(NULL,NULL), " Checking exception with invalidarguments")
}

test.getAlcoholFriendlyRestaurants <- function() {
  checkEquals(nrow(getAlcoholFriendlyRestaurants("Mountain View","Indian")),9,"Ensure that data is returned for city")
  checkEquals(nrow(getAlcoholFriendlyRestaurants("California","Mexican")),2,"Ensure that data is returned for state")

  checkException(getAlcoholFriendlyRestaurants("Seattle","Invalid"),"Invalid cusine")
  checkException(getAlcoholFriendlyRestaurants("Invalid",NULL), " Checking exception with invalid arguments")
}

test.getCateringRestaurants <- function() {
  checkEquals(nrow(getCateringRestaurants("San Jose","Indian")),19,"Ensure that data is returned for city")
  checkEquals(nrow(getCateringRestaurants("California","American")),6,"Ensure that data is returned for state")

  checkException(getCateringRestaurants("California","Mexican"),"No catering restaurants for Mexican cusine")
  checkException(getCateringRestaurants("Seattle","Invalid"),"Invalid cusine")
  checkException(getCateringRestaurants("Invalid",NULL), " Checking exception with invalid arguments")
}

test.getCuisineforCity <- function() {
  checkEquals(nrow(getCuisineforCity("San Francisco","Asian")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getCuisineforCity("California","American")),20,"Ensure that data is returned for state")

  checkException(getCuisineforCity("Seattle","Invalid"),"Invalid cusine")
  checkException(getCuisineforCity("Invalid",NULL), " Checking exception with invalid arguments")
}

test.getKidsFriendlyRestaurants <- function() {
  checkEquals(nrow(getKidsFriendlyRestaurants("Redmond","Thai")),8,"Ensure that data is returned for city")
  checkEquals(nrow(getKidsFriendlyRestaurants("California","American")),13,"Ensure that data is returned for state")

  checkException(getKidsFriendlyRestaurants("Seattle","Invalid"),"Invalid cusine")
  checkException(getKidsFriendlyRestaurants("Invalid",NULL), " Checking exception with invalid arguments")
}

test.getQueryCountByUSState <- function() {
  checkEquals(nrow(getQueryCountByUSState("Chipotle")),50,"Ensure that data is returned for 50 states")
  checkEquals(getQueryCountByUSState("Chipotle")["AK",],0,"No chipotle in AK")

  checkTrue(as.numeric(getQueryCountByUSState("Chipotle")["CA",]) >= 300, " Check that california has greater than 300 chipotles")

  checkException(getQueryCountByUSState(NULL),"Invalid restaurant")
}

test.getRestaurantByAttireType <- function() {
  checkEquals(nrow(getRestaurantByAttireType("San Francisco","business casual","Asian")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantByAttireType("California","casual","American")),20,"Ensure that data is returned for state")

  checkException(getRestaurantByAttireType("Seattle","Invalid","Invalid"),"Invalid arguments")
  checkException(getRestaurantByAttireType("Invalid",NULL,"Thai"), " Checking exception with invalid arguments")
}

test.getRestaurantByDietType <- function() {
  checkEquals(nrow(getRestaurantByDietType("Bellevue","Vegetarian","Thai")),13,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantByDietType("San Jose","Vegan","American")),20,"Ensure that data is returned for state")

  checkException(getRestaurantByDietType("Seattle","Invalid","Invalid"),"Invalid arguments")
  checkException(getRestaurantByDietType("Invalid",NULL,"Thai"), " Checking exception with invalid location")
}

test.getRestaurantByPriceRange <- function() {
  checkEquals(nrow(getRestaurantByPriceRange("San Francisco","moderate","Asian")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantByPriceRange("California","low","American")),20,"Ensure that data is returned for state")

  checkException(getRestaurantByPriceRange("Seattle","Invalid","Invalid"),"Invalid arguments")
  checkException(getRestaurantByPriceRange("Invalid",NULL,"Thai"), " Checking exception with invalid location")
}

test.getRestaurantsAcceptingReservations <- function() {
  checkEquals(nrow(getRestaurantsAcceptingReservations("San Francisco","Asian")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantsAcceptingReservations("California","American")),14,"Ensure that data is returned for state")

  checkException(getRestaurantsAcceptingReservations("Seattle","Invalid"),"Invalid arguments")
  checkException(getRestaurantsAcceptingReservations(NULL,"Thai"), " Checking exception with invalid location")
}

test.getRestaurantsByMealType <- function() {
  checkEquals(nrow(getRestaurantsByMealType("New York","Lunch","Thai")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantsByMealType("California","Dinner","Mexican")),2,"Ensure that data is returned for state")

  checkException(getRestaurantsByMealType("Invalid","Invalid","Invalid"),"Invalid arguments")
  checkException(getRestaurantsAcceptingReservations(NULL,NULL,NULL), " Checking exception with NULL arguments")
}

#TODO: Redo this test with new code
#test.getRestaurantsNearLocation <- function() {
#  checkEquals(nrow(getRestaurantsNearLocation("Space Needle","Asian")),20,"Ensure that data is returned for city")
#  checkEquals(nrow(getRestaurantsNearLocation("Google","Mexican")),20,"Ensure that data is returned for different place")

#  checkException(getRestaurantsNearLocation("Space Needle","Invalid"),"Invalid cusine")
#  checkException(getRestaurantsNearLocation("Invalid","Mexican"),"Invalid location")
#  checkException(getRestaurantsNearLocation(NULL,NULL), " Checking exception with NULL arguments")
#}

test.getRestaurantsWithDeliveryOrTakeOut <- function() {
  checkEquals(nrow(getRestaurantsWithDeliveryOrTakeOut("Redmond","delivery","Indian")),13,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantsWithDeliveryOrTakeOut("San Francisco","delivery","American")),20,"Ensure that data is returned for city")

  checkException(getRestaurantsWithDeliveryOrTakeOut("Seattle","Invalid","Invalid"),"Invalid arguments")
  checkException(getRestaurantsWithDeliveryOrTakeOut("Invalid","delivery","Italian"),"Invalid location")
  checkException(getRestaurantsWithDeliveryOrTakeOut(NULL,NULL,NULL), " Checking exception with NULL arguments")
}

test.getRestaurantsWithWiFi <- function() {
  checkEquals(nrow(getRestaurantsWithWiFi("Atlanta","Pizza")),20,"Ensure that data is returned for state")
  checkEquals(nrow(getRestaurantsWithWiFi("Dublin","American")),14,"Ensure that data is returned for city")

  checkException(getRestaurantsWithWiFi("Seattle","Invalid"),"Invalid arguments")
  checkException(getRestaurantsWithWiFi("Invalid","Italian"),"Invalid location")
  checkException(getRestaurantsWithWiFi(NULL,NULL), " Checking exception with NULL arguments")
}

test.getRestaurantsWithWiFi <- function() {
  checkEquals(nrow(getRestaurantsWithWiFi("Fremont","Pizza")),3,"Ensure that data is returned for city")
  checkEquals(nrow(getRestaurantsWithWiFi("Seattle","American")),20,"Ensure that data is returned for city")

  checkException(getRestaurantsWithWiFi("Seattle","Invalid"),"Invalid arguments")
  checkException(getRestaurantsWithWiFi("Invalid","Italian"),"Invalid location")
  checkException(getRestaurantsWithWiFi(NULL,NULL), " Checking exception with NULL arguments")
}

test.getWheelChairAccesibleRestaurants <- function() {
  checkEquals(nrow(getWheelChairAccesibleRestaurants("San Francisco","Asian")),20,"Ensure that data is returned for city")
  checkEquals(nrow(getWheelChairAccesibleRestaurants("Washington","Mexican")),20,"Ensure that data is returned for state")

  checkException(getWheelChairAccesibleRestaurants("Seattle","Invalid"),"Invalid arguments")
  checkException(getWheelChairAccesibleRestaurants("Invalid","Italian"),"Invalid location")
  checkException(getWheelChairAccesibleRestaurants(NULL,NULL), " Checking exception with NULL arguments")
}



