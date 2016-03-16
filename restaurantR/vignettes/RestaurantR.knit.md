---
title: "restaurantR package"
author: "Amit Bhatnagar and Kavitha Balasubramanian"
date: "2016-03-16"
output: rmarkdown::pdf_document
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
## Introduction 
RestaurantR is a package that provides detailed restaurant information based on filter criteria. Our package allows the user to extract relevant information on local restaurants without needing to know the intricacies of Factual database. We are using 2 major providers - factual and Yelp. 
Factual is an open data platform primarily focused on data for local businesses. Factual database is used by leading players in Local Business  including Facebook, Groupon and Bing. 
Yelp is a trusted source for local restaurant reviews. We used Yelp to get some info missing from the factual API source. 

The typical persona of this package would be a person working on restaurant research for business uses.Target users include:content developers for a restaurant blog or People in new initiatives department of a food chain. 

## Examples 
####Getting list of restauatants in a city meeting certain criteria 

```r
library(restaurantR)
#Get Expensive Thai restaurants in New York
thaiExpensiveNYC = getRestaurantByPriceRange("New York","expensive","Thai")
head(thaiExpensiveNYC,2)
```

```
## [1] NA
```

```r
#Get Pizza restaurants that allow smoking in Houston
pizzaSmokingHouston = getSmokingFriendlyRestaurants("Houston","Pizza")
head(pizzaSmokingHouston,2)
```

```
## [1] NA
```

####Getting full result from Factual for further analysis 

```r
#Get full Factual results Expensive Thai restaurants in New York
fullthaiExpensiveNYC = getRestaurantByPriceRange("New York","expensive","Thai", TRUE)
# Only few names shown because of space constraints. 
head(names(fullthaiExpensiveNYC))
```

```
## NULL
```
###Plot results on a map

```r
#Plot Pizza restaurants that allow smoking in Houston
plotBusinesses(pizzaSmokingHouston)
```

```
## [1] NA
```

### Compare two queries across the US and report which query leads in which state and plot.

```r
#Plot Pizza Hut and Dominoe's contest by state. 
plotQueryComparisonByState("Pizza Hut","Dominoe's")
```

```
## [1] NA
```

### Plot chain's presence by state








