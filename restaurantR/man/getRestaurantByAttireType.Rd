% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRestaurantByAttireType.R
\name{getRestaurantByAttireType}
\alias{getRestaurantByAttireType}
\title{Given a city name, a cusisine type and an attire type, provide a list of restaurants that meet these conditions}
\usage{
getRestaurantByAttireType(city, attireType, cuisine, full = FALSE)
}
\arguments{
\item{city}{A city name}

\item{attireType}{One of these: "streetwear","casual","business casual","smart casual","formal"}

\item{cuisine}{Cuisine name like "Indian", "Thai" etc.}

\item{full}{if TRUE returns full factual response. Default is FALSE,
which results in a smaller data frame with just restaurant name, latitude and longitude}
}
\description{
Given a city name, a cusisine type and an attire type, provide a list of restaurants that meet these conditions
}
\examples{
getRestaurantByAttireType("San Francisco","business casual","Asian")
}

