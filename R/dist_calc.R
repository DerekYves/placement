#' A simple distance calculator for two sets of geo coordinates.

#' This function may be utilized to estimate the "straight line" distance between
#' two sets of, e.g., Google Web Mercator/WGS 84 geocoordinates.

#' @param long1 numeric; the longitude (degrees) of location 1.
#' @param lat1 numeric; the latitude (degrees) of location 1.
#' @param long2 numeric; the longitude (degrees) of location 2.
#' @param lat2 numeric; the latitude (degrees) of location 2.
#' @param units character string; must be either "metric" (the default) or "imperial".
#' Specifying "metric" will return the distance between location 1 and location in kilometers,
#' whereas "imperial" returns the distance in miles.
#'
#' @references \url{http://menugget.blogspot.com/2011/05/r-functions-for-earth-geographic_29.html}

dist_calc <- function (long1, lat1, long2, lat2, units="metric")
{
	if(!grepl("metric|imperial", units))
		stop("Invalid units paramater. Must be 'metric' or 'imperial'")
	rad <- pi/180
	a1 <- lat1 * rad
	a2 <- long1 * rad
	b1 <- lat2 * rad
	b2 <- long2 * rad
	dlon <- b2 - a2
	dlat <- b1 - a1
	a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
	R <- 6378.145
	d <- R * c
	if(units=="imperial") d <- d * 0.621371
	return(d)
}
