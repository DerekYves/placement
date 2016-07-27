#' Pull Google Maps data with error handling
#'
#' This function, primarily, is a helper for \code{\link{drive_time}} and \code{\link{geocode_url}}.
#' It attempts to pull Google Maps data for a complete URL, recording http errors/connection failures
#' within the \code{status} and \code{error_message} paramaters.

#' To debug invalid signature requests, refer to the \href{https://developers.google.com/maps/documentation/business/webservices/auth}{Google Developer documention}.

#' @param urls_out character string; a complete URL with valid character encoding.
#' @param tmout number; the length of time, in seconds, to wait for a valid server
#' response before triggering a connection timeout error (defaults to 10 seconds).
#' @param messages logical; when \code{TRUE}, displays message from the API call(s). Generally,
#' this parameter is passed from \code{\link{drive_time}} or \code{\link{geocode_url}}.
#'
#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
#' @return A list with validly formatted JSON objects whose length equals \code{urls_out}
#' @examples
#' \dontrun{
#' # Fetch URLs contained within character vector "togoogle", waiting 20 seconds
#' # before triggering a server timeout error.
#' output <- pull_geo_data(togoogle, tmout=20)
#' }
#' @export

pull_geo_data <- function(urls_out, tmout=10, messages=TRUE){
	if(is.null(messages) | !is.logical(messages)) messages <- FALSE
	.urlset <- urls_out
	opts = RCurl::curlOptions(connecttimeout=tmout) # set max connection time out
	mapsin <- function(urls){
		errjson <- "{\n   \"error_message\" : \"URL was unreachable. Check your network connection.\",\n   \"results\" : [],\n    "
		out <- tryCatch(
			{
				if(messages) message("Reading:", urls)
				RCurl::getURL(urls, .opts = opts)
			},
			error=function(cond) {
				if(messages) message(paste("This URL is not reachable:", urls))
				if(messages) message("Error message:")
				if(messages) message(cond)
				return(paste0(errjson, "\"status\" : \"CONNECTION_ERROR\"", "}"))
			},
			warning=function(cond){
				if(messages) message(paste("This URL triggered a warning:", urls))
				if(messages) message("Warning message:")
				if(messages) message(cond)
				return(paste0("{", errjson, "\"status\" : \"CONNECTION_WARNING\"", "}"))
			},
			finally={
				if(messages) message(paste("Processed URL:", urls))
			}
		)
		return(out)
	}

	fromgoogle <- lapply(.urlset, mapsin)

	# Invalid signatures do not trigger an http error from the API; additionally,
	# these returns are not transmitted in JSON format The following
	# transformations convert signature errors into a JSON object with status
	# and error_message fields.

	# Shorten signature error message length
	fromgoogle <- lapply(fromgoogle, function (x) gsub("request. Provided 'signature'.*", "request.", x))

	# Convert to JSON
	fromgoogle <- ifelse(grepl("Unable to authenticate the request", fromgoogle, ignore.case=TRUE),
						 paste0("{\n   \"error_message\" : \"Unable to authenticate the request with the signature supplied. Check your client ID and private key.\",\n",
						 	   "\"results\" : [],\n   \"status\" : \"INVALID_SIGNATURE\"\n}\n"), fromgoogle)

	json <- lapply(fromgoogle, jsonlite::fromJSON)
	return(json)
}
