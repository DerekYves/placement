#' Generates digital signatures for Google Maps API calls.
#'
#' This function, primarily, is a helper for \code{\link{drive_time}} and \code{\link{geocode_url}}.
#' It is only necessary when building digital signatures with the private cryptographic key associated with
#' your (paid) \href{https://www.google.com/work/}{Google for Work} account. To build valid signatures your vector of addresses must contain "web-safe" UTF-8 characters.
#' By default, and unlike \code{\link{drive_time}} and \code{\link{geocode_url}},this function assumes that the \code{address} and \code{dest} parameters are \emph{already} URL encoded. If this
#' is not the case, you may apply url encoding to the inputs using the optional \code{urlencode} parameter (see below).
#' (see: \code{\link{address_cleaner}}). For basic geocoding and distance analyses it is generally easier to use \code{\link{drive_time}} and \code{\link{geocode_url}}
#' to encode a request with your Google for Work private key. However, and particularly with large batch jobs,
#' it can be useful to inspect the output of this function for malformed values which yield invalid signatures (see the "debug" option).

#' The function's method of signature generation follows the pattern outlined for Python in
#' the \href{https://developers.google.com/maps/documentation/business/image/auth?hl=en}{Google Developer documention}.

#' @param address A 1xN vector of UTF-8 addresses (if gmode='dtime', this is the origin address).
#' @param dest If gmode is 'dtime', this is the destination address.
#' @param gmode character string; must be either "geocode" (the default) or "dtime" (for distance requests).
#' @param privkey character string; your Google for Work API key
#' @param clientid character string; generally, this ID will be of the form 'gme-[company]'.
#' @param debug logical; when \code{TRUE}, returns a complete data frame of locations and their associated URL signatures (this can be useful for debugging invalid signatures).
#' @param verbose logical; when \code{TRUE}, displays additional progress output.
#' @param travel_mode character string; currently, valid values include (\href{https://developers.google.com/maps/documentation/javascript/distancematrix#distance_matrix_requests}{see this page for details}):
#'   \itemize{
#'   \item driving (the default): indicates standard driving directions using the road network.
#'   \item transit: requests directions via public transit routes.
#'   \item walking: requests walking directions via pedestrian paths & sidewalks (where available).
#'   \item bicycling: requests bicycling directions via bicycle paths & preferred streets (currently only available in the US and some Canadian cities).
#'         }
#' @param units character string; must be either "metric" (the default) or "imperial".
#' Specifying "metric" will return distance between origin and destination as kilometers,
#' whereas "imperial" returns distance in miles. For geocode requests this parameter
#' is ignorned if non-null.
#' @param language character string; localization of the returned object. This parameter is
#' set to "en-EN" by default, but refer to
#' \href{https://developers.google.com/maps/faq#using-google-maps-apis}{this page}
#' for an up-to-date list of Google's supported languages.
#' @param urlencode logical; when \code{TRUE}, applies the \code{\link{url_encode}} function from the \code{\link{urltools}} package.

#' @importFrom digest hmac
#' @importFrom urltools url_encode url_decode
#' @importFrom RCurl base64Decode
#' @importFrom base64enc base64encode
#' @export

google_encode64 <- function(address, dest=NULL, gmode="geocode", privkey=NULL,
							clientid=NULL, debug = FALSE, verbose = FALSE,
							travel_mode="driving", units="metric",
							language="en-EN", urlencode=FALSE){

	options(stringsAsFactors=F)

	# Input validation
	if(is.null(clientid))
		stop("You must specify a client ID to encode the URL!")
	if(is.null(privkey))
		stop("You must specify a Google for work API key to encode the URL!")
	if(!grepl("geocode|dtime", gmode))
		stop("gmode must be set as \'geocode\' or \'dtime\'.")
	if(!is.vector(address, mode="character"))
		stop("Address must be url encoded character vectors!")
	if(gmode=="dtime"){
		if(!is.vector(dest, mode="character"))
			stop("Address must be url encoded character vectors!")
		if(!grepl("metric|imperial", units))
			stop("Invalid units paramater. Must be 'metric' or 'imperial'")
		if(length(address)>1 & length(address)!=length(dest))
			stop("Address must be singular or the same length as destination!")
	}

	# Encode the URLs
	if(urlencode){
		address  <- urltools::url_encode(address)
		if(!is.null(dest)){
		dest     <- urltools::url_encode(dest)
		}
	}

	# Build the data.frame for encoding
	if(gmode == "geocode") {
		x <- data.frame(locations=address)
	} else if(gmode == "dtime") {
		x <- data.frame(address=address, dest=dest)
	}


	if(verbose)
		cat("Number of digitial signatures to that will be generated:", nrow(x), "\n")

	# The steps below follow this Google support manual:
	# https://developers.google.com/maps/premium/previous-licenses/webservices/auth#client_id_and_signature

	#####################################################################
	# Step (1): Set URL parameters:
	#     Note 1: Any non-standard characters need to be URL-encoded.
	#     Note 2: All Google services require UTF-8 character encoding.

	x$domain  <- "https://maps.googleapis.com"
	x$client  <- paste0("&client=", clientid)

	if(gmode == "geocode"){
		x$url     <- "/maps/api/geocode/json?address="
	} else if(gmode == "dtime") {
		x$url     <- "/maps/api/distancematrix/json?origins="
	}

	#####################################################################
	# (2) Strip off the domain portion of request and create the URL to be signed:

	if(gmode == "geocode"){
		x$tosign <- with(x, paste0(url, address, client))
	} else if (gmode == "dtime") {
		x$tosign <- with(x, paste0(url, address,
								   "&destinations=", dest,
								   "&units=", tolower(units),
								   "&mode=", tolower(travel_mode),
								   "&language=", language, client))
	}


	#####################################################################
	# (3) Encode the private key in "url-safe" Base64.
	#     Note 1: "url-safe" keys replace '+' and '/' with '-' and '_' respectively.
	#     Note 2: base::base64decode does not work; base64Decode from RCurl needed.
	#     Note 3: RCurl's base64Decoder is not URL safe; therefore, gsub is needed.
	#See: http://stackoverflow.com/questions/28376071/url-safe-base64-decoding-for-r

	b64dkey   <- RCurl::base64Decode(gsub("-", "+", gsub("_", "/", privkey)))
	x$b64dkey <- b64dkey

	#####################################################################
	# (4) Sign the URL using the HMAC-SHA1 algorithm and decode to binary.
	#     Note: hmac requires the raw=T argument for the Google key

	# Define hash/encoding function
	hmac_sha1 <- function(key, string) {
		hash  <- digest::hmac(key, string, "sha1", raw = TRUE)
		base64enc::base64encode(hash)
	}

	if (verbose) cat("\t* Building the digital signatures\n")

	x$enc_sig <- vapply(x[, 'tosign'],  function(x)
			hmac_sha1(b64dkey, x), character(1), USE.NAMES=FALSE)

	#####################################################################

	#####################################################################
	# (5) Reformat the signature with url-safe Base64.

	x$enc_sig_url <- vapply(x[, 'enc_sig'], function(x)
		gsub("+", "-", fixed = T, gsub("/", "_", x, fixed = T)),
		character(1), USE.NAMES=FALSE)

	#####################################################################

	#####################################################################
	# (6) Build the fully formed, signed URLs

	x$full_url <- with(x, paste0(domain, tosign, "&signature=", enc_sig_url))

	#####################################################################

	#####################################################################
	# (7) Return the key or full data frame for debugging

	if(debug){
		x <- cbind(raw=urltools::url_decode(x$locations), x)
		return(x)
	} else {
		return(as.vector(x$full_url, mode="character"))
	}

}

