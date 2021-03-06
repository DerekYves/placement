% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode_url.R
\name{geocode_url}
\alias{geocode_url}
\title{Geocode an address vector using the Google Maps API.}
\usage{
geocode_url(address, auth = "standard_api", privkey = NULL,
  clientid = NULL, clean = FALSE, verbose = FALSE, add_date = "none",
  messages = FALSE, dryrun = FALSE)
}
\arguments{
\item{address}{A 1xN vector of address(es) with "url-safe" characters. Enabling the "clean" parameter calls the \code{\link{address_cleaner}} function, which strips or replaces common characters that are incompatible with the Maps API. Notes:
\itemize{
       \item Addresses should be in raw form, \emph{not} URL encoded (e.g., of the form: 123 Main Street, Somewhere, NY 12345, USA).
       \item Specifying the country is optional but \emph{recommended}.
       }}

\item{auth}{character string; one of: "standard_api" (the default) or "work".
Although you may specify an empty string for this parameter (see the examples below), we recommend users obtain a (free) standard API key: \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{Google API key}.
Authentication via the "work" method requires the client ID and private API key associated with your (paid) \href{https://www.google.com/work/}{Google for Work/Premium} account.}

\item{privkey}{character string; your Google API key (whether of the "work" or "standard_api" variety).}

\item{clientid}{character string; your Google for Work/Premium Account client ID (generally, these are of the form 'gme-[company]')
This parameter should \emph{not} be set when authenticating through the standard API.}

\item{clean}{logical; when \code{TRUE}, applies \code{\link{address_cleaner}} to the address vector prior to URL encoding.}

\item{verbose}{logical; when \code{TRUE}, displays additional output in the returns from Google.}

\item{add_date}{character string; one of: "none" (the default), "today", or "fuzzy". When set to "today", a column with today's calendar date is added to the returned data frame.
When set to "fuzzy" a random positive number of days between 1 and 30 is added to this date column. "Fuzzy" date values can be useful to avoid sending large batches of geocode requests on the same day if your scripts recertify/retry geocode estimations after a fixed period of time.}

\item{messages}{logical; when \code{TRUE}, displays warning and error messages generated by the API calls within the \code{\link{pull_geo_data}} function (e.g. connection errors, malformed signatures, etc.)}

\item{dryrun}{logical; when \code{TRUE}, aborts script prior to the \code{\link{pull_geo_data}} url call, returning the URL to be encoded. This can be useful for debugging addresses that yield non-conformant JSON returns.}
}
\value{
Geocode_url returns a data frame with (numeric) lat/long coordinates and four additional parameters from the response object (see \href{https://developers.google.com/maps/documentation/geocoding/intro#GeocodingResponses}{this page} for additional information):
\itemize{
  \item \strong{formatted_address:} The formatted address Google used to estimate the geocordinates.
  \item \strong{location_type:} An estimate of the response object's coordinate accuracy. Currently, possible response values are:
  \itemize{
  \item \emph{ROOFTOP:} indicates that the return is accurate to the level of a precise street address.
  \item \emph{RANGE_INTERPOLATED:} indicates that the result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
  \item \emph{GEOMETRIC_CENTER:} indicates that the result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
  \item \emph{APPROXIMATE:} indicates that the result is approximate.
  }
  \item \strong{status:} The geocode status of a response object. Currently, possible response values are:
  \itemize{
  \item \emph{OK:} indicates that no errors occurred; the address was successfully parsed and at least one geocode was returned.
  \item \emph{ZERO_RESULTS:} indicates that the geocode was successful but returned no results. This may occur if the geocoder was passed a non-existent address.
  \item \emph{OVER_QUERY_LIMIT:} indicates that you are over your quota.
  \item \emph{REQUEST_DENIED:} indicates that your request was denied.
  \item \emph{INVALID_REQUEST:} Indicates that some part of the query (address, URL components, etc.) is missing.
  \item \emph{UNKNOWN_ERROR:} indicates that the request could not be processed due to a server error. The request may succeed if you try again.
  \item \emph{INVALID_SIGNATURE:} This response is generated by error-handling within the \code{placement} package. For Google for Work requests, this error (usually)
  indicates that the signature associated with the geocode request was invalid.
  \item \emph{CONNECTION_ERROR:} This status is generated by the package's internal error-handling, and suggests a connection error orcurred
  while fetching the url(s)(e.g. due to intermittent internet connectivity, problems with the Google maps servers, etc.).
  }
  \item \emph{error_message:} Any error messages associated with the API call (e.g. connection timeouts, signature errors, etc.).
  \item \strong{locations:} character; the user supplied values in the \code{address} parameter (this is returned for matching/verification).
  \item \strong{input_url:} character; the full url associated with the response object.
  \item \strong{address:} character; the user supplied physical address (prior to Google's formatting).
  }
}
\description{
geocode_url uses the Google Maps API to estimate latitude and longitude coordinates for a character vector of physical addresses.
Optionally, one may use their (paid) \href{https://www.google.com/work/}{Google for Work/Premium} API key to sign the request with the \code{\link{hmac}} sha1 algorithm.
For smaller batch requests, it is also possible to access Google's "standard API"
using this function (see \href{https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key}{this page} to obtain a free API key).
}
\examples{
# Get coordinates for the Empire State Building and Google
address <- c("350 5th Ave, New York, NY 10118, USA",
			 "1600 Amphitheatre Pkwy, Mountain View, CA 94043, USA")

coordset <- geocode_url(address, auth="standard_api", privkey="",
            clean=TRUE, add_date='today', verbose=TRUE)

# View the returns
print(coordset[ , 1:5])
}

