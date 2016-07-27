#' A general address cleaner.
#'
#' Performs character transformations on a vector of
#' addresses in order to build "web-safe" URLs for the Google API.
#'
#' This function strips character values from a vector of addresses (e.g., a vector of the form: address, city, state, postal code, country)
#' that may inhibit sucessful geocoding with the \href{https://developers.google.com/maps/documentation/geocoding/start}{Google maps API}.
#' Specifically, address_cleaner:
#' \itemize{
#'   \item {Replaces non-breaking spaces with " "}
#'   \item {Removes ASCII control characters (001-031 and 177)}
#'   \item {Trims runs of spaces and spaces which begin/end a string}
#'   \item {Converts special addressing characters, such as ordinals}
#'   \item {Removes single/double quotes and asterisks}
#'   \item {Strips latin1 characters}
#'   \item {Removes leading, trailing, and repeated commas}
#'   \item {Removes various permutations of the "c/o" flag}
#'   }
#'
#' Note: Hypthenated addresses and zipcodes can cause issues with the Maps API. Therefore, prior to
#' applying this function and attempting to geocode a location, we recommend:
#' \itemize{
#'   \item {Deleting the second half of a compound US postal code, e.g. \code{gsub("(?<=\\d)-.*", "", "12345-1234", perl=TRUE)}}
#'   \item {Replacing hypthenated street numbers with a space followed by a pound, e.g. \code{gsub("(?<=\\d)-(?=\\d)", " #", "1234-3332 West 100th", perl=T)}.}
#'   }
#'   Both of these transformations, of course, presuppose that your postal code and street numbers exist in separate columns.
#'   Similarly, you may want to recode any "CA" country code fields to "Canada" to avoid inaccurate
#'   geocoding within California state (this is more more likely to occur when a Canadian address has non-standard features, such as 'c/o' or 'attn' fields, etc.).
#' @param address A raw 1xN vector of UTF-8 encoded addresses.
#' Note: these addresses should be in raw form, \emph{not} URL encoded (e.g., of the form: 123 Main Street, Somewhere, NY 12345 USA)(country is optional but recommended).
#' @param verbose Displays additional progress output
#' @return address_cleaner returns a character vector of addresses of the same length as the input.
#' @examples
#' # Define an incompatible vector of addresses
#' address <- c(" 350 Fifth Ave \u00bd, New York, NY 10118, USA ",
#' 			 "  \u00ba 1600  Amphitheatre Pkwy, Mountain View, CA 94043, USA")
#'
#' # View the return:
#' address_cleaner(address)

#' @importFrom stringi stri_trans_general
#' @export

address_cleaner <- function(address, verbose = TRUE){

	# Input validation
	if(!is.vector(address, mode="character")) stop("Address must be a character vector!")

	# Replace non-breaking spaces with " "
	if (verbose) cat("\t* Replacing non-breaking spaces\n")
	address <- vapply(address, function(x)
		gsub(intToUtf8(160), " ", x), character(1), USE.NAMES = FALSE)

	# Remove ASCII control characters (\001-\031 and \177)
	if (verbose) cat("\t* Removing control characters\n")
	address <- vapply(address, function(x)
		gsub("[\001-\031\177]", " ", x), character(1), USE.NAMES = FALSE)

	# Trim runs of spaces and spaces which lead/trail a string
	if (verbose) cat("\t* Removing leading/trailing spaces, and runs of spaces\n")
	trim <- function(x)
		return(gsub("^ +|(?<= ) +| +$", "", x, perl = T))
	address <- vapply(address, function(x)
		trim(x), character(1), USE.NAMES = FALSE)

	# Strip latin1 characters
	if (verbose) cat("\t* Transliterating latin1 characters\n")
	address <- vapply(address, function(x)
		stringi::stri_trans_general(x, "Latin-ASCII"),
		character(1), USE.NAMES = FALSE)

	# Convert special addressing charecters
	if (verbose) cat("\t* Converting special address markers\n")
	address <- vapply(address, function(x)
		gsub("\u00bd", "1/2", fixed = T,
			 gsub("\u00aa", "a", fixed = T,
			 	 gsub("\u00ba", "o", x, fixed = T))),
		character(1), USE.NAMES = FALSE)

	# Remove remaining non-ASCII characters and replace with " "
	if (verbose) cat("\t* Removing all remaining non-ASCII characters\n")
	address <- vapply(address, function(x) iconv(x, 'ASCII', sub = " "),
					  character(1), USE.NAMES = FALSE)

	# Remove single/double quotes and asterisks
	if (verbose) cat("\t* Remove single/double quotes and asterisks\n")
	address <- vapply(address, function(x) gsub("\\*|\"|\'", "", x),
					  character(1), USE.NAMES = FALSE)

	# Remove leading, trailing, and repeated commas
	if (verbose) cat("\t* Removing leading, trailing, and repeated commas\n")
	address <- vapply(address, function(x) gsub("^,*|(?<=,),|,*$", "", x, perl = T),
					  character(1), USE.NAMES = FALSE)

	# Remove "c/o" flag
	if (verbose) cat("\t* Removing various c/o string patterns\n")
	address <- vapply(address, function(x)
		gsub("c/o|c/0|c/", "", x, perl = T, ignore.case = T),
		character(1), USE.NAMES = FALSE)

	address <- unlist(address, use.names=FALSE)
	return(address)
}
