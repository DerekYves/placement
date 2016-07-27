# placement

The main functions in this package are `drive_time`
	(used for calculating distances between physical addresses or coordinates) and
	`geocode_url` (used for estimating the lat/long coordinates
	of a physical address). Optionally, it generates the cryptographic signatures necessary
	for making API calls with a Google for Work/Premium account within the geocoding process.
	These accounts have larger quota limits than the "standard_api" and, thus, this package
	may be useful for individuals seeking to submit large batch jobs within R to the Google Maps API.
	Placement also provides methods for accessing the standard API using a (free) Google API key
	(see: https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key).

This package is available on Cran and can be installed within R:

install.packages("placement")

To install the development version of this package:

`library(devtools)`<br />
`install_github("DerekYves/placement")`
