# placement

The main functions in this package are **drive_time**
(used for calculating distances between physical addresses) and
**geocode_url** (used for estimating the lat/long coordinates
of a physical address). Currently, Google provides scripts and functions written
in other languages (Python, Ruby, etc.) to generate the cryptogric signatures needed to
access the non-free Maps API, but does not offer a script for the R language
(hence the motivation for this package's authorship).

Google's paid accounts have much larger quota limits than the "standard_api" and,
for this reason, placement may be useful for indivuals needing to submit large batch
jobs to the Google Maps API from directly within R.
While the main benefit of this package is it's ability to generate properly
signed URLs with the hmac sha1 algorithm, it
also provides methods for accessing the standard API using a (free)
Google API key ([see this page](https://developers.google.com/maps/documentation/javascript/get-api-key#get-an-api-key to obtain a key "Google Maps Page")).

To install this package from directly within R:

`library(devtools)`<br />
`install_github("DerekYves/placement")`
