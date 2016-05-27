#'@title Client for the ZEIT ONLINE Content API
#'@description Interface to gather newspaper articles from DIE ZEIT and ZEIT ONLINE,
#'based on a multilevel query. Including sorting algorithms and graphical output options.
#'A personal API key is required for usage.
#'@name rzeit
#'@docType package
#'@references \url{http://developer.zeit.de}
#'@seealso \code{\link{fromZeit}} to expose a search in the ZEIT online archive,
#'\code{\link{zeitClient}} to get client information,
#'\code{\link{zeitFrequencies}} to get a frequency table of the search results,
#'\code{\link{zeitPlot}} to plot results of a search,
#'\code{\link{zeitSetApiKey}} to set the API Key to the environment,
#'\code{\link{zeitToDf}} to get a data frame with sorted articles
#'@importFrom foreign write.foreign write.dta
#'@importFrom ISOweek ISOweek
#'@importFrom ISOweek ISOweek2date
#'@importFrom jsonlite fromJSON
#'@importFrom lubridate %m+%
#'@importFrom plyr count
#'@importFrom plyr rbind.fill
#'@importFrom stringr str_extract str_replace_all
#'@importFrom graphics abline axis lines plot
#'@importFrom stats lowess
#'@importFrom utils setTxtProgressBar tail txtProgressBar write.table
#'@aliases rzeit rzeit-package
NULL
