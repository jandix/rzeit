#'@title Calculate a frequency table
#'@description \code{zeitFrequencies} calculates a frequency table, i.e. a data frame including a continous list of dates in choosen sequences and the related frequencies. Returned objects can directly be used with \code{\link{zeitPlot}}.
#'@param ls list. List, created by \code{\link{fromZeit}}, to calculate the frequency table.
#'@param sort character. Specifies how the results are sorted. Possible options are \code{"year"} (default), \code{"month"}, \code{"week"} or \code{"day"}.
#'@param save character. Specifies the file for automatic saving of the resulting data frame. Possible formats are \code{"txt"}, \code{"sps"}, \code{"sas"} or \code{"dta"}. No data is saved per default.
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitToDf}} \code{\link{zeitPlot}}
#'@return data frame
#'@examples
#'\dontrun{
#'# get data
#'mrkl <- fromZeit(q = "angela merkel", limit = "all", 
#'	dateBegin = "2002-01-01", dateEnd = "2007-12-31")
#'
#'# sort by month without saving
#'zeitFrequencies(ls = mrkl, sort = "month")
#'
#'# sort by day and saved as .sas
#'zeitFrequencies(ls = mrkl, sort = "day", save = "sas")
#'}
#'@author Jan Dix (\email{jan.dix@@uni-konstanz.de}), Jana Blahak (\email{jana.blahak@@uni-konstanz.de}), Christian Graul (\email{christian.graul@@gmail.com})
#'@export
zeitFrequencies <- function(ls, sort = c("year", "month", "week", "day"), save = c("txt", "sps", "sas", "dta")){
	
	# check ls
	if(is.null(ls[["matches"]][["release_date"]])) stop("Field 'release_date' is required to use 'zeitFrequencies' but missing")
	
	# get dates
	dates <- ls[["matches"]][["release_date"]]
	dates <- substr(dates, 1, 10)
	dates <- as.Date(dates)
	
	# calculate frequencies and build data frame
	sortby <- match.arg(sort)
  if(sortby == "year") {
  	yrs <- format(dates, "%Y")
  	freq <- count(yrs)
  	freq$date <- as.Date(paste(freq$x, "01", "01", sep = "-"))
  	freq$year <- as.numeric(as.character(freq$x))
  	freq$yearCount <- seq(1:nrow(freq))
  } else if(sortby == "month") {
  	mnths <- format(dates, "%Y-%m")
  	freq <- count(mnths)
  	freq$date <- as.Date(paste(freq$x, "01", sep = "-"))
  	freq$month <- format(freq$date, "%b %Y")
  	freq <- freq[order(freq$date),]
  	freq$monthCount <- seq(1:nrow(freq))
  } else if(sortby == "week") {
  	wks <- ISOweek(dates)
  	freq <- count(wks)
  	freq$date <- ISOweek2date(paste(freq$x, 1, sep = "-"))
  	freq$week <- ISOweek(freq$date)
  	freq$weekCount <- seq(1:nrow(freq))
  } else if(sortby == "day") {
  	dys <- format(dates, "%Y-%m-%d")
  	freq <- count(dys)
  	freq$date <- as.Date(paste(freq$x, "01", sep = "-"))
  	freq$day <- format(freq$date, "%Y-%m-%d")
  	freq$dayCount <- seq(1:nrow(freq))
  }
	
	# clean data frame
	freq <- freq[,-which(names(freq)=="x")]	# drop col x
	freq$freqShift <- freq$freq	# shift col freq to right
	freq <- freq[,-which(names(freq)=="freq")]	# drop col freq
	names(freq)[which(names(freq)=="freqShift")] <- "freq"
	freq$freqRel <- round(freq$freq * 100 / max(freq$freq, na.rm = TRUE))
	
	# save
	if(length(save) == 1) {
  	save <- match.arg(save)
    saveZeit(freq, path = paste0(getwd(), "/rzeit"), name = paste(str_replace_all(ls$queryTerm, "\\s", "_"), "frequencies_sorted_by", sortby, sep = "_"), format = save)
  }
	
	return(freq)
}
