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
zeitFrequencies <- function(ls, sort = c("years", "months", "weeks", "days"), save = c("txt", "sps", "sas", "dta")){
	
	# check ls
	if(is.null(ls[["matches"]][["release_date"]])) stop("Field 'release_date' is required to use 'zeitFrequencies' but missing")
	
	# convert to data.frame
  df <- as.data.frame(ls[1])
	
	# saving
	if(length(save) > 1) {
  	save <- NA
  } else {
  	save <- match.arg(save)
  }

  # switch between answers
  sortby <- match.arg(sort)
  switch(sortby,
         years = yearsort(ls, save = save, freq = TRUE),
         months = monthsort(ls, save = save, freq = TRUE),
         weeks = weeksort(ls, save = save, freq = TRUE),
         days = daysort(ls, save = save, freq = TRUE))
}
