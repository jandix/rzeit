#'@title Returns a frequency table in choosen format
#'@description The returned data frame includes a continous list of dates in choosen sequences and the related frequencies. The number of either day/week/month/year, are comparable to the data frame, created by \code{\link{zeitToDf}}.
#'@param ls list. List which will be converted into the data frame. Attention: This function only works with the returned element of \code{\link{fromZeit}}.
#'@param save logical. If \code{TRUE} data frame is automatically saved in choosen format.
#'@param sort character. Specifies how the results are sorted. Possible options are \code{year} (default), \code{month}, \code{week} or \code{day}. Note: \code{week} and \code{day} may take some time for a long time period!	
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitToDf}} \code{\link{zeitPlot}}
#'@return data frame
#'@examples
#'\dontrun{
#'# sort by month without saving
#'zeitFrequencies(terms, "month")
#'
#'# sort by day and saved as sas
#'zeitFrequencies(terms, "day", "sas")
#'}
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}
#'@export

zeitFrequencies <- function(ls, sort = c("years", "months", "weeks", "days", "single"), save = FALSE){
	
	# check ls
	if(is.null(ls[["matches"]][["release_date"]])) stop("Field 'release_date' is required to use 'zeitFrequencies' but missing")
	
	# convert to data.frame
  df <- as.data.frame(ls[1])

  # switch between answers
  sortby <- match.arg(sort)
  switch(sortby,
         years = yearsort(ls, save = save, freq = TRUE),
         months = monthsort(ls, save = save, freq = TRUE),
         weeks = weeksort(ls, save = save, freq = TRUE),
         days = daysort(ls, save = save, freq = TRUE))
}
