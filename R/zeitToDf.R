#'@title Returns a data frame with sorted articles
#'@description The returned data frame includes the articles, the related links and the number of either day/week/month/year, comparable to the data frame, created by \code{\link{zeitFrequencies}}.
#'@param ls list. List which will be converted into the data frame. Attention: This function only works with the returned element of \code{\link{fromZeit}}.
#'@param save logical. If \code{TRUE} data frame is automatically saved in choosen format.
#'@param sort character. Specifies how the results are sorted. Possible options are \code{year}, \code{month}, \code{week} or \code{day}. Note: \code{week} and \code{day} may take some time for a long time period!
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitFrequencies}} \code{\link{zeitPlot}}
#'@return data frame
#'@examples
#'\dontrun{
#'# sort by month without saving
#'zeitToDf(terms, "month")
#'
#'# sort by year and save as .sps
#'zeitToDf(terms, "day", "sps")
#'}
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}
zeitToDf <- function(ls, sort = c("years", "months", "weeks", "days", "single"), save = FALSE){
#'@export
	
	# check ls
	if(is.null(ls[["matches"]][["release_date"]])) stop("Field 'release_date' is required to use 'zeitToDf' but missing")
	
  # convert to data.frame
  df <- as.data.frame(ls[1])
  # switch between answers
  sortby <- match.arg(sort)
  switch(sortby,
         years = yearsort(ls, save = save, freq = FALSE),
         months = monthsort(ls, save = save, freq = FALSE),
         weeks = weeksort(ls, save = save, freq = FALSE),
         days = daysort(ls, save = save, freq = FALSE))
}
