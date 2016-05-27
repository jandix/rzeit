#'@title Convert list to data frame with sorted articles
#'@description \code{zeitToDf} converts the list object created by \code{\link{fromZeit}} to a data frame.
#'@param ls list. List, created by \code{\link{fromZeit}}, that will be converted into the data frame.
#'@param sort character. Specifies how the results are sorted. Possible options are available variables: \code{"uuid"}, \code{"title"}, \code{"subtitle"}, \code{"supertitle"}, \code{"release_date"}, \code{"href"}, \code{"uri"}, \code{"snippet"}, \code{"teaser_text"} or \code{"teaser_title"} (or a subset of these, if the \code{fields} parameter was set in \code{fromZeit}-function).
#'@param save character. Specifies the file for automatic saving of the resulting data frame. Possible formats are \code{txt}, \code{sps}, \code{sas} or \code{dta}. No data is saved per default.
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} 
#'@return data frame
#'@examples
#'\dontrun{
#'# get data
#'mrkl <- fromZeit(q = "angela merkel", limit = "all", 
#'	dateBegin = "2002-01-01", dateEnd = "2007-12-31")
#'
#'# convert to data frame without sorting and saving
#'zeitToDf(ls = mrkl)
#'
#'# convert and sort by release_date
#'zeitToDf(ls = mrkl, sort = "release_date")
#'
#'# convert and save as .txt
#'zeitToDf(ls = mrkl, save = "txt")
#'}
#'@author Jan Dix (\email{jan.dix@@uni-konstanz.de}), Jana Blahak (\email{jana.blahak@@uni-konstanz.de}), Christian Graul (\email{christian.graul@@gmail.com})
#'@export
zeitToDf <- function(ls, sort = c("years", "months", "weeks", "days"), save = c("txt", "sps", "sas", "dta")){
	
	# check ls
	if(is.null(ls[["matches"]][["release_date"]])) stop("Field 'release_date' is required to use 'zeitToDf' but missing")
	
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
         years = yearsort(ls, save = save, freq = FALSE),
         months = monthsort(ls, save = save, freq = FALSE),
         weeks = weeksort(ls, save = save, freq = FALSE),
         days = daysort(ls, save = save, freq = FALSE))
}
