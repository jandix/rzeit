#'@title Convert list to data frame with sorted articles
#'@description \code{zeitToDf} converts the list object created by \code{\link{zeitGet}} to a data frame.
#'@param ls list. List, created by \code{\link{zeitGet}}, that will be converted into the data frame.
#'@param sort character. Specifies how the results are sorted. Possible options are available variables: \code{"uuid"}, \code{"title"}, \code{"subtitle"}, \code{"supertitle"}, \code{"release_date"}, \code{"href"}, \code{"uri"}, \code{"snippet"}, \code{"teaser_text"} or \code{"teaser_title"} (or a subset of these, if the \code{fields} parameter was set in \code{zeitGet}-function).
#'@param save character. Specifies the file for automatic saving of the resulting data frame. Possible formats are \code{txt}, \code{sps}, \code{sas} or \code{dta}. No data is saved per default.
#'@details Files are saved to the working directory per default. Use \code{\link{zeitSave}} to change the path.
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{zeitGet}} 
#'@return data frame
#'@examples
#'\dontrun{
#'# get data
#'mrkl <- zeitGet(q = "angela merkel", limit = "all", 
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
zeitToDf <- function(ls, sort = c("uuid", "title", "subtitle", "supertitle", "release_date", "href", "uri", "snippet", "teaser_text", "teaser_title"), save = c("txt", "sps", "sas", "dta")) {
	
	# init variables
	df <- uuid <- title <- subtitle <- supertitle <- release_date <- href <- uri <- snippet <- teaser_text <- teaser_title <- NULL
	
	# prepare data frame
	if(!is.null(ls[["matches"]][["release_date"]])) {
		df <- data.frame(date = as.Date(ls[["matches"]][["release_date"]]))
	}
	if(!is.null(ls[["matches"]][["title"]])) {
		if(is.null(df)) df <- data.frame(title = ls[["matches"]][["title"]])
		else df$title <- ls[["matches"]][["title"]]
	}
	if(!is.null(ls[["matches"]][["subtitle"]])) {
		if(is.null(df)) df <- data.frame(subtitle = ls[["matches"]][["subtitle"]])
		else df$subtitle <- ls[["matches"]][["subtitle"]]
	}
	if(!is.null(ls[["matches"]][["href"]])) {
		if(is.null(df)) df <- data.frame(href = ls[["matches"]][["href"]])
		else df$href <- ls[["matches"]][["href"]]
	}
	if(!is.null(ls[["matches"]][["uri"]])) {
		if(is.null(df)) df <- data.frame(uri = ls[["matches"]][["uri"]])
		else df$uri <- ls[["matches"]][["uri"]]
	}
	if(!is.null(ls[["matches"]][["uuid"]])) {
		if(is.null(df)) df <- data.frame(uuid = ls[["matches"]][["uuid"]])
		else df$uuid <- ls[["matches"]][["uuid"]]
	}
	if(!is.null(ls[["matches"]][["supertitle"]])) {
		if(is.null(df)) df <- data.frame(title = ls[["matches"]][["supertitle"]])
		else df$supertitle <- ls[["matches"]][["supertitle"]]
	}
	if(!is.null(ls[["matches"]][["snippet"]])) {
		if(is.null(df)) df <- data.frame(snippet = ls[["matches"]][["snippet"]])
		else df$snippet <- ls[["matches"]][["snippet"]]
	}
	if(!is.null(ls[["matches"]][["teaser_text"]])) {
		if(is.null(df)) df <- data.frame(teaser_text = ls[["matches"]][["teaser_text"]])
		else df$teaser_text <- ls[["matches"]][["teaser_text"]]
	}
	if(!is.null(ls[["matches"]][["teaser_title"]])) {
		if(is.null(df)) df <- data.frame(teaser_title = ls[["matches"]][["teaser_title"]])
		else df$teaser_title <- ls[["matches"]][["teaser_title"]]
	}
	
	# sort
	sortby <- match.arg(sort)
	if(any(names(df) == sortby)) {
		df <- df[order(df[[sortby]]),]
	} else {
		message(sortby, "not found in data set. Data might be sorted by:", names(df))
	}
	
	# save
	if(length(save) == 1) {
  	save <- match.arg(save)
    zeitSave(df, path = paste0(getwd(), "/rzeit"), name = paste(str_replace_all(ls$queryTerm, "\\s", "_"), "articles_sorted_by", sortby, sep = "_"), format = save)
  }
	
	return(df)
}
