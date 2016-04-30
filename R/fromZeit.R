#'@title Connects to ZEIT Online API
#'@description Exposes a search in the ZEIT online archive and returns results for the given query.
#'@param api character. The personal api code. To request an API key see: \url{http://developer.zeit.de/quickstart/} This parameter is by default set to the R Environment.
#'@param q character (vector). Search query term.
#'@param limit integer. The number of results given back. If \code{limit} exceeds 1000 or is set to \code{"all"}, a comlete list of matches is returned (Attention: this can take quite a while, because after every query the system sleeps for one second).
#'@param dateBegin character. Begin date - Restricts responses to results with publication dates of the date specified or later. In the form YYYY-MM-DD.
#'@param dateEnd character. End date - Restricts responses to results with publication dates of the date specified or earlier. In the form YYYY-MM-DD.
#'@param fields character (vector). Selection of output fields. Possible values are: \code{subtitle}, \code{uuid}, \code{title}, \code{href}, \code{release_date}, \code{uri}, \code{snippet}, \code{supertitle}, \code{teaser_text} and \code{teaser_title}.
#'@details \code{fromZeit.R} is the function, which interacts directly with the ZEIT Online API. We only used the content endpoint for this package. There are further endpoints (e.g. /author, /product) not included into this package to further specify the search if needed. The whole list of possible endpoints can be accessed here \url{http://developer.zeit.de/docs/}.
#'
#'\emph{Query building}
#'
#'By default the search is performed on the article body, headline and byline. Article fields can be queried individually by a combination of the field name and the search term, separated by a semicolon. Available query fields are "title", "subtitle", "supertitle", "teaser_text" and "teaser_title".
#'Multiple tokens are possible using a vector of strings. Strings in a vector are "OR"-combined by default. Boolean operator can be changed to "AND" by starting a string with "+".
#'\tabular{lll}{
#'Query \tab Result \cr
#'q="merkel" \tab articles containing "Merkel" in body, headline or byline \cr
#'q="angela merkel" \tab articles containing "Angela Merkel" in body, headline and byline \cr
#'q=c("angela", "merkel") \tab articles containing "Angela" or "Merkel" in article body, headline and byline \cr
#'q=c("angela", "+merkel") \tab articles containing "Angela" and "Merkel" in article body, headline and byline \cr
#'q="title:merkel" \tab articles containing "Merkel" in headline \cr
#'q="title:angela merkel" \tab articles containing "Angela Merkel" in headline \cr
#'q=c("title:merkel", "+subtitle:merkel") \tab articles containing "Merkel" in headline and byline \cr
#'q=c("merkel", "+title:merkel", "+subtitle:merkel") \tab articles containing "Merkel" in article body, headline and byline
#'}
#'
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{zeitFrequencies}} \code{\link{zeitToDf}} \code{\link{zeitPlot}}
#'@references \url{http://developer.zeit.de}
#'@return A list including articles and meta information about the query.
#'@examples
#'\dontrun{
#' ## Example 1: simple query with the limit of 1000 returned articles 
#' ## between 2002 and 2007, API Key is set manually.
#'
#'    merkel <- fromZeit(api = *set your personal API Key here*,
#'                          q = "angela merkel",
#'                          limit = 1000,
#'                          dateBegin = "2002-01-01",
#'                          dateEnd = "2007-12-31")
#'
#' ## Example 2: Because the number of available articles exceeds the limit of 1000 
#' ## we use \code{limit = "all"}, further we set the API Key in advance.
#'
#'    options("zeitApiKey" = *set your personal API Key here*)
#'
#'    merkel_all <- fromZeit(q = "angela merkel",
#'                          limit = "all",
#'                          dateBegin = "2002-01-01",
#'                          dateEnd = "2007-12-31")
#'}
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}  Christian Graul
#'@export

fromZeit <- function(api = Sys.getenv("zeit_api_key"),
                     q,
                     limit = 50,
                     dateBegin = "2004-01-01",
                     dateEnd = "2014-12-31",
                     fields){
  
  # variables saving additionally in the list
  query <- q
  
  # prepare query
  q <- str_replace_all(q, "\\s", "%20") # replace whitespaces
  q <- sapply(q, function(x) if(length(grep("%20", x, fixed=TRUE))!=0) paste0("%22", x, "%22") else paste(x)) # enclose multiple tokens with "
  # correct fields
  q <- str_replace_all(q, "%22subtitle:", "subtitle:%22")
  q <- str_replace_all(q, "%22title:", "title:%22")
  q <- str_replace_all(q, "%22supertitle:", "supertitle:%22")
  q <- str_replace_all(q, "%22teaser_text:", "teaser_text:%22")
  q <- str_replace_all(q, "%22teaser_title:", "teaser_title:%22")
  # prepare multiple tokens and boolean operators
  if(length(q) > 1) {
  	q <- paste0(q, collapse="%20OR%20")
		q <- gsub("%20OR%20+", "%20AND%20", q, fixed=TRUE)
  }

  # pasting release_date
  dateQuery <- paste0("%20AND%20release_date:[", dateBegin, "T00:00:00Z%20TO%20", dateEnd, "T23:59:59.999Z]")
	
	# prepare fields
  avail.fields <- c("subtitle", "uuid", "title", "href", "release_date", "uri", "snippet", "supertitle", "teaser_text", "teaser_title")
  if (!missing(fields)) {
		fields <- avail.fields[pmatch(fields, avail.fields)]
		if(length(fields)>1) fields <- paste0(fields, collapse=",")
	} else fields <- NULL

  # pasting the URL for API requirements
  if(limit == "all") limit = 1001
  base <- paste0("http://api.zeit.de/content", "?api_key=", api, "&q=", q)
  if(is.null(fields)) {
  	url <- paste0(base, dateQuery, "&limit=", limit)
  } else {
  	url <- paste0(base, dateQuery, "&fields=", fields, "&limit=", limit)
  }

  if(limit > 1000) {
    zeitSplitSearch(base = base,
                    url = url,
                    begin = dateBegin,
                    end = dateEnd,
                    q = query,
                    fields = fields)
  } else {
    # translate JSON into R Object
    returnList <- fromJSON(url)

    # add variables from beginning
    url <- str_replace_all(url, "api_key=[A-Za-z0-9]*&q=", "api_key=APIKEYHIDDEN&q=") # hide key 
    returnList$searchURL <- url
    returnList$queryTerm <- query

    # return matches
    return(returnList)
  }
}
