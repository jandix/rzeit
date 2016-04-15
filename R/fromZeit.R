#'@title Connects to ZEIT Online API
#'@description Exposes a search in the ZEIT online archive and returns results for the given query.
#'@param api character. The personal api code. To request an API key see: \url{http://developer.zeit.de/quickstart/} This parameter is by default set to the R Environment.
#'@param q character. Search query term. Search is performed on the article body, headline and byline.
#'@param limit character. The number of results given back.limit should not exceed 1000.
#'@param dateBegin character. Begin date - Restricts responses to results with publication dates of the date specified or later. In the form YYYY-MM-DD.
#'@param dateEnd character. End date - Restricts responses to results with publication dates of the date specified or earlier. In the form YYYY-MM-DD.
#'@param split logical. If \code{split = TRUE} the search is not performed within the whole date range, but splitted into monthly searches. This enables to exceed the limit of 1000 articles returned. Attention: this can take quite a while, because after every query the system sleeps for one second.
#'@param multipleTokens logical. Should be searched for multiple tokens?
#'@details \code{fromZeit.R} is the function, which interacts directly with the Zeit Online API. We only used the content endpoint for this package. There are further endpoints (eg. /author, /product) not included into this package to further specify the search if needed . The whole list of possible endpoints can be accessed here \url{http://developer.zeit.de/docs/}
#'@details \code{fromZeit.R} is the function, which interacts directly with the ZEIT Online API. We only used the content endpoint for this package. There are further endpoints (e.g. /author, /product) not included into this package to further specify the search if needed. The whole list of possible endpoints can be accessed here \url{http://developer.zeit.de/docs/}.
#'
#' If \code{multipleTokens = TRUE} the search query "Angela Merkel" returns all articles for both "Angela" and "Merkel", elsewise it returns results for the entire string ("Angela Merkel").
#'
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{zeitFrequencies}} \code{\link{zeitToDf}} \code{\link{zeitPlot}}
#'@references \url{http://developer.zeit.de}
#'@return A list including articles and meta information about the query.
#'@examples
#'
#' ## Example 1: simple query with the limit of 1000 returned articles between 2002 and 2007, API Key is set manually.
#'
#'    firstQuery <- fromZeit(api = *set your personal API Key here*,
#'                          q = "Angela Merkel",
#'                          split = FALSE,
#'                          limit = "1000",
#'                          dateBegin = "2002-01-01",
#'                          dateEnd = "2007-12-31")
#'
#' ## Example 2: Because the number of available articles exceeds the limit of 1000 we use \code{split = TRUE}, further we set the API Key in advance.
#'
#'    options("zeitApiKey" = *set your personal API Key here*)
#'
#'    termsSplit <- fromZeit(q = "Angela Merkel",
#'                          split = TRUE,
#'                          limit = "1000",
#'                          dateBegin = "2002-01-01",
#'                          dateEnd = "2007-12-31")
#'}
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}  Christian Graul
#'@export

fromZeit <- function(api = Sys.getenv("zeit_api_key"),
                     q,
                     limit = "50",
                     dateBegin = "2004-01-01",
                     dateEnd = "2014-12-31",
                     split = TRUE,
                     multipleTokens = FALSE){
  # variables saving additionally in the list
  query <- q


  # removing whitespaces
  if (multipleTokens == TRUE) {
  q <- stringr::str_replace_all(q, "\\s", "\\+")
  } else {
    q <- stringr::str_replace_all(q, "\\s", "%20")
  }

  # pasting release_date
  dateQuery <- paste("+release_date:[", dateBegin, "T00:00:00Z%20TO%20", dateEnd, "T23:59:59.999Z]", sep = "")

  # pasting the URL for API requirements
  base <- paste("http://api.zeit.de/content", "?api_key=", api, "&q=", q,  sep = "")
  url <- paste(base, dateQuery, "&limit=", limit,  sep = "")


  if(split == TRUE){

    zeitSplitSearch(base = base,
                    url = url,
                    begin = dateBegin,
                    end = dateEnd,
                    q = query)

  }

  else{

    # translate JSON into R Object
    returnList <- jsonlite::fromJSON(url)

    # add variables from beginning

    returnList$searchURL <- url
    returnList$queryTerm <- query

    # return matches
    return(returnList)

  }
}
