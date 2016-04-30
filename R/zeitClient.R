#'@title Client status and API usage
#'@description \code{zeitClient} lets you get information about your API access status and usage.
#'@param api character. The personal api code. To request an API key see: \url{http://developer.zeit.de/quickstart/} This parameter is by default set to the R Environment.
#'@param print logical. If \code{TRUE} (default), the client information is printed.
#'@return a list of information about the client and API usage
#'@author Christian Graul
#'@export
#'@examples
#'\dontrun{
#'zeitClient()
#'}
zeitClient <- function(api = Sys.getenv("zeit_api_key"), print = TRUE) {
	# make request
  url <- paste0("http://api.zeit.de/client", "?api_key=", api)
  client <- fromJSON(url)
  client$reset <- as.POSIXct(client$reset, origin="1970-01-01")
  
  # return
  if(print) print(client)
  invisible(client)
}
