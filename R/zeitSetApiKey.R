#'@title Sets the API Key to the environment
#'@description Function to set you API Key to the R environment when starting using \code{rzeit} package. Attention: You should only execute this functions once.
#'@param apiKey character. The personal api code. To request an API key see: \url{http://developer.zeit.de/quickstart/}
#'@param path character. Path where the enviornment is stored. Default is the normalized path.
#'@seealso \code{\link{fromZeit}} \code{\link{zeitFrequencies}} \code{\link{zeitToDf}} \code{\link{zeitPlot}}
#'@examples
#'\dontrun{
#'zeitSetApiKey(apiKey = "this_is_your_api_key", path = "")
#'}
#'@author Jan Dix (\email{jan.dix@@uni-konstanz.de}), Jana Blahak (\email{jana.blahak@@uni-konstanz.de})
#'@export
zeitSetApiKey <- function(apiKey,
                          path = paste(normalizePath("~/"), ".Renviron", sep = "/")){

if(!file.exists(path)) file.create(path)

originalFile <- readLines(path, encoding="UTF-8")

zeit_api_key <- paste0(originalFile, "\nzeit_api_key=", apiKey)
writeLines(zeit_api_key, path)
}
