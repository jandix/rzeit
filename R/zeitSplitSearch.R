#'@title Split search request
#'@description Allows to split the search in order to cover larger timespans.
#'@param base base url as character
#'@param url as character
#'@param begin Begin date - Restricts responses to results with publication dates of the date specified or later. In the form YYYY-MM-DD
#'@param end End date - Restricts responses to results with publication dates of the date specified or earlier. In the form YYYY-MM-DD
#'@param q query term as character
#'@param fields selection of output fields as character
#'@details \code{zeitSplitSearch} limits how many article are returend for each month, so that the search can cover a large timespan, even if one month originally contained an extremly large number of articles.

zeitSplitSearch <- function(base, url, begin, end, q, fields) {

  urlreturn <- NULL
  dateVector <- seq.Date(from = as.Date(begin), to = as.Date(end), by = "month")
  startDate <- dateVector[length(dateVector)]
  dateVector[length(dateVector) + 1] <- startDate %m+% months(1)
  i.max <- i <- as.numeric(length(dateVector)) - 1
  
  pb <- txtProgressBar(min = 1, max = i.max, style = 3)
  
  df <- data.frame(NULL)
  lsTest <- fromJSON(url)
  
  if(lsTest$found < 1000) {
		url <- str_replace_all(url, "api_key=[A-Za-z0-9]*&q=", "api_key=APIKEYHIDDEN&q=") # hide key
		lsTest$searchURL <- url
		lsTest$queryTerm <- q
		return(lsTest)
	} else {

		while (i != 0) {

      replaceDate <- paste0("%20AND%20release_date:[", as.character(dateVector[i]), "T00:00:00Z%20TO%20", as.character(dateVector[i+1]-1), "T23:59:59.999Z]")
			
			if(is.null(fields)) {
		  	x <- paste0(base, replaceDate, "&limit=1000")
		  } else {
		  	x <- paste0(base, replaceDate, "&fields=", fields, "&limit=1000")
		  }
			
      urlreturn <- c(urlreturn, x)

      # translate JSON into R Object
      y <- fromJSON(x)

      # transform into DataFrame
      y <- as.data.frame(y[1])

      # combine new and latter DataFrame
      df <- rbind.fill(df, y)

      # system sleep
      Sys.sleep(0.5)

      i <- i - 1
      setTxtProgressBar(pb, i.max-i)
		}
		close(pb)
	}

  ls <- list(matches=df)
  names(ls[["matches"]]) <- str_replace_all(names(ls[["matches"]]), "matches.", "")
  ls$returned <- nrow(df)
  ls$offset <- lsTest$offset
  url <- str_replace_all(url, "api_key=[A-Za-z0-9]*&q=", "api_key=APIKEYHIDDEN&q=") # hide key
  ls$searchURL <- url
  ls$queryTerm <- q

  # return matches
  return(ls)
}
