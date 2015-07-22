#' zeitSplitSearch
#'
#' Allows to split the search in order to cover larger timespans.
#'
#' @param x url as character
#' @param begin Begin date - Restricts responses to results with publication dates of the date specified or later. In the form YYYY-MM-DD
#' @param end End date - Restricts responses to results with publication dates of the date specified or earlier. In the form YYYY-MM-DD
#'
#' @details \code{zeitSplitSearch} limits how many article are returend for each month, so that the search can cover a large timespan, even if one month originally contained an extremly large number of articles.
#' @import lubridate


zeitSplitSearch <- function(base, url, begin, end, q){

  urlreturn <- NULL

  dateVector <- seq.Date(from = as.Date(begin), to = as.Date(end), by = "month")

  startDate <- dateVector[length(dateVector)]
  dateVector[length(dateVector) + 1] <- startDate %m+% months(1)

  i <- as.numeric(length(dateVector)) - 1

  df <- data.frame(NULL)

  lsTest <- jsonlite::fromJSON(url)

  if(lsTest$found < 1000){

    lsTest$searchURL <- url
    lsTest$queryTerm <- q
    return(lsTest)

  } else{

    while (i != 0){

      replaceDate <- paste("+release_date:[", as.character(dateVector[i]), "T00:00:00Z%20TO%20", as.character(dateVector[i+1]-1), "T23:59:59.999Z]", sep = "")

      #+release_date:[2013-01-01T00:00:00Z%20TO%202015-01-01T23:59:59.999Z]

      x <- paste(base, replaceDate, "&limit=1000", sep = "")

      urlreturn <- c(urlreturn, x)

      # translate JSON into R Object
      y <- jsonlite::fromJSON(x)

      # transform into DataFrame
      y <- as.data.frame(y[1])

      # combine new and latter DataFrame
      df <- rbind(df, y)

      # system sleep
      Sys.sleep(0.5)


      i <- i - 1
    }

  }

  ls <- list(df)
  ls$returned <- nrow(df)
  ls$offset <- lsTest$offset
  ls$searchURL <- url
  ls$queryTerm <- q

  # return matches
  return(ls)
}
