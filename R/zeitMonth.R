# function to sort by month
monthsort <- function(ls, save = FALSE, freq){

  # saving list as data frame

  df <- as.data.frame(ls[1])

  # defining start and end date adding weekdays and count of weeks
  dateStart <- df$matches.release_date[1]
  dateEnd <- tail(df$matches.release_date, n = 1)
  dateStart <- stringr::str_extract(dateStart, "\\d{4}-\\d{2}-\\d{2}")
  dateEnd <- stringr::str_extract(dateEnd, "\\d{4}-\\d{2}-\\d{2}")
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)

  dateMinus <- as.numeric(format(dateEnd, format = "%d")) - 1
  dateEnd<- dateEnd - dateMinus

  month <- seq.Date(from = dateEnd, to = dateStart+31, by = "month")
  date <- month
  dfFreqs <- data.frame(date)
  x <- format(month, "%b")
  y <- format(month, "%Y")
  month <- paste(x, y)
  dfFreqs$month <- month
  dfFreqs$monthCount <- seq(1:nrow(dfFreqs))


  i <- as.numeric(nrow(dfFreqs))

  dfFreqs$freq <- 0
  monthnum <- 1:length(ls[1][1])

  while (i > 0){

    j <- as.numeric(nrow(df))
    while (j > 0){

      date <- df$matches.release_date[j]
      date <- stringr::str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
      date <- as.Date(date)
      x <- format(date, "%b")
      y <- format(date, "%Y")
      date <- paste(x, y)

      if(date == dfFreqs$month[i]){

        dfFreqs$freq[i] <- dfFreqs$freq[i] + 1
        monthnum[j] <- dfFreqs$monthCount[i]
      }

      j <- j - 1
    }

    i <- i - 1
  }

  # relative frequencies
  maxi <- max(dfFreqs$freq)
  dfFreqs$freqPro <- round(dfFreqs$freq * 100 / maxi)

  # creating look up table for title, date, link and week

  dfArticle <- data.frame(monthnum)
  dfArticle$date <- as.Date(df$matches.release_date)
  dfArticle$title <- df$matches.title
  dfArticle$link <- df$matches.href

  lastRow <- as.numeric(nrow(dfFreqs))
  if (dfFreqs$freq[lastRow] == 0){
    dfFreqs <- dfFreqs[-lastRow, ]
  }

  if (freq == TRUE){
    # save if TRUE
    if (save == TRUE){
      if(is.null(getOption("zeitSaveDf"))){
        options("zeitSaveDf" = "txt")
      }
      saveZeit(dfFreqs,
               path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "freqs", "byMonth", sep = "_"),
               format = getOption("zeitSaveDf"))
    }


    return(dfFreqs)



  } else{

    # save if TRUE
    if (save == TRUE){
      if(is.null(getOption("zeitSaveDf"))){
        options("zeitSaveDf" = "txt")
      }
      saveZeit(dfArticle, path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "articles", "byMonth", sep = "_"),
               format = getOption("zeitSaveDf"))
    }
    return(dfArticle)
  }
}
