# function to sort by week
weeksort <- function(ls, save = FALSE, freq){

  # saving list as data frame

  df <- as.data.frame(ls[1])

  # defining start and end date adding weekdays and count of weeks
  dateStart <- df$matches.release_date[1]
  dateEnd <- tail(df$matches.release_date, n = 1)
  dateStart <- stringr::str_extract(dateStart, "\\d{4}-\\d{2}-\\d{2}")
  dateEnd <- stringr::str_extract(dateEnd, "\\d{4}-\\d{2}-\\d{2}")
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)
  while (weekdays(dateEnd) != "Sonntag"){
    dateEnd <- dateEnd - 1
  }
  date <- seq.Date(from = dateEnd, to = dateStart, by = "week")
  dfFreqs <- as.data.frame(date)
  dfFreqs$endWeek <- dfFreqs$date + 6
  dfFreqs$week <- seq(1:nrow(dfFreqs))
  dfFreqs$weekdays <- weekdays(date)

  # loop to sort articles into dfFreqs
  i <- as.numeric(nrow(dfFreqs))

  dfFreqs$freq <- 0
  weeknum <- 1:length(ls[1][1])
  while (i > 0){

    j <- as.numeric(nrow(df))

    while (j > 0){
      dateTest <- df$matches.release_date[j]
      dateTest <- stringr::str_extract(dateTest, "\\d{4}-\\d{2}-\\d{2}")
      dateTest <- as.Date(dateTest)

      if(dateTest >= dfFreqs$date[i] && dateTest <= dfFreqs$endWeek[i]){

        dfFreqs$freq[i] <- dfFreqs$freq[i] + 1
        weeknum[j] <- dfFreqs$week[i]
      }
      j <- j - 1
    }



    i <- i - 1
  }

  # relative frequencies
  maxi <- max(dfFreqs$freq)
  dfFreqs$freqPro <- round(dfFreqs$freq * 100 / maxi)

# creating look up table for title, date, link and week

  dfArticle <- data.frame(daynum)
  dfArticle$date <- as.Date(df$matches.release_date)
  dfArticle$title <- df$matches.title
  dfArticle$subtitle <- df$matches.subtitle
  dfArticle$snippet <- df$matches.snippet
  dfArticle$teaserText <- df$matches.teaser_text
  dfArticle$teaserTitle <- df$matches.teaser_title
  dfArticle$link <- df$matches.href



  if (freq == TRUE){
    # save if TRUE
    if (save == TRUE){
      if(is.null(getOption("zeitSaveDf"))){
        options("zeitSaveDf" = "txt")
      }
      saveZeit(dfFreqs,
               path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "freqs", "byWeek", sep = "_"),
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
               name = paste(ls$queryTerm, "articles", "byWeek", sep = "_"),
               format = getOption("zeitSaveDf"))
    }
    return(dfArticle)
  }
}
