# function to sort by day
daysort <- function(ls, save, freq) {

  # saving list as data frame
  df <- as.data.frame(ls[1])

  # create variable "date" seq from first to last date in "ls" by 1
  dateStart <- df$matches.release_date[1]
  dateEnd <- tail(df$matches.release_date, n = 1)
  dateStart <- str_extract(dateStart, "\\d{4}-\\d{2}-\\d{2}")
  dateEnd <- str_extract(dateEnd, "\\d{4}-\\d{2}-\\d{2}")
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)
  date <- seq.Date(from = dateEnd, to = dateStart, by = "day")
  dfFreqs <- data.frame(date)
  dfFreqs$dayCount <- seq(1:nrow(dfFreqs))

  i <- as.numeric(nrow(dfFreqs))

  dfFreqs$freq <- 0
  daynum <- 1:length(ls[1][1])
  while (i > 0){
    j <- as.numeric(nrow(df))

    while (j > 0) {
      date <- df$matches.release_date[j]
      date <- str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
      date <- as.Date(date)

      if(date == dfFreqs$date[i]) {
        dfFreqs$freq[i] <- dfFreqs$freq[i] + 1
        daynum[j] <- dfFreqs$dayCount[i]
      }
      j <- j - 1
    }
    i <- i - 1
  }

  # relative frequencies
  maxi <- max(dfFreqs$freq)
  dfFreqs$freqPro <- round(dfFreqs$freq * 100 / maxi)

  # creating table of articles
  dfArticle <- data.frame(daynum)
  dfArticle$date <- as.Date(df$matches.release_date)
  dfArticle$title <- df$matches.title
  dfArticle$subtitle <- df$matches.subtitle
  dfArticle$snippet <- df$matches.snippet
  dfArticle$teaserText <- df$matches.teaser_text
  dfArticle$teaserTitle <- df$matches.teaser_title
  dfArticle$link <- df$matches.href
  

  if (freq == TRUE){
    if (save == TRUE){
      if(is.null(getOption("zeitSaveDf"))){
        options("zeitSaveDf" = "txt")
      }
      saveZeit(dfFreqs,
               path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "freqs", "byDay", sep = "_"),
               format = getOption("zeitSaveDf"))
    }
    return(dfFreqs)
  } else{
    if (save == TRUE){
      if(is.null(getOption("zeitSaveDf"))){
        options("zeitSaveDf" = "txt")
      }
      saveZeit(dfArticle, path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "articles", "byDay", sep = "_"),
               format = getOption("zeitSaveDf"))
    }
    return(dfArticle)
  }
}
