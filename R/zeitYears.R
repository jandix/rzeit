# function to sort by year
yearsort <- function(ls, save, freq) {

  # saving list as data frame
  df <- as.data.frame(ls[1])

  # defining start and end date adding count of years
  dateStart <- df$matches.release_date[1]
  dateEnd <- tail(df$matches.release_date, n = 1)
  dateStart <- str_extract(dateStart, "\\d{4}-\\d{2}-\\d{2}")
  dateEnd <- str_extract(dateEnd, "\\d{4}-\\d{2}-\\d{2}")
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)

  year <- seq.Date(from = dateEnd, to = dateStart+365, by = "year")
  year <- format(year, "%Y")
  dateOne <- as.Date(paste(year[1], "01", "01", sep = "-"))

  dfFreqs <- data.frame(year)
  dfFreqs$date <- seq.Date(dateOne, length.out = nrow(dfFreqs), by = "year")
  dfFreqs$yearCount <- seq(1:nrow(dfFreqs))

  i <- as.numeric(nrow(dfFreqs))

  dfFreqs$freq <- 0
  yearnum <- 1:length(ls[1][1])
  
  while (i > 0){
    j <- as.numeric(nrow(df))

    while (j > 0) {
      date <- df$matches.release_date[j]
      date <- str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
      date <- as.Date(date)
      date <- format(date, "%Y")
      
      if(date == dfFreqs$year[i]) {
        dfFreqs$freq[i] <- dfFreqs$freq[i] + 1
        yearnum[j] <- dfFreqs$yearCount[i]
      } 
      j <- j - 1
    }
    i <- i - 1
  }

  # relative frequencies
  maxi <- max(dfFreqs$freq)
  dfFreqs$freqPro <- round(dfFreqs$freq * 100 / maxi)

  # creating table of articles
  dfArticle <- data.frame(yearnum)
  dfArticle$date <- as.Date(df$matches.release_date)
  dfArticle$title <- df$matches.title
  dfArticle$subtitle <- df$matches.subtitle
  dfArticle$snippet <- df$matches.snippet
  dfArticle$teaserText <- df$matches.teaser_text
  dfArticle$teaserTitle <- df$matches.teaser_title
  dfArticle$link <- df$matches.href

  lastRow <- as.numeric(nrow(dfFreqs))
  if (dfFreqs$freq[lastRow] == 0) {
    dfFreqs <- dfFreqs[-lastRow, ]
  }

  if (freq) {
    if (!is.na(save)) {
      saveZeit(dfFreqs,
               path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "freqs", "byYear", sep = "_"),
               format = save)
    }
    return(dfFreqs)
  } else {
    if (!is.na(save)) {
      saveZeit(dfArticle, path = paste(getwd(), "/df", sep = ""),
               name = paste(ls$queryTerm, "articles", "byYear", sep = "_"),
               format = save)
    }
    return(dfArticle)
  }
}
