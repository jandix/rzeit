## ----install github, warning = FALSE, results = "hide", message = FALSE, eval = FALSE----
#  devtools::install_github("tollpatsch/rzeit")

## ----loading rzeit, warning = FALSE, results = "hide", message = FALSE, eval = TRUE----
library(rzeit)

## ----loading required packages, warning = FALSE, error = FALSE, results = "hide", message = FALSE, eval=FALSE----
#  library(stringr)
#  library(jsonlite)
#  library(lubridate)

## ---- setKey, warning = FALSE, message = FALSE, eval = FALSE-------------
#  zeitSetApiKey(apiKey = "set_your_api_key_here")

## ----first, eval = TRUE--------------------------------------------------
results <- fromZeit(q = "Angela Merkel",
                    limit = "100",
                    dateBegin = "2015-06-01",
                    dateEnd = "2015-08-01")

## ----second, eval = TRUE-------------------------------------------------
freq <- zeitFrequencies(ls = results,
                        sort = "days",
                        save = FALSE) 
head(freq)

## ----third, eval = TRUE--------------------------------------------------
articles <- zeitToDf(ls = results,
                          sort = "days",
                          save = FALSE) 
names(articles)

## ----fourth, eval = FALSE------------------------------------------------
#  zeitPlot(df = freq)

## ----withSplit, eval = FALSE---------------------------------------------
#  results_split <- fromZeit(q = "Angela Merkel",
#                                   limit = "1000",
#                                   dateBegin = "2013-01-01",
#                                   dateEnd = "2014-12-31",
#                                   multipleTokens = TRUE)
#  
#  results_split <- zeitFrequencies(ls = results_split,
#                                          sort = "month",
#                                          save = FALSE)
#  
#  frequencies_split <- results_split

## ----withoutSplit, eval = FALSE------------------------------------------
#  results_withoutsplit <- fromZeit(q = "Angela Merkel",
#                            split = FALSE,
#                            limit = "1000",
#                            dateBegin = "2013-01-01",
#                            dateEnd = "2014-12-31",
#                            multipleTokens = FALSE)
#  
#  results_withoutsplit <- zeitFrequencies(ls = results_withoutsplit,
#                                   sort = "month",
#                                   save = FALSE)
#  
#  frequencies_withoutsplit <- results_withoutsplit

## ---- fig.show = "hold", eval = FALSE------------------------------------
#  par(mfrow=c(1, 2))
#  zeitPlot(frequencies_withoutsplit, title = "without split", absolute = FALSE)
#  zeitPlot(frequencies_split, title = "with split", absolute = FALSE)

