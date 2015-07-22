## ----loading required packages, warning = FALSE, results = "hide", message = FALSE----
library(rzeit)

## ---- setKey, warning = FALSE, message = FALSE, eval = FALSE-------------
#  zeitSetApiKey(apiKey = "set_your_api_key_here")

## ----first, eval = FALSE-------------------------------------------------
#  results <- fromZeit(q = "Angela Merkel",
#                      limit = "50",
#                      dateBegin = "2012-01-01",
#                      dateEnd = "2015-05-20")

## ----second, eval = FALSE------------------------------------------------
#  freq <- zeitFrequencies(ls = results,
#                          sort = "days",
#                          save = FALSE)

## ----third, eval = FALSE-------------------------------------------------
#  articles <- zeitToDf(ls = results,
#                            sort = "days",
#                            save = FALSE)

## ----fourth, eval = FALSE------------------------------------------------
#  zeitPlot(df = freq)

## ----withoutSplit, eval = FALSE------------------------------------------
#  results_withoutSplit <- fromZeit(q = "Angela Merkel",
#                                   split = FALSE,
#                                   limit = "1000",
#                                   multipleTokens = TRUE,
#                                   dateBegin = "2013-01-01",
#                                   dateEnd = "2014-12-31")
#  
#  results_withoutSplit <- zeitFrequencies(ls = results_withoutSplit,
#                                          sort = "month",
#                                          save = FALSE)
#  
#  frequencies_withoutSplit <- results_withoutSplit

## ----withSplit, eval = FALSE---------------------------------------------
#  results_split <- fromZeit(q = "Angela Merkel",
#                            split = TRUE,
#                            limit = "1000",
#                            multipleTokens = FALSE,
#                            dateBegin = "2013-01-01",
#                            dateEnd = "2014-12-31")
#  
#  results_split <- zeitFrequencies(ls = results_split,
#                                   sort = "month",
#                                   save = FALSE)
#  
#  frequencies_split <- results_split
#  
#  

## ---- fig.show = "hold", eval = FALSE------------------------------------
#  par(mfrow=c(1, 2))
#  zeitPlot(frequencies_split, title = "with split", absolute = FALSE)
#  zeitPlot(frequencies_withoutSplit, title = "without split", absolute = FALSE)
#  par(mfrow=c(1, 1))

