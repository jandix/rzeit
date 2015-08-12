## ----basicconsole, warning = FALSE, results = "hide", message = FALSE----
library(rzeit)
library(stringr)
library(jsonlite)
library(lubridate)
library(rvest)
library(plyr)
library(networkD3)

## ----Gathering Data from Wikipedia, eval = TRUE--------------------------
### parse website
government_url <- "https://de.wikipedia.org/wiki/Bundesregierung_%28Deutschland%29"
government_parsed <- html(government_url, encoding = "UTF8")

### import and tidy table
government_tables <- html_table(government_parsed)
government_df <- government_tables[[1]]

government_df <- rename(government_df,
                        c("Amtsinhaber" = "name"))
government_df <- rename(government_df,
                        c("Partei" = "party"))


government_df$name <- as.character(government_df$name)
government_df$partei <- as.character(government_df$party)
government_df$number <- 0:15

government_df <- government_df[, -1]
government_df <- government_df[, -1]

## ----Show table, eval = TRUE---------------------------------------------
head(government_df)

## ----Query data frame, eval = TRUE---------------------------------------
i <- 1
from <- NULL
to <- NULL

while (i <= nrow(government_df)){
  j <- i + 1
  while (j <= nrow(government_df)){
    from <- rbind(from, government_df$name[i])
    to <- rbind(to, government_df$name[j])
    j <- j + 1
  }
  i <- i + 1
}

count_df <- as.data.frame(from, stringsAsFactors = FALSE)
count_df <- rename(count_df, c("V1" = "from"))
count_df$to <- as.character(to)

count_df$fromNumber <- NA
count_df$toNumber <- NA

i <- 1
while (i <= nrow(count_df)){
  j <- 1
  while(j <= nrow(government_df)){  
    if (government_df$name[j] == count_df$to[i]){
      count_df$toNumber[i] <- government_df$number[j]
    }
    j <- j + 1
  }
  i <- i + 1
}

i <- 1
while (i <= nrow(count_df)){
  j <- 1
  while(j <= nrow(government_df)){  
    if (government_df$name[j] == count_df$from[i]){
      count_df$fromNumber[i] <- government_df$number[j]
    }
    j <- j + 1
  }
  i <- i + 1
}

## ----Show count_df, eval = TRUE------------------------------------------
head(count_df)

## ----Perform queries, eval = FALSE---------------------------------------
#  zeitSetApiKey("set_your_api_key_here")
#  
#  count_df$count <- 0
#  i <- 1
#  
#  while (i <= nrow(count_df)){
#    query = paste(count_df$from[i],
#                  count_df$to[i], sep = " ")
#    articles <- fromZeit(q = query,
#                         limit = "1",
#                         dateBegin = "2014-01-01",
#                         dateEnd = "2015-08-10")
#    count_df$count[i] <- count_df$count[i] + as.numeric(articles$found)
#    Sys.sleep(0.5)
#    i <- i + 1
#  }

## ----Counting, eval = FALSE----------------------------------------------
#  i <- 1
#  government_df$mentioned <- 0
#  
#  while (i <= nrow(government_df)){
#    j <- 1
#    while (j <= nrow(count_df)){
#      government_df$mentioned[i] <- ifelse(count_df$from[j] == government_df$name[i],
#                                           government_df$mentioned[i] + count_df$count[j],
#                                           government_df$mentioned[i])
#  
#      j <- j + 1
#    }
#    i <- i + 1
#  }
#  
#  i <- 1
#  
#  while (i <= nrow(government_df)){
#    j <- 1
#    while (j <= nrow(count_df)){
#      government_df$mentioned[i] <- ifelse(count_df$to[j] == government_df$name[i],
#                                           government_df$mentioned[i] + count_df$count[j],
#                                           government_df$mentioned[i])
#  
#      j <- j + 1
#    }
#    i <- i + 1
#  }

## ----corrections 1, eval = FALSE-----------------------------------------
#  government_df$mentioned <- round(government_df$mentioned / max(government_df$mentioned) * 500)

## ----loading data sets, echo = FALSE-------------------------------------
load("df/count_df.Rda")
load("df/government_df.Rda")

## ----corrections 2-------------------------------------------------------
sample <- count_df[count_df$count > 10, ]

## ----Plot network--------------------------------------------------------
sample <- count_df[count_df$count > mean(count_df$count), ]

forceNetwork(Links = sample,
             Nodes = government_df,
             Source = "fromNumber",
             Target = "toNumber",
             Value = "count",
             NodeID = "name",
             Group = "party",
             Nodesize = "mentioned",
             linkDistance = JS("function(d){return d.value}"),
             linkWidth = JS("function(d){return d.value / 50}"),
             opacity = 0.8,
             width = 800,
             height = 400,
             legend = TRUE,
             colourScale = 
             JS('d3.scale.ordinal()
                .domain(["SPD", "CSU","CDU"])
                .range(["#e1001a", "#007dbd" , "#e95d0f"]);'),)


