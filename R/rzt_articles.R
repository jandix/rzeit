rzt_articles <- function(q, api_key = Sys.getenv("rzt_api_key")) {

  # define the base url
  url <- "http://api.zeit.de/content"

  # define the api key
  api_key <- paste0("api_key=", api_key)

  # define query
  query <- paste0("q=", q)

  # paste and modify url
  url <- paste0(url, "?",
                api_key, "&",
                query)

  # get data
  response <- httr::GET(url)

  # if response is not json
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse result set
  result <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

  # error message
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "ZEIT Online API request failed [%s]\n%s",
        httr::status_code(response),
        result$description
      ),
      call. = FALSE
    )
  }

  # create empty list for articles
  articles <- NULL

  # sort articles into a list
  for (match in result[[1]]) {
    # create an article object
    article <- structure(
      list(
        content = list(
          title = match$title,
          subtitle = match$subtitle,
          supertitle = match$supertitle,
          teaser_title = match$teaser_title,
          teaser_text = match$teaser_text,
          snippet = match$snippet
        ),
        meta = list(
          uuid = match$uuid,
          href = match$href,
          uri = match$uri,
          release_date = match$release_date
        )
      ),
      class = "rzt_article"
    )
    # add the article to the list
    articles <- c(articles, article)
  }

  # create and return result set
  structure(
    list(
      articles = articles,
      meta = list(
        endpoint = "/content",
        found = result$found,
        limit = result$limit,
        offset = result$offset,
        query = stringr::str_extract(url, "q=.+"),
        url = stringr::str_replace(url, "api_key=.+&", "api_key=***&")
      )
    ),
    class = "rzt_result"
  )
}
