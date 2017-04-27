rzt_client <- function(api_key = Sys.getenv("rzt_api_key")) {

  # define the base url
  url <- "http://api.zeit.de/client"

  # define the api key
  api_key <- paste0("api_key=", api_key)

  # paste and modify url
  url <- paste0(url, "?",
                api_key)

  # get data
  response <- httr::GET(url)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  result <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

  structure(
     list(
      reset = result$reset,
      name = result$name,
      tier = result$tier,
      requests = result$requests,
      api_key = result$api_key,
      quota = result$quota,
      email = result$email
    ),
    class = "rzt_client"
  )
}
