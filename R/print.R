print.rzt_client <- function (x, ...) {
  cat("<RZeit Client Information>\n", sep = "")
  for (i in x) {
    str(i)
  }
  invisible(x)
}

print.rzt_result <- function (x, ...) {
  cat("<RZeit Result Set>\n", sep = "")
  str(x$meta)
  invisible(x)
}
