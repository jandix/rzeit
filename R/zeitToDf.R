#' Returns a data frame with sorted articles
#'
#' The data frame includes the articles, the related links and the number of either day/week/month/year, comparable to the data frame, created by \code{\link{zeitFrequencies}}.
#'
#'@param ls list. List which will be converted into the data frame. Attention: This function only works with the returned element of \code{\link{fromZeit}}.
#'@param sort character. Specifies how the results are sorted
#'possible options:
#' \itemize{
#'   \item year
#'   \item month
#'   \item week
#'   \item day
#'   \item single
#'   }
#'@param save logical. If \code{TRUE} data frame is automatically saved in choosen format.
#'@details Dataframes can be saved in different formats. You can choose your preferred format by using \code{options("zeitSaveDf" = > set your option here <)} to set the option to your preferred option. The default format for save is txt.
#'Further options are:
#'  \itemize{
#'   \item txt
#'   \item sps
#'   \item sas
#'   \item dta
#'   }
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitFrequencies}} \code{\link{zeitPlot}}
#'@return data frame
#'@examples
#' ## Example 1: Returns a list with two data frames, sorted by months and saved as .txt
#'
#'    zeitToDf(terms, "month", save = TRUE)
#'
#'## Exapmle 2: Returns a list with two data frames, sorted by month and saved as .sps
#'
#'    options("zeitSaveDf" = "sps")
#'
#'    zeitToDf(terms, "day", save = TRUE)
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}
#' @export
zeitToDf <- function(ls, sort = c("years", "months", "weeks", "days", "single"), save = FALSE){

  df <- as.data.frame(ls[1])

  ### switch between answers

  sortby <- match.arg(sort)
  switch(sortby,
         years = yearsort(ls, save = save, freq = FALSE),
         months = monthsort(ls, save = save, freq = FALSE),
         weeks = weeksort(ls, save = save, freq = FALSE),
         days = daysort(ls, save = save, freq = FALSE),
         single = df)
}
