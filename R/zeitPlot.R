#'@title Plot results of API request
#'@description Function to plot the results of the data frames by frequencies and time automatically. Attention: Only works with the frequency data frames created by the internal function \code{zeitSorting} of the \code{rzeit} package.
#'@param df data frame. Data frame that is the basis for the graph.
#'@param yTitle character. Labels the y-axis.
#'@param xTitle character. Labels the x-axis.
#'@param title character. Title of the graph.
#'@param grey logical. Indicates if print grey or in colored.
#'@param absolute logical. Indicates if absolute or relative frequencies are plotted on the y-axis.
#'@param lowessFactor numeric. "This gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness." For more details see \code{\link[stats]{lowess}}
#'@details If the data frame includes less than 20 observations, no trend line is plotted. The dotted line indicates the mean of frequencies.
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitFrequencies}} \code{\link{zeitToDf}}
#'@return Plots a graph based on given data frame.
#'@examples
#'\dontrun{
#'zeitPlot(dfFrequenciesWeek, yTitle = "Week", xTitle = "Frequencies of articles")
#'}
#'@author Jan Dix, \email{jan.dix@@uni-konstanz.de} Jana Blahak, \email{jana.blahak@@uni-konstanz.de}
#'@export

zeitPlot <- function(df, yTitle = "frequencies", xTitle = "time", title = NULL, grey = FALSE, absolute = TRUE, lowessFactor = 0.2){
		
	if (absolute == TRUE) {
		yAxis <- df$freq
    yLimit <- c(0, max(yAxis) + 10)
	} else {
		yAxis <- df$freqPro
		yLimit <- c(1, 100)
	}

  if (grey == TRUE) {
		colBar <- "#C1C1C1"
		colLine <- "#181818"
	} else {
		colBar <- "#d2691e"
		colLine <- "#CD3333"
	}

	plot(df$date,
       yAxis,
       type = "h",
       col  = colBar,
       main = title,
       ylab = yTitle,
       xlab = xTitle,
       ylim = yLimit)
	abline(h = mean(yAxis), col = "black", lty = "dashed")
	axis(4, at = round(mean(yAxis), digits = 2))

	if (nrow(df) > 20) {
		lines(lowess(yAxis ~ df$date, f = lowessFactor), col = colLine, lwd = 2)
	}
}
