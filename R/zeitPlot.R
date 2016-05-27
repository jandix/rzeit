#'@title Plot frequency table
#'@description \code{zeitPlot} plots the frequency table created by \code{\link{zeitFrequencies}}.
#'@param df data.frame. Data set to be plotted, created by \code{\link{zeitFrequencies}}.
#'@param yTitle character. Labels the y-axis.
#'@param xTitle character. Labels the x-axis.
#'@param title character. Title of the graph.
#'@param absolute logical (default is \code{TRUE}). Indicates if absolute or relative frequencies are plotted on the y-axis.
#'@param trend logical (default is \code{TRUE}). Indicates if a trend line is plotted (only for 20 observations and above).
#'@param mean logical (default is \code{TRUE}). Indicates if a horizontal mean line is plotted.
#'@param lowessFactor numeric (0 < lowessFactor <= 1, default is \code{0.2}). Gives the proportion of points in the plot, which influence the smooth at each value. Larger values give more smoothness. For more details see \code{\link[stats]{lowess}}.
#'@param \dots Optional graphical parameters, see below.
#'@details If the data frame includes less than 20 observations, no trend line is plotted. The dotted line indicates the mean of frequencies.
#'\emph{Optional graphical parameters}
#'The following graphical parameters can optionally be added to customize the plot:
#'\itemize{
#'\item \code{col.bar}: The color to be used for bars. See \code{\link{par}} for usage.
#'\item \code{lwd.bar}: The bar width. See \code{\link{par}} for usage.
#'\item \code{lty.bar}: The bar line type. See \code{\link{par}} for usage.
#'\item \code{col.trend}: The color to be used for the trend line. See \code{\link{par}} for usage.
#'\item \code{lwd.trend}: The trend line width. See \code{\link{par}} for usage.
#'\item \code{lty.trend}: The trend line type. See \code{\link{par}} for usage.
#'\item \code{col.mean}: The color to be used for the mean line. See \code{\link{par}} for usage.
#'\item \code{lwd.mean}: The mean line width. See \code{\link{par}} for usage.
#'\item \code{lty.mean}: The mean line type. See \code{\link{par}} for usage.
#'\item \code{col.axis}: The color to be used for axis annotation. Defaults to \code{"black"}.
#'\item \code{col.lab}: The color to be used for x and y labels. Defaults to \code{"black"}.
#'\item \code{col.main}: The color to be used for plot main titles. Defaults to \code{"black"}.
#'\item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}). To be used for scaling of all texts at once.
#'\item \code{cex.axis}: The magnification to be used for axis annotation relative to the current setting of \code{cex}.
#'\item \code{cex.lab}: The magnification to be used for x and y labels relative to the current setting of \code{cex}.
#'\item \code{cex.main}: The magnification to be used for main titles relative to the current setting of \code{cex}.
#'\item \code{lend}: The line end style. See \code{\link{par}} for usage.
#'}
#'Additional graphical parameters may be set calling \code{\link{par}} before \code{zeitPlot}. 
#'@seealso \code{\link{zeitSetApiKey}} \code{\link{fromZeit}} \code{\link{zeitFrequencies}} \code{\link{zeitToDf}}
#'@return Plots a graph based on given data frame.
#'@examples
#'\dontrun{
#'zeitPlot(dfFrequenciesWeek, yTitle = "Week", xTitle = "Frequencies of articles")
#'}
#'@author Jan Dix (\email{jan.dix@@uni-konstanz.de}), Jana Blahak (\email{jana.blahak@@uni-konstanz.de}), Christian Graul (\email{christian.graul@@gmail.com})
#'@export
zeitPlot <- function(df, yTitle = "Frequency", xTitle = "Time", title = NULL, absolute = TRUE, trend = TRUE, mean = TRUE, lowessFactor = 0.2, ...){
		
	plotPar <- list(...)
	if(any(names(plotPar)=="col.bar")) col.bar <- plotPar$col.bar
	else col.bar <- "#D2691E"
	if(any(names(plotPar)=="lwd.bar")) lwd.bar <- plotPar$lwd.bar
	else lwd.bar <- 1
	if(any(names(plotPar)=="lty.bar")) lty.bar <- plotPar$lty.bar
	else lty.bar <- "solid"
	if(any(names(plotPar)=="col.trend")) col.trend <- plotPar$col.trend
	else col.trend <- "#CD3333"
	if(any(names(plotPar)=="lwd.trend")) lwd.trend <- plotPar$lwd.trend
	else lwd.trend <- 2
	if(any(names(plotPar)=="lty.trend")) lty.trend <- plotPar$lty.trend
	else lty.trend <- "solid"
	if(any(names(plotPar)=="col.mean")) col.mean <- plotPar$col.mean
	else col.mean <- "black"
	if(any(names(plotPar)=="lwd.mean")) lwd.mean <- plotPar$lwd.mean
	else lwd.mean <- 1
	if(any(names(plotPar)=="lty.mean")) lty.mean <- plotPar$lty.mean
	else lty.mean <- "dashed"
	if(any(names(plotPar)=="col.axis")) col.axis <- plotPar$col.axis
	else col.axis <- "black"
	if(any(names(plotPar)=="col.lab")) col.lab <- plotPar$col.lab
	else col.lab <- "black"
	if(any(names(plotPar)=="col.main")) col.main <- plotPar$col.main
	else col.main <- "black"
	if(any(names(plotPar)=="cex")) cex <- plotPar$cex
	else cex <- 1
	if(any(names(plotPar)=="cex.axis")) cex.axis <- plotPar$cex.axis
	else cex.axis <- cex
	if(any(names(plotPar)=="cex.lab")) cex.lab <- plotPar$cex.lab
	else cex.lab <- cex
	if(any(names(plotPar)=="cex.main")) cex.main <- plotPar$cex.main
	else cex.main <- cex + .2	
	if(any(names(plotPar)=="lend")) lend <- plotPar$lend
	else lend <- 1
	
	if (absolute) {
		yAxis <- df$freq
    yLimit <- c(0, max(yAxis, na.rm=TRUE) + 10)
	} else {
		yAxis <- df$freqPro
		yLimit <- c(1, 100)
	}

	plot(df$date,
       yAxis,
       type = "h",
       col  = col.bar,
       lty  = lty.bar,
       lwd  = lwd.bar,
       col.axis = col.axis,
       col.lab = col.lab,
       col.main = col.main,
       cex.axis = cex.axis,
       cex.lab = cex.lab,
       cex.main = cex.main,
       lend = lend,
       main = title,
       ylab = yTitle,
       xlab = xTitle,
       ylim = yLimit)
	if (mean) {
		abline(h = mean(yAxis), col = col.mean, lty = lty.mean, lwd = lwd.mean)
		axis(4, at = round(mean(yAxis), digits = 2), col = col.mean, col.axis = col.mean, cex.axis = cex.axis)
	}

	if (trend && nrow(df) > 20) {
		lines(lowess(yAxis ~ df$date, f = lowessFactor), col = col.trend, lty = lty.trend, lwd = lwd.trend)
	}
}
