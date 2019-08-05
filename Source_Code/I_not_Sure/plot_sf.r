#######################################################################
#
# plot_sf.r    15 Jan 2015
#
# Author: Gregory Garner (ggg121@psu.edu)
#
# Function that plots the survival function of a given vector of data
#
# To use this function, simply source this file:
#   source("plot_sf.r")
#
# Version History:
#   1.0 - 15 Jan 2015 - Initial coding (G.G.)
#
# Note: I wrote this code because I've been asked fairly often for
# code that does this type of plot.  The original code I had included
# a lot of things the regular user wouldn't need, so I wrote a simple
# plot() wrapper to share with people.  It's not fancy, but it does the
# job.
#
# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT
# NOT IMPLICIT).  I SHARE THIS CODE IN HOPES THAT IT IS USEFUL, 
# BUT I AM NOT LIABLE FOR THE BEHAVIOR OF THIS CODE IN YOUR OWN
# APPLICATION.  YOU ARE FREE TO SHARE THIS CODE SO LONG AS THE
# AUTHOR(S) AND VERSION HISTORY REMAIN INTACT.
#
# Function Name: plot.sf
# Parameters:
#   x - Data vector to be plotted
#   xlab - X-axis label (default = name of the data object passed
#          to the function)
#   ylab - Y-axis label (default = SF  [1 - Cum. Freq.]) 
#   make.plot - Boolean value determining whether or not to make a plot
#               (default = T)
#   ... - Other parameters to be passed to the plot() function
#
# The function invisibly returns the survival function value for the
# passed data vector, even if make.plot is false.  This is useful when
# you want to customize your plot or use the survival function data
# in further analyses.
#
# See the accompanying "plot_sf_example.r" file for usage and examples.
#
#######################################################################

plot.sf <- function(x, xlab=deparse(substitute(x)), ylab="SF  [1 - Cum. Freq.]", make.plot=T, ...)
{
  num.x <- length(x)
  order.x <- order(x)
  num.ytics <- floor(log10(num.x))
  sf <- seq(1,1/num.x,by=-1/num.x)
  if(make.plot) {
    plot(x[order.x], sf, log="y", xlab=xlab, ylab=ylab, yaxt="n", ...)
    axis(2, at=10^(-num.ytics:0), label=parse(text=paste("10^", -num.ytics:0, sep="")), las=1)
  }
  invisible(sf[order(order.x)])
}