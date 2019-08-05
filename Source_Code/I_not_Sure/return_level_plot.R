###################################
# file: return_level_plot.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Function adapted from 'gev.rl' (ismev package)
# Allows for customization of return level plot
# with scaling for different return periods
#################################### 

return_level_plot <- function(block_maxima, max_return_period, legend) 
{
  # Data (block_maxima) should be a numeric vector of block maxima tide observations
  # max_return_period should be the maximum number of years returned on plot
    # e.g. 10^4 for a 1/10,000 year return level)
  # legend = TRUE to plot legend
  
  require(ismev)
  
  fit.obj <- gev.fit(block_maxima, show = FALSE)
  
  a <- fit.obj$mle
  mat <- fit.obj$cov
  dat <- fit.obj$data
  
  eps <- 1e-06
  a1 <- a
  a2 <- a
  a3 <- a
  a1[1] <- a[1] + eps
  a2[2] <- a[2] + eps
  a3[3] <- a[3] + eps
  f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
         0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999, 0.9999, 0.99999, 0.999999)
  q <- gevq(a, 1 - f[which((ceiling(-1/log(f)) - max_return_period <= 0))])
  d <- t(gev.rl.gradient(a = a, p = 1 - f))
  v <- apply(d, 1, q.form, m = mat)
  plot(-1/log(f[1:length(q)]), q, log = "x", type = "n", xlim = c(0.1, max_return_period), 
       ylim = c(min(dat, q), max(dat, q + 1.96 * sqrt(v[1:length(q)]))), 
       xaxt = 'n',
       xlab = "Return Period (years)", 
       ylab = "Return Level (cm)")
  #title("Return Level Plot")
  axis(1, lwd = 1.5, at=10^(seq(-1,log10(max_return_period), by = 1)), label=parse(text=paste("10^", seq(-1,log10(max_return_period), by = 1), sep="")))
  axis(2, lwd = 1.5)
  lines(-1/log(f[1:length(q)]), q, lty = 2, lwd = 1.5)
  lines(-1/log(f[1:length(q)]), q + 1.96 * sqrt(v[1:length(q)]), col = "#0080FFFF", lwd = 1.5)
  lines(-1/log(f[1:length(q)]), q - 1.96 * sqrt(v[1:length(q)]), col = "#0080FFFF", lwd = 1.5)
  points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat), lwd = 1.5, cex = 0.75, pch = 21, bg = "white")
  box(lwd = 1.5)
  
  if(legend == TRUE | legend == T)
    {
  legend("topleft",
         c("Block maxima observations", "95% confidence interval", "Best estimate"),
         col = c("black", "#0080FFFF", "black"),
         pt.bg = c("white", NA, NA),
         pch = c(21, NA, NA),
         lty = c(NA, 1, 2),
         lwd = c(1.5, 2, 1.5),
         bty = 'n',
         inset = c(0.01, -0.01))
  }
 
}