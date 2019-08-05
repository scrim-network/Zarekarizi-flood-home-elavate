###################################
# file: parallel_axis.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Produces parallel axis plot to 
# illustrate tension between objectives
#################################### 

# Plot function
plot.parallel <- function(obj, comp, labels=colnames(obj), col.line=rep(par("fg"), nrow(obj)), col.text=par("fg"), make.plot=TRUE, ...) {
  obj.mat.new <- obj
  n.objs <- ncol(obj)-1
  
  obj.range <- apply(obj[,1:n.objs], 2, range)
  obj <- normalize.objective(obj[,1:n.objs])
  
  comp <- normalize.objective(comp)
  
  print(obj.range)
  if(make.plot){
    plot.new()
    par(xpd=T)
    par(mar=c(6,3.5,4,2.5)+0.1)
    plot.window(xlim=c(1,n.objs), ylim=c(0,1), yaxs="i")
    axis(1, at=1:n.objs, labels=F, line=2, lwd = 2)
    mtext(1, at=1:n.objs, text=labels, col=col.text[1:n.objs], line=2, font=1, padj = 1)
    arrows(x0=0.9, y0=0.95, y1=0.05, length=0.1, lwd = 2)
    mtext("Preference", 2, font = 2)
    par(xpd=F)
    invisible(sapply(1:nrow(comp), function(this.item) lines(comp[this.item,], col="light gray", lty = 1, lwd = 0.75)))
    #invisible(sapply(1:nrow(comp), function(this.item) lines(comp[this.item,], col=rgb(128/255, 128/255, 128/255, alpha = 0.25), lty = 1, lwd = 0.5)))
    abline(v=1:n.objs, lty=1, lwd=2)
    par(xpd=T)
    lines(1:n.objs, rep(0, n.objs), col=par("fg"), lty=3, lwd=3)
    for(i in 1:nrow(obj.mat.new)){
      if(obj.mat.new[i,5]==1){
        lines(obj[i,], col = col.line[i], ...)
        points(obj[i,], bg = col.line[i], col = "black", pch = 22, cex = 1.25, lwd = 2)
        #invisible(sapply(1:nrow(obj), function(this.item) lines(obj[this.item,], col=col.line[this.item], ...)))
      } else {
        lines(obj[i,], col = col.line[i], lty = 2, ...)
        points(obj[i,], bg = col.line[i], col = "black", pch = 21, cex = 1.25, lwd = 2)
        #invisible(sapply(1:nrow(obj), function(this.item) lines(obj[this.item,], col=c("red", "black"), lty = 2)))
      }
    }
    #invisible(sapply(1:nrow(obj), function(this.item) lines(obj[this.item,], col=col.line[this.item], ...)))
    #invisible(sapply(1:nrow(obj), function(this.item) points(obj[this.item,], col = rep("black", nrow(obj)), bg = col.line[this.item],pch = 22, lwd = 3, cex = 1.25)))
    par(xpd=F)
    axis(1, at = 1L:ncol(obj), labels = pretty10exp(signif(obj.range[1,]), digits = 3), lty = 0, line = -0.75, cex.axis = 1, col.axis = "black")
    axis(3, at = 1L:ncol(obj), labels = pretty10exp(signif(obj.range[2,]), digits = 3), lty = 0, line = -0.75, cex.axis = 1, col.axis = "black")
    
    #axis(1, at = 1L:ncol(obj), labels = pretty10exp(min.vals), lty = 0, line = -0.85, cex.axis = 1, col.axis = "black")
    #axis(3, at = 1L:ncol(obj), labels = pretty10exp(max.vals), lty = 0, line = -0.85, cex.axis = 1, col.axis = "black")
  
  }
  
  invisible(list(obj.norm = obj))
  
}





### Find min/max boundaries for each objective
M = Objectives
min.vals <- sapply(1:ncol(M), function(x) 
{
  signif(min(M[,x]), digits = 3)
})

max.vals <- sapply(1:ncol(M), function(x) 
{
  signif(max(M[,x]), digits = 3)
})

unit.vals <- c("$", "$", "$", "1/yr")

### Find optimal solutions for each objective
# Total costs:
total_costs.index = which.min(Objectives$total_costs.v)

# Costs:
  # Find all indices where costs are zero
  zero.cost = which(Objectives$costs.v==0)
  
  # Sum Objectives across all states of the world
  obj.sum = apply(Objectives, 1, sum)

  # Find SOW where costs == 0 AND other objectives are minimized
  cost.index = zero.cost[which.min(obj.sum[zero.cost])]

# Damages  
damages.index = which.min(Objectives$NPV_expected_losses.v)

# Flood Probability
flood.index = which.min(Objectives$EV_p_exceed_transient.v)

### Find compromise solutions
  # Solution with highest flood probability
  #comp1 <- which.max(Objectives$EV_p_exceed_transient.v)
  
  # Solution which maximizes sum of all objectives (performs poorly)
  comp2 <- which.min(abs(Objectives$costs.v - quantile(Objectives$costs.v, 0.99)))
  #comp2 <- which.max(obj.sum)
  
  # Solution which shows 99% quantile of total costs
  #comp3 <- which.min(abs(Objectives$total_costs.v - quantile(Objectives$total_costs.v, 0.99)))
  
  comp4 = Objectives[1669726,]
  comp5 = Objectives[1751766,]
### Combine optimal and compromise solutions into matrix for plotting
# In 5th column, 1 denotes optimal while -1 denotes compromise

#obj.cols = c("dark blue", "skyblue3", "green", "darkorchid4", "indianred", "orange")
obj.cols = c("darkorchid", "indianred", "green", "dodgerblue", "sky blue", "orange")
#obj.cols = c(rainbow(6)[5], rainbow(6)[6], rainbow(6)[1], rainbow(6)[2], rainbow(6)[4], rainbow(6)[3])
obj.mat = matrix(data = c(as.numeric(Objectives[total_costs.index,]), 1,
            as.numeric(Objectives[cost.index,]), 1,
            as.numeric(Objectives[damages.index,]), 1,
            as.numeric(Objectives[flood.index,]), 1,
            as.numeric(comp5), -1, 
            as.numeric(comp4), -1 ),nrow = 6, ncol = 5, byrow = T)
comp.mat = as.matrix(Objectives[sample(1:nrow(Objectives), 200, replace = F),], nrow = 200, ncol = 4, byrow = T)

### Plot
pdf("parallel_axis.pdf", width = 6.5, height = 4.25)
plot.parallel(obj.mat, comp = comp.mat, labels = c("Total Costs", "Investment \nCosts", "Expected \nDamages", "Flood \nProbability"),
              col.line = obj.cols,
              col.text = obj.cols, lwd = 2, type = 'l', pch = 22, cex = 1.25, bg = obj.cols)
dev.off() 
