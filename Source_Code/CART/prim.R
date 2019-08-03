library(prim)
data(quasiflow)
qf <- quasiflow[1:1000,1:3]
qf.label <- quasiflow[1:1000,4]
thr <- c(0.25, -0.3)
qf.prim1 <- prim.box(x=qf, y=qf.label, threshold=thr, threshold.type=0)
qf.primp <- prim.box(x=qf, y=qf.label, threshold.type=1)
qf.primp.hdr <- prim.hdr(prim=qf.primp, threshold=0.25, threshold.type=1)
qf.primn <- prim.box(x=qf, y=qf.label, threshold=-0.3, threshold.type=-1)
qf.prim2 <- prim.combine(qf.primp.hdr, qf.primn)


plot(qf.prim1) ## orange=x1>x2, blue x2<x1
points(qf[qf.label==1,], cex=0.5)
points(qf[qf.label==-1,], cex=0.5, col=2)



library(sdtoolkit)
#Load some example data to play with:
data(quakes)
#quakes is a 1000 by 5 dataset of earthquake information.  This has no obvious
#policy significance, but we can use this built-in dataset to illustrate the use
#of PRIM.

#Here are the columns:
colnames(quakes)

#We will say magnitude is the output of interest, and call earthquakes greater
#5.0 'interesting.'  We can then call sdprim two different ways.

#First, make an input matrix from columns 1,2,3 and 5 
inputs <- quakes[,c(1:3,5)]  #could also do quakes[,-4]

#Now put our unthresholded y vector:
yout <- quakes[,"mag"] #could also do quakes[,4]

#Now we can either call sdprim and threshold inside PRIM, like this:
myboxes <- sdprim(x=inputs, y=yout, thresh=5.0, threshtype=">")

#Or we can first threshold yout:
ythresh <- 1*(yout>5.0)

#and then call sdprim without worrying about the thresholds:
myboxes <- sdprim(x=inputs, y=ythresh)
