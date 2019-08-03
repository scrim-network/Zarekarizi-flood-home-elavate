Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)

pdf("Figures/depth_damage_plot.pdf",width = 3.94,height=2.83)#   5.47)
par(cex=0.5)
plot(Depth,Damage_Factors,type="l",lwd=2,col="darkgreen",xlab="Water depth in house [ft]",ylab="Damage [% of house value including contents]")
grid()
dev.off()
