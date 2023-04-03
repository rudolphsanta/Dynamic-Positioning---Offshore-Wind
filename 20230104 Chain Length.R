# Rudolph Santarromana
# Submerged mooring chain calculation

library(tidyverse)
library(dplyr)

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")

#moored. equation from Huang et al. (2021)
Submerged <- function(depth, max.force, w = 0.1) { #depth in m, force in tons, w in tons/m
  length <- depth*sqrt(((2*max.force)/(w*depth))+1) #length of chains in submerged water
  # dist <- ((max.force/w)-depth)*log((length + (max.force/w))/((max.force/w)-depth)) #Maximum horizontal distance from fairlead to the anchor
  dist <- (max.force/w)*acosh(((w*depth)/max.force) + 1)
  return(length) #returns values in m. length should be greater than distance
}

V.Submerged <- Vectorize(Submerged)

Fairlead.force <- function(force, angle = 0) {#force in kN, angle comes from that used in 15 MW turbine design (Gaertner or Allen)
  eff.force <- force/cos(angle) #angle of zero gives the horizontal force.
  force.ton <- eff.force*0.1
  return(force.ton) #returns value in tons
}

Lengths <- function(depth, w.1 = 0.1, plant.MW = 1000, n.chains = 3, fs = 1.67) {
  #maximum forces (in kN) for each turbine felt in the Fairlead
  fairlead.15 <- Fairlead.force(max(DLC1.3_15))*fs
  fairlead.10 <- Fairlead.force(max(DLC1.3_10))*fs
  fairlead.8 <- Fairlead.force(max(DLC1.3_8))*fs
  fairlead.5 <- Fairlead.force(max(DLC1.3_5))*fs

  length.15 <- Submerged(depth,fairlead.15,w = w.1)*n.chains*ceiling(plant.MW/15)
  length.10 <- Submerged(depth,fairlead.10,w = w.1)*n.chains*ceiling(plant.MW/10)
  length.8 <- Submerged(depth,fairlead.8,w = w.1)*n.chains*ceiling(plant.MW/8)
  length.5 <- Submerged(depth,fairlead.5,w = w.1)*n.chains*ceiling(plant.MW/5)
  return(c(length.5, length.8, length.10, length.15))
}
T.Sizes <- c(5,8,10,15)
#Wind speed class 8
L_08.1 <- Lengths(160,w.1 = 0.1)
L_08.2 <- Lengths(160,w.1 = 0.2)
L_08.3 <- Lengths(160,w.1 = 0.3)
L_08.4 <- Lengths(160,w.1 = 0.4)
L_08.5 <- Lengths(160,w.1 = 0.5)
L_08.6 <- Lengths(160,w.1 = 0.6)

#Wind speed class 14
L_14.1 <- Lengths(635,w.1 = 0.1)
L_14.2 <- Lengths(635,w.1 = 0.2)
L_14.3 <- Lengths(635,w.1 = 0.3)
L_14.4 <- Lengths(635,w.1 = 0.4)
L_14.5 <- Lengths(635,w.1 = 0.5)
L_14.6 <- Lengths(635,w.1 = 0.6)

#Turbine Areas
R.diam_5 <- 126 #m
R.diam_8 <- 164
R.diam_10 <-178
R.diam_15 <- 240

n.diams <- 7 
Plant.MW <- 1000

N.turb_5 <- ceiling(Plant.MW/5)
N.turb_8 <- ceiling(Plant.MW/8)
N.turb_10 <- ceiling(Plant.MW/10)
N.turb_15 <- ceiling(Plant.MW/15)

A.turb_5 <- (R.diam_5*n.diams/1000)^2  #km^2
A.turb_8 <- (R.diam_8*n.diams/1000)^2
A.turb_10 <- (R.diam_10*n.diams/1000)^2
A.turb_15 <- (R.diam_15*n.diams/1000)^2

P.area_5 <- N.turb_5*A.turb_5 #km^2
P.area_8 <- N.turb_8*A.turb_8
P.area_10 <- N.turb_10*A.turb_10
P.area_15 <- N.turb_15*A.turb_15

#Figure
# setEPS()
# postscript("Mooring Chain Figure_v2.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = T.Sizes, y = L_14.1/1000, col = '#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2,
     xlab = "Turbine Size [MW]", ylab = "Length of Mooring Chains [km]",
     xaxs = 'i', yaxs = 'i', las = 1, xlim = c(0,16), ylim = c(0,1200), cex.lab = 1.2, cex.axis = 1.2)
# points(x = T.Sizes, y = L_14.2/1000, col ='red', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_14.3/1000, col ='red', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_14.4/1000, col ='red', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_14.5/1000, col ='red', type = 'b', pch = 19, lwd = 2, lty = 2)
points(x = T.Sizes, y = L_14.6/1000, col ='#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2)

points(x = T.Sizes, y = L_08.1/1000, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_08.2/1000, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_08.3/1000, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_08.4/1000, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
# points(x = T.Sizes, y = L_08.5/1000, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
points(x = T.Sizes, y = L_08.6/1000, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)

polygon(x = c(T.Sizes,rev(T.Sizes)), y = c(L_08.1/1000,rev(L_08.6)/1000), 
        col = rgb(0,0,1,0.5), border = NA)
polygon(x = c(T.Sizes,rev(T.Sizes)), y = c(L_14.1/1000,rev(L_14.6)/1000), 
        col = rgb(1,0,0,0.5), border = NA)

text(x=3,y=L_14.1[1]/1000,"w = 0.1 ton/m", col = '#c1272d',adj=c(1,0), cex = 1)
text(x=3,y=L_14.6[1]/1000,"w = 0.6 ton/m", col = '#c1272d',adj=c(1,0), cex = 1)

text(x=3,y=L_08.1[1]/1000,"w = 0.1 ton/m", col = '#0000a7',adj=c(1,0), cex = 1)
text(x=3,y=L_08.6[1]/1000,"w = 0.6 ton/m", col = '#0000a7',adj=c(1,0), cex = 1)

text(x=15, y = mean(c(L_14.1[4],L_14.6[4]))/1000 - 70, "Wind Speed Class 14: \nDepth = 635m", col = '#c1272d', cex = 1, adj = c(1,0))
text(x=15, y = mean(c(L_08.1[4],L_08.6[4]))/1000 - 60, "Wind Speed Class 8: \nDepth = 160m", col = '#0000a7', cex = 1, adj = c(1,0))

dev.off()

#Chain Density in km of chains/km^2 of plant. Class 14
Density_5.14.1 <- (L_14.1[1]/1000)/P.area_5
Density_8.14.1 <- (L_14.1[2]/1000)/P.area_8
Density_10.14.1 <- (L_14.1[3]/1000)/P.area_10
Density_15.14.1 <- (L_14.1[4]/1000)/P.area_15

Density_5.14.6 <- (L_14.6[1]/1000)/P.area_5
Density_8.14.6 <- (L_14.6[2]/1000)/P.area_8
Density_10.14.6 <- (L_14.6[3]/1000)/P.area_10
Density_15.14.6 <- (L_14.6[4]/1000)/P.area_15

#Chain Density in km of chains/km^2 of plant. Class 08
Density_5.08.1 <- (L_08.1[1]/1000)/P.area_5
Density_8.08.1 <- (L_08.1[2]/1000)/P.area_8
Density_10.08.1 <- (L_08.1[3]/1000)/P.area_10
Density_15.08.1 <- (L_08.1[4]/1000)/P.area_15

Density_5.08.6 <- (L_08.6[1]/1000)/P.area_5
Density_8.08.6 <- (L_08.6[2]/1000)/P.area_8
Density_10.08.6 <- (L_08.6[3]/1000)/P.area_10
Density_15.08.6 <- (L_08.6[4]/1000)/P.area_15

plot(x = T.Sizes, y = c(Density_5.08.1,Density_8.08.1,Density_10.08.1,Density_15.08.1),
     pch = 19, col = 'blue', type = 'b', lty = 2, xlim = c(0,15), ylim = c(0,ceiling(Density_5.14.1)),
     xlab = "Turbine Size [MW]", ylab = "Chain Density [km of chains/km^2 of plant area]")
points(x = T.Sizes, y = c(Density_5.08.6,Density_8.08.6,Density_10.08.6,Density_15.08.6),
       col = 'blue', pch = 19, type = 'b', lty = 2)
points(x = T.Sizes, y = c(Density_5.14.1,Density_8.14.1,Density_10.14.1,Density_15.14.1),
       col = 'red', pch = 19, type = 'b', lty = 2)
points(x = T.Sizes, y = c(Density_5.14.6,Density_8.14.6,Density_10.14.6,Density_15.14.6),
       col = 'red', pch = 19, type = 'b', lty = 2)

## O&M Costs
fail.rate <- 0.00378 #failures/km
comp.cost <- 631000 #USD/component = 520000 GBP/component
nchains <- 3

#total chains in plant (no. of components)
N.chain_5 <- N.turb_5*nchains
N.chain_8 <- N.turb_8*nchains
N.chain_10 <- N.turb_10*nchains
N.chain_15 <- N.turb_15*nchains

N.Chains.Plants <- c(N.chain_5, N.chain_8, N.chain_10, N.chain_15)

Fails_08.1 <- Lengths(160,w.1 = 0.1, n.chains = nchains, fs = 1.67)*fail.rate 
Fails.cost_08.1 <- Fails_08.1*N.Chains.Plants/1000000 #cost per year #usd/kw-year. it's very small

Fails_08.6 <- Lengths(160,w.1 = 0.6, n.chains = nchains, fs = 1.67)*fail.rate 
Fails.cost_08.6 <- Fails_08.6*N.Chains.Plants/1000000

Fails_14.1 <- Lengths(635,w.1 = 0.1, n.chains = nchains, fs = 1.67)*fail.rate 
Fails.cost_14.1 <- Fails_14.1*N.Chains.Plants/1000000

Fails_14.6 <- Lengths(635,w.1 = 0.6, n.chains = nchains, fs = 1.67)*fail.rate 
Fails.cost_14.6 <- Fails_14.6*N.Chains.Plants/1000000
