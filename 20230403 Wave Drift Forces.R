# Created By: Rudolph Santarromana
# Wave Drift Forces Calculation
# February 2021

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")

library(tidyverse)
library(dplyr)


#### Using Morrison's equation

rho_water <- 1025 #seawater density [kg/m^3]
Cd <- 1.0 #body drag coefficient [-]
Cm <- 2.0 #body inertia coefficient [-]
diam <- 12.5 #12.5 body diameter [m]
draft <- 20 #20 body draft depth [m]
n.floaters <- 3 #3 number of floaters [-]

period <- 31.7 #wave period [seconds]
H <- 5 #design wave height [m]
depth <- 1000 #1000 site water depth [m]

v.curr <- 0

# Determine wavelength
# lambda <- (9.81*(period^2))/(2*pi) #wavelength [m]

fn.lambda <- function(l,pd = period, d = depth) { #this is the function to be optimized
  lambda <- (9.81*(pd^2))/(2*pi)
  fn <- abs(lambda*tanh((2*pi*d)/l) - l)
  return(fn)
}

lambda <- optimize(fn.lambda, c(0,10000), maximum = FALSE)$minimum

Lambda <- function(pd) {return((9.81*(pd^2))/(2*pi))} #pd in s, Lambda in m
# Lambda(5.66)


# Can we apply Morrison's equation? It is only applicable for small/slender objects
check <- if (diam/lambda < 0.2) print("YES") else print("NO")

Check <- function(diam) { #diam in m. Given a body diameter, what period is morison's equation allowed for?
  d_lambda <- 0.2 #the required ratio between D and Lambda for Morison's equation
  min.pd.sq <- (diam*2*pi)/(d_lambda*9.81)
  min.pd <- round(sqrt(min.pd.sq),2)
  return(min.pd) #in s. The wave period must be longer than this.
}
Check(12.5) #6.33s

#### Apply Morrison's equation if check == YES
area <- draft*diam*n.floaters #wetted area [m^2]
k <- 2*pi/lambda # [rad/m]

# Maximum Wave-induced drag force on ONE floater
F.drag.max <- 0.5*rho_water*Cd*diam*(((pi*H)/period)^2)*((1-exp(-2*k*draft))/(2*k)) # [Newtons]
F.drag.max <- F.drag.max/1000 #[kiloNewtons]

# Maximum Wave-induced inertial force
F.inertia.max <- rho_water*Cm*((pi*(diam^2))/4)*((2*(pi^2)*H)/period^2)*(1/k)*(1-exp(-k*draft)) #[Newtons]
F.inertia.max <- F.inertia.max/1000 #[kiloNewtons]

# Steady current drag force
F.curr <- 0.5*rho_water*area*v.curr^2

### Calculate time fluctuation
time.v <- seq(0,26, by = 0.5)

eta <- function(x,t) {
  a <- H/2
  sigma <- 2*pi/period
  eta <- a*cos(k*x - sigma*t)
  return(eta)
}

# Visualize wave height from SWL
dist.v <- seq(-lambda, lambda, by = lambda/50)
eta.v <- mapply(eta, x = dist.v, t = 0)

plot(x = dist.v, y = eta.v, type = 'l',
     ylim = c(-6,6), 
     main = "Free Water Surface at t = 0s",
     xlab = "Position [m]",
     ylab = "Wave Height [m]")
abline(h = 0, col = 'gray', lty = 2)

# Calculate fluctuating inertia- and drag-induced wave force on ONE floater
fn.F.inertia <- function(x,t,diam,Hs,Pd,dr = draft) {
  lambda <- (9.81*(Pd^2))/(2*pi)
  k <- 2*pi/lambda # [rad/m]
  F.inertia.max <- rho_water*Cm*((pi*(diam^2))/4)*((2*(pi^2)*Hs)/Pd^2)*(1/k)*(1-exp(-k*dr)) #[Newtons]
  F.inertia.max<- F.inertia.max/1000 #[kN]
  sigma <- 2*pi/Pd
  F.inertia <- F.inertia.max*sin(k*x - sigma*t)
  return(F.inertia)
}

fn.F.drag <- function(x,t,diam,Hs,Pd,dr = draft) {
  lambda <- (9.81*(Pd^2))/(2*pi)  
  k <- 2*pi/lambda # [rad/m]
  F.drag.max <- 0.5*rho_water*Cd*diam*(((pi*Hs)/Pd)^2)*((1-exp(-2*k*dr))/(2*k)) # [Newtons]
  F.drag.max <- F.drag.max/1000 #[kN]
  sigma <- 2*pi/Pd
  F.drag <- F.drag.max*cos(k*x - sigma*t)*abs(cos(k*x - sigma*t))
  return(F.drag)
}

#######################################################################
# On One Floater
F.inertia.v <- mapply(fn.F.inertia, x = 0, t = time.v, diam = diam,Hs = H, Pd = period)
F.drag.v <- mapply(fn.F.drag, x = 0, t = time.v,diam = diam,Hs = H, Pd = period)
F.total.v <- F.inertia.v + F.drag.v

plot(x = time.v, y = F.inertia.v, type = 'l', col = 'blue', lty = 2, lwd = 2,
     xlab = "Time [s]",
     ylab = "Force in X-direction [kN]")
lines(x = time.v, y = F.drag.v, col = 'green', lty = 2, lwd = 2)
lines(x = time.v, y = F.total.v, col = 'black', lwd = 2)
abline(h = 0, col = 'gray')
legend('topleft', legend = c("Wave-Induced Drag Force","Wave-Induced Inertia Force", "Total Wave-Induced Force"),
       lty = c(2,2,1), lwd = c(2,2,2), col = c('green','blue','black'),cex = 0.8)

max(F.total.v)
mean(F.total.v)

#######################################################################

# Calculate fluctuating intertia- and drag-induced wave force on three floaters
# oriented in a 1-2 fashion
dist <-77.6 #distance between the front and rear floaters [m]
H <- 1.8
period <- 11

time.v <- seq(0,(period), by = 0.1)

F.inertia.front <- mapply(fn.F.inertia, x = 0, t = time.v, diam = 12.5, Hs = H, Pd = period)
F.inertia.back <- mapply(fn.F.inertia, x = dist, t = time.v, diam = 12.5, Hs = H, Pd = period)

F.inertia.v2 <- F.inertia.front + 2*F.inertia.back

## Plot
setEPS()
postscript("Inertia Wave Force.eps", width = 7.5, height = 5)
par(mar = c(5,9.5,2,2))

plot(x = time.v, y = F.inertia.back*2, type = 'n', col = 'blue', ylim = c(-5000,5000), 
     xaxs = 'i', yaxs = 'i', las = 1, xaxp = c(0,10,2), yaxp = c(-5000, 5000, 2),
     xlab = "Time [s]", ylab = "",
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = 'gray')
lines(x = time.v, y = F.inertia.back*2, col = 'blue', lty = 2)
lines(x = time.v, y = F.inertia.front, col = 'red', lty = 2)
lines(x = time.v, y = F.inertia.v2, col = 'black', lwd = 2)
mtext("Wave-Induced \nInertia Force [kN]", side = 2, cex = 2.2, line = 4)

legend('topleft', 
       legend = c("Floater A (at x = 0)","Sum of Floaters B and C (at x = 77.6)", "Total Wave-Induced Inertia Force"),
       lty = c(2,2,1), lwd = c(2,2,2), col = c('red','blue','black'),cex = 1.4, bty = 'n')

dev.off()
##End Plot

F.drag.front <- mapply(fn.F.drag, x = 0, t = time.v, diam = 12.5, Hs = H, Pd = period)
F.drag.back <- mapply(fn.F.drag, x = dist, t = time.v, diam = 12.5, Hs = H, Pd = period)

F.drag.v2 <- F.drag.front + 2*F.drag.back

##Plot
setEPS()
postscript("Drag Wave Force.eps", width = 7.5, height = 5)
par(mar = c(5,9.5,2,2))

plot(x = time.v, y = F.drag.back*2, type = 'n', col = 'blue', ylim = c(-100,100),
     xaxs = 'i', yaxs = 'i', las = 1, xaxp = c(0,10,2), yaxp = c(-100, 100, 2),
     xlab = "Time [s]", ylab = "",
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = 'gray')
lines(x = time.v, y = F.drag.back*2, col = 'blue',lty = 2)
lines(x = time.v, y = F.drag.front, col = 'red', lty = 2)
lines(x = time.v, y = F.drag.v2, col = 'black', lwd = 2)
mtext("Wave-Induced \nDrag Force [kN]", side = 2, cex = 2.2, line = 4)

legend('topleft', 
       legend = c("Floater A (at x = 0)","Sum of Floaters B and C (at x = 77.6)", "Total Wave-Induced Drag Force"),
       lty = c(2,2,1), lwd = c(2,2,2), col = c('red','blue','black'),cex = 1.4, bty = 'n')

dev.off()
## End Plot

F.total.v2 <- F.inertia.v2 + F.drag.v2
##Plot
setEPS()
postscript("Total Wave Force.eps", width = 7.5, height = 5)
par(mar = c(5,9.5,2,2))

plot(x = time.v, y = F.total.v2, type = 'n', lwd = 2, col = 'black', ylim = c(-5000,5000),
     xaxs = 'i', yaxs = 'i', las = 1, xaxp = c(0,10,2), yaxp = c(-5000, 5000, 2),
     xlab = "Time [s]", ylab = "",
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = 'gray')
lines(x = time.v, y = F.inertia.v2, col = 'red', lwd = 2, lty = 2)
lines(x = time.v, y = F.drag.v2, col = 'blue', lwd = 2, lty = 2)
lines(x = time.v, y = F.total.v2, lwd = 1, col = 'black')
mtext("Wave-Induced \nTotal Force [kN]", side = 2, cex = 2.2, line = 4)

legend('topleft', 
       legend = c("Wave-Induced Drag Force","Wave-Induced Inertia Force", "Total Wave-Induced Force"),
       lty = c(2,2,1), lwd = c(2,2,2), col = c('red','blue','black'),cex = 1.4, bty = 'n')

dev.off()
## End Plot

#bars based on time step
plot(x = time.v, y = F.total.v2, type = 'p', lwd = 2, pch = 19,
     xlab = "Time [s]",
     ylab = "Force in X-direction [kN]")
# lines(x = time.v, y = F.inertia.v3, col = 'blue', lwd = 2, lty = 2)
# lines(x = time.v, y = F.drag.v3, col = 'green', lwd = 2, lty = 2)
arrows(time.v, 0, time.v, F.total.v2, angle = 90, length = 0.08)
abline(h = 0, col = 'gray')

mean(F.total.v2)
max(F.total.v2)

#######################################################################
# oriented in a 1-1-1 fashion
dist1 <- 102.13/2 #distance between the front and middle floaters [m]
dist2 <- 102.13 #distance between the front and rear floaters [m]

F.inertia.1 <- mapply(fn.F.inertia, x = 0, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)
F.inertia.2 <- mapply(fn.F.inertia, x = dist1, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)
F.inertia.3 <- mapply(fn.F.inertia, x = dist2, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)

F.inertia.v3 <- F.inertia.1 + F.inertia.2 + F.inertia.3

plot(x = time.v, y = F.inertia.1, type = 'l', col = 'blue')
lines(x = time.v, y = F.inertia.2, col = 'red')
lines(x = time.v, y = F.inertia.3, col = 'green')
lines(x = time.v, y = F.inertia.v3, col = 'black', lwd = 2)
abline(h = 0, col = 'gray')

F.drag.1 <- mapply(fn.F.drag, x = 0, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)
F.drag.2 <- mapply(fn.F.drag, x = dist1, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)
F.drag.3 <- mapply(fn.F.drag, x = dist2, t = time.v, diam = 12.5, Hs = 4.2, Pd = 10)

F.drag.v3 <- F.drag.1 + F.drag.2 + F.drag.3

plot(x = time.v, y = F.drag.1, type = 'l', col = 'blue')
lines(x = time.v, y = F.drag.2, col = 'red')
lines(x = time.v, y = F.drag.3, col = 'green')
lines(x = time.v, y = F.drag.v3, col = 'black', lwd = 2)
abline(h = 0, col = 'gray')

F.total.v3 <- F.inertia.v3 + F.drag.v3

plot(x = time.v, y = F.total.v3, type = 'l', lwd = 2,
     xlab = "Time [s]",
     ylab = "Force in X-direction [kN]",
     main = "Total Wave Force on Entire Substructure over Two Wave Periods")
# lines(x = time.v, y = F.inertia.v3, col = 'blue', lwd = 2, lty = 2)
# lines(x = time.v, y = F.drag.v3, col = 'green', lwd = 2, lty = 2)
abline(h = 0, col = 'gray')
# legend('bottomleft', 
#        legend = c("Wave-Induced Drag Force","Wave-Induced Inertia Force", "Total Wave-Induced Force"),
#        lty = c(2,2,1), lwd = c(2,2,2), col = c('green','blue','black'),cex = 0.8)

#bars based on time step
plot(x = time.v, y = F.total.v3, type = 'p', lwd = 2, pch = 19,
     xlab = "Time [s]",
     ylab = "Force in X-direction [kN]",
     main = "Time step = 0.5 s; Mean Wave Force = 4.0 kN")
# lines(x = time.v, y = F.inertia.v3, col = 'blue', lwd = 2, lty = 2)
# lines(x = time.v, y = F.drag.v3, col = 'green', lwd = 2, lty = 2)
arrows(time.v, 0, time.v, F.total.v3, angle = 90, length = 0.08)
abline(h = 0, col = 'gray')
# legend('bottomleft',
#        legend = c("Wave-Induced Drag Force","Wave-Induced Inertia Force", "Total Wave-Induced Force"),
#        lty = c(2,2,1), lwd = c(2,2,2), col = c('green','blue','black'),cex = 0.8)

mean(F.total.v3)
sum(F.total.v3)
max(F.total.v3)

#######################################################################

### Considering all orientations, up to 60 degrees
phi <- seq(0,60, by = 6) #degrees
phi.rad <- phi*(pi/180) #radians
radius <- 51.75 #m
tower.radius <- 10 #m

#floater position orientations
floater1.pos <- round(radius*cos(0 + phi.rad),2)
floater2.pos <- round(radius*cos(2*pi/3 + phi.rad),2)
floater3.pos <- round(radius*cos(4*pi/3 + phi.rad),2)

Force.Matrix <- function(sig.ht = H, prd = period, time.step = 0.05, 
                              F1 = floater1.pos, 
                              F2 = floater2.pos,
                              F3 = floater3.pos) {
  time.v <- seq(0,prd, by = time.step) #how often would the system adjust? 1/second?
  #initialize the lists, each value corresponds to an orientation/rotation
  Drag.L <- c()
  Inertia.L <- c()
  Total.L <- c()
  Mean.Force <- c()
  Max.Force <- c()
  
  for(i in 1:length(F1)) {
    F.drag.f1 <- mapply(fn.F.drag, x = F1[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.drag.f2 <- mapply(fn.F.drag, x = F2[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.drag.f3 <- mapply(fn.F.drag, x = F3[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.drag.TS <- mapply(fn.F.drag, x = 0, t = time.v, diam = 10, Hs = sig.ht, Pd = prd)
    
    F.DRAG <- list(round(F.drag.f1 + F.drag.f2 + F.drag.f3 + F.drag.TS, 2))
    
    F.inertia.f1 <- mapply(fn.F.inertia, x = F1[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.inertia.f2 <- mapply(fn.F.inertia, x = F2[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.inertia.f3 <- mapply(fn.F.inertia, x = F3[i], t = time.v, diam = 12.5, Hs = sig.ht, Pd = prd)
    F.inertia.TS <- mapply(fn.F.inertia, x = 0, t = time.v, diam = 10, Hs = sig.ht, Pd = prd)
    
    F.INER <- list(round(F.inertia.f1 + F.inertia.f2 + F.inertia.f3 + F.inertia.TS, 2))
    
    F.TOT <- list(round(F.drag.f1 + F.drag.f2 + F.drag.f3 + F.drag.TS +
                    F.inertia.f1 + F.inertia.f2 + F.inertia.f3 + F.inertia.TS, 2))
    
    F.mean <- round(mean(F.drag.f1 + F.drag.f2 + F.drag.f3 + F.drag.TS +
                    F.inertia.f1 + F.inertia.f2 + F.inertia.f3 + F.inertia.TS), 2)
    
    F.max <- round(max(F.drag.f1 + F.drag.f2 + F.drag.f3 + F.drag.TS +
                     F.inertia.f1 + F.inertia.f2 + F.inertia.f3 + F.inertia.TS), 2)
    
    Drag.L <- c(Drag.L, F.DRAG)
    Inertia.L <- c(Inertia.L, F.INER)
    Total.L <- c(Total.L, F.TOT)
    Mean.Force <- c(Mean.Force, F.mean)
    Max.Force <- c(Max.Force, F.max)
    
  }
  
  return(data.frame(phi,Mean.Force,Max.Force))
}

Morro.period <- c(8,8.1,8.3,8.5,8.7,9.5,10,10.1,12.1,11.8,11.7,11.6,11.5,11.4,11.1,10.9,10.9,10.7,10.4,10.1,10.1,10.2,10.1,9.9,9.5,9.4,9.4,9.4,9.5,9.5,9.4,9.4,9.4,9.2,9.0,8.8,15.4,8.3,8.1,7.9,7.6,7.4,7.7,7.8,14.5,14.4,5.1,14.3)
summary(Morro.period)
Morro.ht <- c(1.8,1.7,1.6,1.6,1.6,1.6,1.5,1.3,1.5,1.5,1.4,1.4,1.5,1.4,1.4,1.3,1.3,1.3,1.3,1.2,1.2,1.1,1.1,1.2,1.2,1.2,1.1,1.0,1.1,1.1,1.2,1.2,1.2,1.1,1.0,1.0,1.0,1.1,1.1,1.2,0.9,0.9,0.9,0.9,0.9,0.9,0.9,1.0)
summary(Morro.ht)


#####Operational Conditions
#Portugal                       
max(Force.Matrix(1.2,11)$Mean.Force) 
max(Force.Matrix(1.2,14)$Mean.Force) 
max(Force.Matrix(1.8,11)$Mean.Force) 
max(Force.Matrix(1.8,14)$Mean.Force) 
#California
max(Force.Matrix(1.1,8.6)$Mean.Force) 
max(Force.Matrix(1.1,10.9)$Mean.Force) 
max(Force.Matrix(1.4,8.6)$Mean.Force) 
max(Force.Matrix(1.4,10.9)$Mean.Force) 

#####Extreme Conditions
#Portugal
max(Force.Matrix(3.2,18.2)$Mean.Force) 
max(Force.Matrix(3.5,18.3)$Mean.Force) 
max(Force.Matrix(4.5,15.7)$Mean.Force) 
max(Force.Matrix(5.2,14.8)$Mean.Force) 
#California
max(Force.Matrix(7,8.8)$Mean.Force) 
max(Force.Matrix(9,12.2)$Mean.Force) 
max(Force.Matrix(11.2,17.3)$Mean.Force) 
max(Force.Matrix(9,21.1)$Mean.Force) 

#######################################################################
#for 5MW semisubersible validation method;
### Only doing one orientation. 1-2 orientation
UC.d <- 12 #m. uppper floater column
BC.d <- 24 #m. base floater column
MC.d <- 6.5 #m. middle column
UC.dr <- 14 #m. depth of upper column below SWL
BC.dr <- 20 #m. depth of bottom of base column below SWL. 6m thick cylinder


#center of floater positions in the wave field (only on the axis in the direction of the wave propagation)
# floater.1.pos <- 0
# floater.2.pos <- 43.3
# floater.3.pos <- 43.3 
# MC.pos <- 28.87

Force.Matrix2 <- function(sig.ht, prd, int = 0.05,
                         F1 = 0, 
                         F2 = 43.3,
                         F3 = 43.3,
                         MC = 28.87) {
  time.v <- seq(0,prd, by = int) #how often would the system adjust? 1/second?
  #initialize the lists, each value corresponds to an orientation/rotation
  Drag.L <- c()
  Inertia.L <- c()
  Total.L <- c()
  Mean.Force <- c()
  Max.Force <- c()
  
  F.drag.f1 <- fn.F.drag(x = F1, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.drag(x = F1, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.drag(x = F1, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)

  F.drag.f2 <- fn.F.drag(x = F2, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.drag(x = F2, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.drag(x = F2, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)

  F.drag.f3 <- fn.F.drag(x = F3, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.drag(x = F3, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.drag(x = F3, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)
  
  F.drag.MC <- fn.F.drag(x = MC, t = time.v, diam = MC.d, Hs = sig.ht, Pd = prd, dr = BC.dr)
    
  F.DRAG <- round(F.drag.f1 + F.drag.f2 + F.drag.f3 + F.drag.MC, 0)
  
  F.inertia.f1 <- fn.F.inertia(x = F1, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.inertia(x = F1, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.inertia(x = F1, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)

  F.inertia.f2 <- fn.F.inertia(x = F2, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.inertia(x = F2, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.inertia(x = F2, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)

  F.inertia.f3 <- fn.F.inertia(x = F3, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = BC.dr) -
    fn.F.inertia(x = F3, t = time.v, diam = BC.d, Hs = sig.ht, Pd = prd, dr = UC.dr) +
    fn.F.inertia(x = F3, t = time.v, diam = UC.d, Hs = sig.ht, Pd = prd, dr = UC.dr)
  
  F.inertia.MC <- fn.F.inertia(x = MC, t = time.v, diam = MC.d, Hs = sig.ht, Pd = prd, dr = BC.dr)
    
  F.INER <- round(F.inertia.f1 + F.inertia.f2 + F.inertia.f3 + F.inertia.MC, 0)
    
  F.TOT <- F.DRAG + F.INER
    
  Mean <- round(mean(F.TOT),0)

  Max <- round(max(F.TOT), 0)

  
  return(data.frame(Mean,Max))
}


#####Operational Conditions
#Portugal                       
Force.Matrix2(1.2,11)$Mean 
Force.Matrix2(1.2,14)$Mean 
Force.Matrix2(1.8,11)$Mean
Force.Matrix2(1.8,14)$Mean 
#California
Force.Matrix2(1.1,8.6)$Mean
Force.Matrix2(1.1,10.9)$Mean
Force.Matrix2(1.4,8.6)$Mean 
Force.Matrix2(1.4,10.9)$Mean 

#####Extreme Conditions
#Portugal from Oliveira
Force.Matrix2(3.2,18.2)$Mean 
Force.Matrix2(3.5,18.3)$Mean 
Force.Matrix2(4.5,15.7)$Mean 
Force.Matrix2(4.1,18.4)$Mean 
#California from Berg (2011)
Force.Matrix2(7,8.8)$Mean 
Force.Matrix2(9,12.18)$Mean 
Force.Matrix2(11.22,17.26)$Mean 
Force.Matrix2(9,21.09)$Mean 

