library(dplyr)
library(tidyverse)

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")

T.Sizes <- c(5,8,10,15)

LCOE14.high <- c(345,244,246,289)
LCOE14.low <- c(326,227,230,273)
ATB14.high <- c(154,142,136,122)
ATB14.low <- c(143,131,125,112)

LCOEpen14.high <- LCOE14.high - ATB14.low
LCOEpen14.low <- LCOE14.low - ATB14.high

LCOE8.high <- c(143,106,112,140)
LCOE8.low <- c(134,99,105,132)
ATB8.high <- c(72,69,67,63)
ATB8.low <- c(66,63,61,57)

LCOEpen8.high <- LCOE8.high - ATB8.low
LCOEpen8.low <- LCOE8.low - ATB8.high

setEPS()
postscript("LCOE Figure_v7.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = T.Sizes, y = LCOE14.high, col = '#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2,
     xlab = "Turbine Size [MW]", ylab = "LCOE [USD/MWh]",
     xaxs = 'i', yaxs = 'i', las = 1, xlim = c(0,16), ylim = c(0,350), cex.lab = 1.2, cex.axis = 1.2)
points(x = T.Sizes, y = LCOE14.low, col ='#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2)
polygon(x = c(T.Sizes,rev(T.Sizes)), y = c(ATB14.high, rev(ATB14.low)), 
        col = '#c1272d', border = NA)
points(x = T.Sizes, y = LCOE8.high, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)
points(x = T.Sizes, y = LCOE8.low, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)
polygon(x = c(T.Sizes,rev(T.Sizes)), y = c(ATB8.high, rev(ATB8.low)), 
        col = '#0000a7', border = NA)

text(x=0.5,y=260,"WSC 14", col = '#c1272d',adj=c(0,1), cex = 1.7)
text(x=0.5,y=110,"WSC 8", col = '#0000a7',adj=c(0,1), cex = 1.7)

text(x=5.1,y=ATB14.high[1]+19,"NREL ATB Results (moored)", col = '#c1272d',adj=c(0,1), cex = 1.0)
text(x=5.1,y=ATB8.low[1]-7,"NREL ATB Results (moored)", col = '#0000a7',adj=c(0,1), cex = 1.0)
text(x=4.9, y = 340, "Model Results (DP)", col = '#c1272d', adj = c(1,1), cex = 1.0)
text(x=4.9, y = 147, "Model Results (DP)", col = '#0000a7', adj = c(1,1), cex = 1.0)

dev.off()

setEPS()
postscript("LCOE Penalty Figure_v3.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = T.Sizes, y = LCOEpen14.high, col = '#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2,
     xlab = "Turbine Size [MW]", ylab = "LCOE Penalty [USD/MWh]",
     xaxs = 'i', yaxs = 'i', las = 1, xlim = c(0,16), ylim = c(0,250), cex.lab = 1.2, cex.axis = 1.2)
points(x = T.Sizes, y = LCOEpen14.low, col ='#c1272d', type = 'b', pch = 19, lwd = 2, lty = 2)

points(x = T.Sizes, y = LCOEpen8.high, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)
points(x = T.Sizes, y = LCOEpen8.low, col ='#0000a7', type = 'b', pch = 19, lwd = 2, lty = 2)

text(x=2.1,y=200,"WSC 14", col = '#c1272d',adj=c(0,1), cex = 1.7)
text(x=2.1,y=80,"WSC 8", col = '#0000a7',adj=c(0,1), cex = 1.7)

dev.off()

##############################
#Thruster Power Functions
## Define system and resulting DP.Power consumption. Fucntions from 20220523 15 MW Turbine and Functions.R
DP.Power.n <- function(force.kn, motor.diam, n = 1) { #power of n motors system, motor.diam in meters
  K <- 1250
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}

# Data from Kongsberg for 3.8 m diameter thruster
Del.Thrust <- c(0,16,35,98,141,192,251,318,392,475,565,663,769,881) #kN
Input.Power <- c(0,11,38,178,308,489,729,1038,1425,1896,2462,3130,3909,4796)/1000 #MW
Diam <- 3.8 #m

setEPS()
postscript("Figure 3.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = Del.Thrust, y = Input.Power, pch = 19, 
     xlim = c(0,2500), ylim = c(0,15), xaxs = 'i', yaxs = 'i', las = 1,
     xlab = "Delivered Thrust [kN]", ylab = "Dynamic Positioning Power Requirement [MW]")
segments(x0 = max(Del.Thrust), y0 = 0, x1 = max(Del.Thrust), y1 = max(Input.Power), 
         lty = 2, lwd = 3, col = 'red')
segments(x0 = max(Del.Thrust)*2, y0 = 0, x1 = max(Del.Thrust)*2, y1 = DP.Power.n(max(Del.Thrust)*2,Diam, n = 2), 
         lty = 2, lwd = 3, col = 'red')

curve(DP.Power.n(x,Diam), from = 0, to = max(Del.Thrust), add = TRUE, lwd = 2)
curve(DP.Power.n(x,Diam, n = 2), from = 0, to = max(Del.Thrust)*2, add = TRUE, 
      lwd = 2, col = '#eecc16')
# curve(DP.Power.n(x,Diam, n = 3), from = 0, to = max(Del.Thrust)*3, add = TRUE, 
#       lwd = 2, col = '#0000a7')
curve(DP.Power.n(x,Diam, n = 6), from = 0, to = max(Del.Thrust)*6, add = TRUE, 
      lwd = 2, col = '#008176')
points(x = Del.Thrust, y = Input.Power, pch = 1, cex = 1.2)
text(x = 2250, y = 10, "6-Thruster \nConfiguration", col = '#008176')
text(x = 1250, y = 8, "2-Thruster \nConfiguration", col = '#eecc16')
text(x = 250, y = 4, "1-Thruster \nConfiguration", col = 'black')
text(x = max(Del.Thrust)+10, y=0.1, "1-Thruster \nConfiguration \nMax Thrust", col = 'red',
     adj = c(0,0), cex = 0.8)

text(x = max(Del.Thrust)*2+10, y=0.1, "2-Thruster \nConfiguration \nMax Thrust", col = 'red',
     adj = c(0,0), cex = 0.8)
legend("topleft", legend = c("Given Manufacturer Data Points","Functional Representation of Power v. Thrust"), 
       col = 'black', pch = c(19,NA), lty = c(NA,1), lwd = c(NA,2),
       cex = 1, bty = 'n')
dev.off()


#################################################
# OrcaFlex Validation

my_DF <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/OrcaFlex/OrcaFlex Validation Data.csv") %>%
  rename(speeds = ï..speeds)

setEPS()
postscript("Figure S3.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = my_DF$speeds, y = my_DF$Link_1, pch = 15, ylim = c(0,1000),
     xaxs = 'i', yaxs = 'i', col = 'black', las = 1,
     xlab = 'Wind Speed [m/s]', ylab = 'Turbine Thrust Force [kN]')
points(x = my_DF$speeds, y = my_DF$DLC1.3, pch = 19, col = '#c1272d', cex = 0.5)
legend('topright', legend = c("Authors' R Model Results",
                              "OrcaFlex Steady-State Results"),
       pch = c(19,15), col = c(palette()[6],'black'), bty = 'n')
dev.off()

#################################################
# Thrust Force Functions


setEPS()
postscript("Thrust Force Functions Figure.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))

plot(x = speeds, y = DLC1.3_15, type = 'n',
     ylim = c(0,3000), yaxs = 'i', xaxs = 'i',
     ylab = "",
     xlab = "Wind Speed [m/s]", las = 1, cex.axis = 1.2, cex.lab = 1.2)
lines(x = speeds, y = DLC1.3_15, col = '#c1272d', lty = 1, lwd = 2)
lines(x = speeds, y = DLC1.3_10, col = '#0000a7', lty = 1, lwd = 2)
lines(x = speeds, y = DLC1.3_8, col = '#008176', lty = 1, lwd = 2)
lines(x = speeds, y = DLC1.3_5, col = 'black', lty = 1, lwd = 2)

mtext("Turbine Thrust Force [kN]", side = 2, line = 4.5, cex = 1.2)

text(x = 20, y = 552, "5 MW", col = 'black', adj = c(0,0))
text(x = 20, y = 702, "8 MW", col = '#008176', adj = c(0,0))
text(x = 20, y = 852, "10 MW", col = '#0000a7', adj = c(0,0))
text(x = 20, y = 1552, "15 MW", col = '#c1272d', adj = c(0,0))


dev.off()
