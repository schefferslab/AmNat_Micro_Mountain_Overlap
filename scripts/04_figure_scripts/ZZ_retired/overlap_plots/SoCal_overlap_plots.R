# David Klinges
# This script generates overlap plots for the Souther California desert sites
# This is a fragment of code from North Carolina Overlap Script
# Also depends on objects made in ameriflux_prep.R


require(tidyverse)


socal <- read.csv("./data/02_derivative/wyoming_all_elev_avg_years_wide.csv")

# socal <- socal %>%
#   filter(year == 2010 | year == 2011)

socal <- na.omit(socal)
## Seasonal Overlap plots #############

par(mfrow = c(1, 2),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

####### Soil panel ##########

# Create figure axes
plot(socal$julian, socal$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-40, 120), xlim=c(0,365), ylab='temperature (C)', 
     xlab='Julian Day')

# Create figure legend
legend("bottomleft", c("lowlands (275 m asl)", "Uplands (1280 m asl)"), bty="n", 
       lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(socal$julian, socal$low_soil_min, col = 'grey')
lines(socal$julian, socal$low_soil_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(socal$julian, rev(socal$julian)),
        c(socal$low_soil_max, rev(socal$low_soil_min)),
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(socal$julian, socal$high_soil_min, col = 'grey')
lines(socal$julian, socal$high_soil_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(socal$julian, rev(socal$julian)),
        c(socal$high_soil_max, rev(socal$high_soil_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)

mtext("Soil", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw

####### Air panel #########

# Create figure axes
plot(socal$julian, socal$low_air_max, axes=FALSE, type='n', bty="n", 
     cex.axis=1.5,ylim=c(-20,50), xlim=c(0,365), ylab="", xlab='Julian Day')

# Create figure legend
legend("bottomleft", c("lowlands (275 m asl)", "Uplands (1280 m asl)"), bty="n", 
       lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(socal$julian, socal$low_air_min, col = 'grey')
lines(socal$julian, socal$low_air_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(socal$julian, rev(socal$julian)),
        c(socal$low_air_max, rev(socal$low_air_min)),
        col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(socal$julian, socal$high_air_min, col = 'grey')
lines(socal$julian, socal$high_air_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(socal$julian, rev(socal$julian)),
        c(socal$high_air_max, rev(socal$high_air_min)),
        col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("Air temp", NORTH<-3, cex=1.5,col="black")

canopy_plot <- recordPlot()
#plot.new() ## clean up device
#canopy_plot # redraw

####### Titles ########

#dev.off()
#plot.new()
mtext("temperature", NORTH<-2, line=1.5, adj=.5,cex=1.5,
      col="black", outer=TRUE)

mtext("Day of Year",
      NORTH<-1, line=1.5, adj=.5, cex=1.2, col="Black", outer=TRUE)

title_plot <- recordPlot()
#plot.new() ## clean up device
#title_plot # redraw

#png(filename="/Users/David/Desktop/Career Stuff/Mountain Passes/Figures/SoCal_seasonal_variance_figure.png", width=24, height=12, units="cm", res=300)
dev.copy(png, "NC_seasonal_variance_figure.png")
plot(title_plot)
dev.off()

#
#
#
#



