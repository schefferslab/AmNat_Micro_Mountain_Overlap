# David Klinges
# File creation date: 2019-04-22
# This script generates overlap plots for Serra da Estrela sites
# This is a modified version of main overlap plots script, which only plots
#   surface temp (as Angelo high elevation site only had surface temp)

## Workspace prep ##############
library(tidyverse)

# Import data
data_raw <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_southwest_wide.csv")

# Designate site name
SITE_NAME <- "Costa Rica Southwest"

# Designate elevations of low and mid sites
LOW_ELEVATION <- "16"
HIGH_ELEVATION <- "3096"
# Set file path where figure will be saved
OUTPUT_FILE_PATH <- "figures/overlap_figures/single_year_only/CR_southwest.png"

# Set time range to plot 
YEAR <- c(1961)

# If there is a year column...which datasets containing averaged multi-year time
# series won't...
if (is.null(data_raw$year) == FALSE) {
  
  # Filter to just the years requested
  data <- data_raw %>%
    filter(year %in% YEAR)
} else {
  data <- data_raw
}

# Remove NA rows
data <- na.omit(data)
## Seasonal Overlap plots #############

# Clean up device of the previous plot
plot.new()

par(mfrow = c(1, 1),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

## * surface panel #########

# Create figure axes
plot(data$month, data$low_surface_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0,12), ylab='temperature (C)', 
     xlab='month Day')

# # Create figure axes
# plot(data$month, data$low_surface_max, axes=FALSE, type='n', bty="n", 
#      cex.axis=1.5,ylim=c(-20,50), xlim=c(0,12), ylab="", xlab='month Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(data$month, data$low_surface_min, col = 'grey')
lines(data$month, data$low_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(data$month, rev(data$month)),
        c(data$low_surface_max, rev(data$low_surface_min)),
        col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(data$month, data$high_surface_min, col = 'grey')
lines(data$month, data$high_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(data$month, rev(data$month)),
        c(data$high_surface_max, rev(data$high_surface_min)),
        col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("surface temp", NORTH<-3, cex=1.5,col="black")

canopy_plot <- recordPlot()
#plot.new() ## clean up device
#canopy_plot # redraw

## * Titles ########

#dev.off()
#plot.new()
mtext("temperature", NORTH<-2, line=1.5, adj=.5,cex=1.5,
      col="black", outer=TRUE)

mtext("Day of Year",
      NORTH<-1, line=1.5, adj=.5, cex=1.2, col="Black", outer=TRUE)

mtext(paste(c("Thermal overlap across elevation at ", SITE_NAME, " in year(s) ", 
              YEAR), collapse=" "),
      NORTH<-1, line=3, adj= 0, cex=1, col="Black", outer=TRUE)

## * Write plot to PNG file ###################

# Record the currently displayed plot to an object
title_plot <- recordPlot()

# Designate the output file type (png), file path to save to, and dimensions
png(filename= OUTPUT_FILE_PATH, width=24, height=12, units="cm", res=300)

# Redraw the plot
title_plot

# I believe the dev.copy method here, if it works, is redundant. Keeping it in
#   for documentation purposes
# dev.copy(png, OUTPUT_FILE_PATH)

# Close the graphics device, thereby telling R to save the plot
dev.off()






#
#
#
#

