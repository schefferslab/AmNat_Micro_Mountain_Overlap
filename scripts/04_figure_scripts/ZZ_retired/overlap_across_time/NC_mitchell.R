# David Klinges
# This script generates overlap plots for the Sierra Mountain forested sites
# This is a fragment of code from North Carolina Overlap Script
# Also depends on objects made in ameriflux_prep.R

## Workspace prep ##############
library(tidyverse)

# Import data
data_raw <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_wide.csv")
fine_res <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_mitchell_fineRes.csv")

# Designate site name
SITE_NAME <- "NC Mt Mitchell (deciduous forests)"

# Designate elevations of low and mid sites
LOW_ELEVATION <- "500"
HIGH_ELEVATION <- "1700"
# Set file path where figure will be saved
OUTPUT_FILE_PATH <- "figures/NC_Mitchell_overlapTime.png"

## 2. Generate daily, monthly, annual, ten-year timestep datasets #######

fine_res <- na.omit(fine_res)

fine_res_test <- fine_res %>% 
  group_by(julian) %>% 
  count()

soil_daily <- fine_res %>% 
  filter(julian == 287) %>% 
  group_by(hour) %>% 
  mutate(low_soil_min = min(low_soil, na.rm = TRUE)) %>% 
  mutate(low_soil_max = max(low_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_min = min(high_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_max = max(high_soil, na.rm = TRUE)) %>% 
  select(hour, low_soil_min, low_soil_max, high_soil_min, high_soil_max) %>% 
  distinct()

surface_daily <- fine_res %>% 
  filter(julian == 287) %>% 
  group_by(hour) %>% 
  mutate(low_surface_min = min(low_surface, na.rm = TRUE)) %>% 
  mutate(low_surface_max = max(low_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_min = min(high_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_max = max(high_surface, na.rm = TRUE)) %>% 
  select(hour, low_surface_min, low_surface_max, high_surface_min, high_surface_max) %>% 
  distinct()

canopy_daily <- fine_res %>% 
  filter(julian == 287) %>% 
  group_by(hour) %>% 
  mutate(low_canopy_min = min(low_canopy, na.rm = TRUE)) %>% 
  mutate(low_canopy_max = max(low_canopy, na.rm = TRUE)) %>% 
  mutate(high_canopy_min = min(high_canopy, na.rm = TRUE)) %>% 
  mutate(high_canopy_max = max(high_canopy, na.rm = TRUE)) %>% 
  select(hour, low_canopy_min, low_canopy_max, high_canopy_min, high_canopy_max) %>% 
  distinct()

## Seasonal Overlap plots #############

# Clean up device of the previous plot
plot.new()

par(mfrow = c(1, 3),     # 1x3 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 


## * Soil panel ##########

# Create figure axes
plot(soil_daily$hour, soil_daily$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0,24), ylab='temperature (C)', 
     xlab='hour Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(soil_daily$hour, soil_daily$low_soil_min, col = 'grey')
lines(soil_daily$hour, soil_daily$low_soil_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(soil_daily$hour, rev(soil_daily$hour)), 
        c(soil_daily$low_soil_max, rev(soil_daily$low_soil_min)), 
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(soil_daily$hour, soil_daily$high_soil_min, col = 'grey')
lines(soil_daily$hour, soil_daily$high_soil_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(soil_daily$hour, rev(soil_daily$hour)),
        c(soil_daily$high_soil_max, rev(soil_daily$high_soil_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)

mtext("Soil", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw

## * surface panel ##########

# Create figure axes
plot(surface_daily$hour, surface_daily$low_surface_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0, 24), ylab='temperature (C)', 
     xlab='hour Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("light green", "dark green"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(surface_daily$hour, surface_daily$low_surface_min, col = 'grey')
lines(surface_daily$hour, surface_daily$low_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(surface_daily$hour, rev(surface_daily$hour)), 
        c(surface_daily$low_surface_max, rev(surface_daily$low_surface_min)), 
        col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(surface_daily$hour, surface_daily$high_surface_min, col = 'grey')
lines(surface_daily$hour, surface_daily$high_surface_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(surface_daily$hour, rev(surface_daily$hour)),
        c(surface_daily$high_surface_max, rev(surface_daily$high_surface_min)),
        col = adjustcolor("dark green", alpha=.5), border = NA)

mtext("surface", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

surface_plot <- recordPlot()
#plot.new() ## clean up device
#surface_plot # redraw

## * canopy panel #########

# Create figure axes
plot(canopy_daily$hour, canopy_daily$low_canopy_max, axes=FALSE, type='n', bty="n", 
     cex.axis=1.5,ylim=c(-20,50), xlim=c(0, 24), ylab="", xlab='hour Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(canopy_daily$hour, canopy_daily$low_canopy_min, col = 'grey')
lines(canopy_daily$hour, canopy_daily$low_canopy_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(canopy_daily$hour, rev(canopy_daily$hour)),
        c(canopy_daily$low_canopy_max, rev(canopy_daily$low_canopy_min)),
        col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(canopy_daily$hour, canopy_daily$high_canopy_min, col = 'grey')
lines(canopy_daily$hour, canopy_daily$high_canopy_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(canopy_daily$hour, rev(canopy_daily$hour)),
        c(canopy_daily$high_canopy_max, rev(canopy_daily$high_canopy_min)),
        col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("canopy", NORTH<-3, cex=1.5,col="black")

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
      NORTH<-1, line=3, adj= 0.5, cex=0.7, col="Black", outer=TRUE)

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

