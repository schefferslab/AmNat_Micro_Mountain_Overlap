# David Klinges
# This script generates overlap plots with different panels for increasing
# temporal representation: day, month, year, 10 years

## 1. Workspace prep ##############
library(tidyverse)
library(scales)
library(gridExtra)

# Import data
data_raw <- read_csv("./data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_wide.csv")
fine_res <- read_csv("./data/01_primary/temperate/OR_andrews/derivative/fineRes/OR_andrews_fineResRS89RS04.csv")

# Designate site name
SITE_NAME <- "Oregon HJ Andrews Exp Forest"

# Designate elevations of low and high sites
LOW_ELEVATION <- "460-499"
HIGH_ELEVATION <- "978-1430"
# Set file path where figure will be saved
OUTPUT_FILE_PATH <- "figures/overlap_figures/OrSoil_overlapTime.png"

## 2. Generate daily, monthly, annual, ten-year timestep datasets #######

## ....A. Daily ##########
soil_daily <- fine_res %>% 
  filter(year == 2000 & julian == 170) %>% 
  group_by(hour) %>% 
  mutate(low_soil_min = min(low_soil, na.rm = TRUE)) %>% 
  mutate(low_soil_max = max(low_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_min = min(high_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_max = max(high_soil, na.rm = TRUE)) %>% 
  select(hour, low_soil_min, low_soil_max, high_soil_min, high_soil_max) %>% 
  distinct()

surface_daily <- fine_res %>% 
  filter(year == 2000 & julian == 170) %>% 
  group_by(hour) %>% 
  mutate(low_surface_min = min(low_surface, na.rm = TRUE)) %>% 
  mutate(low_surface_max = max(low_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_min = min(high_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_max = max(high_surface, na.rm = TRUE)) %>% 
  select(hour, low_surface_min, low_surface_max, high_surface_min, high_surface_max) %>% 
  distinct()

## ....B. Monthly ##########
monthly <- data_raw %>% 
  filter(year == 2000 & julian < 32)

## ....C. Annual #########
annual <- data_raw %>% 
  filter(year == 2000)

## ....D. Decadal #########

decadal <- data_raw %>% 
  filter(year > 1997 & year < 2004) %>% 
  # Scale julian from 0 to 1
  mutate(year_decimal = rescale(julian, to = c(0, 1))) %>% 
  mutate(year = year + year_decimal)

## Temporal overlap plots #############

# Clean up device of the previous plot
plot.new()

par(mfrow = c(1, 4),     # 1x4 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 



## * Daily panel ##########

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

mtext("Daily", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

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

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw

## * Monthly panel ##########

# Create figure axes
plot(monthly$julian, monthly$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0,31), ylab='temperature (C)', 
     xlab='julian Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(monthly$julian, monthly$low_soil_min, col = 'grey')
lines(monthly$julian, monthly$low_soil_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly$julian, rev(monthly$julian)), 
        c(monthly$low_soil_max, rev(monthly$low_soil_min)), 
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(monthly$julian, monthly$high_soil_min, col = 'grey')
lines(monthly$julian, monthly$high_soil_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(monthly$julian, rev(monthly$julian)),
        c(monthly$high_soil_max, rev(monthly$high_soil_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)

mtext("Monthly", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

# lowlands
# Plot lines for lowlands min and max temp
lines(monthly$julian, monthly$low_surface_min, col = 'grey')
lines(monthly$julian, monthly$low_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly$julian, rev(monthly$julian)), 
        c(monthly$low_surface_max, rev(monthly$low_surface_min)), 
        col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(monthly$julian, monthly$high_surface_min, col = 'grey')
lines(monthly$julian, monthly$high_surface_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(monthly$julian, rev(monthly$julian)),
        c(monthly$high_surface_max, rev(monthly$high_surface_min)),
        col = adjustcolor("dark green", alpha=.5), border = NA)

mtext("surface", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw


## * Annual panel ##########

# Create figure axes
plot(annual$julian, annual$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0, 365), ylab='temperature (C)', 
     xlab='julian Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(annual$julian, annual$low_soil_min, col = 'grey')
lines(annual$julian, annual$low_soil_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(annual$julian, rev(annual$julian)), 
        c(annual$low_soil_max, rev(annual$low_soil_min)), 
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(annual$julian, annual$high_soil_min, col = 'grey')
lines(annual$julian, annual$high_soil_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(annual$julian, rev(annual$julian)),
        c(annual$high_soil_max, rev(annual$high_soil_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)

mtext("annual", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

# lowlands
# Plot lines for lowlands min and max temp
lines(annual$julian, annual$low_surface_min, col = 'grey')
lines(annual$julian, annual$low_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(annual$julian, rev(annual$julian)), 
        c(annual$low_surface_max, rev(annual$low_surface_min)), 
        col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(annual$julian, annual$high_surface_min, col = 'grey')
lines(annual$julian, annual$high_surface_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(annual$julian, rev(annual$julian)),
        c(annual$high_surface_max, rev(annual$high_surface_min)),
        col = adjustcolor("dark green", alpha=.5), border = NA)

mtext("surface", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw



## * Decadal panel ##########

# Soil temps

day <- ggplot(soil_daily, aes(hour, low_soil_min)) +
  geom_ribbon(aes(ymin = low_soil_min, ymax = low_soil_max), 
              fill = "purple", alpha = 0.5) +
  geom_ribbon(aes(ymin = high_soil_min, ymax = high_soil_max), 
              fill = "purple", alpha = 0.5) +
  geom_line(aes(y = high_soil_min), 
              color = "blue", alpha = 0.5)

month <- ggplot(monthly, aes(julian, low_soil_min)) +
  geom_ribbon(aes(ymin = low_soil_min, ymax = low_soil_max), 
              fill = "purple", alpha = 0.5) +
  geom_ribbon(aes(ymin = high_soil_min, ymax = high_soil_max), 
              fill = "blue", alpha = 0.5) +
  geom_line(aes(y = high_soil_min), 
            color = "blue", alpha = 0.5)


year <- ggplot(annual, aes(julian, low_soil_min)) +
  geom_ribbon(aes(ymin = low_soil_min, ymax = low_soil_max), 
              fill = "purple", alpha = 0.5) +
  geom_ribbon(aes(ymin = high_soil_min, ymax = high_soil_max), 
              fill = "blue", alpha = 0.5) +
  geom_line(aes(y = high_soil_min), 
            color = "blue", alpha = 0.5)

decade <- ggplot(decadal, aes(year, low_soil_mean)) +
  geom_ribbon(aes(ymin = low_soil_min, ymax = low_soil_max), fill = "purple") +
  geom_ribbon(aes(ymin = high_soil_min, ymax = high_soil_max), fill = "blue")
  
grid.arrange(day, month, year, decade, ncol = 4)

ggplot(decadal, aes(year, low_surface_mean)) +
  geom_ribbon(aes(ymin = low_surface_min, ymax = low_surface_max), 
              fill = "purple", alpha = 0.5) +
  geom_ribbon(aes(ymin = high_surface_min, ymax = high_surface_max), 
              fill = "blue", alpha = 0.5)


# Create figure axes
plot(decadal$year, decadal$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(1998, 2004), ylab='temperature (C)', 
     xlab='year Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(decadal$year, decadal$low_soil_min, col = 'grey')
lines(decadal$year, decadal$low_soil_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(decadal$year, rev(decadal$year)), 
        c(decadal$low_soil_max, rev(decadal$low_soil_min)), 
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(decadal$year, decadal$high_soil_min, col = 'grey')
lines(decadal$year, decadal$high_soil_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(decadal$year, rev(decadal$year)),
        c(decadal$high_soil_max, rev(decadal$high_soil_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)

mtext("decadal", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

# lowlands
# Plot lines for lowlands min and max temp
lines(decadal$year, decadal$low_surface_min, col = 'grey')
lines(decadal$year, decadal$low_surface_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(decadal$year, rev(decadal$year)), 
        c(decadal$low_surface_max, rev(decadal$low_surface_min)), 
        col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(decadal$year, decadal$high_surface_min, col = 'grey')
lines(decadal$year, decadal$high_surface_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(decadal$year, rev(decadal$year)),
        c(decadal$high_surface_max, rev(decadal$high_surface_min)),
        col = adjustcolor("dark green", alpha=.5), border = NA)

mtext("surface", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw



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


