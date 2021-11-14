# David Klinges
# This script generates overlap d-score chi-squared across site plot

## Workspace prep ##############
library(tidyverse)

# Import data
boulder_raw <- read_csv("./data/04_analysis/Boulder_kruskal_outputs.csv")
catalina_raw <- read_csv("./data/04_analysis/Catalina_kruskal_outputs.csv")
sierra_raw <- read_csv("./data/04_analysis/sierra_kruskal_outputs.csv")
socal_raw <- read_csv("./data/04_analysis/SoCal_kruskal_outputs.csv")
wyoming_raw <- read_csv("./data/04_analysis/wyoming_kruskal_outputs.csv")

# Add flag for type of system
boulder <- boulder_raw %>%
  mutate(habitat = "high bald montane")

catalina <- catalina_raw %>%
  mutate(habitat = "desert to forest transition")

sierra <- sierra_raw %>%
  mutate(habitat = "coniferous forest")

socal <- socal_raw %>%
  mutate(habitat = "open")

wyoming <- wyoming_raw %>%
  mutate(habitat = "open")

# Join data
dscore_data <- boulder %>%
  full_join(catalina) %>%
  full_join(sierra) %>%
  full_join(socal) %>%
  full_join(wyoming)

ggplot(dscore_data, aes(micro, p_value)) +
  geom_boxplot(aes(color = factor(site))) +
  geom_point(aes(shape = factor(site), color = factor(site)), size = 3)

plot(chi_squared ~ micro, dscore_data)

dscore_data_avg <- dscore_data %>%
  group_by(site, micro) %>%
  summarize(chi = mean(chi_squared), p = mean(p_value))

# Designate elevations of low and mid sites
LOW_ELEVATION <- "383.7"
HIGH_ELEVATION <- "1249.31"
# Set file path where figure will be saved
OUTPUT_FILE_PATH <- "./figures/overlap_figures/avg_across_timeseries/Angelo_seasonal_variance_figure_avg_years.png"

# Set time range to plot 
YEAR <- c(2010)

# If there is a year column...which datasets containing averaged multi-year time
# series won't...
if (is.null(data_raw$year) == FALSE) {
  
  # Filter to just the years requested
  data <- data_raw %>%
    filter(year %in% YEAR)
} else {
  data <- data_raw
}

# Remove soil columns
data <- data %>%
  select(-high_soil_mean, -high_soil_max, -high_soil_min, -low_soil_mean, 
         -low_soil_max, -low_soil_min)

# Remove NA rows
data <- na.omit(data)
## Seasonal Overlap plots #############

# Clean up device of the previous plot
plot.new()

par(mfrow = c(1, 1),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

## * Air panel #########

# Create figure axes
plot(data$julian, data$low_soil_max, type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 50), xlim=c(0,365), ylab='temperature (C)', 
     xlab='Julian Day')

# # Create figure axes
# plot(data$julian, data$low_air_max, axes=FALSE, type='n', bty="n", 
#      cex.axis=1.5,ylim=c(-20,50), xlim=c(0,365), ylab="", xlab='Julian Day')

# Create figure legend
legend("bottomleft", c(paste0("lowlands (", LOW_ELEVATION, " m asl)"), 
                       paste0("Uplands (", HIGH_ELEVATION, " m asl)")), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# lowlands
# Plot lines for lowlands min and max temp
lines(data$julian, data$low_air_min, col = 'grey')
lines(data$julian, data$low_air_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(data$julian, rev(data$julian)),
        c(data$low_air_max, rev(data$low_air_min)),
        col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(data$julian, data$high_air_min, col = 'grey')
lines(data$julian, data$high_air_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(data$julian, rev(data$julian)),
        c(data$high_air_max, rev(data$high_air_min)),
        col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("Air temp", NORTH<-3, cex=1.5,col="black")

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



