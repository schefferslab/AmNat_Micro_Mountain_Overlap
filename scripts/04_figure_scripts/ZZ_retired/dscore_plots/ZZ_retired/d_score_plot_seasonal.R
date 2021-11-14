# David Klinges
# This script generates plot of the magnitude of change in overlap d-score 
#   values for each site

## 1. Workspace prep ##############
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggnewscale)
library(viridis)
library(grid)
library(gridExtra)
library(e1071)

mountains_janz_season <- read_csv("data/03_compiled/mountains_janz_season_avg.csv",
                                 col_types = cols(
                                   year = col_double(),
                                   elevation_change = col_double()
                                 ))
dscore_data <- mountains_janz_season


## 2. Curate data flags #############
# Change levels of factors
site_factors <- c("OR", "PH", "AU", "NM", "NC", "MD", "NH", "ID", "AZ", "PT", "CO")
macro_factors <- c("coniferous", "evergreen", "deciduous", "non-forest")
foliage_factors <- c("leaf-on", "leaf-off", "no foliage")
season_factors <- c("winter", "spring", "summer", "fall")

# Convert flags to factor
dscore_data <- dscore_data %>%
  mutate(macro = factor(macro, levels = macro_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors)) %>% 
  mutate(season = factor(season, levels = season_factors))

## ....2f. Calculate Standard deviation and error ########
dscore_data_naomit <- dscore_data %>% 
  filter(!is.na(overlap))

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, micro, site, elevation_change) %>%
  dplyr::summarize(mean = mean(overlap)) 

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SD = sd(overlap)) %>%
  dplyr::left_join(dscore_data_sum)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SE = sd(overlap)/sqrt(length(overlap))) %>%
  dplyr::left_join(dscore_data_sum) %>%
  ungroup()

## ....2g. Remove outliers (95%) ############

dscore_data <- dscore_data %>%
  left_join(dscore_data_sum)

dscore_data <- dscore_data %>%
  group_by(site) %>%
  mutate(overlap = ifelse(overlap > (mean + (3 * SD)) |
                            overlap < (mean - (3 * SD)) , NA, overlap)) %>%
  ungroup()

# dscore_data <- na.omit(dscore_data)


## ....2f. Control for elevation difference ########

## ....2f1. Determine data distribution and overlap ~ elevation_change relationship ###########

# Linear model minus outliers and outlier sites:
# 1.13 - 0.00142x
# See the ggplot with geom_smooth below


## Remove outliers
# Calculate mean
mean <- mean(dscore_data$overlap, na.rm = TRUE)

dscore_data_outOmit <- dscore_data %>% 
  filter(site != "NH" & site != "OR")

dscore_data_outOmit <- dscore_data_outOmit %>%
  # Remove values greater than 1.5 * IQR
  filter(overlap <= mean + (1.5 * IQR(dscore_data$overlap, na.rm = TRUE))) %>%
  # Remove values less than (1.5 * IQR)
  filter(overlap >= mean - (1.5 * IQR(dscore_data$overlap, na.rm = TRUE)))

# Check out distribution without outliers
# ggplot(dscore_data_outOmit, aes(overlap)) +
#   geom_histogram(binwidth = .1)

# Find out skewness of distribution
skewness(dscore_data_outOmit$overlap, na.rm = TRUE)
# -0.6810963, not great

## Check out overlap ~ elevation_change relationship
ggplot(dscore_data_outOmit, aes(elevation_change, overlap)) +
  geom_jitter(stat = "identity") +
  geom_line() +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method="lm",se=FALSE) +
  ylim(-5, 5)

# Linear model minus outliers and outlier sites:
# 1.13 - 0.00142x

# Just closed systems
ggplot(filter(dscore_data_outOmit, micro == "soil", macro == "evergreen" | macro == "deciduous" | macro == "coniferous"), aes(elevation_change, overlap)) +
  geom_jitter(stat = "identity") +
  geom_line()

# Just open systems
ggplot(filter(dscore_data_outOmit, micro == "soil", macro == "leaf-off" | macro == "non-forest"), aes(elevation_change, overlap)) +
  geom_jitter(stat = "identity") +
  geom_line()

## Now figure out a model that fits

# Calculate correlation
cor(dscore_data$elevation_change, dscore_data$overlap)
# -0.1617819

x <- dscore_data$elevation_change
y <- dscore_data$overlap

# Build linear model
linearMod <- lm(y ~ x, data = dscore_data_outOmit)  # build linear regression model on full data
# linearMod <- lm(y ~ x, data = filter(dscore_data, macro == "Leaf off" | macro == "Non-forest"))  # build linear regression model on non-leaf on data
# elevation_change2 <- .01 * (filter(dscore_data, macro == "leaf-off" | macro == "Non-forest")$elevation_change)^2
# quadMod <- lm(y ~ x + elevation_change2, data = filter(dscore_data, macro == "Leaf off" | macro == "Non-forest"))  # build linear regression model on full data

cor(y,predict(linearMod))

summary(quadMod)
print(linearMod)

# Build non-linear model
reciprocal <- nls(y ~ a * 1 /(x))

cor(y,predict(reciprocal))

plot(y ~ x, dscore_data)
lines(y, predict(reciprocal),lty=2,col="red",lwd=3)
# Non-linear looks better

## ....2f2. Scale dscores down to slope of 0 #########
# citation: https://www.biostars.org/p/194388/


# Save as new object
dscore_data_lm <- dscore_data

# Overwrite overlap with the residuals from the linear model-predicted
# values
# 1.13 - 0.00142x
residuals <- dscore_data_lm$overlap - (1.13 - 0.00142 * dscore_data_lm$elevation_change)
# dscore_data_lm <- dscore_data_lm %>% 
#   mutate(overlap = overlap - (1.13 - 0.00142 * elevation_change))

mean(dscore_data_lm$overlap) ## ~ zero as it should be
sd(dscore_data_lm$overlap)   ## ~ 1.6, which we should correct for

# dscore_data_lm$overlap<- residuals/sd(residuals)
dscore_data_lm$overlap <- linearMod$residuals
# dscore_data_lm$overlap<- linearMod$residuals/sd(linearMod$residuals)

mean(dscore_data_lm$overlap) ## ~ zero as it should be
sd(dscore_data_lm$overlap)   ## now exactly 1

dscore_data <- dscore_data_lm

## ....2g. Calculate Standard deviation and error, again (for plots now) ########
dscore_data_naomit <- dscore_data %>% 
  filter(!is.na(overlap))

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, micro, site, elevation_change) %>%
  dplyr::summarize(mean = mean(overlap))

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SD = sd(overlap)) %>%
  dplyr::left_join(dscore_data_sum)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SE = sd(overlap)/sqrt(length(overlap))) %>%
  dplyr::left_join(dscore_data_sum)

## 3. Create figure ###########

## ....3A. Boxplot separeted by micro and macro, default boxplot uncertainty ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_boxplot(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description),alpha = 0.7) +
  #guides(colour = guide_legend(override.aes = list(alpha = .7))) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(x = "Micromacro", y = "Overlap")

## ....3B. Violin plot overlayed with mean and standard error....but no jitter ###########
ggplot(dscore_data, aes(x = description, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description), alpha = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  geom_errorbar(aes(x = reorder(micro, macro, fun = mean), y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = dscore_data_sum, width = 0.2, color = "black") +
  labs(x = "Micromacro", y = "Overlap")

## ....3C. Violin plots, 3x panels separeted by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  # new_scale_color() +
  # scale_fill_manual(values = c("#5f7ba8", "#5f7ba8", "#ed9d2d", "#b21798")) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, 
                color = "black") + labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6"), 
                    guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  # scale_color_viridis(discrete = TRUE, option = "C", 
  #                     guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "surface"), 
                width = 0.2, color = "black") + labs(x = "surface")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6"), 
                    guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "C", 
                      guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "canopy"), 
                width = 0.2, color = "black") + labs(x = "Canopy")

# lay <- rbind(c(1, 2, 3, 3))
# grid.arrange(grobs = list(soil, surface + theme(legend.position = 'none'), canopy, legend), layout_matrix = lay)


plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy,
                      legend, 
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots

## ....3D. Point clouds, 3x panels separeted by micro ###########
dscore_data <- dscore_data %>% 
  arrange(season)

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap)) +
  geom_point(aes(color = season), alpha = 0.6, size = 5) +

  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
               aes(x = site, y = overlap)) +
  geom_point(aes(color = season), alpha = 0.6, size = 5) +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8))

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                  aes(x = site, y = overlap)) +
  geom_point(aes(color = season), alpha = 0.6, size = 5) +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8))

plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy,
                      legend, 
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots


## ....3E. Violin plots, grouped by macro and by micro ###########

grouped_stats <- dscore_data %>%
  group_by(micro, macro) %>%
  summarize(mean = mean(mean), SE = mean(SE)) %>%
  ungroup()

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = macro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6")) +
  geom_violin(aes(x = macro, y = overlap, fill = macro), alpha = 0.7, lwd = 1.5) +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  # geom_errorbar(aes(x = macro, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(grouped_stats, micro == "soil"), width = 0.2, color = "black") +
  labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), aes(x = macro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6")) +
  geom_violin(aes(x = macro, y = overlap, fill = macro), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # geom_errorbar(aes(x = macro, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(dscore_data_sum, micro == "surface"), width = 0.2, color = "black") +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), aes(x = macro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6"), guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = macro, y = overlap, fill = macro), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C", guide = FALSE) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)) +
  # geom_errorbar(aes(x = macro, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(dscore_data_sum, micro == "canopy"), width = 0.2, color = "black") +
  labs(x = "Canopy")

# lay <- rbind(c(1, 2, 3, 3))
# grid.arrange(grobs = list(soil, surface, canopy), layout_matrix = lay)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)


plots <- grid.arrange(soil, surface, canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots


## ....3F. Point clouds, grouped by macro and by micro ###########

grouped_stats <- dscore_data %>%
  group_by(micro, macro) %>%
  summarize(mean = mean(mean), SE = mean(SE)) %>%
  ungroup()

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = macro, y = overlap)) +
  geom_jitter(aes(color = season), alpha = 0.6, size = 5) +
  
  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = macro, y = overlap)) +
  geom_jitter(aes(color = season), alpha = 0.6, size = 5) +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))
  

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = macro, y = overlap)) +
  geom_jitter(aes(color = season), alpha = 0.6, size = 5) +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_colour_manual(values = c("#3e9de0", "#e08626", "#307233", "#f4f142")) +
  coord_cartesian(xlim = NULL, ylim = c(-2, 5)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8))

plots <- grid.arrange(soil, surface, canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots




## ....3G. Plot overlap by elevation difference ########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = elevation_change, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = as.factor(elevation_change), y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_errorbar(aes(x = elevation_change, y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, color = "black") +
  labs(x = "Soil")

## ....3H. Boxplot with overlayed mean and standard error...but no jitter ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  stat_boxplot(aes(micro, overlap), geom='errorbar', linetype=1, width=0.5) +  #whiskers
  geom_boxplot( aes(micro, overlap),outlier.shape=1) +    
  stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE)

leafoff <- filter(dscore_data, site == "N Carolina off", micro == "soil")
leafon <- filter(dscore_data, site == "N Carolina on", micro == "soil")

mean(na.omit(leafoff$overlap))
mean(na.omit(leafon$overlap))


## 4. Quick calculation of average climate isolation #######

# Not really any better spot to put this....

## ....4A. Soil ###########
# temp leaf on
dscore_data %>%
  filter(micro == "soil", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "soil", macro == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "soil", macro == "Non-forest") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "soil", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))







dscore_data %>%
  filter(micro == "surface", macro == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "surface", macro == "Leaf off") %>%
  summarize(mean(overlap))


dscore_data %>%
  filter(micro == "canopy", macro == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "canopy", macro == "Leaf off") %>%
  summarize(mean(overlap))

## ....4B. Surface #####
# temp leaf on
dscore_data %>%
  filter(micro == "surface", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "surface", macro == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "surface", macro == "Non-forest") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "surface", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))

## ....4C. Canopy ########

# temp leaf on
dscore_data %>%
  filter(micro == "canopy", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "canopy", macro == "Leaf off") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "canopy", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))
