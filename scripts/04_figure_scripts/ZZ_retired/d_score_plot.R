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

# Import data
angelo_leafoff_raw <- read_csv("./data/01_primary/temperate/CA_Angelo/derivative/Angelo_wssm_leafoff_wide.csv")
angelo_leafoff_raw <- angelo_leafoff_raw %>%
  mutate_all(as.double)

angelo_leafon_raw <- read_csv("./data/01_primary/temperate/CA_Angelo/derivative/Angelo_wssm_leafon_wide.csv")
angelo_leafon_raw <- angelo_leafon_raw %>%
  mutate_all(as.double)

boulder_raw <- read_csv("./data/02_derivative/Boulder_met1-GLV_wide.csv")
boulder_raw <- boulder_raw %>%
  mutate_all(as.double)

catalina_raw <- read_csv("./data/02_derivative/Catalina_wide.csv")
catalina_raw <- catalina_raw %>%
  mutate_all(as.double)

serra_estrala_raw <- read_csv("./data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_wide.csv")
serra_estrala_raw <- serra_estrala_raw %>%
  mutate_all(as.double)

sierra_raw <- read_csv("./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/sierra_wide.csv")
sierra_raw <- sierra_raw %>%
  mutate_all(as.double)

sonoran_raw <- read_csv("./data/01_primary/temperate/CA_sonoran_desert/derivative/CA_sonoran_desert_wide.csv")
sonoran_raw <- sonoran_raw %>%
  mutate_all(as.double)

idaho_raw <- read_csv("./data/01_primary/temperate/ID_sage_brush/derivative/ID_wide.csv")
idaho_raw <- idaho_raw %>%
  mutate_all(as.double)

valles_raw <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_wide.csv")
valles_raw <- valles_raw %>%
  mutate_all(as.double)

NC_leafon_raw <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_on_wide.csv")
NC_leafon_raw <- NC_leafon_raw %>%
  mutate_all(as.double)

NC_leafoff_raw <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_off_wide.csv")
NC_leafoff_raw <- NC_leafoff_raw %>%
  mutate_all(as.double)

Mada_raw <- read_csv("./data/01_primary/tropical/Madagascar/derivative/Mada_wide.csv")
Mada_raw <- Mada_raw %>%
  mutate_all(as.double)

Phili_raw <- read_csv("./data/01_primary/tropical/Philippines/derivative/Phili_wide.csv")
Phili_raw <- Phili_raw %>%
  mutate_all(as.double)

Aust_raw <- read_csv("./data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
Aust_raw <- Aust_raw %>%
  mutate_all(as.double)

## 2. Curate data #############

## ....2a. Brief prelim curation ###########
# Remove mid-elevation from Sierra

# Curate NC and tropical data to work with janzify()
colnames(boulder_raw) <- gsub("air", "surface", colnames(boulder_raw))
colnames(catalina_raw) <- gsub("air", "surface", colnames(catalina_raw))
colnames(sierra_raw) <- gsub("air", "surface", colnames(sierra_raw))
colnames(sonoran_raw) <- gsub("air", "surface", colnames(sonoran_raw))
colnames(idaho_raw) <- gsub("air", "surface", colnames(idaho_raw))

# Change ground to surface for NC and tropical
colnames(NC_leafon_raw) <- gsub("ground", "surface", colnames(NC_leafon_raw))
colnames(NC_leafoff_raw) <- gsub("ground", "surface", colnames(NC_leafoff_raw))
colnames(Mada_raw) <- gsub("ground", "surface", colnames(Mada_raw))
colnames(Phili_raw) <- gsub("ground", "surface", colnames(Phili_raw))
colnames(Aust_raw) <- gsub("ground", "surface", colnames(Aust_raw))

# There's clearly one outlier day in the Mada data
Mada_raw <- Mada_raw %>%
  filter_at(vars(contains("min")), all_vars(. < 40)) %>%
  filter_at(vars(contains("mean")), all_vars(. < 40)) %>%
  filter_at(vars(contains("max")), all_vars(. < 40))

# Subset to just one year for each site
# sonoran_raw <- sonoran_raw %>%
#   filter(year == 2009)
# 
# idaho_raw <- idaho_raw %>%
#   filter(year == 2015)
# 
# Aust_raw <- Aust_raw %>%
#   filter(year == 2008)

## ....2b. Janzify datasets #############
## ....2b1. Janzify by day #########

# Janzify
source("./scripts/data_processing/janzify.R")
angelo_leafoff <- janzify(angelo_leafoff_raw)
angelo_leafon <- janzify(angelo_leafon_raw)
boulder <- janzify(boulder_raw)
catalina <- janzify(catalina_raw)
serra_estrala <- janzify(serra_estrala_raw)
sierra <- janzify(sierra_raw)
sonoran <- janzify(sonoran_raw)
idaho <- janzify(idaho_raw)
valles <- janzify(valles_raw)
NC_leafon <- janzify(NC_leafon_raw)
NC_leafoff <- janzify(NC_leafoff_raw)
Mada <- janzify(Mada_raw)
Phili <- janzify(Phili_raw)
Aust <- janzify(Aust_raw)


## ....2b2. Janzify by month #########

# # Janzify
# source("./scripts/data_processing/janzify.R")
# angelo_leafoff <- janzify_per_month(angelo_leafoff_raw, 151, 280)
# angelo_leafon <- janzify_per_month(angelo_leafon_raw, 151, 280)
# boulder <- janzify_per_month(boulder_raw, 151, 280)
# catalina <- janzify_per_month(catalina_raw, 151, 280)
# serra_estrala <- janzify_per_month(serra_estrala_raw, 151, 280)
# sierra <- janzify_per_month(sierra_raw, 151, 280)
# sonoran <- janzify_per_month(sonoran_raw, 151, 280)
# idaho <- janzify_per_month(idaho_raw, 151, 280)
# valles <- janzify_per_month(valles_raw, 151, 280)
# NC_leafon <- janzify_per_month(NC_leafon_raw, 151, 280)
# NC_leafoff <- janzify_per_month(NC_leafoff_raw, 151, 280)
# Mada <- janzify_per_month(Mada_raw, 151, 280)
# Phili <- janzify_per_month(Phili_raw, 151, 280)
# Aust <- janzify_per_month(Aust_raw, 151, 280)
# 
# 
# # Boulder data sometimes receives an "inf" for overlap, let's fix
# boulder <- boulder %>%
#   filter(is.finite(overlap))

## ....2c. Add habitat flags #############
# Add flag for type of system

angelo_leafoff <- angelo_leafoff %>%
  mutate(description = "Temperate deciduous forest ") %>%
  mutate(habitat = "Leaf off") %>%
  mutate(site = "Ang off")

angelo_leafon <- angelo_leafon %>%
  mutate(description = "Temperate deciduous forest ") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "Ang on")

boulder <- boulder %>%
  mutate(description = "Coniferous forest to high bald montane") %>%
  mutate(habitat = "Non-forest") %>%
  mutate(site = "CO")

catalina <- catalina %>%
  mutate(description = "Desert to montane forest") %>%
  mutate(habitat = "Mixed") %>%
  mutate(site = "NMC")

serra_estrala <- serra_estrala %>%
  mutate(description = "Barren mountain") %>%
  mutate(habitat = "Non-forest") %>%
  mutate(site = "PT") 

sierra <- sierra %>%
  mutate(description = "Coniferous forest") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "CA")

sonoran <- sonoran %>%
  mutate(description = "Scrub and brush") %>%
  mutate(habitat = "Non-forest") %>%
  mutate(site = "AZ")

idaho <- idaho %>%
  mutate(description = "Scrub and brush") %>%
  mutate(habitat = "Non-forest") %>%
  mutate(site = "ID")

valles <- valles %>%
  mutate(description = "Coniferous forest") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "NMV")

NC_leafoff <- NC_leafoff %>%
  mutate(description = "Temperate deciduous forest ") %>%
  mutate(habitat = "Leaf off") %>%
  mutate(site = "NC off")

NC_leafon <- NC_leafon %>%
  mutate(description = "Temperate deciduous forest ") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "NC on")

Mada <- Mada %>%
  mutate(description = "Tropical broad-leaf") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "Mada")

Phili <- Phili %>%
  mutate(description = "Tropical broad-leaf") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "Phili")

Aust <- Aust %>%
  mutate(description = "Tropical broad-leaf") %>%
  mutate(habitat = "Leaf on") %>%
  mutate(site = "Aust")
  
## ....2d. Add attribute for elevation difference ########

angelo_leafoff <- angelo_leafoff %>%
  mutate(elev_diff = 865)

angelo_leafon <- angelo_leafon %>%
  mutate(elev_diff = 865)

boulder <- boulder %>%
  mutate(elev_diff = 1900)

catalina <- catalina %>%
  mutate(elev_diff = 1900)

serra_estrala <- serra_estrala %>%
  mutate(elev_diff = 800)

sierra <- sierra %>%
  mutate(elev_diff = 1550)

sonoran <- sonoran %>%
  mutate(elev_diff = 1005)

idaho <- idaho %>%
  mutate(elev_diff = 686)

valles <- valles %>%
  mutate(elev_diff = 503)

NC_leafoff <- NC_leafoff %>%
  mutate(elev_diff = 1200)

NC_leafon <- NC_leafon %>%
  mutate(elev_diff = 1200)

Mada <- Mada %>%
  mutate(elev_diff = 950)

Phili <- Phili %>%
  mutate(elev_diff = 800)

Aust <- Aust %>%
  mutate(elev_diff = 900)

## ....2e. Join data ###############
dscore_data <- sonoran %>%
  # bind_rows(angelo_leafoff) %>%
  # bind_rows(angelo_leafon) %>%
  # bind_rows(catalina) %>%
  bind_rows(serra_estrala) %>%
  # bind_rows(sierra) %>%
  bind_rows(boulder) %>%
  bind_rows(idaho) %>%
  bind_rows(valles) %>%
  bind_rows(NC_leafoff) %>%
  bind_rows(NC_leafon) %>%
  bind_rows(Mada) %>%
  bind_rows(Phili) %>%
  bind_rows(Aust)

# A few sites sometimes spit out Inf values, remove these values
dscore_data <- dscore_data %>% 
  filter(is.finite())

## Re-order site factors fro plotting later

# Change levels of factors
site_factors <- c("Phili", "Aust", "NMV", "NC on", "Mada", "NC off", "ID", "AZ", "PT", "CO")
habitat_factors <- c("Leaf on", "Leaf off", "Non-forest")

# Convert flags to factor
dscore_data <- dscore_data %>%
  mutate(habitat = factor(habitat, levels = habitat_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>%
  mutate(description = as.factor(description))

## ....2f. Calculate Standard deviation and error ########
dscore_data_naomit <- na.omit(dscore_data)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, micro, site, elev_diff) %>%
  dplyr::summarize(mean = mean(overlap)) 

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, site, micro) %>%
  dplyr::summarize(SD = sd(overlap)) %>%
  dplyr::left_join(dscore_data_sum)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, site, micro) %>%
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

dscore_data <- na.omit(dscore_data)


## ....2f. Control for elevation difference ########

## ....2f1. Determine data distribution and overlap ~ elev_diff relationship ###########

## Remove outliers
# Calculate mean
mean <- mean(dscore_data$overlap, na.rm = TRUE)

dscore_data_outOmit <- dscore_data %>%
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

## Check out overlap ~ elev_diff relationship
# ggplot(dscore_data_outOmit, aes(elev_diff, overlap)) +
#   geom_line() +
#   ylim(-5, 5)

# Just closed systems
# ggplot(filter(dscore_data_outOmit, habitat == "closed"), aes(elev_diff, overlap)) +
#   geom_jitter(stat = "identity") +
#   geom_line()

# Just open systems
# ggplot(filter(dscore_data_outOmit, habitat == "open"), aes(elev_diff, overlap)) +
#   geom_jitter(stat = "identity") +
#   geom_line()

## Now figure out a model that fits

# Calculate correlation
cor(dscore_data$elev_diff, dscore_data$overlap)
# -0.1617819

# Build linear model
linearMod <- lm(overlap ~ elev_diff, data = dscore_data)  # build linear regression model on full data
print(linearMod)

## ....2f2. Scale dscores down to slope of 0 #########
# citation: https://www.biostars.org/p/194388/

# Save as new object
dscore_data_lm <- dscore_data

dscore_data_lm$overlap <- linearMod$residuals

mean(dscore_data_lm$overlap) ## ~ zero as it should be
sd(dscore_data_lm$overlap)   ## ~ 1.6, which we should correct for

dscore_data_lm$overlap<- linearMod$residuals/sd(linearMod$residuals)

mean(dscore_data_lm$overlap) ## ~ zero as it should be
sd(dscore_data_lm$overlap)   ## now exactly 1

dscore_data <- dscore_data_lm

## ....2g. Calculate Standard deviation and error, again (for plots now) ########
dscore_data_naomit <- na.omit(dscore_data)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, micro, site, elev_diff) %>%
  dplyr::summarize(mean = mean(overlap))

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, site, micro) %>%
  dplyr::summarize(SD = sd(overlap)) %>%
  dplyr::left_join(dscore_data_sum)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(habitat, site, micro) %>%
  dplyr::summarize(SE = sd(overlap)/sqrt(length(overlap))) %>%
  dplyr::left_join(dscore_data_sum)
  
## 3. Create figure ###########

## ....3A. Boxplot separeted by micro and habitat, default boxplot uncertainty ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "Habitat")) +
  geom_boxplot(aes(x = reorder(micro, habitat, fun = mean), y = overlap, color = description),alpha = 0.7) +
  #guides(colour = guide_legend(override.aes = list(alpha = .7))) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(x = "Microhabitat", y = "Overlap")

## ....3B. Violin plot overlayed with mean and standard error....but no jitter ###########
ggplot(dscore_data, aes(x = description, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "Habitat")) +
  geom_violin(aes(x = reorder(micro, habitat, fun = mean), y = overlap, color = description), alpha = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  geom_errorbar(aes(x = reorder(micro, habitat, fun = mean), y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = dscore_data_sum, width = 0.2, color = "black") +
  labs(x = "Microhabitat", y = "Overlap")

## ....3C. Violin plots, 3x panels separeted by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5) +
  # new_scale_color() +
  # scale_fill_manual(values = c("#5f7ba8", "#5f7ba8", "#ed9d2d", "#b21798")) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +

  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, 
                color = "black") + labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), 
                    guide = guide_legend(title = "Habitat")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5) +
  # scale_color_viridis(discrete = TRUE, option = "C", 
  #                     guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
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
                 aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), 
                    guide = guide_legend(title = "Habitat")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C", 
                      guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
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

## ....3D. Boxplots, 3x panels separeted by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_boxplot(aes(x = site, y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_boxplot(aes(x = site, y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), aes(x = site, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "Habitat")) +
  geom_boxplot(aes(x = site, y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)) +
  labs(x = "Canopy")

lay <- rbind(c(1, 2, 3, 3))
grid.arrange(grobs = list(soil, surface, canopy), layout_matrix = lay)


plots <- grid.arrange(soil, surface, canopy,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots

## ....3E. Violin plots, grouped by habitat and by micro ###########

grouped_stats <- dscore_data %>%
  group_by(micro, habitat) %>%
  summarize(mean = mean(mean), SE = mean(SE)) %>%
  ungroup()

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = habitat, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = habitat, y = overlap, color = habitat), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  # geom_errorbar(aes(x = habitat, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(grouped_stats, micro == "soil"), width = 0.2, color = "black") +
  labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), aes(x = habitat, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = habitat, y = overlap, color = habitat), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-2.5, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # geom_errorbar(aes(x = habitat, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(dscore_data_sum, micro == "surface"), width = 0.2, color = "black") +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), aes(x = habitat, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "Habitat")) +
  geom_violin(aes(x = habitat, y = overlap, color = habitat), alpha = 0.7, lwd = 1.5) +
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
  # geom_errorbar(aes(x = habitat, y = mean, ymin = mean - SE , ymax = mean + SE), 
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


## ....3F. Plot overlap by elevation difference ########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = elev_diff, y = overlap, fill = factor(habitat))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = as.factor(elev_diff), y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_errorbar(aes(x = elev_diff, y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, color = "black") +
  labs(x = "Soil")

## ....3G. Boxplot with overlayed mean and standard error...but no jitter ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(habitat))) +
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
  filter(micro == "soil", habitat == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "soil", habitat == "Non-forest") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "soil", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))







dscore_data %>%
  filter(micro == "surface", habitat == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "surface", habitat == "Leaf off") %>%
  summarize(mean(overlap))


dscore_data %>%
  filter(micro == "canopy", habitat == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "canopy", habitat == "Leaf off") %>%
  summarize(mean(overlap))

## ....4B. Surface #####
# temp leaf on
dscore_data %>%
  filter(micro == "surface", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "surface", habitat == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "surface", habitat == "Non-forest") %>%
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
  filter(micro == "canopy", habitat == "Leaf off") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "canopy", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))
