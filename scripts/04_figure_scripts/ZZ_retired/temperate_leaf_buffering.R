# Klinges and Scheffers 2018
# This script is used to generate plots of temperate canopy temp x soil temp, and canopy temp x ground temp
# This is for the purpose of determining the buffering effect of leaf cover in the temperate zone
# This script is dependent on objects created in NC_main.R

require(tidyverse)
require(gridExtra)
require(grid)
require(lattice)

###### Data curation #########

## Create necessary dataset: independent columns for soil, ground and can temps
# 500, leafon
arrange(soil_500_leafon_minmax, desc(minmax), julian)
arrange(ground_500_leafon_minmax, desc(minmax), julian)
arrange(can_500_leafon_minmax, desc(minmax), julian)

all_500_leafon_minmax_wide <- all_500_leafon_minmax %>%
  select(julian, minmax)
all_500_leafon_minmax_wide <- all_500_leafon_minmax_wide %>%
  cbind(select(soil_500_leafon_minmax, temp)) %>%
  rename(soil_temp = temp)
all_500_leafon_minmax_wide <- all_500_leafon_minmax_wide %>%
  cbind(select(ground_500_leafon_minmax, temp)) %>%
  rename(ground_temp = temp)
all_500_leafon_minmax_wide <- all_500_leafon_minmax_wide %>%
  cbind(select(can_500_leafon_minmax, temp)) %>%
  rename(can_temp = temp)

# 1700, leafon
arrange(soil_1700_leafon_minmax, desc(minmax), julian)
arrange(ground_1700_leafon_minmax, desc(minmax), julian)
arrange(can_1700_leafon_minmax, desc(minmax), julian)

all_1700_leafon_minmax_wide <- all_1700_leafon_minmax %>%
  select(julian, minmax)
all_1700_leafon_minmax_wide <- all_1700_leafon_minmax_wide %>%
  cbind(select(soil_1700_leafon_minmax, temp)) %>%
  rename(soil_temp = temp)
all_1700_leafon_minmax_wide <- all_1700_leafon_minmax_wide %>%
  cbind(select(ground_1700_leafon_minmax, temp)) %>%
  rename(ground_temp = temp)
all_1700_leafon_minmax_wide <- all_1700_leafon_minmax_wide %>%
  cbind(select(can_1700_leafon_minmax, temp)) %>%
  rename(can_temp = temp)

# 500, leafoff
arrange(soil_500_leafoff_minmax, desc(minmax), julian)
arrange(ground_500_leafoff_minmax, desc(minmax), julian)
arrange(can_500_leafoff_minmax, desc(minmax), julian)

all_500_leafoff_minmax_wide <- all_500_leafoff_minmax %>%
  select(julian, minmax)
all_500_leafoff_minmax_wide <- all_500_leafoff_minmax_wide %>%
  cbind(select(soil_500_leafoff_minmax, temp)) %>%
  rename(soil_temp = temp)
all_500_leafoff_minmax_wide <- all_500_leafoff_minmax_wide %>%
  cbind(select(ground_500_leafoff_minmax, temp)) %>%
  rename(ground_temp = temp)
all_500_leafoff_minmax_wide <- all_500_leafoff_minmax_wide %>%
  cbind(select(can_500_leafoff_minmax, temp)) %>%
  rename(can_temp = temp)

# 1700, leafoff
arrange(soil_1700_leafoff_minmax, desc(minmax), julian)
arrange(ground_1700_leafoff_minmax, desc(minmax), julian)
arrange(can_1700_leafoff_minmax, desc(minmax), julian)

all_1700_leafoff_minmax_wide <- all_1700_leafoff_minmax %>%
  select(julian, minmax)
all_1700_leafoff_minmax_wide <- all_1700_leafoff_minmax_wide %>%
  cbind(select(soil_1700_leafoff_minmax, temp)) %>%
  rename(soil_temp = temp)
all_1700_leafoff_minmax_wide <- all_1700_leafoff_minmax_wide %>%
  cbind(select(ground_1700_leafoff_minmax, temp)) %>%
  rename(ground_temp = temp)
all_1700_leafoff_minmax_wide <- all_1700_leafoff_minmax_wide %>%
  cbind(select(can_1700_leafoff_minmax, temp)) %>%
  rename(can_temp = temp)


## Combine leafon and leafoff

all_500_leafon_minmax_wide <- all_500_leafon_minmax_wide %>%
  mutate(Foliage = "leafon")
all_1700_leafon_minmax_wide <- all_1700_leafon_minmax_wide %>%
  mutate(Foliage = "leafon")

all_500_leafoff_minmax_wide <- all_500_leafoff_minmax_wide %>%
  mutate(Foliage = "leafoff")
all_1700_leafoff_minmax_wide <- all_1700_leafoff_minmax_wide %>%
  mutate(Foliage = "leafoff")

all_500_minmax_wide <- rbind(all_500_leafon_minmax_wide, all_500_leafoff_minmax_wide)
all_1700_minmax_wide <- rbind(all_1700_leafon_minmax_wide, all_1700_leafoff_minmax_wide)


###### Simple difference in temp between canopy and soil/ground ######

# Leaf on
mean(all_500_leafon_minmax_wide$can_temp - all_500_leafon_minmax_wide$soil_temp)
mean(all_500_leafon_minmax_wide$can_temp - all_500_leafon_minmax_wide$ground_temp)
mean(all_1700_leafon_minmax_wide$can_temp - all_1700_leafon_minmax_wide$soil_temp)
mean(all_1700_leafon_minmax_wide$can_temp - all_1700_leafon_minmax_wide$ground_temp)

sd(all_500_leafon_minmax_wide$can_temp - all_500_leafon_minmax_wide$soil_temp)
sd(all_500_leafon_minmax_wide$can_temp - all_500_leafon_minmax_wide$ground_temp)
sd(all_1700_leafon_minmax_wide$can_temp - all_1700_leafon_minmax_wide$soil_temp)
sd(all_1700_leafon_minmax_wide$can_temp - all_1700_leafon_minmax_wide$ground_temp)

wilcox.test(all_500_leafon_minmax_wide$can_temp, all_500_leafon_minmax_wide$soil_temp, paired = TRUE)
wilcox.test(all_500_leafon_minmax_wide$can_temp, all_500_leafon_minmax_wide$ground_temp, paired = TRUE)
wilcox.test(all_1700_leafon_minmax_wide$can_temp, all_1700_leafon_minmax_wide$soil_temp, paired = TRUE)
wilcox.test(all_1700_leafon_minmax_wide$can_temp, all_1700_leafon_minmax_wide$ground_temp, paired = TRUE)

# Leaf off
mean(all_500_leafoff_minmax_wide$can_temp - all_500_leafoff_minmax_wide$soil_temp)
mean(all_500_leafoff_minmax_wide$can_temp - all_500_leafoff_minmax_wide$ground_temp)
mean(all_1700_leafoff_minmax_wide$can_temp - all_1700_leafoff_minmax_wide$soil_temp)
mean(all_1700_leafoff_minmax_wide$can_temp - all_1700_leafoff_minmax_wide$ground_temp)

sd(all_500_leafoff_minmax_wide$can_temp - all_500_leafoff_minmax_wide$soil_temp)
sd(all_500_leafoff_minmax_wide$can_temp - all_500_leafoff_minmax_wide$ground_temp)
sd(all_1700_leafoff_minmax_wide$can_temp - all_1700_leafoff_minmax_wide$soil_temp)
sd(all_1700_leafoff_minmax_wide$can_temp - all_1700_leafoff_minmax_wide$ground_temp)

wilcox.test(all_500_leafoff_minmax_wide$can_temp, all_500_leafoff_minmax_wide$soil_temp, paired = TRUE)
wilcox.test(all_500_leafoff_minmax_wide$can_temp, all_500_leafoff_minmax_wide$ground_temp, paired = TRUE)
wilcox.test(all_1700_leafoff_minmax_wide$can_temp, all_1700_leafoff_minmax_wide$soil_temp, paired = TRUE)
wilcox.test(all_1700_leafoff_minmax_wide$can_temp, all_1700_leafoff_minmax_wide$ground_temp, paired = TRUE)

###### Create canopy x soil/ground plots, just leaf-on ######

# Create 1:1 line dataframe
x <- c(0, 40)
y <- c(0, 40)
line <- as.data.frame(cbind(x, y))

# 500, canopy x soil

plot_500_leafon_cansoil <- ggplot(data = all_500_leafon_minmax_wide, aes(x = can_temp, y = soil_temp)) + 
  geom_point() + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Soil Temperature") +
  ggtitle("Canopy x Soil, Lowlands") +
  theme(text = element_text(size=12))
plot_500_leafon_cansoil

# 500, canopy x ground

plot_500_leafon_canground <- ggplot(data = all_500_leafon_minmax_wide, aes(x = can_temp, y = ground_temp)) + 
  geom_point() + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Ground Temperature") +
  ggtitle("Canopy x Ground, Lowlands") +
  theme(text = element_text(size=12))
plot_500_leafon_canground


# 1700, canopy x soil

plot_1700_leafon_cansoil <- ggplot(data = all_1700_leafon_minmax_wide, aes(x = can_temp, y = soil_temp)) + 
  geom_point() + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Soil Temperature") +
  ggtitle("Canopy x Soil, Uplands") +
  theme(text = element_text(size=12))
plot_1700_leafon_cansoil

# 1700, canopy x ground

plot_1700_leafon_canground <- ggplot(data = all_1700_leafon_minmax_wide, aes(x = can_temp, y = ground_temp)) + 
  geom_point() + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Ground Temperature") +
  ggtitle("Canopy x Ground, Uplands") +
  theme(text = element_text(size=12))
plot_1700_leafon_canground

###### Create canopy x soil/ground plots, full year ######


all_500_minmax_wide$Foliage <- as.factor(all_500_minmax_wide$Foliage)

# 500, canopy x soil

plot_500_cansoil <- ggplot(data = all_500_minmax_wide, aes(x = can_temp, y = soil_temp)) + 
  geom_point(aes(color = Foliage), alpha = 0.5) + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Soil Temperature") +
  ggtitle("C. Canopy x Soil, Lowlands") +
  theme(text = element_text(size=16)) +
  theme(plot.title = element_text(size=20))
# Add statistics
leafon_grob <- grobTree(textGrob("P < 0.0001", x = .4, y = .65, hjust = 0,
                                 gp = gpar(col = "#33cbce", fontsize = 14, fontface = "italic")))
leafoff_grob <- grobTree(textGrob("P = 0.07034", x = .4, y = .15, hjust = 0,
                                  gp = gpar(col = "#f2752e", fontsize = 14, fontface = "italic")))
plot_500_cansoil <- plot_500_cansoil + annotation_custom(leafon_grob)+ annotation_custom(leafoff_grob)
plot_500_cansoil

# 500, canopy x ground

plot_500_canground <- ggplot(data = all_500_minmax_wide, aes(x = can_temp, y = ground_temp)) + 
  geom_point(aes(color = Foliage), alpha = 0.5) + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Ground Temperature") +
  ggtitle("D. Canopy x Ground, Lowlands") +
  theme(text = element_text(size=16)) +
  theme(plot.title = element_text(size=20))
# Add statistics
leafon_grob <- grobTree(textGrob("P < 0.0001", x = .7, y = .4, hjust = 0,
                                 gp = gpar(col = "#33cbce", fontsize = 14, fontface = "italic")))
leafoff_grob <- grobTree(textGrob("P = 0.3087", x = .4, y = .15, hjust = 0,
                                  gp = gpar(col = "#f2752e", fontsize = 14, fontface = "italic")))
plot_500_canground <- plot_500_canground + annotation_custom(leafon_grob)+ annotation_custom(leafoff_grob)
plot_500_canground

# 1700, canopy x soil

plot_1700_cansoil <- ggplot(data = all_1700_minmax_wide, aes(x = can_temp, y = soil_temp)) + 
  geom_point(aes(color = Foliage), alpha = 0.5) + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Soil Temperature") +
  ggtitle("A. Canopy x Soil, Uplands") +
  theme(text = element_text(size=16)) +
  theme(plot.title = element_text(size=20))
# Add statistics
leafon_grob <- grobTree(textGrob("P < 0.0001", x = .68, y = .4, hjust = 0,
                                 gp = gpar(col = "#33cbce", fontsize = 14, fontface = "italic")))
leafoff_grob <- grobTree(textGrob("P = 0.8822", x = .5, y = .15, hjust = 0,
                                  gp = gpar(col = "#f2752e", fontsize = 14, fontface = "italic")))
plot_1700_cansoil <- plot_1700_cansoil + annotation_custom(leafon_grob)+ annotation_custom(leafoff_grob)
plot_1700_cansoil

# 1700, canopy x ground

plot_1700_canground <- ggplot(data = all_1700_minmax_wide, aes(x = can_temp, y = ground_temp)) + 
  geom_point(aes(color = Foliage), alpha = 0.5) + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Ground Temperature") +
  ggtitle("B. Canopy x Ground, Uplands") +
  theme(text = element_text(size=16)) +
  theme(plot.title = element_text(size=20))
# Add statistics
leafon_grob <- grobTree(textGrob("P < 0.0001", x = .6, y = .4, hjust = 0,
                                 gp = gpar(col = "#33cbce", fontsize = 14, fontface = "italic")))
leafoff_grob <- grobTree(textGrob("P = 0.008319", x = .34, y = .15, hjust = 0,
                                  gp = gpar(col = "#f2752e", fontsize = 14, fontface = "italic")))
plot_1700_canground <- plot_1700_canground + annotation_custom(leafon_grob)+ annotation_custom(leafoff_grob)
plot_1700_canground


###### Arrange plots ######

grid.arrange(plot_1700_cansoil, plot_1700_canground, plot_500_cansoil, plot_500_canground)


p1 <- plot_1700_cansoil
p2 <- plot_1700_canground
p3 <- plot_500_cansoil
p4 <- plot_500_canground
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 2,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        nrow = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        nrow = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

grid_arrange_shared_legend(p1, p2, p3, p4)

