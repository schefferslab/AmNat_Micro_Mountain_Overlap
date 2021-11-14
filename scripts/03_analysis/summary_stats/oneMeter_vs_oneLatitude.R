
library(tidyverse)
library(ggpmisc)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

forest_mountains <- mountains %>% 
  filter(macro %in% c("tropical broadleaf", "Ponderosa pine", "deciduous", 
                      "Dense coniferous", "degraded tropical broadleaf"))

nonforest_mountains <- mountains %>% 
  filter(macro %in% c("alpine meadow", "hot desert", "scrub shrub", 
                      "developed", "meadow near forest"))

height <- lm(TAD_elevCorr~ height, data = mountains)


ggplot(mountains, aes(height, TAD_elevCorr)) +
  geom_point() +
  geom_smooth(method = "glm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5)

# y ~ .317x - 0.965


ggplot(nonforest_mountains, aes(abs(latitude), TAD_elevCorr)) +
  geom_point() +
  geom_smooth(method = "glm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5)

# y ~ .00511x - 1.97

## Solve for x

# Height differential
# 0 meter = -1.01
# 1 meter = -0.683
# Difference = 0.327
# A change in one meter equals an increase of 0.327 C in overlap

# y ~ .0622(0) - 1.82 -> -1.82 # 0 degrees latitude
# Add 0.327 to -1.82 => -1.493
# -1.493 ~ .0622(x) - 1.82
# x = 5.257235 degrees latitude

# A change in 5.26 degrees latitude equals an increase of 0.327 C in overlap

# Therefore, a change in one meter of height corresponds to a change of 5.26 degrees latitude





