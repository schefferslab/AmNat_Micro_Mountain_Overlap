# David Klinges
# 2018/07/11
# This script creates the overlap d score boxplots for leaf-off, leaf-on, and tropical
# This is a fragment of code from North Carolina Overlap Script

### You're going to need to combine the three soils, then combine the three grounds, etc.

library(tidyverse)

####### Combine d-score datasets #########

interm_soil_leafoff_d_score <- soil_leafoff_d_score %>%
  mutate(micro = "soil")

interm_ground_leafoff_d_score <- ground_leafoff_d_score %>%
  mutate(micro = "ground")

interm_can_leafoff_d_score <- can_leafoff_d_score %>%
  mutate(micro = "canopy")

leafoff_d_score <- rbind(interm_soil_leafoff_d_score, interm_ground_leafoff_d_score, interm_can_leafoff_d_score)
leafoff_d_score <- leafoff_d_score %>%
  mutate(foliage = "leafoff")

interm_soil_leafon_d_score <- soil_leafon_d_score %>%
  mutate(micro = "soil")

interm_ground_leafon_d_score <- ground_leafon_d_score %>%
  mutate(micro = "ground")

interm_can_leafon_d_score <- can_leafon_d_score %>%
  mutate(micro = "canopy")

leafon_d_score <- rbind(interm_soil_leafon_d_score, interm_ground_leafon_d_score, interm_can_leafon_d_score)
leafon_d_score <- leafon_d_score %>%
  mutate(foliage = "leafon")

NC_d_score <- rbind(leafoff_d_score, leafon_d_score)


####### Reconstruct datasets: group by microhabitat #########

trop_temp_soil_d_score <- rbind(trop_soil_d_score, soil_leafoff_d_score, soil_leafon_d_score)

####### Create figure panel layout ########
dev.off()

par(mfrow = c(1, 2),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(4, 4, 1, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 
#c(bottom, left, top, right)
par(mfrow = c(1, 2))



####### Create soil panel ########

soil_boxplot <- ggplot() +
  geom_boxplot(data = soil_leafoff_d_score, aes(micro, dscore, fill = micro, alpha = 0.5)) +
  scale_fill_manual(breaks = c("soil", "ground", "canopy"), 
                    values=c("purple", "green", "red")) +

# Re-order microhabitats to be ascending height
leafoff_d_score$micro <- factor(leafoff_d_score$micro,
                                levels = c('soil','ground', 'canopy'),ordered = TRUE)
leafon_d_score$micro <- factor(leafon_d_score$micro,
                                levels = c('soil','ground', 'canopy'),ordered = TRUE)


leafoff_boxplot <- ggplot() +
    geom_boxplot(data = leafoff_d_score, aes(micro, dscore, fill = micro, alpha = 0.5)) +
  scale_fill_manual(breaks = c("soil", "ground", "canopy"), 
                    values=c("purple", "green", "red")) +
  
                  #ggplot(leafon_d_score, aes(micro, dscore, fill = micro, alpha = 0.5)) +
  geom_boxplot(data = leafon_d_score, aes(micro, dscore, fill = micro, alpha = 0.5)) +
  scale_fill_manual(breaks = c("soil", "ground", "canopy"), 
                    values=c("purple", "green", "red")) +
  ylim(-25, 2) +
  theme_classic()
leafoff_boxplot

boxplot(NA, xlim=c(1,2.5), ylim=c(-25,2),frame=F, ylab="Overlap (D Score)", xlab="", cex.axis=1.5, cex.lab=1.5)



with(play_500_soil[(play_500_soil$julian < 150) | (play_500_soil$julian  > 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F, add=T, at=1.5, col = adjustcolor("purple", alpha=.2), axes=FALSE))
with(play_500_soil[(play_500_soil$julian > 150) & (play_500_soil$julian  < 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=2, col = adjustcolor("purple", alpha=.2), axes=FALSE))
mtext("Soil", cex=1.3)

#ground / canopy 
boxplot(NA, xlim=c(1,4), ylim=c(-2,2),frame=F, ylab="", xlab="", cex.axis=1.5, cex.lab=1.5)
with(play_500_grnd[(play_500_grnd$julian < 150) | (play_500_grnd$julian  > 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,add=T, at=1.5, col = adjustcolor("light green", alpha=.4), axes=FALSE))
with(play_500_can[(play_500_can$julian < 150) | (play_500_can$julian  > 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=2, col = adjustcolor("red", alpha=.2), axes=FALSE))
mtext("Ground       Canopy", cex=1.3)
with(play_500_grnd[(play_500_grnd$julian > 150) & (play_500_grnd$julian  < 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE,add=T, at=3, col = adjustcolor("light green", alpha=.4),axes=FALSE))
with(play_500_can[(play_500_can$julian > 150) & (play_500_can$julian  < 280),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=3.5, col = adjustcolor("red", alpha=.2),axes=FALSE))

legend("bottomright", c("Soil", "Ground", "Canopy"), bty="n", lwd=5, lty=1, col=adjustcolor(c("purple","light green", "red"), alpha=.2))
abline(h=0)
mtext(text="Microhabitat (Leaf-off / Leaf-on)",side=1,line=-2,outer=TRUE, cex=1.5)

