# David Klinges
# 2018/07/11
# This script creates the temperate variance for leaf-off/leaf-on figure (what is Fig. 2 as of 2018/07/11)
# This is a fragment of code from North Carolina Overlap Script
# Also depends on objects made in NC_main.R

require(lubridate)

par(mfrow = c(1, 3),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

####### Soil panel ##########

# Create figure axes
plot(play_500_soil$julian, play_500_soil$Temp_max, type='n', bty="n", cex.axis=1.5, ylim=c(-20, 40), xlim=c(0,365), ylab='Temperature (C)', xlab='Julian Day')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"), bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(play_500_soil$julian, play_500_soil$Temp_min, col = 'grey')
lines(play_500_soil$julian, play_500_soil$Temp_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(play_500_soil$julian, rev(play_500_soil$julian)), c(play_500_soil$Temp_max, rev(play_500_soil$Temp_min)), col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(play_1700_soil$julian, play_1700_soil$Temp_min, col = 'grey')
lines(play_1700_soil$julian, play_1700_soil$Temp_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(play_1700_soil$julian, rev(play_1700_soil$julian)), c(play_1700_soil$Temp_max, rev(play_1700_soil$Temp_min)), col = adjustcolor("blue", alpha=.5), border = NA)

mtext("Soil", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw

####### Ground panel ########

# Create figure axes
plot(play_500_grnd$julian, play_500_grnd$Temp_max, axes=FALSE,type='n', bty="n", cex.axis=1.5, ylim=c(-20, 40), xlim=c(0,365), ylab="", xlab='Julian Day')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"), bty="n", lwd=10, lty=1, col=adjustcolor(c("light green","dark green"), alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(play_500_grnd$julian, play_500_grnd$Temp_min, col = 'grey')
lines(play_500_grnd$julian, play_500_grnd$Temp_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(play_500_grnd$julian, rev(play_500_grnd$julian)), c(play_500_grnd$Temp_max, rev(play_500_grnd$Temp_min)), col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for min and max temp
lines(play_1700_grnd$julian, play_1700_grnd$Temp_min, col = 'grey')
lines(play_1700_grnd$julian, play_1700_grnd$Temp_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(play_1700_grnd$julian, rev(play_1700_grnd$julian)), c(play_1700_grnd$Temp_max, rev(play_1700_grnd$Temp_min)), col = adjustcolor("dark green", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)
mtext("Ground", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

ground_plot <- recordPlot()
#plot.new() ## clean up device
#ground_plot # redraw

####### Canopy panel #########

# Create figure axes
plot(play_500_can$julian, play_500_can$Temp_max, axes=FALSE, type='n', bty="n", cex.axis=1.5,ylim=c(-20,40), xlim=c(0,365), ylab="", xlab='Julian Day')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"), bty="n", lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(play_500_can$julian, play_500_can$Temp_min, col = 'grey')
lines(play_500_can$julian, play_500_can$Temp_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(play_500_can$julian, rev(play_500_can$julian)), c(play_500_can$Temp_max, rev(play_500_can$Temp_min)), col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(play_1700_can$julian, play_1700_can$Temp_min, col = 'grey')
lines(play_1700_can$julian, play_1700_can$Temp_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(play_1700_can$julian, rev(play_1700_can$julian)), c(play_1700_can$Temp_max, rev(play_1700_can$Temp_min)), col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("Canopy", NORTH<-3, cex=1.5,col="black")

canopy_plot <- recordPlot()
#plot.new() ## clean up device
#canopy_plot # redraw

####### Titles ########

#dev.off()
#plot.new()
mtext("Temperature", NORTH<-2, line=1.5, adj=.5,cex=1.5,
      col="black", outer=TRUE)

mtext("Julian Day",
      NORTH<-1, line=1.5, adj=.5, cex=1.2, col="Black", outer=TRUE)

title_plot <- recordPlot()
#plot.new() ## clean up device
#title_plot # redraw

png(filename="/Users/David/Desktop/Career Stuff/Mountain Passes/Figures/NC_seasonal_variance_figure.png", width=24, height=12, units="cm", res=300)
dev.copy(png, "NC_seasonal_variance_figure.png")
plot(title_plot)
dev.off()

#
#
#
#


####### Janzen-method Monthly panels #######

par(mfrow = c(1, 3),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

####### Monthly Soil panel #######

# Create figure axes
plot(monthly_soil_500$month, monthly_soil_500$monthly_mean_max, type='n',
     bty="n", cex.axis=1.5, ylim=c(-20, 40), xlim=c(0,12), ylab='Temperature (C)', xlab='Month')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"),
       bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","blue"), alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(monthly_soil_500$month, monthly_soil_500$monthly_mean_min, col = 'grey')
lines(monthly_soil_500$month, monthly_soil_500$monthly_mean_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly_soil_500$month, rev(monthly_soil_500$month)),
        c(monthly_soil_500$monthly_mean_max, rev(monthly_soil_500$monthly_mean_min)),
        col = adjustcolor("purple", alpha=.5), border = NA)

# Uplands
# Plot lines for uplands min and max temp
lines(monthly_soil_1700$month, monthly_soil_1700$monthly_mean_min, col = 'grey')
lines(monthly_soil_1700$month, monthly_soil_1700$monthly_mean_max, col = 'grey')

#create filled polygon in between the lines
polygon(c(monthly_soil_1700$month, rev(monthly_soil_1700$month)),
        c(monthly_soil_1700$monthly_mean_max, rev(monthly_soil_1700$monthly_mean_min)),
        col = adjustcolor("blue", alpha=.5), border = NA)
mtext("Soil", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

soil_plot <- recordPlot()
#plot.new() ## clean up device
#soil_plot # redraw



####### Monthly Ground panel ########

# Create figure axes
plot(monthly_ground_500$month, monthly_ground_500$monthly_mean_max, axes=FALSE,type='n', 
     bty="n", cex.axis=1.5, ylim=c(-20, 40), xlim=c(0,12), ylab="", xlab='Month')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"), 
       bty="n", lwd=10, lty=1, col=adjustcolor(c("light green","dark green"), 
                                               alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(monthly_ground_500$month, monthly_ground_500$monthly_mean_min, col = 'grey')
lines(monthly_ground_500$month, monthly_ground_500$monthly_mean_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly_ground_500$month, rev(monthly_ground_500$month)), 
        c(monthly_ground_500$monthly_mean_max, rev(monthly_ground_500$monthly_mean_min)), 
        col = adjustcolor("light green", alpha=.5), border = NA)

# Uplands
# Plot lines for min and max temp
lines(monthly_ground_1700$month, monthly_ground_1700$monthly_mean_min, col = 'grey')
lines(monthly_ground_1700$month, monthly_ground_1700$monthly_mean_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly_ground_1700$month, rev(monthly_ground_1700$month)), 
        c(monthly_ground_1700$monthly_mean_max, rev(monthly_ground_1700$monthly_mean_min)), 
        col = adjustcolor("dark green", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)
mtext("Ground", NORTH<-3, cex=1.5,col="black")

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

ground_plot <- recordPlot()
#plot.new() ## clean up device
#ground_plot # redraw


####### Monthly Canopy panel #########

# Create figure axes
plot(monthly_can_500$month, monthly_can_500$monthly_mean_max, axes=FALSE, type='n', 
     bty="n", cex.axis=1.5,ylim=c(-20,40), xlim=c(0,12), ylab="", xlab='Month')

# Create figure legend
legend("bottomleft", c("Lowlands (500 m asl)", "Uplands (1700 m asl)"), bty="n", 
       lwd=10, lty=1, col=adjustcolor(c("red","dark red"), alpha=.5), cex=1.2)

# Lowlands
# Plot lines for lowlands min and max temp
lines(monthly_can_500$month, monthly_can_500$monthly_mean_min, col = 'grey')
lines(monthly_can_500$month, monthly_can_500$monthly_mean_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly_can_500$month, rev(monthly_can_500$month)), 
        c(monthly_can_500$monthly_mean_max, rev(monthly_can_500$monthly_mean_min)), 
        col = adjustcolor("red", alpha=.5), border = NA)


# Uplands
# Plot lines for uplands min and max temp
lines(monthly_can_1700$month, monthly_can_1700$monthly_mean_min, col = 'grey')
lines(monthly_can_1700$month, monthly_can_1700$monthly_mean_max, col = 'grey')

# Create a polygon to fill in between the lines
polygon(c(monthly_can_1700$month, rev(monthly_can_1700$month)), 
        c(monthly_can_1700$monthly_mean_max, rev(monthly_can_1700$monthly_mean_min)), 
        col = adjustcolor("dark red", alpha=.5), border = NA)

Axis(side=1, xlim=c(0, 365), xlab="",cex.axis=1.5)

segments(150,-10, 150, 40, lty=2)
segments(281,-10, 281, 40, lty=2)

mtext("Canopy", NORTH<-3, cex=1.5,col="black")

canopy_plot <- recordPlot()
#plot.new() ## clean up device
#canopy_plot # redraw

######################CREATE OVERLAP D-SCORE FIGURE######################
#overlap figure
png(filename="/Volumes/PHOTOS/Career Stuff/Mountain Passes/NC_overlap_Dscore_julian.png", width=24, height=12, units="cm", res=300)

#par(mfrow=c(1,3))
#par(mfrow = c(1,2), mar=c(1, 4, 1, 1) + 0.1)
par(mfrow = c(1, 6),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

plot(play_500_soil$julian,play_500_soil$overlap, type="l", ylim=c(-10, 2),cex.axis=1.5)
abline(h=0)
plot(play_500_grnd$julian,play_500_grnd$overlap, type="l",ylim=c(-10, 2),cex.axis=1.5 )
abline(h=0)
plot(play_500_can$julian,play_500_can$overlap, type="l",ylim=c(-10, 2),cex.axis=1.5 )
abline(h=0)

plot(1, type="n", axes=F, xlab="", ylab="")

plot(play_500_grnd$julian,play_500_grnd$overlap, type="l",ylim=c(-2, 2) )
abline(h=0)
plot(play_500_can$julian,play_500_can$overlap, type="l",ylim=c(-2, 2) )
abline(h=0)


mtext("Overlap (D score)", NORTH<-2, line=1.5, adj=.5,cex=1.5,
      col="black", outer=TRUE)

mtext("Julian Day",
      NORTH<-1, line=1.5, adj=.5, cex=1.2, col="Black", outer=TRUE)


dev.off()


############################OVERLAP FIGURE V2################
# 1700 m maximum minus 500 m minimum 
# negative indicates no overlap, positive indiciates overlap between uplands and lowlands

png(filename="/Volumes/PHOTOS/Career Stuff/Mountain Passes/NC_MaxMinoverlap_julian_v3.png", width=24, height=12, units="cm", res=300)
#par(mfrow=c(1,3))
#par(mfrow = c(1,2), mar=c(1, 4, 1, 1) + 0.1)
par(mfrow = c(1, 3),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 2, 1, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 

plot(play_500_soil$julian,play_500_soil$overlap_v2, type="l", ylim=c(-10, 20),cex.axis=1.5)
abline(h=0)
plot(play_500_grnd$julian,play_500_grnd$overlap_v2, type="l",ylim=c(-10, 20),cex.axis=1.5 )
abline(h=0)
plot(play_500_can$julian,play_500_can$overlap_v2, type="l",ylim=c(-10, 20),cex.axis=1.5 )
abline(h=0)

#plot(1, type="n", axes=F, xlab="", ylab="")

#plot(play_500_grnd$julian,play_500_grnd$overlap_v2, type="l",ylim=c(-5, 20) )
#abline(h=0)
#plot(play_500_can$julian,play_500_can$overlap_v2, type="l",ylim=c(-5, 20) )
#abline(h=0)


mtext("Overlap (1700 m Tmax - 500 m Tmin)", NORTH<-2, line=1.5, adj=.5,cex=1.5,
      col="black", outer=TRUE)
mtext("Julian Day",
      NORTH<-1, line=1.5, adj=.5, cex=1.2, col="Black", outer=TRUE)

dev.off()

#########################PLOT OVERLAP BOXPLOTS#####################
#plot boxplots for subset of data based on julian date
#overlap D score

png(filename="/Volumes/PHOTOS/Career Stuff/Mountain Passes/Overlap_DScore_NC_boxplot_leaf_ONOFF.png", width=20, height=16, units="cm", res=300)
par(mfrow = c(1, 2),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(4, 4, 1, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 
#c(bottom, left, top, right)
par(mfrow = c(1, 2))
#soil
boxplot(NA, xlim=c(1,2.5), ylim=c(-25,2),frame=F, ylab="Overlap (D Score)", xlab="", cex.axis=1.5, cex.lab=1.5)
with(play_500_soil[(play_500_soil$julian < 150) | (play_500_soil$julian  > 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F, add=T, at=1.5, col = adjustcolor("purple", alpha=.5), axes=FALSE))
with(play_500_soil[(play_500_soil$julian > 150) & (play_500_soil$julian  < 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=2, col = adjustcolor("purple", alpha=.5), axes=FALSE))
mtext("Soil", cex=1.3)

#ground / canopy 
boxplot(NA, xlim=c(1,4), ylim=c(-2,2),frame=F, ylab="", xlab="", cex.axis=1.5, cex.lab=1.5)
with(play_500_grnd[(play_500_grnd$julian < 150) | (play_500_grnd$julian  > 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,add=T, at=1.5, col = adjustcolor("light green", alpha=.5), axes=FALSE))
with(play_500_can[(play_500_can$julian < 150) | (play_500_can$julian  > 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=2, col = adjustcolor("red", alpha=.5), axes=FALSE))
mtext("Ground       Canopy", cex=1.3)
with(play_500_grnd[(play_500_grnd$julian > 150) & (play_500_grnd$julian  < 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE,add=T, at=3, col = adjustcolor("light green", alpha=.5),axes=FALSE))
with(play_500_can[(play_500_can$julian > 150) & (play_500_can$julian  < 281),], boxplot(overlap,type="l", ylim=c(-8,2),frame=F,axes=FALSE, add=T, at=3.5, col = adjustcolor("red", alpha=.5),axes=FALSE))

legend("bottomright", c("Soil", "Ground", "Canopy"), bty="n", lwd=5, lty=1, col=adjustcolor(c("purple","light green", "red"), alpha=.2))
abline(h=0)
mtext(text="Microhabitat (Leaf-off / Leaf-on)",side=1,line=-2,outer=TRUE, cex=1.5)

dev.off()

################################PLOT BOXPLOTS################
#overlap max minus min

png(filename="/Volumes/PHOTOS/Career Stuff/Mountain Passes/Overlap_NC_boxplot_leaf_ONOFF.png", width=16, height=16, units="cm", res=300)
par(mfrow = c(1, 1),     # 2x2 layout
    oma = c(4, 4, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(4, 4, 1, 1), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row) 
#c(bottom, left, top, right)

#without leaves
boxplot(NA, xlim=c(1,5), ylim=c(-8,20),frame=F, ylab="Overlap (1700 m Tmax - 500 m Tmin)", xlab="Leaf-off            Leaf-on", cex.axis=1.5, cex.lab=1.5)
with(play_500_soil[(play_500_soil$julian < 150) | (play_500_soil$julian  > 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F, add=T, at=1.5, col = adjustcolor("purple", alpha=.2), axes=FALSE))
with(play_500_grnd[(play_500_grnd$julian < 150) | (play_500_grnd$julian  > 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F,add=T, at=2, col = adjustcolor("light green", alpha=.4), axes=FALSE))
with(play_500_can[(play_500_can$julian < 150) | (play_500_can$julian  > 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F,axes=FALSE, add=T, at=2.5, col = adjustcolor("red", alpha=.2), axes=FALSE))

#with leaves 
with(play_500_soil[(play_500_soil$julian > 150) & (play_500_soil$julian  < 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F,axes=FALSE, add=T, at=3.5, col = adjustcolor("purple", alpha=.2), axes=FALSE))
with(play_500_grnd[(play_500_grnd$julian > 150) & (play_500_grnd$julian  < 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F,axes=FALSE,add=T, at=4, col = adjustcolor("light green", alpha=.4),axes=FALSE))
with(play_500_can[(play_500_can$julian > 150) & (play_500_can$julian  < 281),], boxplot(overlap_v2,type="l", ylim=c(-8,20),frame=F,axes=FALSE, add=T, at=4.5, col = adjustcolor("red", alpha=.2),axes=FALSE))

legend("topright", c("Soil", "Ground", "Canopy"), bty="n", lwd=10, lty=1, col=adjustcolor(c("purple","light green", "red"), alpha=.2), cex=1.5)
abline(h=0)
dev.off()

