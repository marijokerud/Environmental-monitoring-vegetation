library(ggplot2)

### POLYNOMIAL REGRESSION PLOT
regression_plot1 <-
ggplot(site.scores, aes(x= year3, y=NMDS1)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x, se=TRUE, col="black", fill="#008C95", lwd=1.2) +
  labs(x = "Analyseår etter 1995", y= "NMDS1 skår") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) 

#save as 575x650
png(filename = "output/regression_plot_vegetation1.png", width = 850, height = 850, units = "px", res = 96)   # or tiff("plot.tiff")
plot(regression_plot1)
dev.off()

regression_plot2 <-
  ggplot(site.scores, aes(x= year3, y=NMDS2)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x, se=TRUE, col="black", fill="#008C95", lwd=1.2) +
  labs(x = "Analyseår etter 1995", y= "NMDS2 skår") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) 

#save as 575x650
png(filename = "output/regression_plot_vegetation2.png", width = 900, height = 900, units = "px", res = 200)   # or tiff("plot.tiff")
plot(regression_plot2)
dev.off()

### NMDS PLOT
#Make polygon values
plott.K1 <- site.scores[site.scores$felt_id == "K1", ][chull(site.scores[site.scores$felt_id == "K1", c("NMDS1", "NMDS2")]), ]  # hull values for plott O1
plott.K2 <- site.scores[site.scores$felt_id == "K2", ][chull(site.scores[site.scores$felt_id == "K2", c("NMDS1", "NMDS2")]), ]  # hull values for plott O1
plott.K3 <- site.scores[site.scores$felt_id == "K3", ][chull(site.scores[site.scores$felt_id == "K3", c("NMDS1", "NMDS2")]), ]  # hull values for plott O2
plott.K4 <- site.scores[site.scores$felt_id == "K4", ][chull(site.scores[site.scores$felt_id == "K4", c("NMDS1", "NMDS2")]), ]  # hull values for plott M1
plott.K5 <- site.scores[site.scores$felt_id == "K5", ][chull(site.scores[site.scores$felt_id == "K5", c("NMDS1", "NMDS2")]), ]  # hull values for plott M2


hull.data <- rbind(plott.K1, plott.K2, plott.K3, plott.K4,plott.K5)  #combine plott.a and plott.b
hull.data <- hull.data 

shapes <- c(1,1,1,1,1) #Tue
NINAcolor <- c("#008C95", "#93328E", "#7A9A01", "#E57200", "#980043") # "#004F71"
opacity <- c(.6, .6, .6, .6, .6)

artssammensetning <-
  ggplot() + 
  geom_polygon(data=hull.data, aes(x=NMDS1, y=NMDS2, fill=felt_id, group=felt_id, alpha=felt_id)) + # add the convex hulls
  geom_text(data=species.scores, aes(x=NMDS1, y=NMDS2, label=norwegian), alpha=0.5, size=3.5, position=position_jitter(height=.12) , check_overlap = TRUE) +  # add the species labels , check_overlap = TRUE
  geom_text(data=hull.data, aes(x=NMDS1, y=NMDS2, label = year2), nudge_x = .06 ) +
  geom_point(data=site.scores, aes(x=NMDS1, y=NMDS2, shape=felt_id, colour=felt_id), size=4) + # add the point markers
  scale_fill_manual(values = NINAcolor, name  = "") +
  scale_alpha_manual(values = opacity, name  = "") +
  scale_shape_manual(name  = "", values=shapes) +
  scale_colour_manual(values = NINAcolor, name  = "") +
  theme_bw() +
  theme(legend.position="top",
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18)) + # remove y-axis labels
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank())

#save as 800x650 or 1240x1090

png(filename = "output/artssammensetning.png", width = 1240, height = 1090, units = "px", res = 200)   # or tiff("plot.tiff")
plot(artssammensetning)
dev.off()
