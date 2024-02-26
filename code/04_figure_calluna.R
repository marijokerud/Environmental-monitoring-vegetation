library(ggplot2)


cbbPalette <- c("#018C95", "#E57201", "#93328E", "#7A9A01") #lilla = #93328E, turkis = #018C95, grønn = #7A9A01, oransje = #E57201

callunaplott<- 
ggplot(callunaLM, aes(x=Year, y=measurement, group=nutrient, colour=nutrient)) +
  facet_wrap(nutrient ~ ., scales = "free") +
  stat_smooth(method="lm", se=TRUE) +
  ylab("mg/100g") +
  xlab("År") +
  scale_colour_manual(values=cbbPalette) +
  theme_bw() +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 16),
        axis.title.x = element_text(size=12,hjust=0.5),
        axis.title.y = element_text(size=12,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text( color="black", size=18),
        legend.text = element_text( color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank()) #Hide all the vertical gridlines


png(filename = "output/callunaplot.png", width = 850, height = 450, units = "px", res = 96)   # or tiff("plot.tiff")
plot(callunaplott)
dev.off()
