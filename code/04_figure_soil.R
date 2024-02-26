library(ggplot2)

elementPLOT <- soilLM %>% 
  filter(element %in% c("Kalium", "Kalsium", "Magnesium", "Natrium"))

nutrientPLOT <- soilLM %>% 
  filter(element %in% c("pH", "Karbon", "Nitrogen", "CN", "LOI"))


cbbPalette <- c("#018C95", "#E57201", "#93328E", "#7A9A01") #lilla = #93328E, turkis = #018C95, grønn = #7A9A01, oransje = #E57201

mineralplott <-
ggplot(elementPLOT, aes(x=Year, y=measurement, group=element, colour=element)) +
  facet_wrap(element ~ ., scales = "free") +
  stat_smooth(method="lm", se=TRUE) +
  ylab("mg/100g") +
  xlab("År") +
  #scale_x_continuous(breaks=c(2006, 2008, 2013, 2018, 2021)) +
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

png(filename = "output/mineralplot.png", width = 850, height = 650, units = "px", res = 96)   # or tiff("plot.tiff")
plot(mineralplott)
dev.off()

cbbPalette <- c("#018C95", "#E57201", "#93328E", "#7A9A01", "#980043")


nutrientplott <-
ggplot(nutrientPLOT, aes(x=Year, y=measurement, group=element, colour=element)) +
  facet_wrap(element ~ ., scales = "free", ncol = 2) +
  stat_smooth(method="lm", se=TRUE) +
  ylab("mg/100g") +
  xlab("År") +
  #scale_x_continuous(labels=c("0" = "1995", "3" = "1998", "8" = "2003", "13" = "2008", "18" = "2013", "23" = "2018", "28" = "2023")) +
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

png(filename = "output/nutrientplot.png", width = 850, height = 950, units = "px", res = 96)   # or tiff("plot.tiff")
plot(nutrientplott)
dev.off()
#save as 800x650


