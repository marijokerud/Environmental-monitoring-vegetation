#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

rawdata <- read_excel(path = "data/rawdata.xlsx", sheet = "NDVI", col_names = TRUE)

NDVIdata <- rawdata %>% 
  select(felt_id, NDVI) 

NDVImedian <- rawdata %>% 
  select(felt_id, NDVI) %>% 
  group_by(felt_id) %>% 
  summarise(med= median(NDVI))

NINAcolor <- c("#008C95", "#93328E", "#7A9A01", "#E57200", "#980043") # "#004F71"

NDVIplot <-
ggplot(NDVIdata, aes(x= felt_id, y= NDVI, fill = felt_id)) +
  geom_boxplot(alpha=0.7) +
  labs(x = "Lokaliteter") +
  scale_fill_manual(values = NINAcolor, name  = "") +
  theme_bw() +
  theme(legend.position="top",
        axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) 

png(filename = "output/NDVI.png", width = 850, height = 782, units = "px", res = 150)   # or tiff("plot.tiff")
plot(NDVIplot)
dev.off()
