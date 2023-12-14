library(ggplot2)

ggplot(datSUM, aes(x=Aar, y=avg, group=Provetype, colour=Provetype)) + 
  geom_line(lwd=1, position = position_dodge(width = 0.4)) +
  geom_point(cex=3, position = position_dodge(width = 0.4)) +
  geom_linerange(aes(ymin=avg-(SD*2/sqrt(6)), ymax=avg+(SD*2/sqrt(6))), position = position_dodge(width = 0.4)) +
  facet_grid(heavymetal~., scales="free_y") +
  ylab("mg/kg") +
  xlab("År") +
  scale_x_continuous(breaks=c(2006, 2008, 2013, 2018, 2021)) +
  scale_colour_manual(values=cbbPalette) +
  theme_bw() +
  theme(legend.position="none") +
  theme(strip.text.y = element_text(size = 12),
        axis.title.x = element_text(size=12,hjust=0.5),
        axis.title.y = element_text(size=12,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text( color="black", size=18),
        legend.text = element_text( color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank())

#save as 800x650
