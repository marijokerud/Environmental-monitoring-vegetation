library(tidyr)
library(tidyverse)
library(dplyr)
library(broom)
library(readxl)
library(ggplot2)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#################### TUNGMETALLER ####################
soil.raw <-  read_excel(path = "data/rawdata.xlsx", sheet = "soil", col_names = TRUE)
dat1 <- read.table("data/jord.txt",header=T,dec=",",row.names=NULL)
dat2 <- read_excel(path = "data/NILU_kjemi2021.xlsx", sheet = "TungR")
summary(dat1)

soil <- soil.raw %>% 
  select(kode, site, year, pH, P, K, Mg, Na_AL, Glødetap, TOC, N_Kjeldahl, C_N) %>% 
  mutate(Year = year) %>% 
  mutate(Year = recode(Year, "1995" = "0", "1998" = "1", "2003" = "2", "2008" = "3", "2013" = "4", "2018" = "5", "2023" = "6")) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(P = as.numeric(gsub(pattern="<", replacement="", P))) %>% 
  mutate(K = as.numeric(gsub(pattern="<", replacement="", K))) %>% 
  mutate(Mg = as.numeric(gsub(pattern="<", replacement="", Mg))) %>% 
  mutate(Na_AL = as.numeric(gsub(pattern="<", replacement="", Na_AL)))
  
soil[] <- lapply(soil, gsub, pattern="<", replacement="")


datLM<- dat %>% 
  #gather("heavymetal", "dose", Pb, Ni, Zn, Hg) %>% 
  mutate(aar = Aar) %>% 
  mutate(aar = recode(aar, "2006" = "0", "2008" = "2", "2013" = "7", "2018" = "12", "2021" = "15")) %>% 
  mutate(aar = as.factor(aar)) %>% 
  mutate(aar = fct_relevel(aar, "15", "0", "2", "7", "12")) #ANOVA med 2021 som intercept
  #mutate(aar = recode(aar, "2006" = 0, "2008" = 2, "2013" = 7, "2018" = 12, "2021" = 15)) # aar som numerisk

datHEI <- datLM %>% 
  filter(Provetype=="hei")

datMYR <- datLM %>% 
  filter(Provetype=="myr")

#mod <- datLM %>% 
  #group_by(heavymetal) %>% 
  #do(fit = lm(dose ~  aar, data = .))

#names(mod)
#res.mod<-tidy(mod, fit)
#tibble(res.ratio)<-tidy(result.ratio, fit)

summary(lm(Hg~aar,data=datLM))
summary(lm(Ni~aar,data=datLM))
summary(lm(Pb~aar,data=datLM))
summary(lm(Zn~aar,data=datLM))

summary(lm(Hg~aar,data=datHEI))
summary(lm(Ni~aar,data=datHEI))
summary(lm(Pb~aar,data=datHEI))
summary(lm(Zn~aar,data=datHEI))


summary(lm(Hg~aar,data=datMYR))
summary(lm(Ni~aar,data=datMYR)) 
summary(lm(Pb~aar,data=datMYR))
summary(lm(Zn~aar,data=datMYR))


par(mfrow = c(2, 2)) # generate a plot window with 2x2 panels
plot(PbMOD); par(mfrow = c(1, 1)) 


#PLOT
datTOT<- dat %>%
  gather("heavymetal", "dose", Pb, Ni, Zn, Hg) %>% 
  group_by(Aar, heavymetal) %>% 
  summarise(avg=mean(dose), SD=sd(dose)) %>% 
  mutate(Provetype = "samlet")

datSUM<- dat %>%
  gather("heavymetal", "dose", Pb, Ni, Zn, Hg) %>% 
  group_by(Provetype, Aar, heavymetal) %>% 
  summarise(avg=mean(dose), SD=sd(dose)) %>% 
  bind_rows(datTOT)

cbbPalette <- c("#93328E", "#018C95", "#E57201") #lilla = #93328E, turkis = #018C95, grønn = #7A9A01, oransje = #E57201

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
        panel.grid.minor.y=element_blank()) #Hide all the vertical gridlines

#sAVE Width: 600, Height: 1000


#################### PAH ####################

pahDat1 <- read.table("PAHjord.txt",header=T,dec=",",row.names=NULL)
pahDat2 <- read_excel("NILU_kjemi2021.xlsx", sheet = "PAH")
summary(pahDat1)

pahDat <- pahDat1 %>% 
  bind_rows(pahDat2) %>% 
  select(Provetype, Felt, Lok, Aar, Sum16EPA, SumKreftfremkallende) %>% 
  filter(Lok == "Forsol") %>% 
  filter(Sum16EPA <340)
  #filter(Aar %in% c("2006", "2008", "2009", "2010", "2013", "2021")) #fjern 2018 pga kontamineringsepisode på NILU laboratoriet

pahLM<- pahDat %>% 
  mutate(aar = Aar) %>% 
  #mutate(aar = as.factor(aar)) %>% 
  filter(Aar %in% c("2009", "2010", "2013", "2018", "2021")) %>%  #kjør analyser for 2009-202
  mutate(aar = recode(aar, "2009" = 0, "2010" = 1, "2013" = 4, "2018" = 9, "2021" = 12)) #aar som numerisk1
  #mutate(aar = fct_relevel(aar, "2021", "2006", "2008", "2009", "2010", "2013", "2018")) #ANOVA med 2021 som intercept
  

pahHEI <- pahLM %>% 
  filter(Provetype=="hei")

pahMYR <- pahLM %>% 
  filter(Provetype=="myr")


summary(lm(Sum16EPA~aar,data=pahHEI))
summary(lm(Sum16EPA~aar,data=pahMYR))
summary(lm(Sum16EPA~aar,data=pahLM))
summary(lm(SumKreftfremkallende~aar,data=pahHEI))
summary(lm(SumKreftfremkallende~aar,data=pahMYR))  
summary(lm(SumKreftfremkallende~aar,data=pahLM)) 

  

#PLOT
pahTOT<- pahDat %>%
  gather("PAH", "concentration", Sum16EPA, SumKreftfremkallende) %>% 
  group_by(Aar, PAH) %>% 
  summarise(avg=mean(concentration), SD=sd(concentration)) %>% 
  mutate(Provetype = "samlet")

pahSUM<- pahDat %>%
  gather("PAH", "concentration", Sum16EPA, SumKreftfremkallende) %>% 
  group_by(Provetype, Aar, PAH) %>% 
  summarise(avg=mean(concentration), SD=sd(concentration)) %>% 
  bind_rows(pahTOT)

newLabel <- c(
  Sum16EPA = "PAH",
  SumKreftfremkallende = "Kreftfremkallende PAH"
  )

ggplot(pahSUM, aes(x=Aar, y=avg, group=Provetype, colour=Provetype)) + 
  geom_line(lwd=1, position = position_dodge(width = 0.4)) +
  geom_point(cex=3, position = position_dodge(width = 0.4)) +
  geom_linerange(aes(ymin=avg-(SD*2/sqrt(6)), ymax=avg+(SD*2/sqrt(6))), position = position_dodge(width = 0.4)) +
  facet_grid(PAH~., scales="free_y", labeller = labeller(PAH = newLabel)) +
  ylab("ng/g") +
  xlab("År") +
  scale_x_continuous(breaks=c(2006, 2008, 2009, 2010, 2013, 2018, 2021)) + # 
  scale_colour_manual(values=cbbPalette) +
  theme_bw() +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size=12,hjust=0.5),
        axis.title.y = element_text(size=12,hjust=0.5),
        axis.text.x = element_text(size=12,color='black', angle = 45, hjust = 1),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text( color="black", size=18),
        legend.text = element_text( color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank()) #Hide all the vertical gridlines

#sAVE Width: 450, Height: 550
