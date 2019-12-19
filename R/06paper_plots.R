# create plots for paper

require(ggplot2)
require(dplyr)
require(rgdal)
require(rkt)

############################################################################################
# MODIS areas

# forest areas
forestarea <- read.csv("results/forest_areas.csv")
forestarea[forestarea$Country=="Alaska",]$Country <- "USA"

# modis results
filelist <- list.files(path="results/areas", pattern="csv", full.names = T)

csvlist <- list()
for (i in 1:length(filelist)) {
  csvlist[[i]] <- read.csv(filelist[i])
}
tots <- bind_rows(csvlist)

tots[tots$Country=="Alaska",]$Country <- "USA"

# Biomes
biomes <- read.csv("results/biome_names.csv")

biomenames <- as.character(biomes$BiomeName)
biomes$BiomeName <- factor(as.character(biomes$BiomeName), levels = biomenames)

tots <- merge(tots, biomes, by="Biome")

tots$Period <- case_when(
  tots$Year < 2004 ~ "2001-03",
  tots$Year > 2003 & tots$Year < 2007 ~ "2004-06",
  tots$Year > 2006 & tots$Year < 2010 ~ "2007-09",
  tots$Year > 2009 & tots$Year < 2013 ~ "2010-12",
  tots$Year > 2012 & tots$Year < 2016 ~ "2013-15",
  TRUE ~ "2016-18"
)

tots$Period2 <- case_when(
  tots$Year < 2004 ~ 1,
  tots$Year > 2003 & tots$Year < 2007 ~ 2,
  tots$Year > 2006 & tots$Year < 2010 ~ 3,
  tots$Year > 2009 & tots$Year < 2013 ~ 4,
  tots$Year > 2012 & tots$Year < 2016 ~ 5,
  TRUE ~ 6
)

#tots$BiomeName <- factor(as.character(tots$Name), levels = biomenames)

##############################################################################
# Plots for paper

# group by Biome, Year and Country

xx1 <- tots %>% group_by(Country, BiomeName, Year) %>%
  summarise(Area=sum(AreaBurned))

xx2 <- tots %>% group_by(BiomeName, Year) %>%
  summarise(Area=sum(AreaBurned))

xx2$Country <- "All"

xx <- rbind(xx1, xx2)

xx$Area2 <- xx$Area/1000

xxSl <- xx %>% group_by(Country, Year) %>%
  summarise(Area3=sum(Area2)) %>% group_by(Country) %>%
    summarise(slope2=rkt(Year, Area3)$B,
              intercept2=(median(Area3) - slope2 * 2009.5))

xxx <- xx %>% group_by(Country) %>%
  summarise(AvgArea=sum(Area2)/18)

samPalette2 <- c("#38A800", "#0072B2", "#56B4E9", "#F0E442", "#EBA800", "#FF5500")

gg <- ggplot(xx, aes(x = Year))+
  geom_bar(aes(y = Area2, fill=BiomeName), stat = "identity", width=0.5)+
  scale_fill_manual(values=samPalette2)+ylab("Area Burned ('000 ha)")+
  geom_hline(data=xxx, aes(yintercept=AvgArea), linetype = "dashed", colour="dark grey")+
  geom_abline(data=xxSl, aes(slope=slope2, intercept=intercept2), size=0.3, colour="black")+
  theme(legend.title = element_blank(), legend.position = c(0.835,0.15), axis.title.x=element_blank())+
  facet_wrap(~ Country, scales = "free_y")

ggsave(filename = "figures/png/figure2.png", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure2.eps", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure2.pdf", gg, width = 26, height = 16, units = "cm")


##############################################################
# average fire size

xx1 <- tots %>% group_by(Country, BiomeName, Period2) %>%
  summarise(Area=sum(AreaBurned),
            NoFires=sum(NoFires),
            AverageSize=sum(AreaBurned)/sum(NoFires))

xx1 <- subset(xx1, NoFires>=50)

xx2 <- xx1 %>% group_by(Country, BiomeName) %>%
  summarise(count=n())

xx1 <- merge(xx1, xx2, by=c("Country","BiomeName"), all.x=T)
xx1 <- subset(xx1, count>3)

xx4 <- xx1 %>% group_by(BiomeName, Period2) %>%
  summarise(Area=sum(Area),
            NoFires=sum(NoFires),
            AverageSize=sum(Area)/sum(NoFires))

xx4$Country <- "All"
xx4$count <- 6

xx <- bind_rows(xx1, xx4)

samPalette2 <- c("#38A800", "#0072B2", "#56B4E9", "#F0E442", "#EBA800", "#FF5500")

gg <- ggplot(xx, aes(x = Period2))+
  geom_line(aes(y = AverageSize, colour=BiomeName), size=1.25)+
  scale_colour_manual(values=samPalette2)+
  ylab("Average Fire Size (ha)")+
  scale_x_continuous(labels=c("01-03", "04-06", "07-09", "10-12", "13-15", "16-18"), breaks=c(1,2,3,4,5,6), limits=c(0.5,6.5))+
  theme(legend.title = element_blank(), legend.position = c(0.835,0.15), 
        axis.title.x=element_blank(), axis.text=element_text(size=8))+
  facet_wrap(~ Country, scales = "free_y")

ggsave(filename = "figures/png/figure3.png", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure3.eps", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure3.pdf", gg, width = 26, height = 16, units = "cm")


#########################################################################
#################################################################################################
# Landsat sample based estimates

# Severity
filelist <- list.files(path="results/sevpoints", pattern=".rds", recursive = T, full.names = T)

pointslist <- list()
for (i in 1:length(filelist)) {
  pointslist[[i]] <- readRDS(filelist[i])
  pointslist[[i]]$Country <- substr(filelist[i],29,nchar(filelist[i])-4)
}

points <- bind_rows(pointslist)

points[points$Country=="Alaska",]$Country <- "USA"

biomes <- read.csv("results/biome_names.csv")

biomenames <- as.character(biomes$BiomeName)
biomes$BiomeName <- factor(as.character(biomes$BiomeName), levels = biomenames)

points <- merge(points, biomes, by="Biome")

points$Country <- as.factor(points$Country)

points$Period2 <- case_when(
  points$Year < 2004 ~ 1,
  points$Year > 2003 & points$Year < 2007 ~ 2,
  points$Year > 2006 & points$Year < 2010 ~ 3,
  points$Year > 2009 & points$Year < 2013 ~ 4,
  points$Year > 2012 & points$Year < 2016 ~ 5,
  TRUE ~ 6
)

sevpoints <- points

###############################################################
# group into period

xx1 <- sevpoints %>% group_by(Country, BiomeName, Period2) %>%
  summarise(severity=mean(nbrdiff),
            count=n())

xx2 <- sevpoints %>% group_by(BiomeName, Period2) %>%
  summarise(severity=mean(nbrdiff),
            count=n())

xx2$Country <- "All"

xx <- rbind(xx1, xx2)

sev_mw <- read.csv('results/mann_whitney_severity.csv')

xx <- merge(xx, sev_mw, by=c("Country","BiomeName","Period2"))

sevcountry <- subset(xx, count>99)

sevcountrysig <- subset(sevcountry, MW_pvalue<0.05)

samPalette2 <- c("#38A800", "#0072B2", "#56B4E9", "#F0E442", "#EBA800", "#FF5500")

gg <- ggplot(data=NULL, aes(x = Period2, y = severity))+
  geom_line(data=sevcountry, aes(colour=BiomeName), size=1)+
  geom_point(data=sevcountrysig, aes(colour=BiomeName), size=3, position=position_dodge(0.1))+
  scale_colour_manual(values=samPalette2)+
  ylab("Change in NBR")+
  scale_x_continuous(labels=c("2001-03", "2004-06", "2007-09", "2010-12", "2013-15", "2016-18"), breaks=c(1,2,3,4,5,6), limits=c(0.5,6.5))+
  theme(legend.title = element_blank(), legend.position = c(0.835,0.15), 
        axis.title.x=element_blank(), axis.text=element_text(size=8))+
  facet_wrap(~ Country)

ggsave(filename = "figures/png/figure5.png", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure5.eps", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure5.pdf", gg, width = 26, height = 16, units = "cm")

# violins

gg <- ggplot(sevpoints, aes(x = Country, y = nbrdiff, fill=BiomeName))+
  geom_violin()+facet_wrap(~ BiomeName)+
  scale_fill_manual(values=samPalette2)+ylab("Change in NBR")+
  scale_y_continuous(limits = c(0,1300))+
  theme(legend.position = "none", axis.title.x=element_blank())

ggsave(filename = "figures/png/figure4.png", gg, width = 28, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure4.eps", gg, width = 28, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure4.pdf", gg, width = 28, height = 16, units = "cm")

###############################################################
# recovery
###############################################################

filelist <- list.files(path="results/recpoints", pattern=".rds", recursive = T, full.names = T)

pointslist <- list()
for (i in 1:length(filelist)) {
  pointslist[[i]] <- readRDS(filelist[i])
  pointslist[[i]]$Country <- substr(filelist[i],29,nchar(filelist[i])-4)
}

points <- bind_rows(pointslist)

points[points$Country=="Alaska",]$Country <- "USA"

biomes <- read.csv("results/biome_names.csv")

biomenames <- as.character(biomes$BiomeName)
biomes$BiomeName <- factor(as.character(biomes$BiomeName), levels = biomenames)

points <- merge(points, biomes, by="Biome")

points$Country <- as.factor(points$Country)

points$Period2 <- case_when(
  points$Year < 2004 ~ 1,
  points$Year > 2003 & points$Year < 2007 ~ 2,
  points$Year > 2006 & points$Year < 2010 ~ 3,
  TRUE ~ 4
)

recpoints <- points


xx1 <- recpoints %>% group_by(Country, BiomeName, Period2) %>%
  summarise(recovery=mean(recov_ind),
            count=n())

xx2 <- recpoints %>% group_by(BiomeName, Period2) %>%
  summarise(recovery=mean(recov_ind),
            count=n())

xx2$Country <- "All"

xx <- rbind(xx1, xx2)

rec_mw <- read.csv('results/mann_whitney_recovery.csv')

xx <- merge(xx, rec_mw, by=c("Country","BiomeName","Period2"))

reccountry <- subset(xx, count>99)

reccountrysig <- subset(reccountry, MW_pvalue<0.05)

samPalette2 <- c("#38A800", "#0072B2", "#56B4E9", "#F0E442", "#EBA800", "#FF5500")

gg <- ggplot(data=NULL, aes(x = Period2, y = recovery))+
  geom_line(data=reccountry, aes(colour=BiomeName), size=1)+
  geom_point(data=reccountrysig, aes(colour=BiomeName), size=3, position=position_dodge(0.1))+
  scale_colour_manual(values=samPalette2)+
  ylab("Percent recoverd at 5 years")+
  scale_x_continuous(labels=c("2001-03", "2004-06", "2007-09", "2010-12"), breaks=c(1,2,3,4), limits=c(0.5,4.5))+
  theme(legend.title = element_blank(), legend.position = c(0.835,0.15), 
        axis.title.x=element_blank(), axis.text=element_text(size=8))+
  facet_wrap(~ Country)

ggsave(filename = "figures/png/figure7.png", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure7.eps", gg, width = 26, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure7.pdf", gg, width = 26, height = 16, units = "cm")


# violins

gg <- ggplot(recpoints, aes(x = Country, y = recov_ind, fill=BiomeName))+
  geom_violin()+facet_wrap(~ BiomeName)+
  scale_fill_manual(values=samPalette2)+ylab("Percent recovered at 5 years")+
  theme(legend.position = "none", axis.title.x=element_blank())

ggsave(filename = "figures/png/figure6.png", gg, width = 28, height = 16, units = "cm")
ggsave(filename = "figures/eps/figure6.eps", gg, width = 28, height = 16, units = "cm")
ggsave(filename = "figures/pdf/figure6.pdf", gg, width = 28, height = 16, units = "cm")

gg <- ggplot(recpoints, aes(x = Country, y = recov_ind, fill=BiomeName))+
  geom_boxplot()+facet_wrap(~ BiomeName)+
  scale_fill_manual(values=samPalette2)+ylab("Percent recovered at 5 years")+
  theme(legend.position = "none", axis.title.x=element_blank())
