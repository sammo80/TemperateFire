# Create MODIS based area stats

# Table 1 in paper
require(dplyr)
require(rkt)

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

biomes <- read.csv("results/biome_names.csv")

tots <- merge(tots, biomes, by="Biome")
forestarea <- merge(forestarea, biomes, by="Biome")

stratum_tots <- tots %>% group_by(Country, BiomeName, Year) %>%
  summarise(Area=sum(AreaBurned))

country_tots <- tots %>% group_by(Country, Year) %>%
  summarise(Area=sum(AreaBurned))
country_tots$BiomeName <- "Total"

biome_tots <- tots %>% group_by(BiomeName, Year) %>%
  summarise(Area=sum(AreaBurned))
biome_tots$Country <- 'zNA'

all_tots <- tots %>% group_by(Year) %>%
  summarise(Area=sum(AreaBurned))
all_tots$Country <- 'zzNA'
all_tots$BiomeName <- 'zzNA'

tots_merge <- bind_rows(stratum_tots, country_tots, biome_tots, all_tots)

forest_country <- forestarea %>% group_by(Country) %>%
  summarise(ForestArea=sum(ForestArea))
forest_country$BiomeName <- "Total"

forest_biome <- forestarea %>% group_by(BiomeName) %>%
  summarise(ForestArea=sum(ForestArea))
forest_biome$Country <- "zNA"

forest_total <- forestarea %>% 
  summarise(ForestArea=sum(ForestArea))
forest_total$Country <- 'zzNA'
forest_total$BiomeName <- 'zzNA'

forest_merge <- bind_rows(forestarea, forest_country, forest_biome, forest_total)

countrylist <- unique(tots_merge$Country)

i<-1
df <- data.frame(matrix(nrow = 40, ncol=6, 0))
for (country in countrylist) {
  xx <- subset(tots_merge, Country==country)
  biomelist <- as.character(unique(xx$BiomeName))
  
  for (biome in biomelist) {
    xxx <- subset(xx, BiomeName==biome)
    xxxForest <- subset(forest_merge, Country==country & BiomeName==biome)$ForestArea
    xxxArea <- xxx$Area
    xxxYear <- xxx$Year
    xxxTrend <- rkt(xxxYear, xxxArea)
    df[i,1] <- country
    df[i,2] <- biome
    df[i,3] <- xxxForest
    df[i,4] <- sum(xxxArea)
    df[i,5] <- xxxTrend$B
    df[i,6] <- xxxTrend$sl
    i <- i+1
  }
}

colnames(df) <- c("Country","BiomeName","ForestArea", "BurnedArea", "Slope", "Pvalue")
df$ForestArea2 <- df$ForestArea/1000
df$BurnedArea2 <- df$BurnedArea/1000
df$BurnedAreaPC <- df$BurnedArea/df$ForestArea*100
df$BurnedPY <- df$BurnedArea2/18
df$BurnedPYPC <- df$BurnedAreaPC/18
df$pcSlope <- df$Slope/(df$BurnedArea/18)*100

write.csv(df, "results/table1.csv", row.names = F)

##################################################################################
# Table 2 - Large Fires only
# modis results
filelist <- list.files(path="results/areas", pattern="csv", full.names = T)

csvlist <- list()
for (i in 1:length(filelist)) {
  csvlist[[i]] <- read.csv(filelist[i])
}
tots <- bind_rows(csvlist)

tots[tots$Country=="Alaska",]$Country <- "USA"

biomes <- read.csv("results/biome_names.csv")

tots <- merge(tots, biomes, by="Biome")


stratum_tots <- tots %>% group_by(Country, BiomeName) %>%
  summarise(Area=sum(AreaBurned)/1000, AreaL=sum(AreaLarge)/1000,
            Percent=sum(AreaLarge)/sum(AreaBurned)*100,
            NumF=sum(NoFires), NumLF=sum(NoLargeFires))

country_tots <- tots %>% group_by(Country) %>%
  summarise(Area=sum(AreaBurned)/1000, AreaL=sum(AreaLarge)/1000,
            Percent=sum(AreaLarge)/sum(AreaBurned)*100,
            NumF=sum(NoFires), NumLF=sum(NoLargeFires))
country_tots$BiomeName <- "Total"

biome_tots <- tots %>% group_by(BiomeName) %>%
  summarise(Area=sum(AreaBurned)/1000, AreaL=sum(AreaLarge)/1000,
            Percent=sum(AreaLarge)/sum(AreaBurned)*100,
            NumF=sum(NoFires), NumLF=sum(NoLargeFires))
biome_tots$Country <- 'zNA'

all_tots <- tots %>%
  summarise(Area=sum(AreaBurned)/1000, AreaL=sum(AreaLarge)/1000,
            Percent=sum(AreaLarge)/sum(AreaBurned)*100,
            NumF=sum(NoFires), NumLF=sum(NoLargeFires))
all_tots$Country <- 'zzNA'
all_tots$BiomeName <- 'zzNA'

tots_merge <- bind_rows(stratum_tots, country_tots, biome_tots, all_tots)

write.csv(tots_merge, "results/table2.csv", row.names = F)