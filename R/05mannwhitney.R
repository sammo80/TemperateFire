# Calculate Mann-Whitney significance tests for severity and recovery periods

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

countrylist <- as.character(unique(sevpoints$Country))

sevcountry <- sevpoints %>% group_by(Country, Biome, Period2) %>%
  summarise(severity=mean(nbrdiff),
            sd=sd(nbrdiff),
            count=n())

df <- data.frame(matrix(nrow=162, ncol=6, 0))
colnames(df) <- c("Country","BiomeName","Period2","NoSamples","MW_pvalue","TT_pvalue")

i <- 1
for (country in countrylist) {
  xx <- subset(sevpoints, Country==country)
  biomelist <- as.character(unique(xx$BiomeName))
  for (biome in biomelist) {
    xxx <- subset(xx, BiomeName==biome)
    population <- xxx$nbrdiff
    for (period in 1:6) {
      xxxx <- subset(xxx, Period2==period)
      periodsample <- xxxx$nbrdiff
      df[i,1] <- country
      df[i,2] <- biome
      df[i,3] <- period
      df[i,4] <- nrow(xxxx)
      if (nrow(xxxx)>29) {
        wt <- wilcox.test(periodsample, population)
        df[i,5] <- wt$p.value
        tt <- t.test(periodsample, population)
        df[i,6] <- tt$p.value
      }
      i <- i+1
    }
  }
}

# all countries combined
xx <- sevpoints
biomelist <- as.character(unique(xx$BiomeName))
for (biome in biomelist) {
  xxx <- subset(xx, BiomeName==biome)
  population <- xxx$nbrdiff
  for (period in 1:6) {
    xxxx <- subset(xxx, Period2==period)
    periodsample <- xxxx$nbrdiff
    df[i,1] <- "All"
    df[i,2] <- biome
    df[i,3] <- period
    df[i,4] <- nrow(xxxx)
    if (nrow(xxxx)>29) {
      wt <- wilcox.test(periodsample, population)
      df[i,5] <- wt$p.value
      tt <- t.test(periodsample, population)
      df[i,6] <- tt$p.value
    }
    i <- i+1
  }
}

df$pvalueGT <- case_when(df$MW_pvalue<0.05 ~ TRUE,
          TRUE ~ FALSE)

write.csv(df, "results/mann_whitney_severity.csv", row.names = F)


#######################
# recovery

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

countrylist <- as.character(unique(recpoints$Country))

reccountry <- recpoints %>% group_by(Country, Biome, Period2) %>%
  summarise(count=n())

df <- data.frame(matrix(nrow=104, ncol=6, 0))
colnames(df) <- c("Country","BiomeName","Period2","NoSamples","MW_pvalue","TT_pvalue")

i <- 1
for (country in countrylist) {
  xx <- subset(recpoints, Country==country)
  biomelist <- as.character(unique(xx$BiomeName))
  for (biome in biomelist) {
    xxx <- subset(xx, BiomeName==biome)
    population <- xxx$recov_ind
    for (period in 1:6) {
      xxxx <- subset(xxx, Period2==period)
      periodsample <- xxxx$recov_ind
      df[i,1] <- country
      df[i,2] <- biome
      df[i,3] <- period
      df[i,4] <- nrow(xxxx)
      if (nrow(xxxx)>29) {
        wt <- wilcox.test(periodsample, population)
        df[i,5] <- wt$p.value
        tt <- t.test(periodsample, population)
        df[i,6] <- tt$p.value
      }
      i <- i+1
    }
  }
}

# all countries combined
xx <- recpoints
biomelist <- as.character(unique(xx$BiomeName))
for (biome in biomelist) {
  xxx <- subset(xx, BiomeName==biome)
  population <- xxx$recov_ind
  for (period in 1:6) {
    xxxx <- subset(xxx, Period2==period)
    periodsample <- xxxx$recov_ind
    df[i,1] <- "All"
    df[i,2] <- biome
    df[i,3] <- period
    df[i,4] <- nrow(xxxx)
    if (nrow(xxxx)>29) {
      wt <- wilcox.test(periodsample, population)
      df[i,5] <- wt$p.value
      tt <- t.test(periodsample, population)
      df[i,6] <- tt$p.value
    }
    i <- i+1
  }
}

df$pvalueGT <- case_when(df$MW_pvalue<0.05 ~ TRUE,
                         TRUE ~ FALSE)

write.csv(df, "results/mann_whitney_recovery.csv", row.names = F)

