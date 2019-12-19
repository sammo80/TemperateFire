require(rgdal)
require(raster)
require(SDMTools)

processmodis <- function(country, projstr="blank") {
  
  filelist <- list.files(path=paste0("rasters/countries/",country,"/gee"), pattern="tif$", full.names = T)
  
  if (length(filelist)>1) {
    yr <- 2001
    for (j in 1:18) {
      rlist <- list()
      for (i in 1:length(filelist)) {
        r <- brick(filelist[i])
        rlist[[i]] <- r[[j]]
      }
      
      r <- do.call(merge, rlist)
      if (projstr != "blank") {
        crs(r) <- projstr
      }
      writeRaster(r, paste0("rasters/countries/",country,"/yrs/fires_",yr,".tif"), datatype="INT2S")
      yr <- yr+1
    }
  } else {
    yr <- 2001
    r <- brick(filelist)
    for (j in 1:18) {
      rsub <- r[[j]]
      if (projstr != "blank") {
        crs(rsub) <- projstr
      }
      writeRaster(rsub, paste0("rasters/countries/",country,"/yrs/fires_",yr,".tif"), datatype="INT2S")
      yr <- yr+1
    }
  }
}

processmodis("Australia")
processmodis("USA")
processmodis("Canada", "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
processmodis("Europe", "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
processmodis("China", "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
processmodis("Russia", "+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
processmodis("Alaska")
processmodis("Chile", "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")


# split fire areas to biomes
yearlybiomes <- function(country) {
  
  biomelist <- list.dirs(path=paste0("rasters/countries/",country,"/biomes"), recursive = F, full.names = F)
  
  for (yr in 2001:2018) {
    r <- raster(paste0("rasters/countries/",country,"/yrs/fires_",yr,".tif"))
    for (biome in biomelist) {
      mask <- raster(paste0("rasters/countries/",country,"/biomes/",biome,"/mask.tif"))
      r2 <- crop(r, mask)
      r2 <- r2*mask
      writeRaster(r2, paste0("rasters/countries/",country,"/biomes/",biome,"/yrs/fires_",yr,".tif"), overwrite=T)
    }
  }
}

yearlybiomes("Australia")
yearlybiomes("USA")
yearlybiomes("Canada")
yearlybiomes("China")
yearlybiomes("Chile")
yearlybiomes("Alaska")
yearlybiomes("Russia")
yearlybiomes("Europe")



#################################################################################
# calculate forest area in each country / biome

forestarea <- function(country) {
  biomelist <- list.dirs(path=paste0("rasters/countries/",country,"/biomes"), recursive = F, full.names = F)
  r <- raster(paste0("rasters/countries/",country,"/masks/forest_extent_", country, ".tif"))
  df <- data.frame(matrix(nrow=length(biomelist), ncol=3, 0))
  i <- 1
  for (biome in biomelist) {
    mask <- raster(paste0("rasters/countries/",country,"/biomes/",biome,"/mask.tif"))
    crs(r) <- crs(mask)
    r2 <- crop(r, mask)
    r2 <- r2*mask
    #writeRaster(r2, paste0("rasters/countries/",country,"/biomes/",biome,"/forest_area.tif"), datatype="INT2S", overwrite=T)
    df[i,1] <- country
    df[i,2] <- biome
    df[i,3] <- cellStats(r2, sum)*6.25
    i <- i+1
  }
  return(df)
}

areaF <- list()
areaF[[1]] <- forestarea("Australia")
areaF[[2]] <- forestarea("USA")
areaF[[3]] <- forestarea("Canada")
areaF[[4]] <- forestarea("Europe")
areaF[[5]] <- forestarea("China")
areaF[[6]] <- forestarea("Chile")
areaF[[7]] <- forestarea("Alaska")
areaF[[8]] <- forestarea("Russia")

df <- do.call(rbind, areaF)
colnames(df) <- c("Country", "Biome", "ForestArea")
write.csv(df, "results/forest_areas.csv", row.names = F)



###############################################################################
# calculate number of fires and area burned

###############################################################################
# Function for removing clumps MODIS 250 - 32 pixels = 200 ha or 80 = 500 ha
clumpfunction <- function(x, y) {
  
  x_clumps <- clump(x, directions=8)
  
  clumpFreq1 <- as.data.frame(freq(x_clumps))
  excludeID1 <- clumpFreq1$value[which(clumpFreq1$count<y)] 
  
  x[x_clumps %in% excludeID1] <- 0
  #x[x==0] <- NA
  
  return(x)
}

calcarea <- function(country) {
  
  biomelist <- list.dirs(path=paste0("rasters/countries/",country,"/biomes"), recursive = F, full.names = F)
  rstats <- as.data.frame(matrix(NA, 18*length(biomelist), 7))
  colnames(rstats) <- c("Country","Biome","Year","NoFires","AreaBurned","NoLargeFires","AreaLarge")
  i <- 1
  for (yr in 2001:2018) {
    for (biome in biomelist) {
      r <- raster(paste0("rasters/countries/",country,"/biomes/",biome,"/yrs/fires_",yr,".tif"))
      rlarge <- clumpfunction(r, 32)
      x <- ConnCompLabel(r)
      xlarge <- ConnCompLabel(rlarge)
      rarea <- cellStats(r, sum)*6.25
      rnum <- max(values(x), na.rm=T)
      rLarea <- cellStats(rlarge, sum)*6.25
      rLnum <- max(values(xlarge), na.rm=T)
      rstats[i,] <- c(country, biome, yr, rnum, rarea, rLnum, rLarea)
      i <- i+1
      print(paste(yr,"biome",biome))
    }
  }
  write.csv(rstats, paste0("results/areas/",country,".csv"), row.names = F)
}

calcarea("Europe")
calcarea("China")
calcarea("USA")
calcarea("Australia")
calcarea("Canada")
calcarea("Chile")
calcarea("Alaska")
calcarea("Russia")
