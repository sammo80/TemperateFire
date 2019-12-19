# make points for Google Earth Engine Landsat extract

require(rgdal)
require(raster)

# set temporary directory for temp rasters if required (otherwise temp files can fill up C:/)
# rasterOptions(tmpdir=paste0("J:/Temp"))

# modis results
filelist <- list.files(path="results/areas", pattern="csv", full.names = T)

csvlist <- list()
for (i in 1:length(filelist)) {
  csvlist[[i]] <- read.csv(filelist[i])
}
tots <- do.call(rbind, csvlist)

projstr = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# this function generates sample points based on burned areas
generatepoints <- function(country) {
  totsub <- subset(tots, Country==country)
  totsub <- subset(totsub, AreaBurned>0)
  biomelist <- unique(totsub$Biome)
  pointslist <- list()
  j <- 1
  for (biom in biomelist) {
    biomesub <- subset(totsub, Biome==biom)
    for (i in 1:nrow(biomesub)) {
      r <- raster(paste0("rasters/countries/",country,"/biomes/",biomesub[i,2],"/yrs/fires_",biomesub[i,3],".tif"))
      v <- getValues(r)
      v.eq1 <- which(v==1)
      if (length(v.eq1) > 500) {
        x <- sample(v.eq1, 500)
        points <- as.data.frame(xyFromCell(r, x))
        points$Year <- biomesub[i,3]
        points$Biome <- biomesub[i,2]
        spdf <- SpatialPointsDataFrame(coords = points[,1:2], data = points, proj4string = crs(r))
        pointslist[[j]] <- spdf
        j <- j+1
      } else {
        points <- as.data.frame(xyFromCell(r, v.eq1))
        points$Year <- biomesub[i,3]
        points$Biome <- biomesub[i,2]
        spdf <- SpatialPointsDataFrame(coords = points[,1:2], data = points, proj4string = crs(r))
        pointslist[[j]] <- spdf
        j <- j+1
      }
    }
    print(paste("biome",biom))
  }
  newpoints <- do.call(rbind, pointslist)
  newpoints <- spTransform(newpoints, projstr)
  newpoints$ID <- 1:length(newpoints)
  writeOGR(newpoints, dsn=paste0("shapes/countries/",country), layer=paste0("points_",country), driver="ESRI Shapefile")
}

generatepoints("Europe")
generatepoints("Australia")
generatepoints("China")
generatepoints("USA")
generatepoints("Canada")
generatepoints("Chile")
generatepoints("Alaska")
generatepoints("Russia")

