# Process the extracted Landsat pixels from Google Earth Engine

require(rgdal)
require(raster)
require(sf)
require(dplyr)

countrylist <- c("Alaska", "Australia", "Canada", "Chile", "China", "Europe", "Russia", "USA")

df <- data.frame(matrix(nrow=8, ncol=6, 0))
colnames(df) <- c("Country","NumPoints","ForestPoints","FirePoints","SevPoints","RecPoints")

i<-1
for (country in countrylist) {

  # spatial points
  spoints <- st_read(paste0("shapes/countries/",country,"/points_",country,".shp"))
  
  # check if there are duplicates in spatial points and remove
  st_geometry(spoints) <- NULL
  spoints <- distinct(spoints, x, y, .keep_all = T)
  
  df[i,1] <- country
  df[i,2] <- nrow(spoints)
  
  # extracted points
  epoints <- read.csv(paste0("gee/nbr_extract_", country,".csv"), check.names = F)
  
  # subset to forested points only
  epoints <- subset(epoints, treecover2000==1)
  
  # drop unnecessary columns
  epoints <- epoints[,2:31]
  
  # merge with spoints
  epoints <- merge(epoints, spoints[,3:5], by="ID")
  
  df[i,3] <- nrow(epoints)
  
  # make zeros NA
  epoints[epoints==0] <- NA
  
  # add fire year column
  epoints$fireyearcol <- epoints$Year-1991+2
  
  # calculate pre-fire mean and sd
  premean <- apply(epoints, 1, function(x) mean(x[(x[33]-10):(x[33]-1)], na.rm = T))
  presd <- apply(epoints, 1, function(x) sd(x[(x[33]-10):(x[33]-1)], na.rm = T))
  
  # calculate min from fire year and 1 year after
  firemin <- apply(epoints, 1, function(x) min(x[x[33]],x[(x[33]+1)], na.rm = T))
  
  epoints <- cbind(epoints, premean, presd, firemin)
  
  # is the min the same year as the fire
  whichmin <- apply(epoints, 1, function(x) x[x[33]]==x[36])
  
  epoints <- cbind(epoints, whichmin)
  
  # calculate NBR difference
  epoints$nbrdiff <- epoints$premean-epoints$firemin
  
  # check to see if NBR difference is less than 2 sds below the mean
  epoints$sddiff <- epoints$nbrdiff-(2*epoints$presd)
  
  # remove pixels not meeting criteria
  epoints <- subset(epoints, !is.infinite(firemin))
  epoints <- subset(epoints, sddiff>0)
  
  df[i,4] <- nrow(epoints)
  
  # calculate five year post-fire mean (1 year either side)
  postfiremean <- apply(epoints, 1, function(x) mean(x[(x[33]+4):(x[33]+6)], na.rm = T))
  epoints <- cbind(epoints, postfiremean)
  epoints$recov_ind <- (epoints$postfiremean-epoints$firemin)/epoints$nbrdiff*100
  epoints[epoints$postfiremean<epoints$firemin,]$recov_ind <- 0
  epoints[epoints$postfiremean>epoints$premean,]$recov_ind <- 100

  
  saveRDS(epoints, paste0("results/firepoints/nbrTSpoints_",country,".rds"))
  
  sevpoints <- subset(epoints, whichmin==TRUE)
  recpoints <- subset(epoints, Year<2013)
  
  saveRDS(sevpoints, paste0("results/sevpoints/sevpoints_",country,".rds"))
  saveRDS(recpoints, paste0("results/recpoints/recpoints_",country,".rds"))
  
  df[i,5] <- nrow(sevpoints)
  df[i,6] <- nrow(recpoints)
  
  i<-i+1
  
}

df$pc_fire <- df$FirePoints/df$ForestPoints*100
write.csv(df, "results/fire_check.csv", row.names = F)


