# validate (QA) points

#require(dplyr)
require(raster)
require(ggplot2)
require(reshape2)
require(sf)


country <- "Russia"



#######################################################################################
# function to subset and crop scenes
subsetScenes <- function(x, targ, padding=NULL, filename = "", verbose=TRUE, ...){
  
  # set padding (if not already done)
  if(!is.numeric(padding)){
    padding <- 0
  }
  
  # check if filenames correspond with targ
  if(filename != "" & length(filename) != length(targ)){
    stop("filename should be of same length as targ")
  }
  
  # narrow down targ vector/list based on extent overlaps
  # targ <- selectScenes(x, targ, padding=padding, verbose=verbose)
  
  # adjust extent of the input object
  e <- extent(x)
  e <- extent(c(xmin(e) - padding,
                xmax(e) + padding,
                ymin(e) - padding,
                ymax(e) + padding))
  
  # read from file and subset these rasters and (optionally) write to output file
  if(is.character(targ)){
    # loop through files and subset and write separately
    b <- list()
    for(i in 1:length(targ)){
      b[[i]] <- brick(targ[i])
      # TODO: detect whether input files are single or multi-layered
      # if single, b <- raster(); else b <- brick()
      if(filename != ""){
        b[[i]] <- crop(b[[i]], e, filename=filename[i], ...)
      } else {
        b[[i]] <- crop(b[[i]], e)
      }
    }
  } else if(is.list(targ)){
    b <- targ
    # then, same as above:
    for(i in 1:length(b)){
      if(filename != ""){
        b[[i]] <- crop(b[[i]], e, filename=filename[i], ...)
      } else {
        b[[i]] <- crop(b[[i]], e)
      }
      # TODO: detect if resulting image is only NA's or defined background value (e.g. 0)
      # if yes, reject it
    }
  } else {
    stop("targ must be either a character vector (filenames) or a list of raster layers.")
  }
  
  # return a list of cropped raster layers
  return(b)
}

# load in points
sev <- readRDS(paste0("results/sevpoints/sevpoints_",country,".rds"))
sev <- subset(sev, Year<2013)
spoints <- st_read(paste0("shapes/countries/", country, "/points_", country, ".shp"))

# load in raster
rb <- brick(paste0("J:/tmp/worldfire_valid/use/nbr_sample_",country,".tif"))

# crop points to raster
spoints <- st_crop(spoints, extent(rb))

# merge with sev points
spoints <- merge(spoints, sev, by="ID")

# get a sample of 100 points
set.seed(8)
spoints <- spoints[sample(nrow(spoints),100),]

# create empty dataframe
df <- data.frame(matrix(nrow=100, ncol=2, 0))
colnames(df) <- c("ID","IsCorrect")

#####################################
# loop through each point to validate
for (j in 1:100) {

  curpoint <- spoints[j,]
  #st_crs(curpoint) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  year <- curpoint$Year.y
  id <- curpoint$ID
  
  # list of rasters
  rlist <- list()
  i <- 1
  for (yr in (year-1992):(year-1983)) {
    rlist[[i]] <- rb[[yr]]
    i <- i+1
  }
  
  croppedlist <- subsetScenes(curpoint, rlist, 0.02)
  
  nbrsub <- curpoint[6:34]
  st_geometry(nbrsub) <- NULL
  
  checkpoint <- unname(extract(rb, curpoint))
  y <- nbrsub
  
  if (length(which(is.na(y)) > 0)) {
    y[which(is.na(y))] <- 0
  }
  
  print(all.equal(as.numeric(checkpoint[1:29]), as.numeric(y)))

  if (checkpoint[30] != 1) {
    print("Not Forest!")
  }
  
  mm <- melt(nbrsub)
  mm$variable <- as.numeric(as.character(mm$variable))

  gg <- ggplot(mm, aes(variable)) + 
    geom_line(aes(y=value), colour="#D55E00", size=1.5) + 
    geom_point(aes(y=curpoint$postfiremean, x=curpoint$Year.y+5), size=5) +
    xlab("Year")+ylab("NBR") + theme(text = element_text(size=18)) +
    annotate("text", x=curpoint$Year.y+7, y=curpoint$postfiremean-50, label=paste("Rec:",round(curpoint$recov_ind)), size=6) +
    annotate("text", x=curpoint$Year.y-5, y=curpoint$premean-150, label=paste("Sev:",round(curpoint$nbrdiff)), size=6)
  
  graphics.off()
  
  print(gg)
  
  dev.new(width = 16, height = 6.5, noRStudioGD = TRUE)
  par(mfrow=c(2,5), mar=c(0.5,0.5,0.5,0.5))
  
  for (i in 1:10) {
    plot(croppedlist[[i]], axis=FALSE, box=FALSE, legend=FALSE, zlim=c(-200,800))
    plot(st_geometry(curpoint), add=T)
  }
  
  df[j,1] <- id
  df[j,2] <- readline(prompt = "Correct (Y or N)? ")
  print(paste("Done with",j))
  
}
#####################################

######################################################################################
# write out results

write.csv(df, paste0("results/validation_",country,"_.csv"))

filelist <- list.files(path="results", pattern="validation", full.names = T)
csvlist <- lapply(filelist, function(x) read.csv(x))
results <- do.call(rbind, csvlist)
summary(as.factor(results$IsCorrect))
#634/700=91%
