## This scripts creates profile metadata for annotated photos only and creates Fig 1


# Import metadata --------------------------------------------------------

# Note: this data has been downloaded from PANGAEA. We modified the data 
# the CSV file to remove headers only 

#Antarctic Peninsula
AP_profile <- read.table("data/ps118_6-9_depth-coords.csv",
                        sep = ",", header = TRUE)
AP_profile$n <- row.names(AP_profile) #the image number
AP_profile <- na.omit(AP_profile) #Remove ANY photos that don't have lat, long or depth 


#Powell Basin 
PB_profile <- read.table("data/ps118_69-1_depth-coords.csv",
                         sep = ",", header = TRUE)
PB_profile$n <- row.names(PB_profile) #the image number
PB_profile <- na.omit(PB_profile) #Remove ANY photos that don't have lat, long or depth 



# Create profile information  ---------------------------------------------

#import the file names of marked photos 

#Antarctic Peninsula 
AP_transect <- list.files(path = "SVGs/AP/")
AP_transect <- gsub('.svg','', AP_transect)
AP_transect <- gsub(' ', '_', AP_transect)
AP_transect <- as.matrix(AP_transect)
colnames(AP_transect) <- "name"

#Get metadata for the relevant photos only 
AP_profile <- merge(AP_transect, AP_profile, by.x = "name", 
                by.y = "name")

#Powell Basin 

PB_transect <- list.files(path = "SVGs/PB/")
PB_transect <- gsub('.svg','', PB_transect)
PB_transect <- gsub(' ', '_', PB_transect)
PB_transect <- as.matrix(PB_transect)
colnames(PB_transect) <- "name"

#Get metadata for the relevant photos only 
PB_profile <- merge(PB_transect, PB_profile, by.x = "name", 
                    by.y = "name")

save(AP_profile, AP_transect, PB_profile, PB_transect,
     file = "data/profileMetadata.RData")


# Create Figure 1  --------------------------------------------------------

#load GEBCO 2019 data, downloaded from https://www.gebco.net/data_and_products/gridded_bathymetry_data/

library(raster)
library(ncdf4)

ncdf4::nc_open("GEBCO/gebco_2023_n-57.498_s-79.4355_w-77.7656_e-41.9062.nc")
gebco <- raster::raster("GEBCO/gebco_2023_n-57.498_s-79.4355_w-77.7656_e-41.9062.nc")


## NOTE: this function is taken from the following tutorial: 
## https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-getting-and.html

# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
colbr <- function(x, b1=50, b2=50, r1=-2, r2=-2) {
  # Min/max values of the raster (x)
  mi <- cellStats(x, stat="min")-100
  ma <- cellStats(x, stat="max")+100
  # Create sequences, but only use unique numbers
  s1 <- unique(round(seq(mi, 0, 0-mi/b1),r1))
  s2 <- unique(round(seq(0, ma, ma/b2),r2))
  # Combine sequence for our break points, removing duplicate 0
  s3 <- c(s1, s2[-1])
  # Create a list with the outputs
  # [[1]] = length of the first sequence minus 1 (water)
  # [[2]] = length of the second sequence minus 1 (land)
  # [[3]] = The break points
  x <- list(length(s1)-1, length(s2)-1, s3)
}



# Crop GEBCO to the extent of map needed 

ant.e <- raster::extent(-78, -30, -75, -56)
ant.gebco <- raster::crop(gebco, ant.e)
ant.br <- colbr(ant.gebco)
ant.br[[2]] <- 1 #change all terrestrial colours to white 

#get country shapefiles 
ant <- raster::getData("GADM", country = "ATA", level = 0)

#colour palette
blue.col <- colorRampPalette(c("darkblue", "lightblue"))

#Plot the figure 
windows(h = 10, w = 10)
plot(ant.gebco, col=c(blue.col(ant.br[[1]]), terrain.colors(ant.br[[2]])), 
     breaks=ant.br[[3]])
plot(ant, add=TRUE)
points(AP_profile$Longitude, AP_profile$Latitude, pch = 24, cex = 2, 
       col = "black", bg = 'red', lwd = 2)
points(PB_profile$Longitude, PB_profile$Latitude, pch = 24, cex = 2, 
       col = "black", bg = 'red', lwd = 2)

# This figure was further modified in Inkscape 

