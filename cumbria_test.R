# Compare with set of results from ArcGIS Cumbria
# Have 250m and 500m resolution files. Start with 500m as
# worried about slow performance

library(raster)
library(sf)
source("initial_LOS.R")

elev500 <- raster("elevation500m.tif")
view500 <- raster("Viewshed500.tif")
res(elev500)

mast_coords <- read.csv("mast_coords.csv")


# Let's try and return output from viewTo as a points map
mastx <- as.numeric(mast_coords[1])
masty <- as.numeric(mast_coords[2])
h1 <- 1.5
h2 <- 75

xy1_pt <- st_point(c(mastx, masty))
plot(elev500)


# Let us work on a small area to begin with
# Create a mask and subset main elevation
rmask = raster(nrows = 50, ncols = 50, res = 500, 
               xmn = 335000, xmx = 360000, ymn = 525000, ymx = 550000,
               vals = 1)
crs(rmask) <- crs(elev500)              
          
plot(rmask, add=TRUE)
plot(xy1_pt, add=TRUE, pch=16)

elev_sub <- crop(elev500, rmask)
plot(elev_sub)
plot(xy1_pt, add=TRUE, pch=16)


xy2mat <- matrix(coordinates(elev_sub),
                 ncol=2,
                 nrow=nrow(coordinates(elev_sub)))

viewTo.res <- viewTo(elev_sub, xy=as.vector(xy1_pt), xy2=xy2mat, h1=h1, h2=h2)
seenby <- xy2mat[viewTo.res,]

seenby_mpt <- st_multipoint(seenby)

plot(elev_sub)
plot(xy1_pt, add=TRUE, pch=16)
plot(seenby_mpt, add=TRUE, cex=0.5)

plot(elev_sub)
plot(view500, add=TRUE)
