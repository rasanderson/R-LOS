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


my_viewshed_rst <- viewshed(dem=elev_sub, windfarm=xy1_pt, radius=5000)
plot(elev_sub)
plot(my_viewshed_rst, add=TRUE, legend=FALSE)

my_viewshed_mpt <- viewshed(dem=elev_sub, windfarm=xy1_pt, radius=5000, vector=TRUE)
plot(elev_sub)
plot(my_viewshed_mpt, add=TRUE, legend=FALSE)


xy2mat <- matrix(coordinates(elev_sub),
                 ncol=2,
                 nrow=nrow(coordinates(elev_sub)))

viewTo.res <- viewTo(elev_sub, xy=as.vector(xy1_pt), xy2=xy2mat, h1=h1, h2=h2)
seenby <- xy2mat[viewTo.res,]

seenby_mpt <- st_multipoint(seenby)
seenby_mpt <- st_sfc(seenby_mpt, crs=proj4string(elev_sub))
#seenby_mpt <- st_sfc(seenby_mpt, crs=27700) # EPSG version


# rasterize only works properly (I think) with SpatialPoints
tmp <- as_Spatial(seenby_mpt)
tmp2 <- SpatialPoints(matrix(unlist(tmp@coords[[1]]), ncol=2,
                             byrow=TRUE),
                      proj4string=CRS(projection(elev_sub)))
df <- data.frame(rep(1, dim(seenby)[1]))
tmp2 <- SpatialPointsDataFrame(matrix(coordinates(tmp), ncol=2,
                             byrow=TRUE), data=df,
                      proj4string=CRS(projection(elev_sub)))



# Create a raster version
# This is much messier than expected
# Convert multipoint sf into point sf
seenby_p = st_cast(x = st_sfc(seenby_mpt), to = "POINT")
# Convert point sf into point sp
seenby_p_sp <- as_Spatial(seenby_p) # Gives CRS warning
# Raster conversion
raster_template = raster(extent(elev_sub), resolution = res(elev_sub),
                         crs = crs(elev_sub))

seenby_rst <- rasterize(seenby_p_sp, raster_template, field=1)

plot(elev_sub)
plot(xy1_pt, add=TRUE, pch=16)
plot(seenby_mpt, add=TRUE, cex=0.5)

plot(elev_sub)
plot(view500, add=TRUE)

plot(elev_sub)
plot(seenby_rst, add=TRUE)
