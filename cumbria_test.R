# Compare with set of results from ArcGIS Cumbria
# Have 250m and 500m resolution files. Start with 500m as
# worried about slow performance

library(raster)
source("initial_LOS.R")

elev500 <- raster("elevation500m.tif")
res(elev500)
plot(elev500)

mast_coords <- read.csv("mast_coords.csv")

# Let's try and return output from viewTo as a points map
mastx <- as.numeric(mast_coords[1])
masty <- as.numeric(mast_coords[2])
h1 <- 1.5
h2 <- 75

xy1_pt <- st_point(c(mastx, masty))
plot(elev500)
plot(xy1_pt, add=TRUE, pch=16)





xy2mat <- matrix(coordinates(elev500),
                 ncol=2,
                 nrow=coordinates(elev500))

viewTo.res <- viewTo(elev500, xy=as.vector(xy1_pt), xy2=xy2mat, h1=h1, h2=h2)
seenby <- xy2mat[viewTo.res,]

