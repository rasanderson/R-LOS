# Test it out

library(spData)
data(elev)

plot(elev)
rcl = matrix(c(28, 36, 20), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)
plot(recl)

coordinates(recl)

#viewTo(r = recl, xy = matrix(coordinates(recl)), xy2 = matrix(coordinates(recl)))

# rasterprofile samples a raster along a straight line between two points
rasterprofile(recl, c(37,38), c(1,1))