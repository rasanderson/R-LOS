# Test it out

library(raster)

# Elevation raster, 1 m resolution
elev1 = raster(nrows = 6, ncols = 6, res = 1, 
                       xmn = 0, xmx = 6, ymn = 0, ymx = 6,
                       vals = c(1:18,18:1))
plot(elev1)


# rasterprofile samples a raster along a straight line between two points
# first pair is the coords in raster space (not row,col)
# 2nd pair ditto
# each pair of items is x-y coords at start and end
# e.g. track along top row of map horizontally
rasterprofile(elev1, c(0.5,5.5), c(5.5,5.5))

# profile along southern most row of map west to east
rasterprofile(elev1, c(0.5,0.5), c(5.5,0.5))

# profile along western most edge of map, south to north
# the first value is at the south (bottom left) and the
# last value north (top left). One elevation is repeated
# as it uses map units rather than rows and columns
rasterprofile(elev1, c(0.5,0.5), c(0.5,5.5))


# Diagonally from north west to south east
rasterprofile(elev1, c(0.5,5.5), c(5.5,0.5))

# Diagonally from south west to north east
rasterprofile(elev1, c(0.5,0.5), c(5.5,5.5))

# Diagonally along highest point of map
rasterprofile(elev1, c(0.5,2.5), c(5.5,3.5))


# So what does cansee() return: TRUE or FALSE
rasterprofile(elev1, c(0.5,0.5), c(0.5,4.5))
cansee(elev1, c(0.5,0.5), c(0.5,4.5), h1=0, h2=23)

# Does viewTo now work? No. Gives an error.
xy2mat <- matrix(coordinates(elev1),
                 ncol=2,
                 nrow=36,
                 byrow = TRUE)
xy2mat
viewTo(elev1, c(0.5,0.5), xy2=xy2mat, h1=0, h2=23)
