# Initial code based on this stackoverflow question but poor documentation
# https://stackoverflow.com/questions/21841387/r-code-that-evaluates-line-of-sight-los-between-two-lat-lon-points
#
# Below is the text before the code, provided by 'Spacedman'
# If you just want to know if point A can see point B then sample a large number 
# of elevations from the line joining A to B to form a terrain profile and then
# see if the straight line from A to B intersects the polygon formed by that 
# profile. If it doesn't, then A can see B. Coding that is fairly trivial. 
# Conversely you could sample a number of points along the straight line from A
# to B and see if any of them have an elevation below the terrain elevation.
# 
# If you have a large number of points to compute, or if your raster is very
# detailed, or if you want to compute the entire area visible from a point,
# then that might take a while to run.
# 
# Also, unless your data is over a large part of the earth, convert to a regular
# metric grid (eg a UTM zone) and assume a flat earth.
# 
# I don't know of any existing package having this functionality, but using 
# GRASS really isn't that much of a hassle.
# 
# Here's some code that uses raster and plyr:

# The author provides three functions: cansee, viewTo and rasterprofile, but
# comments after the code (below) suggest errors in x and y might be wrong way
# around. Otherwise there seems to be nothing on the internet.
# 
# Spacedman, Thank you, but I think my lack of experience with the raster
# package is causing me problems. I used fra<-getData('alt', country="FRA", mask=T)
# and passed it into cansee(fra,c(43.96757, 4.534960),c(44.99035, 5.253814), 5,10)
# and got an NA. I'm guessing the getData was wrong from the start. How do I go
# about getting a small dem, like in your example? Thank you again. – user2461125 Feb 19 '14 at 2:27
# 
# I get a 404 not found from getData, but I downloaded the DEM from the divagis
# web site. Then I noticed your x and y coords are the wrong way round, unless
# France is now 4 degrees north of the equator... Also, you will have to convert
# degrees in lat-long to metres otherwise the equations aren't correct. – Spacedman Feb 19 '14 at 10:26 
# 
# I am commenting on this old thread since I am looking for a way to calculate
# viewshed in R without the use of additional tools (GRASS, ArcGIS) (as 
# previously asked here: gis.stackexchange.com/questions/272122/…).
# With reference to the image posted by @Spacedman, I am wondering which part of
# his code he actually used to get that output. I guess it should be the viewTo()
# function, right? Also, did he use a matrix of all the xy coordinates of the DTM
# as xy2 parameter? – NewAtGis Mar 21 '18 at 18:26 

cansee <- function(r, xy1, xy2, h1=0, h2=0){
  ### can xy1 see xy2 on DEM r?
  ### r is a DEM in same x,y, z units
  ### xy1 and xy2 are 2-length vectors of x,y coords
  ### h1 and h2 are extra height offsets
  ###  (eg top of mast, observer on a ladder etc)
  xyz = rasterprofile(r, xy1, xy2)
  np = nrow(xyz)-1
  h1 = xyz$z[1] + h1
  h2 = xyz$z[np] + h2
  hpath = h1 + (0:np)*(h2-h1)/np
  return(!any(hpath < xyz$z))
}

viewTo <- function(r, xy, xy2, h1=0, h2=0, progress="none"){
  ## xy2 is a matrix of x,y coords (not a data frame)
  require(plyr)
  aaply(xy2, 1, function(d){cansee(r,xy,d,h1,h2)}, .progress=progress)
}

rasterprofile <- function(r, xy1, xy2){
  ### sample a raster along a straight line between two points
  ### try to match the sampling size to the raster resolution
  dx = sqrt( (xy1[1]-xy2[1])^2 + (xy1[2]-xy2[2])^2 )
  nsteps = 1 + round(dx/ min(res(r)))
  xc = xy1[1] + (0:nsteps) * (xy2[1]-xy1[1])/nsteps
  yc = xy1[2] + (0:nsteps) * (xy2[2]-xy1[2])/nsteps
  data.frame(x=xc, y=yc, z=r[cellFromXY(r,cbind(xc,yc))])
}
