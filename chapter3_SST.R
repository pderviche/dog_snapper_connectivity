#Packages
install.packages("pacman")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, spatial.tools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools, gbif, rgbif,sdm)

library(terra)

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R/SST")

list_layers()
list_layers <- list_layers()



#Download layers

load_layers("BO22_templtmax_ss") -> sstmax
load_layers("BO22_templtmin_ss") -> sstmin
load_layers("BO22_tempmean_ss") -> sstmean
load_layers("BO22_temprange_ss") -> sstrange
load_layers("BO22_dissoxmean_ss") -> dissox

e <- extent(-43,-36,-23,-15)

sstmax <- crop(sstmax, e)
sstmin <- crop(sstmin, e)
sstmean <- crop(sstmean, e)
sstrange <- crop(sstrange, e)
dissox <- crop(dissox, e)

writeRaster(sstmax, "sstmax", format="raster",overwrite=TRUE)
writeRaster(sstmin, "sstmin", format="raster",overwrite=TRUE)
writeRaster(sstmean, "sstmean", format="raster",overwrite=TRUE)
writeRaster(sstrange, "sstrange", format="raster",overwrite=TRUE)
writeRaster(dissox, "dissox", format="raster",overwrite=TRUE)


###
# Abrolhos MPA
###

round(extract(sstmin, matrix(c(-38.701614, -17.969225), ncol = 2)), 1)
round(extract(sstmean, matrix(c(-38.701614, -17.969225), ncol = 2)), 1)
round(extract(sstrange, matrix(c(-38.701614, -17.969225), ncol = 2)), 1)
round(extract(dissox, matrix(c(-38.701614, -17.969225), ncol = 2)), 1)

###
# Marataízes
###

round(extract(sstmin, matrix(c(-40.722006, -21.023861), ncol = 2)), 1)
round(extract(sstmean, matrix(c(-40.722006, -21.023861), ncol = 2)), 1)
round(extract(sstrange, matrix(c(-40.722006, -21.023861), ncol = 2)), 1)
round(extract(dissox, matrix(c(-40.722006, -21.023861), ncol = 2)), 1)

###
# Guarapari islands
###

round(extract(sstmin, matrix(c(-40.389630, -20.686229), ncol = 2)), 1)
round(extract(sstmean, matrix(c(-40.389630, -20.686229), ncol = 2)), 1)
round(extract(sstrange, matrix(c(-40.389630, -20.686229), ncol = 2)), 1)
round(extract(dissox, matrix(c(-40.389630, -20.686229), ncol = 2)), 1)

###
# Forgotten
###

round(extract(sstmin, matrix(c(-39.530469, -18.830440), ncol = 2)), 1)
round(extract(sstmean, matrix(c(-39.530469, -18.830440), ncol = 2)), 1)
round(extract(sstrange, matrix(c(-39.530469, -18.830440), ncol = 2)), 1)
round(extract(dissox, matrix(c(-39.530469, -18.830440), ncol = 2)), 1)











