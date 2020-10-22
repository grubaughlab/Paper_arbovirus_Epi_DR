# File; AaS estimates

# This script clips monthly AaS rasters to fit a shape file for the Dominican Republic,
# normalizes these values by province, and color-codes a province-level map according to these values

# Data needed: TIF files with global AaS rasters, shapefile for Dominican Republic

# Load necessary libraries
library(raster)
library(tiff)
library(rgdal)
library(shapefiles)
library(RColorBrewer)

# (1) Load rasters !! NB these rasters are for the entire world !!
AaS_jan = "climate_rasters/Aegypti_Jannormal.tif"
AaS_feb = "climate_rasters/Aegypti_Febnormal.tif"
AaS_march = "climate_rasters/Aegypti_Marnormal.tif"
AaS_apr = "climate_rasters/Aegypti_Aprnormal.tif"
AaS_may = "climate_rasters/Aegypti_Maynormal.tif"
AaS_june = "climate_rasters/Aegypti_Junnormal.tif"
AaS_july = "climate_rasters/Aegypti_Julnormal.tif"
AaS_aug = "climate_rasters/Aegypti_Augnormal.tif"
AaS_sept = "climate_rasters/Aegypti_Sepnormal.tif"
AaS_oct = "climate_rasters/Aegypti_Octnormal.tif"
AaS_nov = "climate_rasters/Aegypti_Novnormal.tif"
AaS_dec = "climate_rasters/Aegypti_Decnormal.tif"

tif_list = c(AaS_jan, AaS_feb, AaS_march, AaS_apr, AaS_may, AaS_june, AaS_july, AaS_aug,
             AaS_sept, AaS_oct, AaS_nov, AaS_dec)

AaS_rasters = list()
AaS_rasters = lapply(tif_list, FUN = raster)  # Convert to rasters

# (2) Crop rasters to the Dominican Republic

# Assign the correct projection to tif files
for(jj in 1:length(AaS_rasters)){
  crs(AaS_rasters[[jj]]) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
}

# Clip tifs to shapefile
# https://data.humdata.org/dataset/dominican-republic-administrative-boundaries-levels-0-6
fn = "dom_adm_2020_shp/dom_admbnda_adm2_2020.shp"
prov.map = readOGR(fn)
crop.shp = spTransform(prov.map, CRS(proj4string(AaS_rasters[[1]])))

# Check projections
for(ii in 1:length(AaS_rasters)){
  cr2 = crop(AaS_rasters[[ii]], crop.shp, snap="out")
  clip = rasterize(crop.shp, cr2)
  clip = mask(x=cr2, mask=clip)
  plot(clip)
}

# Calculate mean AaS probability by province
month = c("jan", "feb", "march", "april", "may", "june", "july", "aug", "sept", "oct", "nov", "dec")
clip.mean = clip.sd = rep(NA, length(month))
prov = crop.shp$ADM2_ES
AaS.values = list()

for(m in 1:32){
  for(k in 1:length(AaS_rasters)){
    cr2 = crop(AaS_rasters[[k]], crop.shp[m,], snap="out")
    clip = rasterize(crop.shp[m,], cr2)
    clip = mask(x=cr2, mask=clip)
    clip.values = matrix(values(clip), nrow = clip@nrows, ncol = clip@ncols, byrow = T)
    clip.mean[k] = mean(clip.values, na.rm = T)
    
    AaS = as.data.frame(cbind(month, clip.mean))

  }
  AaS.values[[m]] = AaS
}

write.csv(AaS.values, "AaS-by-prov.csv")

AaS = read.csv("AaS-by-prov.csv", header = T)

my.cols = seq(3,65,2)
prov.means = AaS[,my.cols]
AaS.avs = as.data.frame(colMeans(prov.means))
names(AaS.avs) = "AaS"
AaS.avs$norm = AaS.avs$AaS/max(AaS.avs)

## Color map by AaS intensity
my.colors = brewer.pal(10, "RdBu")
plot(prov.map)
plot(prov.map[24,], col = my.colors[1], add = T)
plot(prov.map[c(5,8,15,17,25),], col = my.colors[2], add = T)
plot(prov.map[c(1,2,3,6,7,9,10,14,16,19,22,23,26,27,32),], col = my.colors[3], add = T)
plot(prov.map[c(4,11,13,18,20,28,30,31),], col = my.colors[4], add = T)
plot(prov.map[21,], col = my.colors[5], add = T)
plot(prov.map[12,], col = my.colors[6], add = T)
plot(prov.map[29,], col = my.colors[7], add = T)

