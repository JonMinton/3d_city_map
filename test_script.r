# Script for extracting centroids from points
rm(list=ls())

# Example from:
#http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes

#install.packages("GISTools")
#require(GISTools)
require(rgdal)
require(plyr)
require(tidyr)
require(rgl)
require(fBasics)
require(raster)
require(rgeos)
require(ggplot2)
require(akima)
require(dplyr)

# 19-3-2015

# The aim now is to simply produce a series of greyscale images on the same scale, 
# each of a different attribute. There should be no need for interpolation, simply generate 
# the greyscale images, save them as high resolution bitmaps of the same 
# resolution, and then manually merge these files to produce the stl files


# bitmap image approach ---------------------------------------------------


# The data required are:

#datazones within Glasgow
# house prices by datazone
#roads 
# buildings 

# Need to look at the local authority of glasgow

dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

dz_hprices <- read.csv("data/attributes/weighted_median_house_prices_2007_2010.csv") %>%
    tbl_df

dz_hprices_gcity <- dz_hprices %>%
    inner_join(dzs_in_gcity) %>%
    select(datazone, hprice=avg_median_price)

dz_hprices_gcity <- dz_hprices_gcity %>%
    mutate(hrank=min_rank(hprice), prank=hprice/max(hprice))


rgdal::ogrInfo("data/shapefiles/scotland_2001_datazones", "scotland_dz_2001")
scot_2001_dz_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_datazones", "scotland_dz_2001"
)

glas_2001_dz_shp <- scot_2001_dz_shp[scot_2001_dz_shp$zonecode %in% dzs_in_gcity$datazone,] 

# attach house prices
glas_2001_dz_shp@data <- merge(glas_2001_dz_shp@data, dz_hprices_gcity, by.x="zonecode", by.y="datazone")
glas_2001_dz_shp@data <- glas_2001_dz_shp@data[c("zonecode", "gid","ons_code", "label", "hprice", "hrank", "prank")]

plot(glas_2001_dz_shp)

spplot(glas_2001_dz_shp, "hprice",
       col.regions=rev(gray(seq(0, 1, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(axis.line = list(col = 'transparent'))
       )
dir.create("bitmaps")
bmp("bitmaps/house_price_rank_glasgow.bmp", height=1000, width=1310)
spplot(glas_2001_dz_shp, "hrank",
       col.regions=rev(gray(seq(0, 1, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(axis.line = list(col = 'transparent'))
)
dev.off()

# Now to read in as a big matrix!

require(readbitmap)

btmp <- read.bitmap(f="bitmaps/house_price_rank_glasgow.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp
btmp <- (btmp + 0.2)/1.2


image(btmp)

persp3d(btmp, col="grey", zlim=c(0,1))
dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/house_price.stl")



#######

# SIMD 2012 scores -------------------------------------------------------




simd <- read.csv("data/attributes/00410767.csv", header=T) %>%
    tbl_df %>%
    select(datazone=Data.Zone, simd=Overall.SIMD.2012.Score) %>%
    mutate(simd_rank=min_rank(simd))


dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 


dz_simd_gcity <- simd %>%
    inner_join(dzs_in_gcity) %>%
    select(datazone, simd, simd_rank)


rgdal::ogrInfo("data/shapefiles/scotland_2001_datazones", "scotland_dz_2001")
scot_2001_dz_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_datazones", "scotland_dz_2001"
)

glas_2001_dz_shp <- scot_2001_dz_shp[scot_2001_dz_shp$zonecode %in% dzs_in_gcity$datazone,] 

# attach house prices
glas_2001_dz_shp@data <- merge(glas_2001_dz_shp@data, dz_simd_gcity, by.x="zonecode", by.y="datazone")
glas_2001_dz_shp@data <- glas_2001_dz_shp@data[c("zonecode", "gid","ons_code", "label", "simd", "simd_rank")]


spplot(glas_2001_dz_shp, "simd",
       col.regions=rev(gray(seq(0, 1, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(axis.line = list(col = 'transparent'))
)

dir.create("bitmaps")
bmp("bitmaps/simd_2012_glasgow.bmp", height=1000, width=1310)
spplot(glas_2001_dz_shp, "simd",
       col.regions=rev(gray(seq(0, 1, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(axis.line = list(col = 'transparent'))
)
dev.off()

btmp <- read.bitmap(f="bitmaps/simd_2012_glasgow.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp
btmp <- (btmp + 0.2)/1.2


image(btmp)

persp3d(btmp, col="grey", zlim=c(0,1))
dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/simd.stl")

