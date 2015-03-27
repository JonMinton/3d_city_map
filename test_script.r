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

require(akima)
require(dplyr)
require(r2stl)
require(readbitmap)
require(lattice)
require(latticeExtra)

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

dz_hprices <- read.csv("data/attributes/weighted_median_house_prices_2005_2010.csv") %>%
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

# Now to go back and use code for extracting road network
scotland_road <- rgdal::readOGR(    
    "data/shapefiles/scotland-roads-shape", "roads"
)

# transform projection system

scotland_road_reprojected <- spTransform(scotland_road, CRS(proj4string(glas_2001_dz_shp)))
# Robin Lovelace function

gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}


scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(glas_2001_dz_shp))

# trying to add as layer within spplot

bmp("bitmaps/hprice_2005_2010_glasgow.bmp", height=1000, width=1310)
spplot(glas_2001_dz_shp, "hprice",
       col.regions=rev(gray(seq(0, 0.95, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(
           axis.line = list(col = 'transparent'),
           panel.background=list(col=gray(0.95))
       )
) + latticeExtra::layer(
    sp.lines(scotland_road_reprojected_clipped, col = "white", alpha=0.1) 
)
dev.off()    


btmp <- read.bitmap(f="bitmaps/hprice_2005_2010_glasgow.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")



#persp3d(btmp, col="grey", zlim=c(0,1))
dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/hprice_rank.stl")



#######

# SIMD 2012 scores -------------------------------------------------------




simd <- read.csv("data/attributes/00410767.csv", header=T) %>%
    tbl_df %>%
    select(datazone=Data.Zone, simd=Overall.SIMD.2012.Score, simd_rank=Overall.SIMD.2012.Rank)


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


spplot(glas_2001_dz_shp, "simd_rank",
       col.regions=rev(gray(seq(0, 1, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(axis.line = list(col = 'transparent'))
)

# Now to go back and use code for extracting road network
scotland_road <- rgdal::readOGR(    
    "data/shapefiles/scotland-roads-shape", "roads"
)

# transform projection system

scotland_road_reprojected <- spTransform(scotland_road, CRS(proj4string(glas_2001_dz_shp)))
# Robin Lovelace function

gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}


scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(glas_2001_dz_shp))

# trying to add as layer within spplot

bmp("bitmaps/simd_rank_2012_glasgow.bmp", height=1000, width=1310)
spplot(glas_2001_dz_shp, "simd_rank",
       col.regions=rev(gray(seq(0, 0.95, 0.01))),
       col=NA,
       colorkey=FALSE,
       par.settings = list(
           axis.line = list(col = 'transparent'),
           panel.background=list(col=gray(0.95))
           )
) + latticeExtra::layer(
  sp.lines(scotland_road_reprojected_clipped, col = "white", alpha=0.1) 
    )
dev.off()    


btmp <- read.bitmap(f="bitmaps/simd_rank_2012_glasgow.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

# this removes the edges 
rows_are_zero <- apply(btmp, 1, function(x) sum(x)==0)
cols_are_zero <- apply(btmp, 2, function(x) sum(x)==0)
btmp <- btmp[!rows_are_zero,!cols_are_zero]

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")

#persp3d(btmp, col="grey", zlim=c(0,1))
dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/simd_rank.stl")

