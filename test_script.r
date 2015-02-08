# Script for extracting centroids from points
rm(list=ls())

# Example from:
#http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes

#install.packages("GISTools")
require(GISTools)
require(plyr)
require(tidyr)
require(dplyr)
require(rgl)
require(fBasics)
require(raster)


glasgow_shapes <- readShapePoly(
    fn="data/clipped/glasgow_clipped"
    )

# Prices is not correct here; it needs to be merged again from the source file


area_prices <- read.csv("data/attributes/weighted_median_house_prices_2007_2010.csv")

glasgow_shapes@data <- data.frame(
    glasgow_shapes@data,
    area_prices[match(glasgow_shapes@data[, "zonecode"], area_prices[,"datazone"]),]
    )

glasgow_centroids <- gCentroid(glasgow_shapes, byid=TRUE, id=glasgow_shapes$zonecode)
glasgow_centroids <- as.data.frame(glasgow_centroids)
glasgow_centroids <- data.frame(zonecode=rownames(glasgow_centroids), glasgow_centroids)
rownames(glasgow_centroids) <- NULL 



glasgow_tuples <- glasgow_centroids %>% tbl_df() %>%
    rename(datazone=zonecode) %>% 
    left_join(area_prices)


plot3d(x=glasgow_tuples$x, y = glasgow_tuples$y, z = glasgow_tuples$avg_median_price,
    axes=F, box=F
    )


#krige to make a regular lattice

glasgow_tuples %>% summarise(min_x=min(x), max_x=max(x), min_y=min(y), max_y=max(y))

interpolated <- linearInterp(    
    x=glasgow_tuples$x,
    y=glasgow_tuples$y,
    z=glasgow_tuples$avg_median_price,
    gridPoints=500
)
# to specify precise points use linearInterpp (extra p)
# and the xo and yo arguments

interpolated$z[is.na(interpolated$z)] <- 0

surface3d(x=interpolated$x, y = interpolated$y, z = interpolated$z,
       axes=F, box=F
)

# z is too large relative to x and y
# need to know limits 
dx <- max(interpolated$x) - min(interpolated$x)
dy <- max(interpolated$y) - min(interpolated$y)
dz <- max(interpolated$z) - min(interpolated$z)

interpolated$z <- interpolated$z *dy/ (dz *5)

surface3d(x=interpolated$x, y = interpolated$y, z = interpolated$z,
          axes=F, box=F, col="white"
)


# Now Glasgow singular

glasgow_many_areas <- readOGR(
   dsn="data/shapefiles/Glasgow_City",
   "Glasgow_elect_codes_2010"
)

glasgow_single_area <- gUnionCascaded(glasgow_many_areas)

points_to_check <- as(raster(extent(glasgow_single_area), nrows=200, ncols=200), "SpatialPoints")
proj4string(points_to_check) <- proj4string(glasgow_single_area)

point_within <- gWithin(points_to_check, glasgow_single_area, byid=T)

plot(glasgow_single_area)
points(points_to_check, col=1+point_within)

vals <- data.frame(
    x=points_to_check@coords[,"x"],
    y=points_to_check@coords[,"y"],
    z=point_within[1,]
    )
vals_matrix <- vals %>% spread(key=y, value=z)
rownames(vals_matrix) <- vals_matrix$x
vals_matrix <- vals_matrix[,-1]
vals_matrix <- as.matrix(vals_matrix)

vals_list <- list(
    x=rownames(vals_matrix) %>% as.numeric(),
    y=colnames(vals_matrix) %>% as.numeric(),
    z=vals_matrix %>% as.numeric()
)
dimnames(vals_list$z) <- NULL
surface3d(x=vals_list$x, y = vals_list$y, z = vals_list$z * 3000,
          axes=F, box=F, col="white"
)

masked_list <- list(
    x=interpolated$x,
    y=interpolated$y,
    z = val_list$z * interpolated$z 
    )


surface3d(x=masked_list$x, y = masked_list$y, z = masked_list$z,
          axes=F, box=F, col="grey"
)


road_lines <-  readOGR(
    dsn="data/shapefiles/major-roads-link-network-2013",
    "Major_roads_link_network2013"
)
proj4string(road_lines) <- proj4string(glasgow_single_area)

road_lines_in_glasgow <- gIntersection(road_lines, glasgow_single_area)

tmp <- raster(road_lines_in_glasgow, nrows=200, ncols=200)

tmp2 <- rasterize(
    x=road_lines,
    y=tmp
    )


## Required packages 
library(rgdal)
library(raster)  ## For example polygon & functions used to make example points
library(rgeos)   

## Reproducible example
poly <- readOGR(system.file("external", package="raster"), "lux")[1,]
points <- as(raster(extent(poly)), "SpatialPoints")
proj4string(points) <- proj4string(poly)

## Test which points fall within polygon 
win <- gWithin(points, poly, byid=TRUE)

## Check that it works
plot(poly)
points(points, col=1+win)
