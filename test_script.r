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

glasgow_single <- readShapePoly(
    fn="data/clipped/glasgow_single"
)

points_to_check <- expand.grid(
    x=interpolated$x,
    y=interpolated$y
    )

fn <- function(X){
    this_coord <- data.frame(lon=X["x"], lat=X["y"])
    this_point <- SpatialPoints(this_coord)
    out <- gContains(glasgow_single, this_point)
    out <- data.frame(x=X["x"], y=X["y"], val=out)
    return(out)
}

vals <- adply(points_to_check, 1, fn, .progress="text")
vals$val <- as.numeric(vals$val)

val_list <- list(
    x=interpolated$x,
    y=interpolated$y,
    z= vals %>% tbl_df() %>% spread(key=y, value=val) %>% select(-x) %>% as.matrix()
    )
dimnames(val_list$z) <- NULL


surface3d(x=val_list$x, y = val_list$y, z = val_list$z * 100,
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


road_lines <- readShapeLines(
    "data/shapefiles/major-roads-link-network-2013/Major_roads_link_network2013"
    )

road_lines_in_glasgow <- gIntersection(road_lines, glasgow_single)


