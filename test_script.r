# Script for extracting centroids from points
rm(list=ls())

# Example from:
#http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes

#install.packages("GISTools")
require(GISTools)
require(rgdal)
require(plyr)
require(tidyr)
require(dplyr)
require(rgl)
require(fBasics)
require(raster)
require(rgeos)
require(ggplot2)
require(akima)


# City 
rgdal::ogrInfo("data/shapefiles/Glasgow_City", "Glasgow_elect_codes_2010")
glasgow_city <- rgdal::readOGR("data/shapefiles/Glasgow_City", "Glasgow_elect_codes_2010")

# Buildings

rgdal::ogrInfo("data/shapefiles/building_heights_glasgow", "glasgow_p")
glasgow_building_heights <- rgdal::readOGR(
    "data/shapefiles/building_heights_glasgow", "glasgow_p"
    )

# This has no .prj file associated
# use the same crs as glasgow_city
proj4string(glasgow_city)
proj4string(glasgow_building_heights)
proj4string(glasgow_building_heights) <- proj4string(glasgow_city)
proj4string(glasgow_building_heights)


# Now Glasgow singular

glasgow_mask <- gUnionCascaded(glasgow_city)


# plot(glasgow_mask)
# plot(glasgow_building_heights, add=T, col="red", border=NA)
# NOTE : will take ages to run. 
# Projection seems to be appropriate. But needs to be clipped to glasgow mask


glasgow_buildings_in_mask <- gIntersection(spgeom1 = glasgow_mask, spgeom2=glasgow_building_heights)

plot(glasgow_mask)
plot(glasgow_buildings_in_mask, add=T, col="red", border=NA)

tmp <- gWithin(glasgow_building_heights, glasgow_mask, byid=T)

plot(glasgow_mask)
plot(glasgow_building_heights, col=ifelse(tmp, "red", "blue"), border=NA, add=T)


buildings_in_glasgow <- glasgow_building_heights[as.vector(tmp),]
plot(glasgow_mask)
plot(buildings_in_glasgow, col="red", border=NA, add=T)

# Write out 

writeOGR(buildings_in_glasgow, 
         "data/generated_shapefiles/building_heights_glasgow", 
         "building_heights_in_glasgow", driver="ESRI Shapefile"
         )


# Now to do this with some other features
# Scotland Roads Shape

rgdal::ogrInfo("data/shapefiles/scotland-roads-shape", "roads")
scotland_road <- rgdal::readOGR(
    "data/shapefiles/scotland-roads-shape", "roads"
)

# transform projection system
scotland_road_reprojected <- spTransform(scotland_road, CRS(proj4string(glasgow_mask)))

# Robin Lovelace function
gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}

scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(glasgow_mask))

plot(glasgow_mask, border=NA, col="grey")
plot(scotland_road_reprojected_clipped, add=T, col="blue")


tmp <- gWithin(scotland_road_reprojected_clipped, glasgow_mask, byid=T)
roads_in_glasgow <- scotland_road_reprojected_clipped[as.vector(tmp),]

plot(glasgow_mask, border=NA, col="grey")
plot(roads_in_glasgow, add=T, col="blue", alpha=0.3)
plot(buildings_in_glasgow, add=T, col="red", border=NA)

# R base graphics cannot handle transparency properly

# Instead moving to ggplot2 with fortify

glasgow_mask_fortified <- fortify(glasgow_mask)

theme_clean <- function(base_size=12){    
    require(grid)
    theme_grey(base_size) %+replace%
    theme(
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        axis.ticks.length=unit(0, "cm"),
        axis.ticks.margin=unit(0, "cm"),
        panel.margin = unit(0, "lines"),
        plot.margin=unit(c(0, 0, 0, 0), "lines"),
        complete=TRUE
    )
}   
 
ggplot(glasgow_mask_fortified, aes(x=long, y=lat, group=group)) + 
    geom_polygon(colour="grey") + theme_clean() +
    geom_polygon(aes(x=long, y=lat, group=group, colour="black"), data=buildings_in_glasgow_fortified)

buildings_in_glasgow_fortified <- fortify(buildings_in_glasgow)
g2 <- ggplot(buildings_in_glasgow_fortified, aes(x=long,y=lat, group=group)) + geom_polygon()
ggplot() + theme_clean() +
    geom_polygon(
        data=glasgow_mask_fortified, 
        mapping = aes(x=long, y=lat, group=group), 
        colour="grey"
        ) +
    geom_polygon(
        data=buildings_in_glasgow_fortified, 
        mapping=aes(x=long, y=lat, group=group), colour="black", alpha=0.3
        )



# Now, finally, to add the house prices

area_prices <- read.csv("data/attributes/weighted_median_house_prices_2007_2010.csv")

scotland_datazones <- 
    
rgdal::ogrInfo("data/shapefiles/scotland_2001_datazones", "scotland_dz_2001")
scotland_2001_datazones <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_datazones", "scotland_dz_2001"
)
scotland_2001_datazones_reprojected <- spTransform(scotland_2001_datazones, CRS(proj4string(glasgow_mask)))

tmp <- scotland_2001_datazones_reprojected

tmp2@data <- data.frame(
    tmp@data,
    area_prices[match(tmp@data[,"zonecode"], area_prices[,"datazone"]),]
    )

tmp3 <- gWithin(tmp2, glasgow_mask, byid=T)
glasgow_dzs <- tmp2[as.vector(tmp3),]

glasgow_dzs_fortified <- fortify(glasgow_dzs)
tmp <- glasgow_dzs@data
tmp <- data.frame(tmp, id=rownames(tmp))
glasgow_dzs_fortified <- glasgow_dzs_fortified %>% left_join(tmp)


ggplot() + theme_clean() +
    geom_polygon(
        data=glasgow_mask_fortified, 
        mapping = aes(x=long, y=lat, group=group), 
        colour="grey"
    ) +
    geom_polygon(
        data=glasgow_dzs_fortified,
        mapping=aes(x=long, y=lat, group=group, fill=avg_median_price)
    ) + 
geom_polygon(
    data=buildings_in_glasgow_fortified, 
    mapping=aes(x=long, y=lat, group=group), colour="black", alpha=0.3
)



glasgow_tuples <- glasgow_centroids %>% tbl_df() %>%
    left_join(area_prices)

houseprice_rasterized <- rasterize()
houseprice_interpolated <- linearInterp(    
    x=glasgow_tuples$x,
    y=glasgow_tuples$y,
    z=glasgow_tuples$avg_median_price,
    gridPoints=500
)

houseprice_interpolated$z[is.na(houseprice_interpolated$z)] <- 0

dx <- max(houseprice_interpolated$x) - min(houseprice_interpolated$x)
dy <- max(houseprice_interpolated$y) - min(houseprice_interpolated$y)
dz <- max(houseprice_interpolated$z) - min(houseprice_interpolated$z)

houseprice_interpolated$z <- houseprice_interpolated$z *dy/ (dz *5)

surface3d(x=houseprice_interpolated$x, y = houseprice_interpolated$y, z = houseprice_interpolated$z,
          axes=F, box=F, col="white"
)

# Now want to identify when cells are within glasgow_mask

tmp <- houseprice_interpolated$z
dimnames(tmp) <- list(houseprice_interpolated$x, houseprice_interpolated$y)

price_df <- adply(tmp, c(1,2))
names(price_df) <- c("lat", "long", "price")


ggplot() + theme_clean() +
    geom_tile(
        data=price_df, 
        aes(x=lat, y=long, fill=price)
        ) + 
    geom_polygon(
        data=glasgow_mask_fortified, 
        mapping = aes(x=long, y=lat, group=group), 
        colour="grey"
    ) +
    geom_polygon(
        data=buildings_in_glasgow_fortified, 
        mapping=aes(x=long, y=lat, group=group), colour="black", alpha=0.3
    )

######################################################################################



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
    axes=F, box=F, xlab="eastings", "northings", "price"
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


# z is too large relative to x and y
# need to know limits 




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
