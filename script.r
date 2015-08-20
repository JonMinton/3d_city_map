rm(list=ls())


require(repmis)

require(readr)
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
require(readbitmap)

require(ggmap)
# 19-3-2015

# The aim now is to simply produce a series of greyscale images on the same scale, 
# each of a different attribute. There should be no need for interpolation, simply generate 
# the greyscale images, save them as high resolution bitmaps of the same 
# resolution, and then manually merge these files to produce the stl files


# bitmap image approach ---------------------------------------------------


# English cities 


# I should use data I've already processed from UK migrant demography, and 
# link data used elsewhere 


# Life expectancy (From UK migrant demography data)

la_demography <- read_csv("data/attributes/england_and_wales/england_la_count.csv")

# Links to other aggregations

area_links <- source_DropboxData(
    
    
)
# Life expectancy ---------------------------------------------------------


#Life expectancy is available only at IG level rather than DZ level


# source: https://data.glasgow.gov.uk/dataset/life-expectancy-figures-in-males/
# resource/c452a10e-c03a-443c-ad8e-2881862830b5

male_le <- read_csv("data/attributes/life-exp-males-2005-2009.csv")


male_le <- male_le  %>% select(intermed=Intermediate.Geography.Code, e0_male=EOLAB)

dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

big_link <- read.csv("data/latestpcinfowithlinkpc.csv") %>%
    tbl_df

dz_ig_link <- big_link %>% 
    select(datazone=Datazone, intermed=Intermed)
rm(big_link)

igs_in_gcity <- dzs_in_gcity %>%
    left_join(dz_ig_link) 

ig_e0male_gcity <- male_le %>%
    inner_join(igs_in_gcity)%>%
    select(intermed, e0_male) %>%
    unique

rgdal::ogrInfo("data/shapefiles/scotland_2001_intermed", "scotland_igeog_2001")
scot_2001_ig_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_intermed", "scotland_igeog_2001"
)


glas_2001_ig_shp <- scot_2001_ig_shp[scot_2001_ig_shp$zonecode %in% igs_in_gcity$intermed,] 

# attach house prices
glas_2001_ig_shp@data <- merge(glas_2001_ig_shp@data, ig_e0male_gcity, by.x="zonecode", by.y="intermed")
glas_2001_ig_shp@data <- glas_2001_ig_shp@data[c("zonecode", "gid", "label", "e0_male")]


spplot(glas_2001_ig_shp, "e0_male",
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


scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(glas_2001_ig_shp))

# trying to add as layer within spplot

bmp("bitmaps/e0_male_ig_glasgow.bmp", height=1000, width=1310)
spplot(glas_2001_ig_shp, "e0_male",
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

# intermediate geographies overlaid on ggmap

map_reprojected <- spTransform(scot_2001_ig_shp,
                               CRS("+proj=longlat +datum=WGS84"))
                               
map_df <- fortify(map_reprojected)

att_dta <- data.frame(id=rownames(scot_2001_ig_shp@data), zonecode = scot_2001_ig_shp@data$zonecode)
att_dta <- merge(att_dta, ig_e0male_gcity, by.x="zonecode", by.y="intermed", all.y=T)
map_df <- merge(map_df, att_dta, by="id")

map_df <- map_df %>% arrange(group, order)

get_map(location = "Glasgow", zoom=11)  %>% ggmap(extent="panel") + 
    geom_polygon(aes(x=long, y=lat, group=group, fill = e0_male), col="black", alpha=0.3, data=map_df)

ggsave(filename="maps/intermediate_geographies_in_context.png",
       height=30, width=30, dpi=300, units="cm"
       )

get_map(location = "Glasgow", zoom=12)  %>% ggmap(extent="panel") + 
    geom_polygon(aes(x=long, y=lat, group=group, fill = e0_male), col="black", alpha=0.3, data=map_df)

ggsave(filename="maps/intermediate_geographies_in_context_zoomed.png",
       height=30, width=30, dpi=300, units="cm"
)



# Read back lattice bitmap


btmp <- read.bitmap(f="bitmaps/e0_male_ig_glasgow.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

# this removes the edges 
rows_are_zero <- apply(btmp, 1, function(x) sum(x)==0)
cols_are_zero <- apply(btmp, 2, function(x) sum(x)==0)
btmp <- btmp[!rows_are_zero,!cols_are_zero]

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")

dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/e0_male_intermed.stl")




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
require(readbitmap)
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


#  Quick query from Gwilym  -----------------------------------------------

# Need to look at the local authority of glasgow

dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

other_links <- read.csv("data/latestpcinfowithlinkpc.csv", header=T) %>%
    tbl_df

file_for_gwilym <- other_links %>%
    select(postcode=PC8, easting=Grid_Reference_Easting, 
           northing=Grid_Reference_Northing, datazone=Datazone) %>%
    right_join(dzs_in_gcity) %>%
    select(postcode, x=easting, y=northing, datazone)

file_for_gwilym %>%
    write.csv(., file="data/postcode_link_for_gwilym.csv", row.names=F)


# Quality Adjusted House Price Surface from Gwilym ------------------------



const_hp <- read.csv("data/attributes/house_price_quality_adjusted/CQP_2007q3_predicted___collapsed_to_datazone_median.csv") %>%
    tbl_df 

dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 


dz_hp_gcity <- const_hp %>%
    inner_join(dzs_in_gcity)%>%
    select(datazone, sellingp) %>%
    unique

rgdal::ogrInfo("data/shapefiles/scotland_2001_datazones", "scotland_dz_2001")
scot_2001_dz_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_datazones", "scotland_dz_2001"
)

# Borrowing from
# http://stackoverflow.com/questions/3650636/how-to-attach-a-simple-data-frame-to-a-spatialpolygondataframe-in-r

df <- const_hp
df$datazone <- as.character(df$datazone)

sp <- scot_2001_dz_shp
sp$zonecode <- as.character(sp$zonecode)
sp@data$zonecode <- as.character(sp@data$zonecode)

#sp@data <- data.frame(sp@data, df[match(sp@data[,"zonecode"], df[,"datazone"]),])

sp <- sp[sp$zonecode %in% df$datazone,] 

# attach house prices
sp@data <- merge(sp@data, const_hp, by.x="zonecode", by.y="datazone")
sp@data <- sp@data[c("zonecode", "gid", "label", "sellingp")]


spplot(sp, "sellingp",
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

scotland_road_reprojected <- spTransform(scotland_road, CRS(proj4string(sp)))
# Robin Lovelace function

gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}


scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(sp))

# trying to add as layer within spplot

bmp("bitmaps/const_hp_2007.bmp", height=1000, width=1310)
spplot(sp, "sellingp",
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


btmp <- read.bitmap(f="bitmaps/const_hp_2007.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

# this removes the edges 
rows_are_zero <- apply(btmp, 1, function(x) sum(x)==0)
cols_are_zero <- apply(btmp, 2, function(x) sum(x)==0)
btmp <- btmp[!rows_are_zero,!cols_are_zero]

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")

dir.create("stls")
r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/const_hp_2007_dz.stl")




# Identify locations of intermediate geographies --------------------------

dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

big_link <- read.csv("data/latestpcinfowithlinkpc.csv") %>%
    tbl_df

dz_ig_link <- big_link %>% 
    select(datazone=Datazone, intermed=Intermed)
rm(big_link)

igs_in_gcity <- dzs_in_gcity %>%
    left_join(dz_ig_link) 


scot_2001_ig_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_intermed", "scotland_igeog_2001"
)


glas_2001_ig_shp <- scot_2001_ig_shp[scot_2001_ig_shp$zonecode %in% igs_in_gcity$intermed,] 



# GWR Houseprice surface data ---------------------------------------------

#data
dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

dz_hprices <- read.csv("data/attributes/house_price_quality_adjusted/Mediandatazonelevelconstantqualityhouseprice.csv") %>%
    tbl_df


dz_hprices <- dz_hprices  %>% 
    gather(key=year, value=price, -datazone)  %>% 
    mutate(year=str_replace(year, ".price", ""))  %>% 
    separate(col=year, into=c("year", "qtr"), sep="Q")  %>% 
    mutate(year=as.numeric(str_replace(year, "Y", "")), qtr=as.numeric(qtr))

hp_inf <- read.csv("data/attributes/scot_hprice_inflation.csv") %>%
    tbl_df

hp_inf <- hp_inf  %>% 
    select(year=Year, inflation=scot_avg)  %>% 
    mutate(inflation = as.numeric(str_replace(inflation, ",", "")))   %>% 
    mutate(index=inflation/inflation[year==2005])

hp_avg <- dz_hprices   %>% 
    group_by(datazone, year)  %>% 
    summarise(price=mean(price))  %>% 
    left_join(hp_inf)  %>% 
    select(-inflation)  %>% 
    ungroup  %>% 
    mutate(hp_adjust=price*index)  %>% 
    group_by(datazone)  %>% 
    summarise(hprice = mean(hp_adjust))


scot_2001_dz_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_datazones", "scotland_dz_2001"
)

glas_2001_dz_shp <- scot_2001_dz_shp[scot_2001_dz_shp$zonecode %in% dzs_in_gcity$datazone,] 

# attach house prices
glas_2001_dz_shp@data <- merge(glas_2001_dz_shp@data, hp_avg, by.x="zonecode", by.y="datazone")
glas_2001_dz_shp@data <- glas_2001_dz_shp@data[c("zonecode", "gid","ons_code", "label", "hprice")]

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

bmp("bitmaps/quality_adjusted_houseprice_dz.bmp", height=1000, width=1310)
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


btmp <- read.bitmap(f="bitmaps/quality_adjusted_houseprice_dz.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

# this removes the edges 
rows_are_zero <- apply(btmp, 1, function(x) sum(x)==0)
cols_are_zero <- apply(btmp, 2, function(x) sum(x)==0)
btmp <- btmp[!rows_are_zero,!cols_are_zero]

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")



r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/quality_adjusted_houseprice_dz.stl")


# quality adjusted house prices - intermediate geographies ----------------



dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 

big_link <- read.csv("data/latestpcinfowithlinkpc.csv") %>%
    tbl_df

dz_ig_link <- big_link %>% 
    select(datazone=Datazone, intermed=Intermed)
rm(big_link)

igs_in_gcity <- dzs_in_gcity %>%
    left_join(dz_ig_link) 

hp_avg_ig <- hp_avg  %>% 
    left_join(dz_ig_link)  %>% 
    group_by(intermed)  %>% 
    summarise(hprice=mean(hprice))  %>% 
    ungroup

scot_2001_ig_shp <- rgdal::readOGR(
    "data/shapefiles/scotland_2001_intermed", "scotland_igeog_2001"
)


glas_2001_ig_shp <- scot_2001_ig_shp[scot_2001_ig_shp$zonecode %in% igs_in_gcity$intermed,] 

# attach house prices
glas_2001_ig_shp@data <- merge(glas_2001_ig_shp@data, hp_avg_ig, by.x="zonecode", by.y="intermed")
glas_2001_ig_shp@data <- glas_2001_ig_shp@data[c("zonecode", "gid", "label", "hprice")]


# Now to go back and use code for extracting road network
scotland_road <- rgdal::readOGR(    
    "data/shapefiles/scotland-roads-shape", "roads"
)

# transform projection system

scotland_road_reprojected <- spTransform(scotland_road, CRS(proj4string(glas_2001_ig_shp)))
# Robin Lovelace function

gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}

scotland_road_reprojected_clipped <- gClip(shp=scotland_road_reprojected, bb=bbox(glas_2001_ig_shp))

# trying to add as layer within spplot

bmp("bitmaps/quality_adjusted_houseprice_ig.bmp", height=1000, width=1310)
spplot(glas_2001_ig_shp, "hprice",
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


btmp <- read.bitmap(f="bitmaps/quality_adjusted_houseprice_ig.bmp")
btmp <- btmp / max(btmp)
btmp <- 1 - btmp

# this removes the edges 
rows_are_zero <- apply(btmp, 1, function(x) sum(x)==0)
cols_are_zero <- apply(btmp, 2, function(x) sum(x)==0)
btmp <- btmp[!rows_are_zero,!cols_are_zero]

surface3d(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp*100, col="lightgrey")



r2stl(x=1:nrow(btmp), y=1:ncol(btmp), z=btmp, z.expand=T, file="stls/quality_adjusted_houseprice_ig.stl")


