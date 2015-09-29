# ggmap choropleth of glasgow

rm(list=ls())

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

require(ggmap)
# 19-3-2015

# The aim now is to simply produce a series of greyscale images on the same scale, 
# each of a different attribute. There should be no need for interpolation, simply generate 
# the greyscale images, save them as high resolution bitmaps of the same 
# resolution, and then manually merge these files to produce the stl files


# bitmap image approach ---------------------------------------------------




# Life expectancy ---------------------------------------------------------


#Life expectancy is available only at IG level rather than DZ level


# source: https://data.glasgow.gov.uk/dataset/life-expectancy-figures-in-males/
# resource/c452a10e-c03a-443c-ad8e-2881862830b5

male_le <- read.csv("data/attributes/life-exp-males-2005-2009.csv") %>%
    tbl_df 

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

glas_2001_ig_shp <- spTransform(glas_2001_ig_shp, CRS("+proj=longlat +datum=WGS84"))
# Link based on https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles

glas_2001_ig_shp@data$id = rownames(glas_2001_ig_shp@data)
dta_pts = fortify(glas_2001_ig_shp, region="id")
glas_df = join(dta_pts, glas_2001_ig_shp@data, by="id")

map <- ggmap(get_map("glasgow", maptype= "hybrid", zoom = 11))
map + geom_polygon(aes(x = long, y = lat, group = id, fill = e0_male), data = glas_df, alpha = 0.7)

ggsave(filename = "maps/ggmap_choropleth.png", dpi = 300, width = 30, height = 30, units = "cm")
# spplot(glas_2001_ig_shp, "e0_male",
#        col.regions=rev(gray(seq(0, 1, 0.01))),
#        col=NA,
#        colorkey=FALSE,
#        par.settings = list(axis.line = list(col = 'transparent'))
# )



