###### An imputing procedure

require(dplyr)
require(plyr)
require(maptools)
require(spdep)


# import the datazone shapefile with a correct geographcial topology
datazone_shp <- readShapePoly("C:/Users/Guanpeng/Dropbox/Scotland_dz_2001/datazone_correct.shp")
row.names(datazone_shp) <- as.character(datazone_shp$datazone)
# assign the correct projection
proj4string(datazone_shp) <- CRS("+init=epsg:27700")

names(datazone_shp)
str(datazone_shp@data)
datazone_shp$datazone <- as.character(datazone_shp$datazone)

# datazones in Glasgow
index <- read.csv("dzs_in_gcity.csv",header=TRUE) %>% tbl_df()
index <- as.character(index$datazone)
# datazones without house transactions
index.1 <- read.csv("datazones_without_samples.csv",header=TRUE)
index.1 <- as.character(index.1$datazone)
# check where there are no house transactions
glasgow.shp <- datazone_shp[row.names(datazone_shp) %in% index,]
plot(glasgow.shp)
nodata <- glasgow.shp[row.names(glasgow.shp) %in% index.1,]
plot(glasgow.shp)
plot(nodata,col="red",add=TRUE)

# create a neighbour list for all datazones in Glasgow
# row names of data zones used for selection
Rnames <- row.names(glasgow.shp)
nbs <- poly2nb(glasgow.shp,row.names=Rnames,queen=FALSE)

# neighbours for datazones without house transactions
neighbours <- lapply(nbs[attr(nbs,"region.id") %in% index.1], function(x) Rnames[x])

#### a simple imputing function, which will be used in ddply function

imputFun <- function(data1) {
  # extract the rownames of neighbours
  res <- sapply(neighbours,function(x) mean(data1$price[data1$datazone %in% x]))
  # calculate mean values of neighbours
  return(res)
}

## import predicted house prices at the datazone level --- Note, these datazones are those with hosue transactions
# median price
data <- read.csv("median_price_year_quater.csv",header=TRUE) %>% tbl_df()
data$X <- NULL
# a long form
data.L <- data %>% gather("period","price",2:21)

# use the ddply function
price.datazone.without <- ddply(data.L, "period",imputFun)

# Transpose the data for combining with the other datazones
tt <- price.datazone.without[,1]
t.temp <- t(price.datazone.without[,-1])
t.temp <- data.frame(t.temp)
names(t.temp) <- tt
row.names(t.temp) <- NULL
t.temp <- data.frame(datazone=index.1,t.temp)

#### final house price prediction for all datazones

final.data <- rbind(data,t.temp)
write.csv(final.data,file="Mediandatazonelevelconstantqualityhouseprice.csv",row.names=FALSE)

# meam house price

data <- read.csv("mean_price_year_quater.csv",header=TRUE) %>% tbl_df()
data$X <- NULL
# a long form
data.L <- data %>% gather("period","price",2:21)

# use the ddply function
price.datazone.without <- ddply(data.L, "period",imputFun)

# Transpose the data for combining with the other datazones
tt <- price.datazone.without[,1]
t.temp <- t(price.datazone.without[,-1])
t.temp <- data.frame(t.temp)
names(t.temp) <- tt
row.names(t.temp) <- NULL
t.temp <- data.frame(datazone=index.1,t.temp)

#### final house price prediction for all datazones

final.data <- rbind(data,t.temp)
write.csv(final.data,file="Meandatazonelevelconstantqualityhouseprice.csv",row.names=FALSE)
