# find life expectancy data 

require(repmis)
require(plyr)
require(tidyr)
require(dplyr)


# let's go for SIMD rank

simd <- read.csv("data/attributes/00410767.csv", header=T) %>%
    tbl_df

simd <- simd %>%
    select(datazone=Data.Zone, simd=Overall.SIMD.2012.Score)


dz_to_la <- read.csv("data/la_to_dz.csv", header=T) %>%
    tbl_df

dzs_in_gcity <- dz_to_la %>%
    filter(local_authority=="Glasgow City") 


dz_simd_gcity <- simd %>%
    inner_join(dzs_in_gcity) %>%
    select(datazone, simd)
