rm(list=ls())

# Prepare data for adding to qgis
require(repmis)
require(dplyr)


# median house prices

# median house prices in 2010
data_house <- source_DropboxData(
    file="house_sales_and_prices.csv",
    key="uvttugwad87uleb"
    ) %>% tbl_df()


data_house <- data_house %>% 
    filter(year>=2007 & year <=2010) %>%
    select(datazone, year, median_price=HO.hpricemedian, number_of_sales=HO.hsalesno)

# Want a weighted average of median prices by number of sales
data_house_weighted <- data_house %>% 
    mutate(product=median_price * number_of_sales) %>% 
    group_by(datazone) %>%
    summarise(avg_median_price = sum(product) / sum(number_of_sales))
data_house_weighted$avg_median_price[is.na(data_house_weighted$avg_median_price)] <- 0


write.csv(data_house_weighted,
          file="data/attributes/weighted_median_house_prices_2007_2010.csv", 
          row.names=F
          )

# life expectancy at birth

