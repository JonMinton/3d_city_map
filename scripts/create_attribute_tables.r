rm(list=ls())

# Prepare data
require(repmis)
require(dplyr)
require(stringr)

# median house prices

# median house prices in 2010
data_house <- source_DropboxData(
    file="house_sales_and_prices.csv",
    key="uvttugwad87uleb"
    ) %>% tbl_df()


data_house <- data_house %>% 
    filter(year>=2005 & year <=2010) %>%
    select(datazone, year, median_price=HO.hpricemedian, number_of_sales=HO.hsalesno)

# Want a weighted average of median prices by number of sales
data_house_weighted <- data_house %>% 
    mutate(product=median_price * number_of_sales) %>% 
    group_by(datazone) %>%
    summarise(avg_median_price = sum(product) / sum(number_of_sales))
data_house_weighted$avg_median_price[is.na(data_house_weighted$avg_median_price)] <- 0


write.csv(data_house_weighted,
          file="data/attributes/weighted_median_house_prices_2005_2010.csv", 
          row.names=F
          )


# Let's think how to represent the data most meaningfully: 

data_hprice_inf <- read.csv("data/attributes/scot_hprice_inflation.csv") %>%
    tbl_df
data_hprice_inf$scot_avg <- data_hprice_inf$scot_avg %>%
    str_replace_all(",", "") %>%
    as.numeric
data_hprice_inf <- data_hprice_inf %>%
    rename(year=Year)

lt_rel_avg <- data_house %>%
    left_join(data_hprice_inf) %>%
    select(year, datazone, hp_mean=HO.hpricemean, hp_med=HO.hpricemedian,
           num_sales =HO.hsalesno, scot_avg_price=scot_avg) %>%
    mutate(adj_mean = ifelse(num_sales==0, NA, hp_mean/scot_avg_price),
           adj_med = ifelse(num_sales==0, NA, hp_med/scot_avg_price)
    ) %>%
    group_by(datazone) %>% 
    summarise(
        lt_mean = sum(adj_mean * num_sales, na.rm=T) / sum(num_sales, na.rm=T),
        lt_med = sum(adj_med * num_sales, na.rm=T) / sum(num_sales, na.rm=T)
    )

write.csv(lt_rel_avg,
          file="data/attributes/long_term_relative_house_prices.csv", 
          row.names=F
)

# percentage of children in poverty?

data_chpov <- source_DropboxData(
    file="children_in_low_income_househol.csv",
    key="wigibl94qcg0q0b"
) %>% tbl_df()

chpov_2010 <- data_chpov %>%
    filter(year==2010) %>%
    rename(child_pov_pc=CS.CHILD_POV_R) %>%
    select(-year)

write.csv(chpov_2010,
          file="data/attributes/percent_children_in_poverty_2010.csv", 
          row.names=F
)
