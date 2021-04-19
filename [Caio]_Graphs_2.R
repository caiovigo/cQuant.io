
library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr) 
library(magrittr)
library(data.table)
library(readxl)
library(ggplot2)
library(ggridges)
library(gridExtra)
# library(PerformanceAnalytics)   # To use chart.Correlation()
library(zoo)                    # For time series [using only as.Date() & as.yearmon()]
library(corrplot)               # To use corrplot()
library(gt)
# library(haven)                  # To read sas files
library(stargazer)

my_path <- "C:/Users/caioa/OneDrive - The University of Kansas/Documents/PhD/13. WPs/12. Sparse Signals (LASSO) in Exchange Rates/2.Empirical/"
my_path_merged_tables <- "G:/My Drive/Documents/PhD/13. WPs/12. Sparse Signals (LASSO) in Exchange Rates/2.Empirical/11.Merged/"
currency_pairs_list <- c("AUDUSD", "EURGBP", "EURUSD", "NZDUSD", "USDCHF", "USDSEK", "EURCHF", "EURJPY", "GBPUSD", "USDCAD", "USDJPY")
years_list <- c(2019, 2018, 2017, 2016)  
weekdays_list <- tibble(
                        weekday_name = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                        weekday_code = 1:7
                      )

merged_currency_pair <- NULL

for (currency_pairs_j in currency_pairs_list){
  my_data <- NULL
  
  for (year_j in years_list){
    my_data <- bind_rows(my_data,
                         read_csv(paste0(my_path_merged_tables,
                                         year_j,
                                         "/", 
                                         currency_pairs_j, 
                                         "/merged_", 
                                         currency_pairs_j, 
                                         ".csv"))
    )
  }

  my_data %<>%
    rename("Open_Bid" = as.name(paste0("Open_", currency_pairs_j, "_Bid")),
           "Open_Ask" = as.name(paste0("Open_", currency_pairs_j, "_Ask"))) %>%
    mutate(
      # time_est=hms(format(est, "%H:%M:%S")),
      currency_pair = currency_pairs_j,
      dummy_30_min_interval = case_when(lubridate::minute(time)==0 | lubridate::minute(time)==30 ~ 1),
      dummy_5_min_interval = case_when(lubridate::minute(time)==0 | 
                                         lubridate::minute(time)==5 |
                                         lubridate::minute(time)==10 |
                                         lubridate::minute(time)==15 |
                                         lubridate::minute(time)==20 |
                                         lubridate::minute(time)==25 |
                                         lubridate::minute(time)==30 |
                                         lubridate::minute(time)==35 |
                                         lubridate::minute(time)==40 |
                                         lubridate::minute(time)==45 | 
                                         lubridate::minute(time)==50 |
                                         lubridate::minute(time)==55 ~ 1),
      mid_price=(Open_Bid + Open_Ask)/2,
      log_mid_price_ret=(log(mid_price/lag(mid_price))),
      log_mid_price_ret_annual=( ((1+log_mid_price_ret)^(60*24*6*52))-1)*100) %>%
    mutate_at(vars(dummy_30_min_interval, dummy_5_min_interval), function(x) ifelse(is.na(x)==TRUE, 0, x)) %>% 
    rename("obs_n"="X1") %>% 
    select(obs_n, currency_pair, date_time, date, est, gmt, time, dummy_30_min_interval, dummy_5_min_interval, 
           forecast, log_mid_price_ret, log_mid_price_ret_annual, mid_price, dplyr::everything())


  merged_currency_pair <- bind_rows(merged_currency_pair, my_data)
  # assign(paste0("merged_",currency_pairs_j), my_data)
}

rm(my_data)

#--------------------------------------------------------------------------

# mid_price X days
get(currency_pair) %>% 
  # filter(month(date_time)<5) %>%
  ggplot(aes(mid_price, date(date_time), group = date(date_time), height = ..density..)) + 
  geom_density_ridges(stat = "density", trim = TRUE) + 
  coord_flip()

# log_mid_price_ret X days
get(currency_pair)%>% 
  na.omit() %>% 
  ggplot(aes(log_mid_price_ret*100, date(date_time), group = date(date_time), height = ..density..)) + 
  geom_density_ridges(stat = "density", trim = TRUE, scale=3) + 
  xlim(-.05,.05) +
  coord_flip()

#--------------------------------------------------------------------------
# Mean Annualized Return per hour (2016-2019)
mean_per_hour_per_currency_pair <-    
  merged_currency_pair %>%
    filter(dummy_5_min_interval == 1) %>% 
    group_by(currency_pair) %>%
    mutate(mid_price_5=(Open_Bid+Open_Ask)/2,
           log_mid_price_ret_5=(log(mid_price_5/lag(mid_price_5))),
           log_mid_price_ret_annual_5=( ((1+log_mid_price_ret_5)^(12*24*250))-1)*100) %>% 
    na.omit() %>% 
    filter(log_mid_price_ret_annual!="Inf") %>%
    mutate(hour_est=hour(est)) %>% 
    group_by(currency_pair, hour_est) %>%
    # filter(hour_est==16) %>% 
    # select(log_mid_price_ret ) %>% 
    summarize(n=n(),
              mean_log_mid_price_ret_5 = mean(log_mid_price_ret_5),
              mean_log_mid_price_ret_annual_5 = ( ((1+mean_log_mid_price_ret_5)^(12*24*250))-1),
              sd_log_mid_price_ret_5 = sd(log_mid_price_ret_5),
              se_log_mid_price_ret_5 = sd_log_mid_price_ret_5/sqrt(n),
              sd_log_mid_price_ret_annual_5 = ( ((1+se_log_mid_price_ret_5)^(12*24*250))-1),
              crit_val_95ci = qt(0.025, df=n-1, lower.tail = FALSE, log.p = FALSE)
    ) %>% 
    ungroup() 

mean_per_hour_per_currency_pair %>% 
  mutate_at(vars(hour_est), function(x) parse_date_time(paste0("01-01-01 ", x, ":00:00"), "ymd %H:%M:%S")) %>% 
  ggplot(aes(hour_est, mean_log_mid_price_ret_annual_5*100)) +
  facet_wrap(.~currency_pair, scales = "free", ncol = 3) +
  theme_light() +
  geom_line(size=1.3) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_ribbon(aes(ymin =  mean_log_mid_price_ret_annual_5*100 - crit_val_95ci*sd_log_mid_price_ret_annual_5*100, 
                  ymax =  mean_log_mid_price_ret_annual_5*100 + crit_val_95ci*sd_log_mid_price_ret_annual_5*100), fill = "grey", alpha = .5) +
  scale_x_datetime(breaks = "2 hour", date_labels = "%H:%M") + #, expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid = element_blank(),
        axis.text.x=element_text(colour="black", angle = 90),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(title = "Mean per hour (2016-2019)", x = "Hour (EST)", y = "Mean Annualized Return per hour (2016-2019)")
  # ggsave(paste0(".pdf"), width = 5.75, height = 4.5)


#--------------------------------------------------------------------------
# Mean Annualized Return per Weekday (2016-2019)
mean_per_Weekday_per_currency_pair <-    
  merged_currency_pair %>%
    filter(dummy_5_min_interval == 1) %>% 
    group_by(currency_pair) %>%
    mutate(mid_price_5=(Open_Bid+Open_Ask)/2,
           log_mid_price_ret_5=(log(mid_price_5/lag(mid_price_5))),
           log_mid_price_ret_annual_5=( ((1+log_mid_price_ret_5)^(12*24*250))-1)*100) %>% 
    na.omit() %>% 
    filter(log_mid_price_ret_annual!="Inf") %>%
    group_by(currency_pair, wday) %>%
    summarize(n=n(),
              mean_log_mid_price_ret_5 = mean(log_mid_price_ret_5),
              mean_log_mid_price_ret_annual_5 = ( ((1+mean_log_mid_price_ret_5)^(12*24*250))-1),
              sd_log_mid_price_ret_5 = sd(log_mid_price_ret_5),
              se_log_mid_price_ret_5 = sd_log_mid_price_ret_5/sqrt(n),
              sd_log_mid_price_ret_annual_5 = ( ((1+se_log_mid_price_ret_5)^(12*24*250))-1),
              crit_val_95ci = qt(0.025, df=n-1, lower.tail = FALSE, log.p = FALSE)
    ) %>% 
    ungroup() 

mean_per_Weekday_per_currency_pair %>%
  left_join(weekdays_list, by=c("wday"="weekday_code")) %>% 
  mutate_at(vars(weekday_name), function(x) as.factor(x)) %>% 
  filter(wday!=7) %>% 
  # mutate_at(vars(hour_est), function(x) parse_date_time(paste0("01-01-01 ", x, ":00:00"), "ymd %H:%M:%S")) %>% 
    ggplot(aes(wday, mean_log_mid_price_ret_annual_5*100)) +
    facet_wrap(.~currency_pair, scales = "free", ncol = 3) +
    theme_light() +
    geom_line(size=1.3) + 
    scale_y_continuous(expand = c(0, 0)) +
    geom_ribbon(aes(ymin =  mean_log_mid_price_ret_annual_5*100 - crit_val_95ci*sd_log_mid_price_ret_annual_5*100, 
                    ymax =  mean_log_mid_price_ret_annual_5*100 + crit_val_95ci*sd_log_mid_price_ret_annual_5*100), fill = "grey", alpha = .5) +
    scale_x_datetime(breaks = "2 hour", date_labels = "%H:%M") + #, expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.grid = element_blank(),
          axis.text.x=element_text(colour="black", angle = 90),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(title = "Mean per hour (2016-2019)", x = "Hour (EST)", y = "Mean Annualized Return per Weekday (2016-2019)")
  # ggsave(paste0(".pdf"), width = 5.75, height = 4.5)

#--------------------------------------------------------------------------
# log_mid_price_ret X hour    
merged_currency_pair %>% 
  mutate(hour_est=hour(est)) %>%
  na.omit() %>% 
  ggplot(aes(log_mid_price_ret*100, hour_est, group = hour_est, height = ..density..)) + 
  # geom_density_ridges(stat = "density", trim = TRUE, scale=5, alpha=.6, fill="red") +
  facet_wrap(.~currency_pair, scales = "free") +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5, trim = TRUE, scale=5, alpha=.6, fill="red") +
  xlim(-.03,.03) +
  coord_flip()


# log_mid_price_ret X wday
merged_currency_pair %>% 
  na.omit() %>% 
  ggplot(aes(log_mid_price_ret*100, wday, group = wday, height = ..density..)) + 
  facet_wrap(.~currency_pair, scales = "free") +
  geom_density_ridges(stat = "density", trim = TRUE, scale=3, alpha=.6, fill="red") + 
  xlim(-.5,.5) #+
# coord_flip()

merged_currency_pair %>% 
  na.omit() %>% 
  ggplot(aes(log_mid_price_ret*100, wday, group = wday)) +
  facet_wrap(.~currency_pair, scales = "free") +
  geom_boxplot()















merged_currency_pair %>% 
  filter(currency_pair=="AUDUSD") %>% 
  filter(dummy_5_min_interval == 1) %>% 
  mutate(mid_price_5=(Open_Bid+Open_Ask)/2,
         log_mid_price_ret_5=(log(mid_price_5/lag(mid_price_5))),
         log_mid_price_ret_annual_5=( ((1+log_mid_price_ret_5)^(12*24*250))-1)*100) %>% 
  na.omit() %>% 
  filter(log_mid_price_ret_annual!="Inf") %>% 
  mutate(hour_est=hour(est)) %>% 
  group_by(currency_pair, hour_est) %>%
  summarize(mean_log_mid_price_ret_5 = mean(log_mid_price_ret_5),
            mean_log_mid_price_ret_annual_5 = ( ((1+mean_log_mid_price_ret_5)^(12*24*250))-1)) %>%
  # select(mid_price_5, log_mid_price_ret_5, log_mid_price_ret_annual_5, hour_est, mid_price, indicator, Open_Bid, Open_Ask, 
         # ar_prediction, bic_prediction, rw_prediction, forecast, log_mid_price_ret, log_mid_price_ret_annual, everything()) %>% 
  ggplot(aes(hour_est, mean_log_mid_price_ret_annual_5)) +
    geom_line()

mean_per_hour_per_currency_pair <-    
  merged_currency_pair %>%
  filter(dummy_5_min_interval == 1) %>% 
  mutate(mid_price_5=(Open_Bid+Open_Ask)/2,
         log_mid_price_ret_5=(log(mid_price_5/lag(mid_price_5))),
         log_mid_price_ret_annual_5=( ((1+log_mid_price_ret_5)^(12*24*250))-1)*100) %>% 
  na.omit() %>% 
  filter(log_mid_price_ret_annual!="Inf") %>%
  mutate(hour_est=hour(est)) %>% 
  group_by(currency_pair, hour_est) %>%
  # filter(hour_est==16) %>% 
  # select(log_mid_price_ret ) %>% 
  summarize(mean_log_mid_price_ret_5 = mean(log_mid_price_ret_5),
            mean_log_mid_price_ret_annual_5 = ( ((1+mean_log_mid_price_ret_5)^(12*24*250))-1)
  ) %>% 
  ungroup() 

mean_per_hour_per_currency_pair %>% 
  ggplot(aes(hour_est, mean_log_mid_price_ret_annual_5*100)) +
  facet_wrap(.~currency_pair, scales = "free") +
  geom_line()
