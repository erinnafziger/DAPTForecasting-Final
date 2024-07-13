library("fpp3")
library("readr")
library("gridExtra")

#1.Explore the Data
#1.a Summarize, Measures of Central Tendency ----------
#Read in data
tuna <- read.csv("tokyo_wholesale_tuna_prices.csv") #update file path

#Determine if there are missing values
na_values <- which(is.na(tuna), arr.ind = TRUE)
print(na_values) ## No missing data

#View Data Summary and Distinct Values of categorical variables
tuna
summary(tuna)
distinct(tuna,species, fleet, state)

###COMMENTS:
#The data available covers monthly wholesale prices and  auctioned quantities of tuna covering 
#the years 2003 to 2016. Measurements in the data measure  given in two different metrics: 
#price is expressed in Japanese yen per kilogram, while quantity is expressed in metric tonnes 
#or 1,000 kilograms.Limited availability of market data and the complexity of fishery harvesting behaviors
#pose challenges to conducting a market analysis. In this study, monthly auction data
#from the Tokyo Central Wholesale Market (Tsukiji Market) are used.Data serve as the basis for 
#time-series analysis to identify trends and seasonality in tuna pricing and sales volumes over 
#the 14-year period. At Tsukiji Market, different species (Bluefin Tuna,Southern bluefin and Bigeye Tuna) 
#and product forms (fresh and frozen) of tuna are auctioned separately and treated as different products. 
#There is also a clear distinction between Japanese fleet,Foreign and unknown fleet products.

#Create separate columns for Quantity and Price
tuna <- tuna %>%
  pivot_wider(names_from = measure, values_from = value)


#Create tsibble
tuna <- tuna %>% 
  mutate(Month = yearmonth(sprintf("%04d-%02d", year, month))) %>% 
  select(Month, state, species, fleet, Quantity, Price) %>%
  as_tsibble(index=Month, key = c(species, state, fleet))


#View summary statistics 
summary_stats <- tuna %>% as.data.frame(tuna) %>%
  group_by(species,state,fleet) %>%
  summarise(
    Avg_Qty = mean(Quantity),
    Stdev_Qty = sd(Quantity),
    min_Qty = min(Quantity),
    max_Qty = max(Quantity),
    range = max_Qty -  min_Qty,
    n = n()
  )   

summary_stats  #all in metric tonnes

###COMMENTS:
#Focusing on Quantity is useful for planning production operations, 
#streamlining the supply chain, and managing inventory—especially for 
#companies that process or sell fish.Predicting the amount of fish that 
#will be caught or needed can help businesses in the fishing industry and 
#associated sectors allocate resources like manpower, boats, and fuel.
#In our models forecasting parameter  will be Quantity and will decide 
#indenpendet paarameters influence in later stage.

#Based on mean and stdev,range there significant variability and complexity in the tuna market. 
#For example, Bluefin Tuna, when fresh and handled by the Japanese Fleet, averages 250MT  
#per month with a high standard deviation of 123MT. This implies  
#Monthly auctioned  qty fluctuations likely driven by seasonal variations, 
#fishing success, market demand, and regulatory impacts.This variation explains 
#necessity of robust supply analysis.


#1.b View trends between Quantity and Sales ----------
#Read in Historical Daily USD/YEN exchange rate
exchange_data <- read.csv("currency_exchange_rates.csv")
summary(exchange_data)

#Convert to tsibble
exchange_rates <- exchange_data %>%
  mutate(Date = mdy(Date)) %>%
  mutate(YearMonth = yearmonth(Date)) %>%
  group_by(YearMonth) %>%
  summarise(MonthlyAvgRate = mean(USD.YEN, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(index = YearMonth)

#Filtered to  Jan 2003 to Dec 2016 -to match rows with tuna data.
exchange_rates_filtered <- exchange_rates %>%
  filter(YearMonth >= yearmonth("2003-01-01") & YearMonth <= yearmonth("2016-12-31"))

#Determine if there are missing values
na_values_n <- which(is.na(exchange_rates_filtered), arr.ind = TRUE)
print(na_values_n) #No missing data

#View exchange rate monthly trend
exchange_rates_filtered %>% autoplot(MonthlyAvgRate)
exchange_rates_filtered %>% gg_subseries(MonthlyAvgRate)

###COMMENTS:
#Exchange rate dropped drastically from 2007 till 2012,Then increased again.


#Price Yen/Kilogram converted to USD based on respective month exchange rates.
tuna_ts <- tuna %>%
  left_join(exchange_rates_filtered, by = c("Month" = "YearMonth")) %>%
  mutate(Price_USD =  Price / MonthlyAvgRate ) %>%  #Price_USD is Dollar/Kilogram fish
  select(-MonthlyAvgRate)

#Yearly summary of auctioned quantity by year and species,Total in 10Million USD,Billion Yen
year_summary <- 
  tuna_ts %>%
  index_by(Year = year(Month)) %>%
  group_by(Year,species, .add = TRUE) %>%  
  summarise(total_quantity_1000MT = sum(Quantity)/1000, 
            USD_10Million = sum(Quantity* Price_USD * 1000)/10000000, 
            yen_Billion = sum(Quantity* Price* 1000 ) / 1000000000,.groups = "drop")

year_summary 

#Yearly summary of auctioned quantity by year,Total in 10Million USD,Billion Yen
year_summary_TOTAL <-
  tuna_ts %>%
  index_by(Year = year(Month)) %>%
  group_by(Year,.add = TRUE) %>%  # Group by Year and other key ,# Remove other key elements in group_by to get total quantity in MT per year
  summarise(total_quantity_1000MT = sum(Quantity)/1000, 
            USD_10Million = sum(Quantity* Price_USD * 1000)/10000000, 
            yen_Billion = sum(Quantity* Price* 1000 ) / 1000000000,.groups = "drop")

year_summary_TOTAL


#Plot to see Qty,Price trend over the years.
year_summary_TOTAL %>%
  pivot_longer(cols = c(total_quantity_1000MT, USD_10Million, yen_Billion), 
               names_to = "variable", 
               values_to = "value") %>%
  as_tsibble(key = variable) %>%
  autoplot(value) + labs(title = "Yearly Qty and Price Trends")

###COMMENTS:
#Continuous decrease of Auction qty from approx 25000MT to 12500 MT in 2012,
#then increased to 15000MT in 2016.Same as auction revenue fell down to 57 Billion
#to approx 37 billion Yen.Revenue in USD during year 2010-2012 may attributed to 
#exchange rate.Even though qty decreased,revenue in USD increased.
#Subject to import/Export % of fishes USD can be explained.



#Species wise % Qty distribution during each year.
species_distribution <- tuna_ts %>%
  index_by(Year = year(Month)) %>%
  group_by(Year) %>%
  mutate(year_total_quantity = sum(Quantity)) %>%
  group_by(Year, species) %>%
  summarise(total_quantity = sum(Quantity), .groups = "drop") %>%
  left_join(
    tuna_ts %>%
      index_by(Year = year(Month)) %>%
      summarise(year_total_quantity = sum(Quantity), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(species_percent = (total_quantity / year_total_quantity) * 100)


#Scatter Plot Qty vs Price(Yen/kg),Correlation coeffcients. how extent these are correlated?
correlation_data <- as.data.frame(tuna_ts) %>% 
  group_by(fleet, state, species) %>%
  summarize(
    correlation = cor(Quantity, Price),
    .groups = 'drop'
  )

#Merge correlation data with tuna_ts
tuna_tss <- left_join(tuna_ts, correlation_data, by = c("fleet", "state", "species"))

Scatterplot <- ggplot(data = tuna_tss, aes(x = Price , y = Quantity, color = species)) +
  geom_point() +
  geom_text(aes(label = sprintf("r = %.2f", correlation), x = Inf, y = Inf), 
            hjust = 1.1, vjust = 1.1, check_overlap = TRUE, inherit.aes = FALSE) +
  facet_wrap(~fleet + state + species, scales = "free_y") +
  theme_minimal() +
  labs(title = "Price vs Quantity-Correlation by Fleet, State, and Species")

Scatterplot 

###COMMENTS:
#scatterplot indicates varying relationships between Quantity and Price 
#among different tuna species, with a general trend suggesting that as Quantity 
#increases, Price tends to decrease—a pattern most commonly associated with a 
#negative correlation. This trend is also observed across different fleets and 
#whether the fish is fresh or frozen

#1.c View Distribution of Species, Fleet, & State ----------

## Distribution plot
species_distribution %>% autoplot(species_percent) + labs(title = "% Distribution Qty Species")

###COMMENTS:
#Bluefintune fishes % is increased from approx 42% from 2003 to approx 60% in 2016.
#overall (Blue fin Tuna quantity also decreased from 10600MT to 9030MT.where as 
#Total auction Qty decreased from 25000to 15000MT). Southern bluefin tuna decreased 
#to 25% from 42% (2003-2007),2007 to 2010 - fluctuated around 25-30%,then started increasing trend.
#Bigeye Tuna initially increased,then decreased from 2012 to 2016.Reached lowest level.


## How fleets are influencing auctioned quantities
tuna_ts %>% 
  index_by(Year = year(Month)) %>%
  group_by(Year) %>%
  mutate(year_total_quantity = sum(Quantity)) %>%
  group_by(Year, fleet) %>%
  summarise(total_quantity = sum(Quantity), .groups = "drop") %>%
  left_join(
    tuna_ts %>%
      index_by(Year = year(Month)) %>%
      summarise(year_total_quantity = sum(Quantity), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(fleet_percent = (total_quantity / year_total_quantity) * 100) %>% autoplot(fleet_percent) +
  labs(title = "% Distribution Qty Fleet")

###COMMENTS:
#Its interesting unknown fleet contributes highest % on any year but it 
#is in continuous decreasing trend (Aprox 85% to 74%).Not clear with unknown fleet means.
#Japanese fleet contribution increased to 27%.Almost stagnant from 2014.Interestingly 2010 
#is most lowest exchange rate with USD.Which might lead to increased its own japanese fleet 
#fishing qty. Even though Yen recovery own Japanese production increased.

tuna_ts %>% 
  index_by(Year = year(Month)) %>%
  group_by(Year) %>%
  mutate(year_total_quantity = sum(Quantity)) %>%
  group_by(Year, state) %>%
  summarise(total_quantity = sum(Quantity), .groups = "drop") %>%
  left_join(
    tuna_ts %>%
      index_by(Year = year(Month)) %>%
      summarise(year_total_quantity = sum(Quantity), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(state_percent = (total_quantity / year_total_quantity) * 100) %>% autoplot(state_percent) +
  labs(title = "% Distribution Qty state")

###COMMENTS:
#Frozen fish % is continuously decreased to 50% from approx 65%.Sudden drop(approx 10%) 
#in 2006,further stabilized at approx 50% frozen and 50% fresh.


#1.d View Quantity throughout time for all species, fleet, & states ----------
autoplot_6 <- 
  tuna_ts %>% 
  autoplot(Quantity)  +
  facet_wrap(~fleet + state + species, scales = "free_y") +
  theme_minimal()

#show autoplot

autoplot_6  

###COMMENTS:
#With data on several different species, states, and fleets, viewing each plot helps determine 
#which slice of the tsibble we want to analyze. We are going to focus on bluefin tuna/fresh/Japanese 
#fleet because there seems to be seasonality and a positive trend.




#2.Visualize and Describe the Data ----------
#Visualize Bluefin Tuna, Fresh, Japanese Fleet
tuna <- tuna %>% 
  filter(state=="Fresh", fleet == "Japanese Fleet", species == "Bluefin Tuna") %>% 
  mutate(Date = Month)

#Visualize the Data
tuna %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Quantity))

tuna %>% gg_subseries(Quantity) + 
  labs(x = "Date",
       y = "Quantity (1000 kgs)",
       title = "Tuna Quantity by Month")


tuna %>% gg_season(Quantity) + 
  labs(x = "Date",
       y = "Quantity (1000 kgs)",
       title = "Tuna Quantity by Month")


###COMMENTS:
#There seems to be a positive potentially linear trend and seasonality. Throughout the seasonal cycle, the peaks seem to 
#take place in the summer months (June and July) while the valley are in the winter months (oct - april). Most years tend
#to follow this pattern, but there are certainly deviations from this pattern, like 2003. Overall, the trend seems to increase.
#That is, apart from a few exceptions (like 2008), as time increase, the quantity of fish caught per month increases as 
#compared to its corresponding month in the previous year.


#3.Do appropriate diagnostics/visualizations that lead you to the type of models you try ----------
#STL Decomp
tuna %>%
  model(STL(Quantity ~ trend(window=24) + season(window=12),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

###COMMENTS:
#Viewing the STL decomposition can help understand the trend and seasonal components of the data to assist with 
#model selection. Because it appears that the trend is somewhat linear and there is apparent seasonality, we will 
#move forward with the auto ETS model, the auto ARIMA model, a time series linear regression model, and the seasonal naive model.

#4.Try at least 3 models on training data, and choose at least 2 to asses with cross-validation ----------

#define training set by removing last two years in dataset
tuna_train <- tuna %>%
  filter(row_number() <= n()-24)

#build models
tuna_train_fit <- tuna_train %>%
  model(
    SNAIVE = SNAIVE(Quantity),
    reg = TSLM(Quantity ~ trend() + season()), 
    auto_ETS = ETS(Quantity),
    auto_ARIMA = ARIMA(Quantity),
    ETS = ETS(Quantity~ error("M")+trend("A")+season("M")),
    ETS2 = ETS(Quantity~ error("M")+trend("M")+season("M")),
    ETS3 = ETS(Quantity~ error("M")+trend("Ad")+season("M")),
  )

#Update as of 4/21: After presenting and hearing feedback from Saturday, we decided to explore the other
#ETS model options. The output confirms that the other ETS models do not outperform the auto arima, seasonal
#naive, and the regression. Therefore, we are going to proceed.


#Generate forecasts on test set and generate accuracy measures
future_data <- new_data(tuna_train, 24)
tuna_forecasts <- forecast(tuna_train_fit, new_data = future_data)

tuna %>%
  autoplot(Quantity) +
  autolayer(tuna_forecasts, level = NULL)

accuracy(tuna_forecasts, tuna) %>% 
  arrange(RMSE)

#check residuals on training set for each model
#SNAIVE
SNAIVE <- tuna_train_fit %>% select(SNAIVE)
SNAIVE %>% gg_tsresiduals()
augment(SNAIVE) %>%
  features(.innov, ljung_box, lag = 24)

#auto_ETS
auto_ETS <- tuna_train_fit %>% select(auto_ETS)
auto_ETS %>% gg_tsresiduals()
augment(auto_ETS) %>%
  features(.innov, ljung_box, lag = 24, dof=10)

#reg
reg <- tuna_train_fit %>% select(reg)
reg %>% gg_tsresiduals()
augment(reg) %>%
  features(.innov, ljung_box, lag = 24, dof=13)


#auto ARIMA
auto_ARIMA <- tuna_train_fit %>% select(auto_ARIMA)
auto_ARIMA %>% gg_tsresiduals()
augment(auto_ARIMA) %>%
  features(.innov, ljung_box, lag = 24, dof=5)



###COMMENTS:
#Based on the plot of the original data and the forecasted models, the auto ARIMA and the regression model seem to fit best.
#Based on the accuracy measures, the auto ARIMA and the naive model have the lowest RMSE, but the auto ARIMA and the regression
#model have the lowest MAPE and MAE. Based on the visual assessment and accuracy measures, we are going to move forward
#with the auto ARIMA and the regression models for cross-validation. Both models, however, seem to be overestimating on average
#as evidence by the positive mean error measures. Additionally, there are two significant auto correlations on the regression 
#residuals that may be concerning. The residuals for the regression model seem to be normally distributed, but do not pass the 
#ljung box test further emphasizing that there may be some issues with autocorrelation. The residuals for the auto ARIMA model 
#do resemble white noise based on the Ljung Box test and do not appear to have any major issues with autocorrelation. Further,
#the distribution of the residuals seem to be somewhat normal. Moving forward with both models, we will need to reassess the 
#regression model if it seems to out-perform the auto ARIMA model.


#data for cross-validation
tuna_cv <- tuna %>%
  stretch_tsibble(.init = 36, .step = 6)

#Models for cross-validation
tuna_fits_cv <- tuna_cv %>%
  model(
    reg = TSLM(Quantity ~ trend() + season()), 
    auto_ARIMA = ARIMA(Quantity)
  )

#Forecast ahead and view the accuracy measures to choose a model
tuna_forecasts_cv <- tuna_fits_cv %>%
  forecast(new_data(tuna_cv, 1))

accuracy(tuna_forecasts_cv, tuna)

###COMMENTS:
#The auto ARIMA model has a lower RMSE, MAE, and MAPE compared to the regression model. For this reason,
#we will move forward with the auto ARIMA model for our forecasts.

#5. Choose a final model and justify your choice ----------

#move forward with the auto_arima model.
tuna_final_mod <- tuna %>%
  model(auto_ARIMA = ARIMA(Quantity))

#view the components, parameters, and model
report(tuna_final_mod)

tidy(tuna_final_mod)

augment(tuna_final_mod)

#Non seasonal terms p=1,d=0,q=0
#seasonal terms P=2,D=1,Q=1 ,seasonal period 12 months with drift 
#(Drift term means trend compomnent included in model)

#check residuals on entire dataset
tuna_final_mod %>%
  gg_tsresiduals()

augment(tuna_final_mod) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) 

###COMMENTS:
#Looking at the residuals from the auto ARIMA model and the entire dataset, the model does not seem to pose any major
#concerns. There is one significant lag, but it does not occur on a multiple of 12 which would correspond to the seasonal 
#period. The distribution of the residuals seems to be approximately normal and besides the first couple of months, the 
#residuals seem to have somewhat constant variance. Additionally, the results from Ljung box test support the claim that 
#the residuals resemble white noise and there are no glaring issues with autocorrelation.

#6. Show the final forecast ----------
#Final forecast
fc_tuna_final_mod <- tuna_final_mod %>%
  forecast(h=12)

#View of just the forecasts
autoplot(fc_tuna_final_mod)

#forecasts with the data
tuna %>%
  autoplot(Quantity) +
  autolayer(fc_tuna_final_mod)

#table of prediction intervals
fc_tuna_ints <- fc_tuna_final_mod %>%
  hilo() %>%
  unpack_hilo(c(`95%`, `80%`))

View(fc_tuna_ints)
