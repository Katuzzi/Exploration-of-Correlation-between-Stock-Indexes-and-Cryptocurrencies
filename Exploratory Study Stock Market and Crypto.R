#All data gathered from publicly available sources at https://www.investing.com/
#Installing packages
install.packages("GGally")
install.packages("dplyr")
install.packages("reshape2")
install.packages("corrplot")
install.packages("vars")
#Loading packages
library(lmtest)
library(reshape2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)
library(vars)
library(patchwork)
#Importing the data or creating the environment

     
merged_data <- read_csv("merged_data.csv")

#Stock Market index first:
sp500 <- merged_data %>% 
  dplyr::select(Date, sp500)
nasdaq <- merged_data %>% 
  dplyr::select(Date, nasdaq)
dowjones <- merged_data %>% 
  dplyr::select(Date, dowjones)

#The two most prominent cryptocurrencies known as an storage of value on the market:
bitcoin <- merged_data %>% 
  dplyr::select(Date, bitcoin)
ethereum <- merged_data %>% 
  dplyr::select(Date, ethereum)

##Cleaning and exploration phase

#Lets the structure of each data set

#Stock market
str(sp500)
str(dowjones)
str(nasdaq)
#Crypto
str(bitcoin)
str(ethereum)


# Check for and remove duplicate rows

sp500 <- sp500[!duplicated(sp500), ]
dowjones <- dowjones[!duplicated(dowjones), ]
nasdaq <- nasdaq[!duplicated(nasdaq), ]
bitcoin <- bitcoin[!duplicated(bitcoin), ]
ethereum <- ethereum[!duplicated(ethereum), ]


#Making exploratory plots

#SP500

ggplot(sp500, aes(x = Date, y = sp500)) + geom_point(alpha = 0.5, size = 1.5) + 
  labs(title = "S&P 500 Closing Price 2021-2025", x = "Date by week", y = "Closing Price USD") + 
  theme_minimal()+
  theme_economist_white()

#Dowjones

ggplot(dowjones, aes(x = Date, y = dowjones)) + 
  geom_point(alpha = 0.5, size = 1.5) + 
  labs(title = "DowJones Closing Price 2021-2025", x = "Date by week", y = "Closing Price USD") + 
  theme_minimal()+
  theme_economist_white()

#Nasdaq

ggplot(nasdaq, aes(x = Date, y = nasdaq)) + 
  geom_point(alpha = 0.5, size = 1.5) + 
  labs(title = "Nasdaq Closing Price 2021-2025", x = "Date by week", y = "Closing Price USD") + 
  theme_minimal()+
  theme_economist_white()

#Bitcoin

ggplot(bitcoin, aes(x = Date, y = bitcoin)) + 
  geom_point(alpha = 0.5, size = 1.5) + 
  labs(title = "Bitcoin Price 2021-2025", x = "Date by week", y = "Closing Price USD") + 
  theme_minimal()+
  theme_economist_white()
  

#Ethereum

ggplot(ethereum, aes(x = Date, y = ethereum)) + 
  geom_point(alpha = 0.5, size = 1.5) + 
  labs(title = "Ethereum Price 2021-2025", x = "Date by week", y = "Closing Price USD") + 
  theme_minimal()+
  theme_economist_white()


#First insights: Nasdaq and SP500 share a similar tendency in their prices, while
#Dow Jones shows less consistency or similarity with the other two, this can be explain
#to the different industries each index is composed of and seems like
#sp500 and NASDAQ share an overlap

# Merge datasets by Date

merged_data <- sp500 %>%
  inner_join(dowjones, by = "Date") %>%
  inner_join(nasdaq, by = "Date") %>%
  inner_join(bitcoin, by = "Date") %>%
  inner_join(ethereum, by = "Date")

write.csv(merged_data, "merged_data.csv", row.names = FALSE)


#Now that we got our data sorted we can begin a basic statistical analysis first
#with the function summary

#SP500
summary(merged_data$sp500)
#DowJones
summary(merged_data$dowjones)
#Nasdaq
summary(merged_data$nasdaq)
#Bitcoin
summary(merged_data$bitcoin)
#Ethereum
summary(merged_data$ethereum)

#Now we shall begin our correlation study running a test for each combination between the 3 stock
#market indexes an the two selected cryptocurrencies

Pn_sp500_Bit <- cor.test(merged_data$sp500, merged_data$bitcoin, method = "pearson")
print(Pn_sp500_Bit)
Pn_sp500_Eth <- cor.test(merged_data$sp500, merged_data$ethereum, method = "pearson")
print(Pn_sp500_Eth)

#With a correlation pearson coefficient of 0.833617 for SP500 & Bitcoin and 
#0.6338 indicates that in both cases a moderate positive linear relationship
#exist between the prices of the stock index and the movements of the two analysed
#cryptocurrencies with a 95% confidence interval.


Pn_Nas_Bit <- cor.test(merged_data$nasdaq, merged_data$bitcoin, method = "pearson")
print(Pn_Nas_Bit)
Pn_Nas_Eth <- cor.test(merged_data$nasdaq, merged_data$ethereum, method = "pearson")
print(Pn_Nas_Eth)
#Analyzing the same test for the combinations with NASDAQ, we got an even stronger
#linear correlation than in the previous case, 0.90 for NASDAQ/Bitcoin and 0.719 for
#NASDAQ/Ethereum, given how NASDAQ is related to high tech capital this is expected. With a very
#low P-value of <2.2e^-16 in both cases being less than the significance label of 0.05
#with a confidence interval of 95%.


Pn_Dow_Bit <- cor.test(merged_data$dowjones, merged_data$bitcoin, method = "pearson")
print(Pn_Dow_Bit)
Pn_Dow_Eth <- cor.test(merged_data$dowjones, merged_data$ethereum, method = "pearson")
print(Pn_Dow_Eth)
#The Pearse coefficient -0.07 suggest not a statistically significant correlation between Dow Jones
#prices and bitcoin prices; with a p-value being > than 0.05 and a confidence
#interval of 95%. In contrast with a correlation coefficient of 0.1675 a linear correlation
#exist although is a weak one, with a p-value > 0.05 it is statistically significant with a 
#95% confidence interval

#Given these results we will use a heat map to better see them in accordance to its 
#statistical relevancy 

heatmap_data_Pearson <- matrix(c(Pn_sp500_Bit$estimate, Pn_sp500_Eth$estimate,
                         Pn_Nas_Bit$estimate, Pn_Nas_Eth$estimate,
                         Pn_Dow_Bit$estimate, Pn_Dow_Eth$estimate), 
                       nrow = 3, ncol = 2, byrow = TRUE) %>%
  `rownames<-`(c("S&P 500", "NASDAQ", "Dow Jones")) %>%
  `colnames<-`(c("Bitcoin", "Ethereum")) %>% melt()

ggplot(data = heatmap_data_Pearson,aes(Var1, Var2, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.05, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Pearson Correlation Heatmap Stocks & Crypto", x = "", y = "", 
       subtitle = "Weekly Prices from 2021-2025", caption = "Data collected from investing.com")

#Given this linear correlation distribution we will study further this behavior with
#more graphs describing the correlative behavior between the series.

ggplot(merged_data, aes(x = nasdaq, y = bitcoin)) +
  geom_point(alpha = 0.5, size = 1.5, color = "red") + 
  labs(title = "Nasdaq-Bitcoin Visual Correlation", x = "Nasdaq", y = "Bitcoin") + 
  theme_minimal()+
  theme_economist_white()

ggplot(merged_data, aes(x = nasdaq, y = ethereum)) +
  geom_point(alpha = 0.5, size = 1.5, color = "red") +
  labs(title = "Nasdaq-Ethereum Visual Correlation", x = "Nasdaq", y = "Ethereum") +
  theme_minimal()+ 
  theme_economist_white()

ggplot(merged_data, aes(x = sp500, y = bitcoin)) +
  geom_point(alpha = 0.5, size = 1.5, color = "red") +
  labs(title = "SP500-Bitcoin Visual Correlation", x = "SP500", y = "Bitcoin") +
  theme_minimal()+ 
  theme_economist_white()

ggplot(merged_data, aes(x = sp500, y = ethereum))+
  geom_point(alpha = 0.5, size = 1.5, color = "red") +
  labs(title = "SP500-Ethereum Visual Correlation", x = "SP500", y = "Ethereum") +
  theme_minimal()+ 
  theme_economist_white()

ggplot(merged_data, aes(x = dowjones, y = bitcoin))+
  geom_point(alpha = 0.5, size = 1.5, color = "blue")+
  labs(title = "DowJones-Bitcoin Visual Correlation", x = "DowJones", y = "Bitcoin") +
  theme_minimal()+ 
  theme_economist_white()

ggplot(merged_data, aes(x = dowjones, y = ethereum))+
  geom_point(alpha = 0.5, size = 1.5, color = "orange")+
  labs(title = "DowJones-Ethereum Visual Correlation", x = "DowJones", y = "Ethereum") +
  theme_minimal()+ 
  theme_economist_white()


#Given these results and to add more robustness to our analysis we will
#use another correlation test, one that is less sensitive to outlyers, the
#Kendall's Tau Correlation test.

Kt_sp500_Bit <- cor.test(merged_data$sp500, merged_data$bitcoin, method = "kendall")
print(Kt_sp500_Bit)
Kt_sp500_Eth <- cor.test(merged_data$sp500, merged_data$ethereum, method = "kendall")
print(Kt_sp500_Eth)

#We get similar results as in the Pearson test but the coefficients are less pronounced
#0.83 for Pearson and 0.61 for Kendall in sp500 & bitcoin and a 0.64 Pearson and 0.56 in Kendall's
#for the Sp500 ethereum correlation.

Kt_Nas_Bit <- cor.test(merged_data$nasdaq, merged_data$bitcoin, method = "kendall")
print(Kt_Nas_Bit)
Kt_Nas_Eth <- cor.test(merged_data$nasdaq, merged_data$ethereum, method = "kendall")
print(Kt_Nas_Eth)
#Same trend for the combinations of Nasdaq-Bitcoin and Nasdaq-Ethereum, with 0.72 and 0.60 coefficients
#respectively.

Kt_Dow_Bit <- cor.test(merged_data$dowjones, merged_data$bitcoin, method = "kendall")
print(Kt_Dow_Bit)
Kt_Dow_Eth <- cor.test(merged_data$dowjones, merged_data$ethereum, method = "kendall")
print(Kt_Nas_Eth)

#Surprisingly we get a more significant correlation coefficient than the past results with the Pearson's method
#specially concerning DowJones-Bitcoin, suggesting the possibility that outlyers were distorting the results, making
#the correlation less significant than it really was. This goes in contrast to the trend observed running the 
#Kendall's Tau Coefficient methodology.

heatmap_data_Kendall_Tau <- matrix(c(Kt_sp500_Bit$estimate, Kt_sp500_Eth$estimate,
                                 Kt_Nas_Bit$estimate, Kt_Nas_Eth$estimate,
                                 Kt_Dow_Bit$estimate, Kt_Dow_Eth$estimate), 
                               nrow = 3, ncol = 2, byrow = TRUE) %>%
  `rownames<-`(c("S&P 500", "NASDAQ", "Dow Jones")) %>%
  `colnames<-`(c("Bitcoin", "Ethereum")) %>% melt()

ggplot(data = heatmap_data_Kendall_Tau,aes(Var1, Var2, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.10, limit = c(-0.5, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Kendall-Tau Correlation Heatmap Stocks & Crypto", x = "", y = "", 
       subtitle = "Weekly Prices from 2021-2025", caption = "Data collected from investing.com")

#This general trend moderation of the correlation indicators under this method is something
#to be expected due to the qualities of the Kendall-Tau methodology being less sensitive to 
#possible outlyers within the data. However, the results remain largely the same, expressing 
#the same relationships between the variables.

#As last correlation analysis we will dwell into the cross-correlation, this is nothing more
#than an examination of how similar are the time series, with normalized values

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
normalized_data <- as.data.frame(lapply(merged_data[,-1], normalize))
normalized_data$Date <- merged_data$Date

#Storing the time series as values:

ts_sp500 <- ts(normalized_data$sp500)
ts_nasdaq <- ts(normalized_data$nasdaq)
ts_dowjones <- ts(normalized_data$dowjones)
ts_bitcoin <- ts(normalized_data$bitcoin)
ts_ethereum <- ts(normalized_data$ethereum)

#After storing the values as a numerical vector of normalized values we take a 
#look to the correlation between them as normalized time series
ccf(ts_sp500, ts_bitcoin, main = "Cross-Correlation Plot SP500/BTC")
#We see that the highest correlation coefficient is achieved at 0-lags at 0.83
#as per Pearson test, alike our previous results. However it can be seen that this
#correlation gets weaker over time.

ccf(ts_sp500, ts_ethereum, main = "Cross-Correlation Plot SP500/ETH")
#On a similar note we see the same behavior of correlation here, getting the maximum,
#Pearson coefficient at 0-lags decreasing in statistical relevancy overtime.

ccf(ts_nasdaq, ts_bitcoin, main = "Cross-Correlation Plot Nasdaq/BTC")
#Remembering the highest correlation recorded in this study the Pearson coefficient of 0.90
#is achieved at 0-lags decreasing over time at less than 0.60 by the lag 20.

ccf(ts_nasdaq, ts_ethereum, main = "Cross Correlation Nasdaq/ETH")
#Same behavior regarding lags in the time series, with slightly more sudden decrease in statistical
#relevancy after the apex of the value at 0-lags.

ccf(ts_dowjones, ts_bitcoin, main = "Cross Correlation DowJones/BTC")
#Recalling the lowest linear correlation between Dow Jones weekly closing prices and the same for Bitcoin, 
#recorded a 0.08 Pearson coefficient. However in the graph we can see that at times it presents a negative 
#correlation between lag at {-20, -13} and then again at {-7, 3} lags, the correlation is only statistically 
#relevant at {4, ∞+) on wards.This inconsistency shows an overall weak correlation between the two and sensitive to
#outlyers like we saw with using less sensitive tests.

ccf(ts_dowjones, ts_ethereum, main = "Cross Correlation DowJones/ETH")
#Differently to other combinations we see that both combination on Dow Jones are not stable, 
#through the series we see and increase and subsequent decrease in statistical relevancy with two spikes from {-19, -4} and then
#again from {-1, 17}. Overall the correlation is weak but not as weak as the Dow Jones/Bitcoin combination.



#Summarizing plot using corr plot package. 

library(corrplot)
#Removing date:
norm_for_corrplot <- normalized_data %>% 
  dplyr::select(-Date)

corrplot(cor(norm_for_corrplot),
         method = "number",
         type = "upper")

#In this last plot we can see for reference the linear correlation between the stock indexes as well.

#CORRELATION DOES NOT MEAN CAUSATION

#As one of the most well known phrases of econometric it is worth remembering that correlation does not means
#that one variable or time series cause the other, it could be a link to thing in that direction but by itself
#does not prove by itself the causation relationship. Various external factors and complex dynamics can influence 
#these relationships, and further investigation is required to establish any causal connections.


# Example of loading data
ts_data <- cbind(ts_bitcoin, ts_nasdaq, ts_dowjones, ts_ethereum, ts_sp500)
# Select the optimal number of lags using AIC and BIC criteria
lag_selection <- VARselect(ts_data, lag.max = 20, type = "const")

# Saving the optimal lag
print(lag_selection)
optimal_lags <- lag_selection$selection["AIC(n)"]
optimal_lags <- as.numeric(optimal_lags)

#NASDAQ

granger_nas_bit <- grangertest(ts_bitcoin ~ ts_nasdaq, order = optimal_lags) %>% 
  print()
granger_bit_nas <- grangertest(ts_nasdaq ~ ts_bitcoin, order = optimal_lags) %>% 
  print()
#With a p-value below 0.05 it suggests that prices of NASDAQ can have a causation 
#effect on bitcoin, however the same thing cannot be said the other way around as
#bitcoin prices don't seem to have a causation effect on NASDAQ prices p-value > 0.05

granger_nas_eth <- grangertest(ts_ethereum ~ ts_nasdaq, order = optimal_lags) %>% 
  print()
granger_eth_nas <- grangertest(ts_nasdaq, ts_ethereum, order = optimal_lags) %>% 
  print()
#With a p-value < 0.01 there are reasons to believe that there is some causation
#in both directions.

#SP500
granger_sp500_bit <- grangertest(ts_bitcoin ~ ts_sp500, order = optimal_lags) %>% 
  print()
granger_bit_sp500 <- grangertest(ts_sp500, ts_bitcoin, order = optimal_lags) %>% 
  print()
#In both directions with a P-value > 0.01, the null hypothesis is approved that there is
#not significant evidence that suggest causation.
granger_sp500_eth <- grangertest(ts_ethereum ~ ts_sp500, order = optimal_lags) %>% 
  print()
granger_eth_sp500 <- grangertest(ts_sp500 ~ ts_ethereum, order = optimal_lags) %>% 
  print()

#Just like the last combination the results suggest that there are not evidence to suggest
#a causation relation between the variables in both directions. With a P-value of 0.08302 causation
#ethereum-sp500 and a P-value of 0.4276 for Sp500-ethereum.

#DOWJONES
#
granger_dowj_bit <- grangertest(ts_bitcoin ~ ts_dowjones, order = optimal_lags) %>% 
  print()
#Very weak link for causation of the Dow Jones prices to cause bitcoin prices, in line
#with the weak correlation between the two with a P-value < 0.05 but at a weak presentation 
#0.04127.
granger_bit_dowj <- grangertest(ts_dowjones ~ ts_bitcoin, order = optimal_lags) %>% 
  print()
#With a P-value at 0.5402 implies P-value > 0.05, with evidences for not refusing the 
#null hypothesis implying that past values of bitcoin does not provide significant information
#on predicting Dow Jones prices.

granger_dowj_eth <- grangertest(ts_ethereum ~ ts_dowjones, order = optimal_lags) %>% 
  print()
#Same result regarding past values of Dow Jones on providing relevant information for prices of
#Ethereum, contrary to the little correlation that was shown, with a 0.18 P-value, 
#the P-value > 0.05, there are evidence to disrpove a relation of causality.

granger_eth_dowj <- grangertest(ts_dowjones ~ ts_ethereum, order = optimal_lags) %>% 
  print()

#With a P-value of 0.9261 the result of the test is well above P-value > 0.05. It can be disproven
#any relevant relationship of causality.



#CONCLUSION

#The stock market and the most valuable cryptocurrencies show interesting correlations between them,
#NASDAQ and SP500 show in overall more significant results than Dow Jones, fact that can be linked with
#the composition of the industries within said index. Nasdaq very related with "Big Tech" makes it logically
#more related to newer systems like crypto while more traditional stocks like those in Dow Jones are less 
#related. SP500 is a interesting case, the stock index is used many times as a "general picture" for the american
#economy, the relation between the index and crypto was always present suggesting the growing importance of 
#technological stocks within this index. We can see this growing importance of crypto and connection between this 
#financial markets with the creation of ETF (Exchange-Traded Fund) for both Bitcoin and Ethereum last year listed 
#in NASDAQ and other stock index.

#It is worth mentioning that further studies need to be made, the objective of this study was an exploratory
#focus for the possible links between both traditional markets and newer less regulated markets, it does not suggest
#a direct link or the nature of the relation between them. Further methods need to be applied considering 
#other variables into a wider model, considering inflation expectations, interest rate and other variables.
#Also it is recommended the decomposition of variables into a more complex auto regressive model such as VAR, SVAR among others. 




