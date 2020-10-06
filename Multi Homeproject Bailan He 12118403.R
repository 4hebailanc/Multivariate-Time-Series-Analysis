# load library
library(lmtest)
library(tidyverse)
library(vars)
library(urca)
library(forecast)
library(tseries)
library(fUnitRoots)
library(xts)
library(reshape2)
# set work dictionary
#please put the R.file in the same dictionary with three data file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# a) Three economic indicators with download link and public data sources

# all data are downloaded on 18th Aug.

# 1. Real Gross Domestic Product for France (CLVMNACSCAB1GQFR)
# https://fred.stlouisfed.org/series/CLVMNACSCAB1GQFR
# Source: Eurostat
gdp <- read_csv("CLVMNACSCAB1GQFR.csv")

# 2. Consumer Price Index of All Items in France (FRACPIALLMINMEI)
# https://fred.stlouisfed.org/series/FRACPIALLMINMEI
# Source: Organization for Economic Co-operation and Development
cpi <- read_csv("FRACPIALLMINMEI.csv")

# 3. Harmonized Unemployment Rate: Total: All Persons for France (LRHUTTTTFRM156S)
# https://fred.stlouisfed.org/series/LRHUTTTTFRM156S
# Source: Organization for Economic Co-operation and Development
ump <- read_csv("LRHUTTTTFRM156S.csv")

# b) Ensure that your data foundation is of proper format and quality, e.g. correct data type,
# no missing values, etc..

#### check if there is NA. There is no missing value.
sum(is.na(ori_data))

#### write function to prepare data, This function converts the data into growth rate form ####
prepare_fun <- function(data) {
  data %>%
    dplyr::select(!(DATE)) %>%
    mutate_all(funs(100 * ((. - dplyr::lag(.)) / .))) %>%
    add_column(date = data$DATE) %>%
    na.omit() %>%
    relocate(date)
}

# To get a weak stationary data. we converts the ump into ump change#
pre_ump <- ump[-1, ] %>% rename(date = DATE, value = LRHUTTTTFRM156S)
pre_ump$value <- diff(ump$LRHUTTTTFRM156S)

# To get a weak stationary data. we converts the other two data into growth rate#
pre_cpi <- prepare_fun(cpi) %>% rename(value = FRACPIALLMINMEI)
pre_gdp <- prepare_fun(gdp) %>% rename(value = CLVMNACSCAB1GQFR)

#### create datasets ####
ori_data <- gdp %>%
  rename(gdp = CLVMNACSCAB1GQFR, date = DATE) %>%
  add_column(ump = ump$LRHUTTTTFRM156S, cpi = cpi$FRACPIALLMINMEI) %>%
  mutate(gdp = gdp / 1000)

pre_data <- pre_gdp %>%
  rename(gdp_ratio = value) %>%
  add_column(cpi_ratio = pre_cpi$value, ump_change = pre_ump$value)

# d) Plot your data and make sure that they are properly formatted, labeled and comprehensible
#### prepare for the plot data####

# ggplot for original data
colors <- c(
  "CPI" = "lightblue",
  "GDP" = "lightgreen",
  "Ump Rate" = "darkred"
)

ggplot(ori_data, aes(x = date)) +
  geom_line(aes(y = gdp / 50, color = "GDP"), size = 1) +
  geom_line(aes(y = cpi / 10, color = "CPI"), size = 1) +
  geom_line(aes(y = ump, color = "Ump Rate"), size = 1) +
  ggtitle("Comparison plot of GDP, CPI and unemployment rate") +
  scale_color_manual(values = colors) +
  labs(color = "") +
  scale_y_continuous(
    # Add the first axis to show the CPI and Ump Rate,
    # In order to get a nice plot,I rescale CPi by dividing by 10
    name = "CPI/10 & Ump Rate",
    # Add a second axis to show the GDP
    sec.axis = sec_axis(~ . * 50, name = "GDP (billion)")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#### ADF unit root test ####

adfTest(ori_data$gdp)
adfTest(ori_data$gdp[1:80])
adfTest(pre_data$gdp_ratio)
adfTest(pre_data$gdp_ratio[1:79])

adfTest(ori_data$cpi)
adfTest(ori_data$cpi[1:80])
adfTest(pre_data$cpi_ratio)
adfTest(pre_data$cpi_ratio[1:79])

adfTest(ori_data$ump)
adfTest(ori_data$ump[1:80])
adfTest(pre_data$ump_change)
adfTest(pre_data$ump_change[1:79])

#### KPSS unit root test ####

#original data KPSS test
gdp$CLVMNACSCAB1GQFR %>%
  ur.kpss() %>%
  summary()

gdp$CLVMNACSCAB1GQFR[1:80] %>%
  ur.kpss() %>%
  summary()

cpi$FRACPIALLMINMEI %>%
  ur.kpss() %>%
  summary()
ump$LRHUTTTTFRM156S %>%
  ur.kpss() %>%
  summary()

# Ratio data KPSS test(removed data in 2020)
pre_data$gdp_ratio[1:79] %>%
  ur.kpss() %>%
  summary()
pre_data$cpi_ratio[1:79] %>%
  ur.kpss() %>%
  summary()
pre_data$ump_change[1:79] %>%
  ur.kpss() %>%
  summary()

## ts plot for Ratio data
ts_pre_data <- ts(pre_data[1:79,-1],start = c(2000.4),frequency = 4)
plot(as.xts(ts_pre_data), type="l", 
     multi.panel=TRUE, theme="white",
     main="Comparison plot of GDP, CPI ratio and unemployment change rate",
     major.ticks="years",
     grid.ticks.on = "years")

#### save the full data, and remove data in 2020
full_data <- pre_data

ori_data <- ori_data[1:80, ]
pre_data <- pre_data[1:79, ]

##### acf plot ####
acf(ori_data$gdp)
acf(ori_data$cpi)
acf(ori_data$ump)

acf(pre_data$gdp_ratio)
acf(pre_data$cpi_ratio)
acf(pre_data$ump_change)


#### cointegration test ####
# when  r=0 , test(37.5)>1pct(37.22)
jotest <- ca.jo(pre_data[, -1], type = "trace", K = 2, ecdet = "none", spec = "longrun")
summary(jotest)


#### VAR ####
# select order with season
#with season
VARselect(pre_data[,-1],
  lag.max = 7, type = "const",
  season = NULL, exogen = NULL)
# select order with season
VARselect(pre_data[,-1],
  lag.max = 7, type = c("const"),
  season = 4, exogen = NULL)
# splite data
df_train <- full_data[1:71, -1]
df_test <- full_data[-(1:71), -1]
# build model VAR(1)
var_model <- VAR(df_train, p = 1, type = "const", season = 4)
summary(var_model)


#### Diagnostic Test ####
#Serial-correlation test
serial.test(var_model, lags.pt = 16, lags.bg = 5, type = c("PT.asymptotic"))

# Normality test, no pass. I think my sample is not large enough, so  
# the jarque.bera test will almost always reject normality of the residuals for a VAR
normality.test(var_model, multivariate.only = T)

# qq plot for the residuals
qqnorm(var_model$varresult$gdp_ratio$residuals, main="GDP_ratio residuals Q-Q Plot", ylab = "GDP_ratio Quantiles")
qqnorm(var_model$varresult$cpi_ratio$residuals, main="CPI_ratio residuals Q-Q Plot", ylab = "CPI_ratio Quantiles")
qqnorm(var_model$varresult$ump_change$residuals, main="Ump_change residuals Q-Q Plot", ylab = "Ump_change Quantiles")

# Test for System stability
var_stabil <- stability(var_model, type = c("OLS-CUSUM"), h = 0.15, dynamic = FALSE, rescale = TRUE)
plot(var_stabil)

#plot dataset for the residuals
resi_gdp_ratio <- var_model$varresult$gdp_ratio$residuals
resi_cpi_ratio <- var_model$varresult$cpi_ratio$residuals
resi_ump_change <- var_model$varresult$ump_change$residuals
plot_df <- bind_cols(resi_gdp_ratio, resi_cpi_ratio, resi_ump_change)
plot_df <- bind_rows(list(plot_df[1],plot_df[2],plot_df[3]),.id='id') 
plot_df <- plot_df %>% add_column(sample_number=rep(1:70,3)) %>% rename(Residual_value = ...1)
#line plot for the residuals
ggplot(data = plot_df, aes(x=sample_number, y = Residual_value, color = id)) +
  geom_line() + scale_colour_discrete(breaks = c(1, 2, 3), labels = c("GDP growth rate residual", "CPI growth rate residual", "Ump change rate residual"))  +
  theme(legend.position="bottom") +
  ggtitle("Residuals Plots for VAR(1)")


####One-step and Three-step Forecasting ####
#One-step:
one_step_results <- tibble()
for(index in 71:80){
  var_model <- VAR(full_data[1:index,-1], p = 1, type = "const", season = 4)
  var_predict <- predict(var_model,n.ahead=1,ci=0.95)
  one_step_results <- bind_rows(one_step_results,var_predict$fcst)
}

#Three-step:
three_step_results <- tibble()
index=71
while(index+3<=84){
  var_model <- VAR(full_data[1:index,-1], p = 1, type = "const", season = 4)
  var_predict <- predict(var_model,n.ahead=3,ci=0.95)
  three_step_results <- bind_rows(three_step_results,var_predict$fcst)
  index=index+3
}
three_step_results <- three_step_results[1:10,]

# MSE data
one_step_dif <- (one_step_results - df_test) %>% dplyr::select(ends_with("1"))
one_step_mse <- one_step_dif * one_step_dif
three_step_dif <- (three_step_results - df_test) %>% dplyr::select(ends_with("1"))
three_step_mse <- three_step_dif * three_step_dif
sum(one_step_mse)/10
sum(three_step_mse)/10
####GDP growth ratio plot ####
fc_gdp_data <- tibble(one_step_results$gdp_ratio[,1],three_step_results$gdp_ratio[,1],df_test$gdp_ratio) %>%
  rename(one_step_forecast = `one_step_results$gdp_ratio[, 1]`,
         three_step_forecast = `three_step_results$gdp_ratio[, 1]`,
         real_data =`df_test$gdp_ratio`)
# plot data
fc_gdp_plot <- melt(fc_gdp_data) %>% add_column(x=rep(1:10,3))
# GDP forecast plot
ggplot(data=fc_gdp_plot) + 
  geom_line(aes(x=x,y=value,color=variable)) +
  ggtitle("Forecast for GDP growth ratio") +
  theme(legend.position="bottom")
# MSE forecast plot GDP growth ratio
mse_gdp_data <- tibble(one_step_mse$gdp_ratio.1,three_step_mse$gdp_ratio.1) %>% 
  rename(one_step_mse= `one_step_mse$gdp_ratio.1`,three_step_mse=`three_step_mse$gdp_ratio.1`)
mse_gdp_plot <- melt(mse_gdp_data) %>% add_column(x=rep(1:10,2))
ggplot(data=mse_gdp_plot) + 
  geom_point(aes(x=x,y=log(value),color=variable)) +
  geom_line(aes(x=x,y=log(value),color=variable)) +
  ggtitle("MSE of Forecast for GDP growth ratio") +
  theme(legend.position="bottom")

####CPI growth ratio plot ####
fc_cpi_data <- tibble(one_step_results$cpi_ratio[,1],three_step_results$cpi_ratio[,1],df_test$cpi_ratio) %>%
  rename(one_step_forecast = `one_step_results$cpi_ratio[, 1]`,
         three_step_forecast = `three_step_results$cpi_ratio[, 1]`,
         real_data =`df_test$cpi_ratio`)
# forecast cpi plot data
fc_cpi_plot <- melt(fc_cpi_data) %>% add_column(x=rep(1:10,3))
# CPI forecast plot
ggplot(data=fc_cpi_plot) + 
  geom_line(aes(x=x,y=value,color=variable)) +
  ggtitle("Forecast for CPI growth ratio") +
  theme(legend.position="bottom")
# MSE forecast plot CPI growth ratio
mse_cpi_data <- tibble(one_step_mse$cpi_ratio.1,three_step_mse$cpi_ratio.1) %>% 
  rename(one_step_mse= `one_step_mse$cpi_ratio.1`,three_step_mse=`three_step_mse$cpi_ratio.1`)
mse_cpi_plot <- melt(mse_cpi_data) %>% add_column(x=rep(1:10,2))
ggplot(data=mse_cpi_plot) + 
  geom_point(aes(x=x,y=log(value),color=variable)) +
  geom_line(aes(x=x,y=log(value),color=variable)) +
  ggtitle("MSE of Forecast for CPI growth ratio") +
  theme(legend.position="bottom")

####Ump change rate plot ####
fc_ump_data <- tibble(one_step_results$ump_change[,1],three_step_results$ump_change[,1],df_test$ump_change) %>%
  rename(one_step_forecast = `one_step_results$ump_change[, 1]`,
         three_step_forecast = `three_step_results$ump_change[, 1]`,
         real_data =`df_test$ump_change`)
# forecast ump plot data
fc_ump_plot <- melt(fc_ump_data) %>% add_column(x=rep(1:10,3))
# ump forecast plot
ggplot(data=fc_ump_plot) + 
  geom_line(aes(x=x,y=value,color=variable)) +
  ggtitle("Forecast for Umployment change rate") +
  theme(legend.position="bottom")
# MSE forecast plot Umployment change rate
mse_ump_data <- tibble(one_step_mse$ump_change.1,three_step_mse$ump_change.1) %>% 
  rename(one_step_mse= `one_step_mse$ump_change.1`,three_step_mse=`three_step_mse$ump_change.1`)
mse_ump_plot <- melt(mse_ump_data) %>% add_column(x=rep(1:10,2))
ggplot(data=mse_ump_plot) + 
  geom_point(aes(x=x,y=log(value),color=variable)) +
  geom_line(aes(x=x,y=log(value),color=variable)) +
  ggtitle("MSE of Forecast for Umployment change rate") +
  theme(legend.position="bottom")


#### Granger-causality test ####
grangertest(pre_data$gdp_ratio~pre_data$cpi_ratio)
grangertest(pre_data$cpi_ratio~pre_data$gdp_ratio)
# gdp growth ratio  Granger causes cpi growth ratio , 
#means past values of gdp growth ratio  improve prediction of cpi growth ratio 
# (compared to only past values of cpi growth ratio )

grangertest(pre_data$gdp_ratio~pre_data$ump_change)
grangertest(pre_data$ump_change~pre_data$gdp_ratio)
# gdp growth ratio Granger causes ump change rate , 
#means past values of gdp growth ratio  improve prediction of ump change rate 
# (compared to only past values of ump change rate )

grangertest(pre_data$ump_change~pre_data$cpi_ratio)
grangertest(pre_data$cpi_ratio~pre_data$ump_change)

# Granger-Causality test results after building VAR model

causality_gdp <- causality(var_model, cause = c("gdp_ratio"), vcov.=NULL, boot=T, boot.runs=100)
causality_gdp
#GDP growth ratio Granger causes the sub-process CPI growth ratio and Unemployment change rate.
#There is instantaneous causality between GDP growth ratio and (CPI growth ratio, Unemployment change rate).

causality_cpi <- causality(var_model, cause = c("cpi_ratio"), vcov.=NULL, boot=T, boot.runs=100)
causality_cpi
#no Granger causality and instantaneous causality 

causality_ump <- causality(var_model, cause = c("ump_change"), vcov.=NULL, boot=T, boot.runs=100)
causality_ump
#no Granger causality but there is instantaneous causality 

# Impulse-Response Analysis
var_irf<-irf(var_model, boot = T)
plot(var_irf)

#### Forecast Error Variance Decomposition ####
model_fevd <- fevd(var_model,n.ahead = 10)
plot(model_fevd)
