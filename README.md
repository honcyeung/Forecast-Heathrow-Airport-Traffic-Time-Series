# Forecast Heathrow Airport Traffic Time Series

Heathrow Airport, situated in London, United Kingdom, is one of the busiest traveling hub in the world. It acts as not only the major domestic flight destination but also an irreplaceable transit for international travellers. We can have a better understanding in this outstanding airport by studying and predicting the traffic.

<p align = "center">
  <img src = "https://airwaysmag.com/wp-content/uploads/2020/07/Heathrow_Airport_03_Pascall_Watson_4_3_9.jpg">
</p>

The xlsx file contains the original dataset downloaded from https://www.heathrow.com/company/investor-centre/reports/traffic-statistics. The csv file contains data suitbale for data analysis. This time series project includes the data of monthly traffic of the airport from period of January 2005 to December 2019, in total 180 observations. The data table composes of 3 columns: year, month, and traffic of the airport. The project deatils are:

- check the timer series with ACF and PACF
- stationarization with log transformation
- 1st order difference to remove trend
- 12th order difference to remove seasonality
- identification of orders
- remove insignificant coefficients and deploy new models
- estimate ARMA coefficients
- p value check on the model
- check homoscedasticity(McLeod Li test), p value(Jarque Bera Test)
- plot Q-Q plot and check p value(Box Ljung test and Shapiro test)
- compare different models and their AIC and SBC
- choose the best model and predict


