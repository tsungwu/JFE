# JFE
R Package: Just Finance and Econometrics
Check http://www.iclick-r.idv.tw/R_PkgDev/main.htm for more information.

Price Analytics
1. Price Analytics loads price data from .RData.  Technical charting needs OHLC data
2. To use this function, you must have a time series dataset with R format, xts is most encourgaed; and the file is saved in .RData or .rda. Users may download the dataset IBM.RData in above web page. 

Assets Selection
1. Assets Selection here is basically based on, for example, Sharpe ratio inequality. The method is designed for Internaitonal Asset Selection, interested reader please refer to Bekaert and Hodrick(2009,International Financial Management,PP.466-468). We provide more performance indices based on the R package PerformanceAnalytics.
2. To use this function, you must have a multivariate time series dataset with R format, xts is most encourgaed; and the file is saved in .RData or .rda. Users may download the dataset world20.Rdata in above web page, which is 20 country price indices of MSCI world index.
3. If the loaded data is price, then you have to pull down the menu and choose Transform Price Data, else, Load Returns Data.
4. Frequency conversion here has a bug, I have fixed it already here.

Portfolio Backtesting
1. Portfolio Backtesting here is basically based on R package fPortfolio. The method is designed for portfolio optimization, JFE provides more covariance estimators and GMVP  strategy for backtesting. JFE offers a comprehensive computation(iClick All in One) for 6 covariance estimators combined with 2 strategies, which is a little bit time-consuming, 3-min for DJ30 dataset.
2. To use this function, you must have a multivariate time series dataset with R format, xts is most encourgaed; and the file is saved in .RData or .rda. Users may download the dataset DJ30.Rdata in above web page, which is close price of Dow Jones 30 component Stocks.
3. If the loaded data is price, then you have to pull down the menu and choose Transform Price Data, else, Load Returns Data.
4. The Next-Month Advice is the output bottom is the assets weights suggestion computed by backtesting for the next period from the end of data. The rolling length is 1 month and estimation is 1 year, which are not allowed to change so far   
