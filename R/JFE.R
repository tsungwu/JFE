.JFEEnv <- new.env()
JFE <- function ()
  {
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

window <-tktoplevel(borderwidth=10)
tkwm.title(window, "Just Finance and Econometrics")
tkwm.geometry(window, "+100+100")

menuBar <- tkmenu(window)
tkconfigure(window, menu = menuBar, width=550, height=100)

#1.
fileMenu <- tkmenu(menuBar)
tkadd(menuBar, "cascade", label="File", menu = fileMenu)
tkadd(fileMenu, "command", label="Set working directory...",
      command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
          setwd(dir_name)} )
tkadd(fileMenu, "command", label = "Save workspace as...",
      command = .saveWorkSpace)

#2. Price
priceMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Price Analythics", menu=priceMenu)
tkadd(priceMenu,"command", label = "Load Price Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getPrice()})
tkadd(priceMenu, "command", label="Descriptive stat", command =.priceSummary)
tkadd(priceMenu, "command", label="Time series plot", command =.pricePlot)
tkadd(priceMenu, "command", label="ACF/PACF plot", command =.PriceAcfPlots)
tkadd(priceMenu, "command", label="Box plot", command =.PriceBoxPlots)
tkadd(priceMenu, "command", label="Technical charting(For OHLC only)", command =.priceCharting)
tkadd(priceMenu, "command", label="iClick charting", command =.iClickPrice_Menu)

#3. Returns
returnsMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Returns Analythics", menu=returnsMenu)
tkadd(returnsMenu,"command", label = "Load Price Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getReturns()})
tkadd(returnsMenu,"command", label = "Load Returns Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getRawData()})
tkadd(returnsMenu, "command", label="Descriptive stat", command =.retSummary)
tkadd(returnsMenu, "command", label="Returns plot", command =.returnsTSPlot)
tkadd(returnsMenu, "command", label="Cumulative returns plot", command =.cumulativePlot)
tkadd(returnsMenu, "command", label="Drawdown plot", command =.drawdownPlot)
tkadd(returnsMenu, "command", label="ACF related plots", command =.acfPlots)
tkadd(returnsMenu, "command", label="Four QQ plots", command =.QQPlot)
tkadd(returnsMenu, "command", label="NIG triangle", command =.nigTriangle)
tkadd(returnsMenu, "command", label="Box plot", command =.ReturnsBoxPlots)

#4. Assets selections
assetSelectionMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Assets Selection", menu=assetSelectionMenu)
tkadd(assetSelectionMenu, "command", label="Load Price Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getReturns4Selection()})

tkadd(assetSelectionMenu,"command", label = "Load Returns Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getRawData()})


tkadd(assetSelectionMenu,"command", label = "Change dataset in this Dir",command = .getReturns4Selection)
tkadd(assetSelectionMenu, "command", label= "Assets Selection by Performance Index", command =.sharpeIneqMenu)


#5.Backtesting
backtestMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Portfolio Backtesting", menu=backtestMenu)
tkadd(backtestMenu, "command", label="Load Price Data",command = function() {
  dir_name <- tkchooseDirectory()
  if(nchar(dir_name <- as.character(dir_name)))
    setwd(dir_name)
  .getReturns4backtesting()})

tkadd(backtestMenu,"command", label = "Load Returns Data",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getRawData()})

tkadd(backtestMenu,"command", label = "Change dataset in this Dir",command = .getReturns4backtesting)

tkadd(backtestMenu, "command", label= "Backtesting", command =.backtestingMenu)

tkadd(backtestMenu, "command", label= "Run iClick", command =.iClickPortfolio_Menu)

# 6.
#performanceMenu <- tkmenu(menuBar)
#tkadd(menuBar,"cascade",label="Performance", menu=performanceMenu)
#tkadd(performanceMenu,"command", label = "Load Price data",command = function() {
#        dir_name <- tkchooseDirectory()
#        if(nchar(dir_name <- as.character(dir_name)))
#     setwd(dir_name)
#     .getReturns4Performance()})
#tkadd(performanceMenu,"command", label = "Load Returns Data",command = function() {
#        dir_name <- tkchooseDirectory()
#        if(nchar(dir_name <- as.character(dir_name)))
#     setwd(dir_name)
#     .getRawData()})

#tkadd(performanceMenu, "command", label="Performance index", command =.performanceIndexMenu)


#7.
#RiskMenu <- tkmenu(menuBar)
#tkadd(menuBar,"cascade",label="Risk", menu=RiskMenu)
#invisible()
}



#=====================================#








