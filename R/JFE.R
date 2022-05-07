Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
.JFEEnv <- new.env()
JFE <- function ()
  {


window <-tktoplevel(borderwidth=10)
tkwm.title(window, "JFE simplies data analytics for Just Finance and Econometrics")
tkwm.geometry(window, "+150+150")

menuBar <- tkmenu(window)
tkconfigure(window, menu = menuBar, width=700, height=100)

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
tkadd(menuBar,"cascade",label="Price Analytics", menu=priceMenu)
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
tkadd(priceMenu, "command", label="Advanced Visualization", command =.iClickPrice_Menu)

#3. Returns
returnsMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Returns Analytics", menu=returnsMenu)
tkadd(returnsMenu,"command", label = "Price data: Load then Transform",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getReturns()})
tkadd(returnsMenu,"command", label = "Returns data: Just Load",command = function() {
     dir_name <- tkchooseDirectory()
     if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getRawData()})

tkadd(returnsMenu, "command", label="Descriptive stat", command =.retSummary)
tkadd(returnsMenu, "command", label="Time series plot", command =.returnsTSPlot)
tkadd(returnsMenu, "command", label="Cumulative returns plot", command =.cumulativePlot)
tkadd(returnsMenu, "command", label="Drawdown plot", command =.drawdownPlot)
tkadd(returnsMenu, "command", label="Dependency plots", command =.acfPlots)
tkadd(returnsMenu, "command", label="Four QQ plots", command =.QQPlot)
tkadd(returnsMenu, "command", label="NIG triangle", command =.nigTriangle)
tkadd(returnsMenu, "command", label="Box plot", command =.ReturnsBoxPlots)
tkadd(returnsMenu, "command", label="Advanced Visualization", command =.iClickReturn_Menu)

#4. Assets selections
assetSelectionMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Assets Selection", menu=assetSelectionMenu)
tkadd(assetSelectionMenu, "command", label="Price Data: Load and Transform",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
     .getReturns4Selection()})
tkadd(assetSelectionMenu,"command", label = "Returns data: Just Load",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
        .getRawData4Selection()})
tkadd(assetSelectionMenu,"command", label = "Change dataset in this Dir",command = .getReturns4Selection)
tkadd(assetSelectionMenu, "command", label= "Assets Selection by Performance Index", command =.sharpeIneqMenu)


#5.Backtesting
backtestMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="Portfolio Backtesting", menu=backtestMenu)
tkadd(backtestMenu, "command", label="Price data: Load and Transform",command = function() {
  dir_name <- tkchooseDirectory()
  if(nchar(dir_name <- as.character(dir_name)))
    setwd(dir_name)
  .getReturns4backtesting()})

tkadd(backtestMenu,"command", label = "Returns data: Just Load",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
        .getRawData4backtesting()})

tkadd(backtestMenu,"command", label = "Change dataset in this Dir",command = .getReturns4backtesting)
tkadd(backtestMenu, "command", label= "Backtesting", command =.backtestingMenu)
tkadd(backtestMenu, "command", label= "Backtesting All in One", command =.iClickBacktesting_Menu)

# 6. GARCH
garchMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="GARCH", menu=garchMenu)

tkadd(garchMenu,"command", label = "Price data: Load then Transform",command = function() {
  dir_name <- tkchooseDirectory()
  if(nchar(dir_name <- as.character(dir_name)))
    setwd(dir_name)
  .getReturns4GARCH()})
tkadd(garchMenu,"command", label = "Returns data: Just Load",command = function() {
  dir_name <- tkchooseDirectory()
  if(nchar(dir_name <- as.character(dir_name)))
    setwd(dir_name)
  .getRawData4GARCH()})
tkadd(garchMenu, "command", label="GARCH", command =.garchMenu)


#7. Predictive Modelling
PMMenu <- tkmenu(menuBar)
tkadd(menuBar,"cascade",label="PredictiveModelling", menu=PMMenu)
tkadd(PMMenu,"command", label = "Price data: Load then Transform",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
        .getReturns4GARCH()})
tkadd(PMMenu,"command", label = "Returns data: Just Load",command = function() {
        dir_name <- tkchooseDirectory()
        if(nchar(dir_name <- as.character(dir_name)))
     setwd(dir_name)
        .getRawData4GARCH()})




}



#=====================================#








