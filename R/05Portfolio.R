.readme4backtesting <- function() {
cat("\n","1. Portfolio Backtesting here is basically based on R package fPortfolio.","\n","The method is designed for portfolio optimization, JFE provides more covariance estimators and GMVP  strategy for backtesting. JFE offers a comprehensive computation(Backtesting All in One) for 6 covariance estimators combined with 2 strategies, which is a little bit time-consuming, 3-min for DJ30 dataset","\n","2. To use this function, you must have a multivariate time series dataset with R format, xts is most encourgaed; and the file is saved in .RData or .rda. Users may use the dataset DJ30.rda located in the data directory of this package, detail is explained in the manual.","\n","3.  If the loaded data is price, then you have to pull down the menu and choose Transform Price Data, else, Load Returns Data","\n","4. The Next-Month Advice is the output bottom is the assets weights suggestion computed by backtesting for the next period from the end of data. The rolling length is 1 month and estimation is 1 year, which are not allowed to change so far","\n")
}

.getReturns4backtesting <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dataz=eval(parse(text=temp))
  dataz=diff(log(dataz))
  dat=timeSeries::as.timeSeries(dataz)
  assign("retAS", dat, envir = .JFEEnv)
  cat("Data is imported sucessfully","\n")
  print(tail(dat,2))
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
}




.backtesting <- function(dataset, assetsRM, bench, Type="MV",Strategy="tangencyStrategy",Cov="covEstimator",Rf=0,lambda,Constraint){

  if (assetsRM=="None") {
    dat=dataset } else {
      k1=which(colnames(dataset) %in% as.character(assetsRM))
      dat=dataset[,-c(k1)]}
  if (bench=="None") {
    assetReturns=dat
    Rp=rowMeans(dat) } else {
    k2=which(colnames(dat)==as.character(bench))
    assetReturns=dat[,-k2]
    Rp=dat[,k2]
  }

  newData=na.omit(cbind(Rp,assetReturns))
  colnames(newData)=c("Rp",colnames(assetReturns))

  mySpec = portfolioSpec()

  setType(mySpec)=Type
  setEstimator(mySpec)=Cov
  setRiskFreeRate(mySpec) <- as.numeric(Rf)

  if (Constraint=="Short") {
    setSolver(mySpec)= "solveRshortExact"
  } else if (getType(mySpec)=="CVaR") {
    setSolver(mySpec)= "solveRglpk.CVAR"}  else {
      setSolver(mySpec)= "solveRquadprog"
    }

  #===back testing
  myBacktestSpec = portfolioBacktest()
  setStrategyFun(myBacktestSpec)=Strategy
  setSmootherLambda(myBacktestSpec) = lambda
  #setWindowsHorizon(myBacktestSpec) <- "12m"
Eq=paste(names(newData)[1], paste(names(newData)[-1], collapse= "+"),sep="~")

  myFormula = as.formula(Eq)
  myBacktest = portfolioBacktesting(myFormula,data = newData, spec = mySpec,backtest = myBacktestSpec,constraints = Constraint, trace = FALSE)

  Weights = round(100*myBacktest$weights, 4)

  SmoothPort = portfolioSmoothing(object=myBacktest, trace = FALSE)

  smoothWeights = round(100*SmoothPort$smoothWeights,2)
  dev.new();plot.new();backtestPlot(SmoothPort, cex = 0.6, font = 1, family = "mono", which="all")
  netPerformance(SmoothPort)
  print(SmoothPort$stats)

  ID12=IDab=colnames(assetReturns)

  weightDF=SmoothPort$smoothWeights
  id.tmp=which(ID12 %in% colnames(weightDF))
  ID12.tmp=ID12[id.tmp]
  dates=rownames(weightDF)
  T=length(dates)
  colnames(weightDF)=IDab[id.tmp]
  id=which(weightDF[T,]>0)
  NextMonth=round(weightDF[dates[T],id],7);
  Code=as.character(ID12.tmp[id])

  cat("Last Three Months ","\n");print(t(round(weightDF[(T-2):T,],3)))
  cat("\n", "Next-Month Advice","\n");print(as.data.frame(NextMonth))


}


.backtestingMenu <- function(){
  retAS=get("retAS",envir = .JFEEnv)
  top <- tktoplevel(borderwidth=45)
  tkwm.title(top, "Back testing")

  xBox <- .variableListBox(top,c("None",colnames(retAS)),title="Pick 1 bench asset")
  xBoxRM <- .variableListBox(top, c("None", colnames(retAS)), title="Pick assets you won't use", selectmode = "extended")

  onOK <- function(){
    bench <- .getSelection(xBox)
    assetsRM <- .getSelection(xBoxRM)
    if (length(bench)==0){
      bench=NULL
      tkmessageBox(message = "You did not select a bench asset. rowMeans will be used", icon = "info", type = "ok")
      # return()
    }

    COV <-  tclvalue(CovVariable)
    Type <- tclvalue(riskTypeVariable)
    Strategy <- tclvalue(StrategyVariable)
    Constraint <- tclvalue(ConstraintVariable)
    Rf <- tclvalue(rfVariable)
    lambda <- tclvalue(lambdaVariable)

    .backtesting(dataset=retAS,assetsRM,bench,Type,Strategy,COV,Rf,lambda,Constraint)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxRM), sticky="n")


    rightFrame <- tkframe(top)
#====  Strategy Frame
StrategyFrame <- tkframe(rightFrame)
.radioButtons(top,name="Strategy", buttons=c("Tangency", "GMVP"), values=c("tangencyStrategy", "GMVPStrategy"), labels=c("Tangency", "GMVP"), title="Select Portfolio Strategy")
StrategyVariable <- StrategyVariable
tkgrid(StrategyFrame,sticky="w")

#==== Risk type Frame
riskTypeFrame <- tkframe(rightFrame)
.radioButtons(top,name="riskType", buttons=c("MV", "CVaR"), values=c("MV", "CVaR"), labels=c("Mean-Variance", "CVaR"), title="Portfolio Risk Type")
riskTypeVariable <- riskTypeVariable
tkgrid(riskTypeFrame,sticky="w")

#====  Constraint Frame
ConstraintFrame <- tkframe(rightFrame)
.radioButtons(top,name="Constraint", buttons=c("LongOnly", "Short"), values=c("LongOnly", "Short"), labels=c("LongOnly", "Short"), title="Select Portfolio constraint")
ConstraintVariable <- ConstraintVariable
tkgrid(ConstraintFrame,sticky="w")

#====Covariance Frame
CovFrame <- tkframe(rightFrame)
.radioButtons(top,name="Cov", buttons=c("covEstimator", ".covLedoit",".covstudent"), values=c("covEstimator", ".covLedoit",".covstudent"), labels=c("Sample Covariance", "LedoitWolf Shrinkage","Multivariate Student t"), title="Select Correlation Estimator")
CovVariable <- CovVariable
tkgrid(CovFrame,sticky="w")


tkgrid(StrategyFrame,riskTypeFrame, ConstraintFrame, rightFrame,CovFrame,sticky="w")

  #====Risk-free rate entry
  rfFrame <- tkframe(rightFrame)
  rfVariable <- tclVar("0")
  rfField <- tkentry(rfFrame, width="6", textvariable=rfVariable)
  tkgrid(tklabel(rfFrame, text="Risk free rate = ", fg="blue"), rfField, sticky="w")
  tkgrid(rfFrame, sticky="w")
  tkgrid.configure(rfField, sticky="e")

  #====Lambda entry
  lambdaFrame <- tkframe(rightFrame)
  lambdaVariable <- tclVar("1m")
  lambdaField <- tkentry(lambdaFrame, width="6", textvariable=lambdaVariable)
  tkgrid(tklabel(lambdaFrame, text="Smooth Lambda = ", fg="blue"), lambdaField, sticky="w")
  tkgrid(lambdaFrame, sticky="w")
  tkgrid.configure(lambdaField, sticky="e")

  #====Ok and Quit
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  okButton <- tkbutton(buttonsFrame, text = "OK", command = onOK, anchor = "center", relief="ridge", width = "9")
  tkbind(top,"Q",function() tcl(okButton,"invoke"))
  tkfocus(okButton)
  tkconfigure(okButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(okButton, side = "left",fill = "x",ipady=2)

  quitCMD <- function(){ tkdestroy(top) }

  quitButton<-tkbutton(buttonsFrame, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "9")
  tkconfigure(quitButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(quitButton, side = "left",fill = "x",ipady=2)
  tkfocus(top)
}



.iClickPortfolio <- function (dataset, assetsRM, bench, Type="MV",Rf=0,lambda="1m",plots="simple"){

  if (assetsRM=="None") {
    dat=dataset} else {
      k1=which(colnames(dataset) %in% as.character(assetsRM))
      dat=dataset[,-c(k1)]}

  if (bench=="None") {
    assetReturns=dat
    Rp=rowMeans(dat)
  } else {
    k2=which(colnames(dat)==as.character(bench))
    assetReturns=dat[,-k2]
    Rp=dat[,k2]
  }

  newData=na.omit(cbind(Rp,assetReturns))
  colnames(newData)=c("Rp",colnames(assetReturns))
  # plots="all","simple","no"
  #.iClick.backTesting(newData,Type,Lambda=lambda,plots,Rf)
  .iClick.backTesting(newData,Type,Lambda=lambda,plots,Rf)
}



.iClickBacktesting_Menu <- function(){
  retAS=get("retAS",envir = .JFEEnv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "iClick for Portfolio")

  xBox <- .variableListBox(top,c("None",colnames(retAS)),title="Pick 1 bench asset")
  xBoxRM <- .variableListBox(top, c("None", colnames(retAS)), title="Pick assets you won't use", selectmode = "extended")

  onOK <- function(){
    bench <- .getSelection(xBox)
    assetsRM <- .getSelection(xBoxRM)
    if (length(bench)==0){
      bench=NULL
      tkmessageBox(message = "You did not select a bench asset. rowMeans will be used", icon = "info", type = "ok")
      # return()
    }

    Type <- tclvalue(TypeVariable)
    plots <- tclvalue(plotsVariable)
    Rf <- tclvalue(rfVariable)
    lambda <- tclvalue(lambdaVariable)

    .iClickPortfolio(dataset=retAS,assetsRM,bench,Type,Rf,lambda,plots)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxRM), sticky="n")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

####===== Radio button frame
  rightFrame <- tkframe(top)

  ###====== Risk Type Frame
TypeFrame <- tkframe(rightFrame)
  .radioButtons(top,name="Type", buttons=c("MV", "CVaR"), values=c("MV", "CVaR"), labels=c("Mean-Variance", "CVaR"), title="Type of Portfolio Risk")
TypeVariable <- TypeVariable
  tkgrid(TypeFrame,sticky="w")

  ###====== Plot Frame
plotsFrame <- tkframe(rightFrame)
  .radioButtons(top,name="plots", buttons=c("simple", "all"), values=c("simple", "all"), labels=c("Simple", "All"), title="Numbers of plots")
plotsVariable <- plotsVariable
  tkgrid(plotsFrame,sticky="w")

  #tkgrid(rightFrame,sticky="w")
  #tkgrid(tklabel(rightFrame, text=""))


  tkgrid(plotsFrame, TypeFrame,rightFrame,sticky="w")

#### Entry Frame
  #Risk-free rate entry
  rfFrame <- tkframe(rightFrame)
  rfVariable <- tclVar("0")
  rfField <- tkentry(rfFrame, width="6", textvariable=rfVariable)
  tkgrid(tklabel(rfFrame, text="Risk free rate = ", fg="blue"), rfField, sticky="w")
  tkgrid(rfFrame, sticky="w")
  tkgrid.configure(rfField, sticky="e")

  #Lambda entry
  lambdaFrame <- tkframe(rightFrame)
  lambdaVariable <- tclVar("1m")
  lambdaField <- tkentry(lambdaFrame, width="6", textvariable=lambdaVariable)
  tkgrid(tklabel(lambdaFrame, text="Smooth Lambda = ", fg="blue"), lambdaField, sticky="w")
  tkgrid(lambdaFrame, sticky="w")
  tkgrid.configure(lambdaField, sticky="e")




  #====Ok and Quit
  okButton<-tkbutton(buttonsFrame, text = "OK", command = onOK, anchor = "center", relief="ridge", width = "8")
  tkbind(top,"Q",function() tcl(okButton,"invoke"))
  tkfocus(okButton)
  tkconfigure(okButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(okButton, side = "left",fill = "x",ipady=2)

  quitCMD <- function(){
    tkdestroy(top)
  }

  quitButton<-tkbutton(buttonsFrame, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "8")
  tkconfigure(quitButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(quitButton, side = "left",fill = "x",ipady=2)

  tkfocus(top)
}









.iClick.backTesting <- function(dat,Type="MV",Lambda="1m",plots="all",Rf) {

  #    stopifnot(class(dat) == "timeSeries")

  newData=timeSeries::as.timeSeries(dat)
  idNAMEs=names(newData)[-1]

  myFormula = as.formula(paste("Rp", paste(names(newData)[-1], collapse= "+"),sep="~"))
  COV=c("covEstimator",".covLedoit",".ShrinkCC",".covstudent",".GoldSach","kendallEstimator")


  if (Type=="MV") {
    mySpec1 = portfolioSpec()
    setType(mySpec1)=Type
    setRiskFreeRate(mySpec1) <- as.numeric(Rf)
    #setSolver(mySpec1)= "solveRquadprog"

    OUT.tangency=list()
    for (i in 1:length(COV)){
      setEstimator(mySpec1)=COV[i]
      myBacktest1 = portfolioBacktest()
      setStrategyFun(myBacktest1)="tangencyStrategy"
      myPort1 = portfolioBacktesting(myFormula,data = newData, spec = mySpec1, backtest=myBacktest1,constraints = "LongOnly",trace = FALSE)
      setSmootherLambda(myBacktest1) = Lambda
      mySmoothPort.tmp1 = portfolioSmoothing(object=myPort1,trace = FALSE)
      OUT.tangency[[i]]=mySmoothPort.tmp1
    }

    OUT.GMVP=list()
    mySpec2 = portfolioSpec()
    setType(mySpec2)=Type
    setRiskFreeRate(mySpec2) <- as.numeric(Rf)
    for (i in 1:length(COV)){
      setEstimator(mySpec2)=COV[i]
      myBacktest2 = portfolioBacktest()
      setStrategyFun(myBacktest2)="GMVPStrategy"
      myPort2 = portfolioBacktesting(myFormula,data = newData, spec = mySpec2,trace = FALSE, backtest=myBacktest2,constraints = "LongOnly")
      setSmootherLambda(myBacktest2) = Lambda
      mySmoothPort.tmp2 = portfolioSmoothing(object=myPort2,trace = FALSE)
      OUT.GMVP[[i]]=mySmoothPort.tmp2
    }

  } else if (Type=="CVaR") {
    mySpec3 = portfolioSpec()
    setType(mySpec3)=Type
    setRiskFreeRate(mySpec3) <- as.numeric(Rf)
    setSolver(mySpec3)= "solveRglpk.CVAR"
    OUT.tangency=list()
    for (i in 1:length(COV)){
      setEstimator(mySpec3)=COV[i]
      myBacktest3 = portfolioBacktest()
      setStrategyFun(myBacktest3)="tangencyStrategy"
      myPort3 = portfolioBacktesting(myFormula,data = newData, spec = mySpec3,trace = FALSE, backtest=myBacktest3,constraints = "LongOnly")
      setSmootherLambda(myBacktest3) = Lambda
      mySmoothPort.tmp3 = portfolioSmoothing(object=myPort3,trace = FALSE)
      OUT.tangency[[i]]=mySmoothPort.tmp3
    }

    mySpec4 = portfolioSpec()
    setType(mySpec4)= Type
    setRiskFreeRate(mySpec4) <- as.numeric(Rf)
    setSolver(mySpec4)= "solveRglpk.CVAR"
    OUT.GMVP=list()
    for (i in 1:length(COV)){
      setEstimator(mySpec4)=COV[i]
      myBacktest4 = portfolioBacktest()
      setStrategyFun(myBacktest4)="GMVPStrategy"
      myPort4 = portfolioBacktesting(myFormula,data = newData, spec = mySpec4, trace = FALSE, backtest=myBacktest4,constraints = "LongOnly")
      setSmootherLambda(myBacktest4) = Lambda
      mySmoothPort.tmp4 = portfolioSmoothing(object=myPort4,trace = FALSE)
      OUT.GMVP[[i]]=mySmoothPort.tmp4
    }

  }
  savedfile1=paste(".",Type,"_Tangency.RData",sep="")
  savedfile2=paste(".",Type,"_GMVP.RData",sep="")

  NAMES=c(
    paste("1.  ", paste(Type," + Tangency", sep=""),sep=""),
    paste(2:(length(COV)+1),COV,sep=".  "),
    paste("8.  Save ", savedfile1, sep=""),
    paste("9.  ", Type," + GMVP", sep=""),
    paste(10:(length(COV)+9),COV,sep=". "),
    paste("16. Save ", savedfile2, sep=""))

  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

  dataRefreshCode <- function(...)  {
    type = as.integer(.iClickBackTesting(obj.name = "plotType"))


    ## 1. Show results
    if (type == 1) {
      print(paste(Type," + Tangency", sep=""))
    }

    ## 2.
    if (type == 2) {
      if (plots=="no"){} else if (plots=="simple") {
        dev.new;
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[1]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        dev.new;
        backtestPlot(OUT.tangency[[1]], cex = 0.6, font = 1, family = "mono", which="all")}
      netPerformance(OUT.tangency[[1]])

      smoothWeights = round(100*OUT.tangency[[1]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))

    }

    ## 3.
    if (type == 3) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[2]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.tangency[[2]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.tangency[[2]])

      smoothWeights = round(100*OUT.tangency[[2]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 4.
    if (type == 4) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[3]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.tangency[[3]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.tangency[[3]])

      smoothWeights = round(100*OUT.tangency[[3]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 5.
    if (type == 5) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[4]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.tangency[[4]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.tangency[[4]])

      smoothWeights = round(100*OUT.tangency[[4]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))

    }

    # 6
    if (type == 6) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[5]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.tangency[[5]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.tangency[[5]])

      smoothWeights = round(100*OUT.tangency[[5]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ##  7
    if (type == 7) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.tangency[[6]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.tangency[[6]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.tangency[[6]])

      smoothWeights = round(100*OUT.tangency[[6]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))

    }

    ## 8  save results
    if (type == 8) {

      save(OUT.tangency,file=savedfile1)}

    ## 9. Show results
    if (type == 9) {
      print(paste(Type," + GMVP", sep=""))
    }

    ## 10.
    if (type == 10) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[1]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[1]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[1]])

      smoothWeights = round(100*OUT.GMVP[[1]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 11.
    if (type == 11) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[2]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[2]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[2]])
      smoothWeights = round(100*OUT.GMVP[[2]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 12.
    if (type == 12) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[3]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[3]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[3]])

      smoothWeights = round(100*OUT.GMVP[[3]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 13.
    if (type == 13) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[4]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[4]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[4]])

      smoothWeights = round(100*OUT.GMVP[[4]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    # 14
    if (type == 14) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[5]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[5]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[5]])

      smoothWeights = round(100*OUT.GMVP[[5]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ##  15
    if (type == 15) {
      dev.new;
      if (plots=="simple") {
        par(mfrow=c(3,1))
        backtestPlot(OUT.GMVP[[6]], cex = 0.6, font = 1, family = "mono", which=c(4:6))
        par(mfrow=c(1,1))
      } else if (plots=="all"){
        backtestPlot(OUT.GMVP[[6]], cex = 0.6, font = 1, family = "mono", which="all")} else if (plots=="no"){}
      netPerformance(OUT.GMVP[[6]])
      smoothWeights = round(100*OUT.GMVP[[6]]$smoothWeights,2)
      ID=colnames(smoothWeights)
      colnames(smoothWeights)=unlist(lapply(strsplit(ID,"X"),function(x) x[2]))
      id.no=which(tail(smoothWeights,1)!=0)
      selection=tail(smoothWeights,1)[,id.no]
      selection=data.frame(idNAMEs[id.no],selection)
      colnames(selection)=c("Asset","Weights")
      print(paste("Next-Period Advice by ",COV[1],sep=""))
      print(data.frame(selection))
    }

    ## 16  save results
    if (type == 16) {
      save(OUT.GMVP,file=savedfile2)   }

  }  #End of dataRefreshCode()

  nAssets = dim(dat)[2]

  .iClickBackTesting(
    dataRefreshCode,
    names       = c("Selected Asset"),
    minima      = c(      0),
    maxima      = c(      nAssets),
    resolutions = c(      1),
    starts      = c(      0),

    button.functions = list(
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "1")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "2")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "3")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "4")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "5")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "6")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "7")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "8")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "9")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "10")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "11")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "12")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "13")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "14")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "15")
        dataRefreshCode()},
      function(...){
        .iClickBackTesting(obj.name = "plotType", obj.value = "16")
        dataRefreshCode()}
    ),

    button.names = NAMES,

    title = "iClick Portfolio Analysis: Back-testing"
  )

  .iClickBackTesting(obj.name = "type", obj.value = "1", no = 1)

  # Return Value()
  invisible()
}


.iClickBackTesting.env = new.env()


.iClickBackTesting <-
  function(names, minima, maxima, resolutions, starts,button.functions, button.names, no, set.no.value, obj.name, obj.value,reset.function, title)
  {

    if(!exists(".iClickBackTesting.env")) {
      .iClickBackTesting.env <<- new.env()
    }
    if(!missing(obj.name)){
      if(!missing(obj.value)) {
        assign(obj.name, obj.value, envir = .iClickBackTesting.env)
      } else {
        obj.value <- get(obj.name, envir = .iClickBackTesting.env)
      }
      return(obj.value)
    }
    if(missing(title)) {
      title = "Control Widget"
    }

    # GUI Settings:
    myPane <- tktoplevel()
    tkwm.title(myPane, title)
    tkwm.geometry(myPane, "+0+0")

    # Buttons:
    PADDING= c(3,3,3,10)
    framed.buttonA <- ttkframe(myPane,padding=PADDING)
    tkpack(framed.buttonA,side="left")
    framed.button1 <- ttkframe(framed.buttonA,padding=PADDING)
    tkpack(framed.button1)
    framed.button2 <- ttkframe(framed.buttonA,padding=PADDING)
    tkpack(framed.button2, fill = "x")



    if (missing(button.names)) {
      button.names <- NULL
    }

    #loop through button names
    for (i in 1:8) {
      button.fun <-button.functions[[i]]
      plotButtons<-tkbutton(framed.button1, text = button.names[i], command = button.fun, anchor = "nw",relief="ridge",width = "45")
      tkconfigure(plotButtons,foreground="blue",font=tkfont.create(size=12
                                                                   ,weight="bold"))
      tkpack(plotButtons,fill = "x", pady=1)

    }
    for (i in 9:16) {
      button.fun <-button.functions[[i]]
      plotButtons<-tkbutton(framed.button2, text = button.names[i], command = button.fun, anchor = "nw",relief="ridge",width = "45")
      tkconfigure(plotButtons,foreground="blue",font=tkfont.create(size=12
                                                                   ,weight="bold"))
      tkpack(plotButtons,fill = "x", pady=1)
    }




    #===== Quit Button:
    quitCMD = function() {
      tkdestroy(myPane)
      Sys.setlocale(category = "LC_ALL", locale = "Chinese (Traditional)_Taiwan.950")
    }

    quitButton<-tkbutton(framed.buttonA, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "8")
    tkbind(myPane,"Q",function() tcl(quitButton,"invoke"))
    tkfocus(quitButton)
    tkconfigure(quitButton,foreground="indianred2", font=tkfont.create(weight="bold",size=12))
    tkconfigure(quitButton,underline=0)

    tkpack(quitButton, side = "right",fill = "x",padx=1)


    #assign("iClickBackTesting.values.old", starts, envir = .iClickBackTesting.env)

    # Return Value:
    invisible(myPane)
  }



.covLedoit <- function (data, spec = NULL) {
  x.mat = as.matrix(data)
  list(mu = colMeans(x.mat), Sigma = .SKCov(x.mat)$sigma)
}


.SKCov <- function(data) {
  dat=data
  t=nrow(dat)
  n=ncol(dat)

  x=(diag(t)-matrix(1,t,t)/t)%*%dat

  xmkt=apply(x,1,mean)
  sMat=data.frame(x, xmkt)
  sample=cov(sMat)*(t-1)/t
  covmkt=sample[1:n,n+1]
  varmkt=sample[n+1,n+1]

  sample=sample[,-(n+1)]
  sample=sample[-(n+1),]

  prior=covmkt%*%t(covmkt)/varmkt
  diag(prior)=diag(sample)

  c=base::norm(sample-prior,"f")^2
  y=x^2
  p=1/t*sum(sum(t(y)%*%y))-sum(sum(sample^2))

  # r is divided into diagonal
  # and off-diagonal terms, and the off-diagonal term
  # is itself divided into smaller terms
  rdiag=1/t*sum(sum(y^2))-sum(diag(sample)^2)
  z=x*replicate(n,as.numeric(xmkt))

  v1=1/t*(t(y)%*%z)-replicate(n,covmkt)*sample

  roff1=sum(sum(v1*t(replicate(n,covmkt))))/varmkt-sum(diag(v1)*covmkt)/varmkt
  v3=1/t*t(z)%*%z-varmkt*sample
  roff3=sum(sum(v3*(covmkt%*%t(covmkt))))/varmkt^2-sum(diag(v3)*covmkt^2)/varmkt^2
  roff=2*roff1-roff3
  r=rdiag+roff
  # compute shrinkage constant
  k=(p-r)/c
  skg=max(0,min(1,k/t))

  # compute the estimator
  sig=skg*prior+(1-skg)*sample
  w=list(sigma=sig,shrinkage=skg,prior=prior)
  return(w)
}

.covStudent <- function (data, spec = NULL) {
  ###===Multivariate Student t=======###
  x.mat =as.matrix(data)
  list(mu = colMeans(x.mat), Sigma = MASS::cov.trob(x.mat)$cov)
}


.GMVPStrategy <-function (data, spec = portfolioSpec(), constraints = "LongOnly", backtest = portfolioBacktest()) {
  strategyPortfolio <- try(minriskPortfolio(data, spec, constraints))
  if (class(strategyPortfolio) == "try-error") {
    strategyPortfolio <- minvariancePortfolio(data,spec,constraints)
  }
  strategyPortfolio
}


###==========###

.GoldSach <- function(data, spec = NULL){
  x.mat = data
  rho=0.95
  t=nrow(x.mat)
  p=(t-1):0
  w=rho^p
  dat=sqrt(w)*x.mat
  Sig=cov(dat)*t/sum(w)
  list(mu =colMeans(x.mat) , Sigma =Sig)
}

###===Constant Correlation===###

.ShrinkCC <- function (data,spec = NULL) {
  x.mat =data
  t=nrow(x.mat)
  p=(t-1):0
  rho=0.95
  w=rho^p
  list(mu = colMeans(x.mat), Sigma = BurStFin::var.shrink.eqcor(x.mat,weights=1))
}





###==========###
#.LPM<-function(data, spec = NULL){
#  list(mu = fAssets::assetsLPM(data)$mu, Sigma = fAssets::assetsLPM(data)$Sigma)
#}

