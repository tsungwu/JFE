.readme4Selection <- function() {
cat("\n","1. Assets Selection here is basically based on Sharpe ratio inequality. The method is designed for Internaitonal Asset Selection, interested readers please refer to Bekaert and Hodrick(2009),International Financial Management,PP.466-468. We provide more performance indices based on the R package PerformanceAnalytics.","\n","2. To use this tool, you must have a multivariate time series dataset with R format, xts is most encourgaed; and the file is saved in .RData or .rda. Users may ude the dataset world20.rda located in the data directory of this package, detail is explained in the manual.","\n","3.  If the loaded data is price, then you have to pull down the menu and choose Transform Price Data, else, Load Returns Data","\n")
}

.getReturns4Selection <- function(){
  if ("PerformanceAnalytics" %in% (.packages())) {print("package PerformanceAnalytics is loaded")} else {
    eval(parse( text="library(PerformanceAnalytics)"))}

  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData}  } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dataz=eval(parse(text=temp))
  dat=timeSeries::returns(timeSeries::as.timeSeries(dataz))
  dat=xts::as.xts(dat)
  assign("retAS", dat, envir = .JFEEnv)
  #  .evalCmdChunk("summary(dat)")
  cat("Returns data is imported sucessfully","\n")
  print(tail(dat,2));print(head(dat,2))
  cat("\n")
}

#====================================
.sharpeIneq <- function(datx0, home,removal,index,Rf=0,MAR=0,Rb,splitDate,FREQtype){

if (FREQtype != "daily") {
y0=timeSeries::as.timeSeries(datx0)
datx=xts::as.xts(timeSeries::as.timeSeries(y0,as.Date(index(datx0))))} else {datx=datx0}

print(head(datx))
  T0=as.integer(nrow(datx)*as.numeric(splitDate))

  dat0=datx[1:T0,]

  MAR=as.numeric(MAR)
  Rf=as.numeric(Rf)

  if(Rb== "None"){
    target=as.xts(apply(datx,1,mean))
    benchTitle="bench portfolio by average"
  } else {
    target=datx[,Rb]
    benchTitle=paste0("bench portfolio is ", Rb)
  }

  if(removal== "None"){
    datz=dat0
  } else {
    NO=which(colnames(dat0) %in% removal)
    datz=dat0[,-c(NO)]
  }

  IDab=colnames(datz)
  dataz=na.omit(datz)

  domestic=which(colnames(dataz)==home)
  homeAsset=dataz[,domestic]
  foreign=dataz[,-domestic]

  HomeID=IDab[domestic]
  foreignID=IDab[-domestic]

  rho=as.data.frame(cor(homeAsset,foreign))
  if(index=="StdDev"){
    left=PerformanceAnalytics::SharpeRatio(foreign,Rf,FUN="StdDev")
    right=PerformanceAnalytics::SharpeRatio(homeAsset,Rf,FUN="StdDev")
  }
  else if(index=="VaR"){
    left=PerformanceAnalytics::SharpeRatio(foreign,Rf,FUN="VaR")
    right=PerformanceAnalytics::SharpeRatio(homeAsset,Rf,FUN="VaR")
  }
  else if(index=="ES"){
    left=PerformanceAnalytics::SharpeRatio(foreign,Rf,FUN="ES")
    right=PerformanceAnalytics::SharpeRatio(homeAsset,Rf,FUN="ES")
  }
  else {
  left=eval(parse(text=index))(foreign,Rf)
  right=eval(parse(text=index))(homeAsset,Rf)
  }

  Ineq=left-rho*right
  id=Ineq>0

  A=c(colnames(foreign[,which(id)]),colnames(dataz[,domestic]))
  dataSelected=datx[(T0+1):nrow(datx),A]
  Selected <- foreignID[which(id)]

  portfolioReturns=as.xts(apply(dataSelected,1,mean))

  HomeIDk =as.character(HomeID)
  cat("\n","The home asset is","\n"); print(HomeIDk)
  cat(length(Selected), "assets selected by",index, "are ", "\n")
  print(as.character(Selected))

  if (index=="StdDev"||index=="VaR"||index=="ES"){
  Performance=PerformanceAnalytics::SharpeRatio(portfolioReturns,Rf,FUN=index)
  cat("\n",FREQtype,paste(" Performance by Sharpe ratio with", index,"is"),Performance ,"\n")
  Title=paste(FREQtype," Performance by Sharpe ratio with", index)
  } else {

  Performance=eval(parse(text=index))(portfolioReturns,Rf)
  print(paste(FREQtype," Performance by",  index)); print(Performance)
  cat("\n",paste(FREQtype," Performance by",  index,"is"),Performance ,"\n")
  Title=paste(FREQtype," Performance by ", index)
}
    cat("\n","Table of Annualized Returns is","\n")
    print(PerformanceAnalytics::table.AnnualizedReturns(as.xts(portfolioReturns)))

  ###===== Plotting
  bench=timeSeries::as.timeSeries(target[(T0+1):nrow(datx)])
  Y=timeSeries::as.timeSeries(portfolioReturns)
  colnames(Y)=Title
  MIN=min(c(as.numeric(cumsum(Y)),as.numeric(cumsum(bench))))*0.98
  MAX=max(c(as.numeric(cumsum(Y)),as.numeric(cumsum(bench))))*1.15

  dev.new();plot.new();fBasics::cumulatedPlot(Y,ylab="",col="red",ylim=100*exp(c(MIN,MAX)));abline(h=100);lines(100*exp(timeSeries::colCumsums(bench)),col="blue");graphics::legend("topleft", c("Portfolio",benchTitle), text.col = c("red","blue"),bty="n")

  return(list(HomeID=as.character(HomeID),Selected=as.character(Selected)))
}


.sharpeIneqMenu <- function(){
  retAS=get("retAS",envir = .JFEEnv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "Asset Selection by Sharpe inequality")

  xBox <- .variableListBox(top, colnames(retAS), title="Pick home asset")
  xBoxNo <- .variableListBox(top, c("None",colnames(retAS)), title="Pick 1 or more to remove", selectmode = "extended")
  xBoxRb <- .variableListBox(top, c("None",colnames(retAS)), title="Pick bench asset")


  onOK <- function(){
    home <- .getSelection(xBox)
    removal <- .getSelection(xBoxNo)
    Rb <- .getSelection(xBoxRb)

    if (ncol(retAS) == 0){
      tkmessageBox(message = "You must import a dataset.", icon = "error", type = "ok")
      return()
    }

    FREQtype <-  tclvalue(freqVariable)
    index <- tclvalue(selectionVariable)

    Rf <- tclvalue(rfVariable)
    MAR <- tclvalue(marVariable)
    splitDate <- tclvalue(splitVariable )
    if (FREQtype=="daily"){
      x=retAS } else {
#        transForm=paste("timeSeries::",FREQtype,"(retAS)",sep="")
    transForm=paste0("xts::to.",FREQtype,"(retAS,OHLC = FALSE)")
        x=eval(parse(text=transForm))
      }

    output=.sharpeIneq(x,home,removal,index,Rf,MAR,Rb,splitDate,FREQtype)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxNo), .getFrame(xBoxRb), sticky="n")

   rightFrame <- tkframe(top)

freqFrame <- tkframe(rightFrame)
.radioButtons(top,name="freq", buttons=c("Daily", "Week", "Month","Quarter"), values=c("daily", "weekly", "monthly", "quarterly"), labels=c("Daily data(Default)", "Weekly freq", "Monthly freq","Quarterly freq"), title="Frequency Conversion")
freqVariable <- freqVariable
tkgrid(freqFrame,rightFrame,sticky="w")


selectionFrame <- tkframe(rightFrame)
.radioButtons(top,name="selection", buttons=c("StdDev", "VaR", "ES","AdjustedSharpeRatio","SharpeRatio.annualized", "BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), values=c("StdDev", "VaR", "ES","AdjustedSharpeRatio", "SharpeRatio.annualized","BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), labels=c("Sharpe By Std Dev.", "Sharpe By VaR","Sharpe By ES(CVaR)","Adjusted Sharpe", "Annualized Sharpe","Burke Ratio", "Kelly Ratio","Pain Ratio","Bernardo-Ledoit","Martin Ratio","Mean Absolute Deviation"), title="Performance Index")
selectionVariable <- selectionVariable
tkgrid(selectionFrame,sticky="w")

tkgrid(selectionFrame, rightFrame,sticky="w")



  ## Risk-free frame
  rfFrame <- tkframe(rightFrame)
  rfVariable <- tclVar("0")
  rfField <- tkentry(rfFrame, width="4", textvariable=rfVariable)
  tkgrid(tklabel(rfFrame, text="Risk free rate = ", fg="blue"), rfField, sticky="w")
  tkgrid(rfFrame, sticky="w")

  ## MAR frame
  marFrame <- tkframe(rightFrame)
  marVariable <- tclVar("0")
  marField <- tkentry(marFrame, width="4", textvariable=marVariable)
  tkgrid(tklabel(marFrame, text="Min. acceptable return = ", fg="blue"), marField, sticky="w")
  tkgrid(marFrame, sticky="w")

  ## Sample Split frame

  splitFrame <- tkframe(rightFrame)
  splitVariable <- tclVar("0.7")
  splitField <- tkentry(splitFrame, width="6", textvariable=splitVariable)
  tkgrid(tklabel(splitFrame, text="Training Percentage = ", fg="blue"), splitField, sticky="w")
  tkgrid(splitFrame, sticky="w")

  #tkgrid.configure(splitField, sticky="nw")
  #tkgrid.configure(rfField,marField, sticky="w")



  #====Ok and Quit
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  okButton <- tkbutton(buttonsFrame, text = "OK", command = onOK, anchor = "center", relief="ridge", width = "9")
  tkbind(top,"Q",function() tcl(okButton,"invoke"))
  tkfocus(okButton)
  tkconfigure(okButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(okButton, side = "left",fill = "x",ipady=2)

  quitCMD <- function(){
    tkdestroy(top)
  }

  quitButton<-tkbutton(buttonsFrame, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "9")
  tkconfigure(quitButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(quitButton, side = "left",fill = "x",ipady=2)

  tkfocus(top)
}