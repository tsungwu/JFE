
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
}

#====================================
.sharpeIneq <- function(datx, home,removal,index,Rf=0,MAR=0,Rb,splitDate){

  T0=which(as.character(time(datx))==as.character(splitDate))

  dat0=datx[1:T0,]

  MAR=as.numeric(MAR)
  Rf=as.numeric(Rf)
  if(removal== "None"){
    datz=dat0
  } else {
    NO=which(colnames(dat0) %in% removal)
    datz=dat0[,-c(NO)]

  }

  IDab=colnames(datz)
  dataz=na.omit(datz)


  if(Rb== "None"){
    target=as.xts(apply(datx,1,mean))
  } else {
    target=datx[,Rb]
  }



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
  else if(index=="CVaR"){
    left=PerformanceAnalytics::SharpeRatio(foreign,Rf,FUN="ES")
    right=PerformanceAnalytics::SharpeRatio(homeAsset,Rf,FUN="ES")
  }
  else if(index=="AdjustedSharpeRatio"){
    left=PerformanceAnalytics::AdjustedSharpeRatio(foreign,Rf)
    right=PerformanceAnalytics::AdjustedSharpeRatio(homeAsset,Rf)
  }
  else if(index=="BurkeRatio"){
    left=PerformanceAnalytics::BurkeRatio(foreign,Rf)
    right=PerformanceAnalytics::BurkeRatio(homeAsset,Rf)
  }

  else if(index=="SharpeRatio.annualized"){
    left=PerformanceAnalytics::SharpeRatio.annualized(foreign,Rf)
    right=PerformanceAnalytics::SharpeRatio.annualized(homeAsset,Rf)
  }

  else if(index=="KellyRatio"){
    left=PerformanceAnalytics::KellyRatio(foreign,Rf)
    right=PerformanceAnalytics::KellyRatio(homeAsset,Rf)
  }

  else if(index=="PainRatio"){
    left=PerformanceAnalytics::PainRatio(foreign,Rf)
    right=PerformanceAnalytics::PainRatio(homeAsset,Rf)
  }

  else if(index=="BernardoLedoitRatio"){
    left=PerformanceAnalytics::BernardoLedoitRatio(foreign,Rf)
    right=PerformanceAnalytics::BernardoLedoitRatio(homeAsset,Rf)
  }


  else if(index=="MartinRatio"){
    left=PerformanceAnalytics::MartinRatio(foreign,Rf)
    right=PerformanceAnalytics::MartinRatio(homeAsset,Rf)
  }


  else if(index=="MeanAbsoluteDeviation"){
    left=PerformanceAnalytics::MeanAbsoluteDeviation(foreign,Rf)
    right=PerformanceAnalytics::MeanAbsoluteDeviation(homeAsset,Rf)
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

  sr1=PerformanceAnalytics::SharpeRatio(portfolioReturns,Rf,FUN="StdDev")
  sr2=PerformanceAnalytics::SharpeRatio(portfolioReturns,Rf,FUN="VaR")
  sr3=PerformanceAnalytics::SharpeRatio(portfolioReturns,Rf,FUN="ES")
  cat("\n","Performance by Sharpe ratio","\n")
  print(rbind(sr1,sr2,sr3))
  cat("\n","Table of Annualized Returns","\n");
  print(PerformanceAnalytics::table.AnnualizedReturns(as.xts(portfolioReturns)))

  #SR=rbind(sr1,sr2,sr3)
  #assign("SR", SR, envir = .JFEEnv)
  #.evalCmdChunk("print(table.Stats(portfolioReturns));cat('\n');print(table.AnnualizedReturns(portfolioReturns));cat('\n');print(SR)")

  bench=timeSeries::as.timeSeries(target[(T0+1):nrow(datx)])
  Y=timeSeries::as.timeSeries(portfolioReturns)
  colnames(Y)="Portfolio"
  MIN=min(c(as.numeric(cumsum(Y)),as.numeric(cumsum(bench))))*0.98
  MAX=max(c(as.numeric(cumsum(Y)),as.numeric(cumsum(bench))))*1.15

  dev.new();plot.new();fBasics::cumulatedPlot(Y,ylab="",col="red",ylim=100*exp(c(MIN,MAX)))
  abline(h=100);lines(100*exp(timeSeries::colCumsums(bench)),col="blue");graphics::legend("topleft", c("Portfolio","Bench"), text.col = c("red","blue"),bty="n")

  return(list(HomeID=as.character(HomeID),Selected=as.character(Selected)))
}


.sharpeIneqMenu <- function(){
  retAS=get("retAS",envir = .JFEEnv)
  top <- tktoplevel(borderwidth=45)
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
        transForm=paste("timeSeries::",FREQtype,"(retAS)",sep="")
        x=eval(parse(text=transForm))
      }

    output=.sharpeIneq(x,home,removal,index,Rf,MAR,Rb,splitDate)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxNo), .getFrame(xBoxRb), sticky="n")
  #tkgrid(.getFrame(xBoxNo), sticky="w")

   rightFrame <- tkframe(top)

freqFrame <- tkframe(rightFrame)
.radioButtons(top,name="freq", buttons=c("Daily", "Week", "Month"), values=c("daily", "daily2weekly", "daily2monthly"), labels=c("Default daily data", "Use weekly freq", "Use monthly freq"), title="Frequency Conversion")
freqVariable <- freqVariable
tkgrid(freqFrame,sticky="w")


selectionFrame <- tkframe(rightFrame)
.radioButtons(top,name="selection", buttons=c("StdDev", "VaR", "CVaR","AdjustedSharpeRatio","SharpeRatio.annualized", "BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), values=c("StdDev", "VaR", "CVaR","AdjustedSharpeRatio", "SharpeRatio.annualized","BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), labels=c("Sharpe By Std Dev.", "Sharpe By VaR","Sharpe By CVaR","Adjusted Sharpe", "Annualized Sharpe","Burke Ratio", "Kelly Ratio","Pain Ratio","Bernardo-Ledoit","Martin Ratio","Mean Absolute Deviation"), title="Performance Index")
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

  cut=as.integer(length(time(retAS))*0.7)
  splitFrame <- tkframe(rightFrame)
  splitVariable <- tclVar(as.character(as.Date(time(retAS)[cut])))
  splitField <- tkentry(splitFrame, width="12", textvariable=splitVariable)
  tkgrid(tklabel(splitFrame, text="Cutting date = ", fg="blue"), splitField, sticky="w")
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
