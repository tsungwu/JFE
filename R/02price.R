.getPrice <- function() {
  if ("quantmod" %in% (.packages())) {print("package quantmod is loaded")} else {
   eval(parse( text="library(quantmod)"))}

  name <- tclvalue(tkgetOpenFile(
 filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
	temp=print(load(name))
	dat=eval(parse(text=temp))
  assign("Price", dat, envir = .JFEEnv)

importedFileName=last(unlist(strsplit(name,"/")))
assign("importedFileName", importedFileName, envir = .JFEEnv)
print(paste("You are loading ",importedFileName,sep=" "))
print(head(dat,3))
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
#.evalCmdChunk(head(dat))
}


.priceSummary <- function(){
Price=get("Price",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Descriptive Statistics")

xBox <- .variableListBox(top, colnames(Price), title="Variables (Pick 1 or more)", selectmode = "extended")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
#eval_cmd_chunk("fBasics::basicStats(Price[,x])")

print(fBasics::basicStats(timeSeries::as.timeSeries(Price[,x])))
}
  tkgrid(.getFrame(xBox), sticky="nw")
# tkgrid(xBox, sticky="nw")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

okButton<-tkbutton(buttonsFrame, text = "OK", command = onOK, anchor = "center", relief="ridge", width = "8")
tkbind(top,"Q",function() tcl(okButton,"invoke"))
tkfocus(okButton)
tkconfigure(okButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
tkpack(okButton, side = "left",fill = "x",ipady=2)

quitCMD <- function(){
    tkdestroy(top)
}

quitButton <- tkbutton(buttonsFrame, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "8")
tkconfigure(quitButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
tkpack(quitButton, side = "left",fill = "x",ipady=2)
tkfocus(top)
}


.pricePlot <- function(){
Price=get("Price",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Plotting")

xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
dev.new();fBasics::seriesPlot(timeSeries::as.timeSeries(Price[,x]))

}
  tkgrid(.getFrame(xBox), sticky="nw")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

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


.PriceAcfPlots <- function(){
Price=get("Price",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "ACF Plots")

xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick 1 )")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }

dev.new();par(mfrow=c(2,1))
fBasics::acfPlot(timeSeries::as.timeSeries(Price[,x]))
fBasics::pacfPlot(timeSeries::as.timeSeries(Price[,x]))
par(mfrow=c(1,1))
}
  tkgrid(.getFrame(xBox), sticky="nw")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

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

.PriceBoxPlots <- function(){
Price=get("Price",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "ACF Plots")

xBox <- .variableListBox(top, colnames(Price), title="Variables (Pick 1 or more)", selectmode = "extended")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
dev.new();fBasics::boxPlot(timeSeries::as.timeSeries(Price[,x]))

}
  tkgrid(.getFrame(xBox), sticky="nw")

#OK and Quit
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

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



.priceCharting <- function(){
  Price=get("Price",envir = .JFEEnv)
  Price=xts::as.xts(Price)

  top <- tktoplevel(borderwidth=20)
  tkwm.title(top, "Charting series")


  importedFileName=get("importedFileName",envir = .JFEEnv)

  onOK <- function(){

    if (ncol(Price) == 0 || !quantmod::is.OHLCV(Price)|| !quantmod::is.OHLC(Price)){
      tkmessageBox(message = "You must import a OHLC dataset.", icon = "error", type = "ok")
      return()
    }

    FREQtype <-  tclvalue(freqVariable)
    addType <-   tclvalue(addVariable)
    a <- tclvalue(startVariable)
    b <- tclvalue(endVariable)
    themeCOLOR <- tclvalue(themeColVariable)
    dataz=Price[paste(a,b,sep="/")]
    transForm=paste("xts::to.",FREQtype,"(dataz)",sep="")
    x=eval(parse(text=transForm))

    NAMES=paste(importedFileName,FREQtype,sep=" by ")

    if (addType=="addVo()") {
      dev.new()
      print(paste("You are charting",importedFileName,sep=" "))
      quantmod::chartSeries(x,name=NAMES,theme=quantmod::chartTheme(themeCOLOR))}
    else {
      dev.new()
      ADD=paste("addVo();",addType,sep="")
      print(paste("You are charting",importedFileName,sep=" "))
      quantmod::chartSeries(x,name=NAMES,TA=ADD,theme=quantmod::chartTheme(themeCOLOR))
    }

  }

  rightFrame <- tkframe(top)
## frequency frame by radio button
  freqFrame <- tkframe(rightFrame)
.radioButtons(top,name="freq", buttons=c("Daily", "Week", "Month","Quarter"), values=c("daily", "weekly", "monthly", "quarterly"), labels=c("Daily data(Default)", "Weekly freq", "Monthly freq","Quarterly freq"), title="Frequency Conversion")
  freqVariable<-freqVariable
  tkgrid(freqFrame, sticky="w")
  tkgrid.configure(freqFrame, sticky="nw")

## themeColor frame  by radio button
#  leftFrame <- tkframe(top)
themeColFrame <- tkframe(rightFrame)
.radioButtons(top,name="themeCol",buttons=c("black","white"),values=c("black","white"), labels=c("black(default)","white"), title="Theme Color")
  themeColVariable <- themeColVariable
  tkgrid(themeColFrame,freqFrame,rightFrame,sticky="n")
#  tkgrid.configure(themeColFrame, sticky="nw")

## TA add() frame  by radio button
LABEL01 = c("Chart with volume","Add Directional Movement Index","Add Bollinger Bands to Chart","Add Commodity Channel Index","Add Contract Expiration Bars","Add Rate Of Change","Add Relative Strength Index","Add Parabolic Stop and Reversal")
LABEL02 = c("Add Stochastic Momentum Indicator","Add William's Percent R to Chart","Add SMA Moving Average to Chart","Add EMA Moving Average","Add DEMA Moving Average to Chart", "Add EVMA Moving Average to Chart", "Add EVWMA Moving Average to Chart","Add ZLEMA Moving Average to Chart","Add Moving Average Convergence Divergence to Chart")
buttons1=c("addVo","addADX", "addBBands","addCCI","addExpiry","addROC","addRSI","addSAR")
buttons2=c("addSMI","addWPR","addSMA","addEMA","addWMA", "addDEMA","addEVMA","addZLEMA","addMACD")
values1=c("addVo()","addADX()","addBBands()","addCCI()","addExpiry()","addROC()","addRSI()","addSAR()")
values2=c("addSMI()","addWPR()","addSMA()","addEMA()","addWMA()", "addDEMA()","addEVWMA()","addZLEMA()","addMACD()")
  LABELS=c(LABEL01,LABEL02)
  BUTTONS=c(buttons1,buttons2)
  VALUES=c(values1,values2)

  addFrame <- tkframe(rightFrame)
.radioButtons(top,name="add",buttons=BUTTONS,values=VALUES, labels=LABELS, title="Add")
  addVariable <- addVariable
  tkgrid(addFrame, sticky="w")
  tkgrid.configure(addFrame, sticky="nw")


 tkgrid(freqFrame,addFrame,themeColFrame,rightFrame,sticky="w")


 ## startFrame by entry
  startFrame <- tkframe(rightFrame)
  startVariable <- tclVar(as.character(as.Date(time(Price)[1])))
  startField <- tkentry(startFrame, width="12", textvariable=startVariable)
  tkgrid(tklabel(startFrame, text="Start date = ", fg="blue"), startField, sticky="w")
  tkgrid(startFrame, sticky="w")
  tkgrid.configure(startField, sticky="nw")

#  endFrame by entry
  endFrame <- tkframe(rightFrame)
  endVariable <- tclVar(as.character(as.Date(time(Price)[nrow(Price)])))
  endField <- tkentry(endFrame, width="12", textvariable=endVariable)
  tkgrid(tklabel(endFrame, text="End  date = ", fg="blue"), endField, sticky="w")
  tkgrid(endFrame, sticky="w")
  tkgrid.configure(endField, sticky="nw")



  #====ok and quit
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  okButton <- tkbutton(buttonsFrame, text = "OK", command = onOK, anchor = "center", relief="ridge", width = "8")
  tkbind(top,"Q",function() tcl(okButton,"invoke"))
  tkfocus(okButton)
  tkconfigure(okButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(okButton, side = "left",fill = "x",ipady=1)

  quitCMD <- function(){
    tkdestroy(top)
  }

  quitButton<-tkbutton(buttonsFrame, text = "Quit", command = quitCMD, anchor = "center",relief="ridge",width = "8")
  tkconfigure(quitButton,foreground="red",font=tkfont.create(size=9,weight="bold"))
  tkpack(quitButton, side = "left",fill = "x",ipady=1)

  tkfocus(top)
}







#.iClickPrice <- function (dataz){
#assetPrice=dataz
#print(head(assetPrice))
#iClick::iClick.VisAssetPrice(assetPrice)
#}

.iClickPrice <- function() {
Price=get("Price",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Execute iClick for asset price")

xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }

#infile=Price[,x]
#.iClickPrice(infile)
iClick::iClick.VisAssetPrice(Price[,x])

}
  tkgrid(.getFrame(xBox), sticky="nw")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

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
