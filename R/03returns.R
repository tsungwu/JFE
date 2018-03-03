.getReturns <- function(){
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
	temp=print(load(name))
	dataz=eval(parse(text=temp))
	dataz=zoo::as.zoo(dataz)
	dataz=timeSeries::as.timeSeries(dataz)
	dat=timeSeries::returns(dataz)
  assign("retDF", dat, envir = .JFEEnv)
importedFile=last(unlist(strsplit(name,"/")))
print(paste("You are loading ",importedFile,sep=" "))
print(head(dat))
}

#=====Returns data analysis
.retSummary <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Descriptive Statistics")

xBox <- .variableListBox(top, colnames(retDF), title="Variables (Pick 1 or more)", selectmode = "extended")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
   }
print(fBasics::basicStats(na.omit(retDF[,x])))
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



.returnsTSPlot  <- function(){
retDF=get("retDF",envir = .JFEEnv)

top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Plotting")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
fBasics::seriesPlot(na.omit(retDF[,x]))
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

.ReturnsBoxPlots <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "ACF Plots")

xBox <- .variableListBox(top, colnames(retDF), title="Variables (Pick 1 or more)", selectmode = "extended")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
fBasics::boxPlot(na.omit(retDF[,x]))

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


.cumulativePlot <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Plotting")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
   }
    dev.new();plot.new()
fBasics::cumulatedPlot(na.omit(retDF[,x]),ylab="", main=paste("Cumulative returns of",names(retDF[,x])))

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

.drawdownPlot <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Plotting")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
    fBasics::drawdownPlot(na.omit(retDF[,x]))

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

.QQPlot <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "Plotting")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
par(mfrow=c(2,2))
fBasics::qqnormPlot(na.omit(retDF[,x]))
fBasics::qqnigPlot(na.omit(retDF[,x]))
fBasics::qqghtPlot(na.omit(retDF[,x]))
fBasics::qqgldPlot(na.omit(retDF[,x]))
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



.acfPlots <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "ACF Plots")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
par(mfrow=c(2,2))
fBasics::acfPlot(na.omit(retDF[,x]))
fBasics::pacfPlot(na.omit(retDF[,x]))
fBasics::lacfPlot(na.omit(retDF[,x]))
fBasics::teffectPlot(na.omit(retDF[,x]))
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


.nigTriangle <- function(){
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "NIG Plot")

xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")

onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }
    dev.new();plot.new()
    fBasics::nigShapeTriangle(fBasics::nigFit(na.omit(retDF[,x])))
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


.iClickReturn <- function() {
retDF=get("retDF",envir = .JFEEnv)
top <- tktoplevel(borderwidth=10)
tkwm.title(top, "iClick for 1 Asset Return")
xBox <- .variableListBox(top, colnames(retDF), title="Variable (Pick one variable)")
onOK <- function(){
    x <- .getSelection(xBox)
   if (length(x) == 0){
   tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
    return()
      }

iClick::iClick.VisOneReturns(na.omit(retDF[,x]))

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



#====================================
#===========   GARCH  ===============
#====================================
.garch <- function(datx0, home,exoInd,exoGARCH,model,distribution,arch,garch,archm,AR,MA,arfima){

dat=datx0

  Y=dat[,home]
  archOrder=as.numeric(arch)
  garchOrder=as.numeric(garch)
  arOrder=as.numeric(AR)
  maOrder=as.numeric(MA)


if (archm=="FALSE") {archmTF=eval(parse(text=archm))} else {archmTF=TRUE}
if (arfima=="FALSE") {arfimaTF=eval(parse(text=arfima))} else {arfimaTF=TRUE}

if (exoInd == "None") {
meanSpec=list(armaOrder=c(arOrder,maOrder),include.mean=TRUE,archm=archmTF,archpow = archm, external.regressors = NULL,arfima = arfimaTF)} else {
 x_mean=dat[,exoInd]
meanSpec=list(armaOrder=c(arOrder,maOrder),include.mean=TRUE,archm=archmTF,archpow = archm, external.regressors = as.matrix(x_mean),arfima = arfimaTF)
}

if (exoGARCH == "None"){
varSpec=list(model=model,garchOrder=c(archOrder,garchOrder),external.regressors=NULL)} else {
  x_garch=dat[,exoGARCH]
 varSpec=list(model=model,garchOrder=c(archOrder,garchOrder),external.regressors=as.matrix(x_garch))
}
distSpec=distribution

mySpec=rugarch::ugarchspec(mean.model=meanSpec, variance.model=varSpec, distribution.model=distSpec)

myFit=rugarch::ugarchfit(data= Y, spec=mySpec,solver="hybrid")
print(round(slot(myFit,"fit")$matcoef,4))
cat("\n","Nyblom test","\n")
print(rugarch::nyblom(myFit))
cat("\n","Sign Bias Test","\n")
print(rugarch::signbias(myFit))
cat("\n","Goodness-of-Fit Test","\n")
print(rugarch::gof(myFit,c(20,30,40,50)))
cat("\n","Info Criteria","\n")
print(rugarch::infocriteria(myFit))
cat("\n","Likelihood","\n")
print(rugarch::likelihood(myFit))

savedFile=paste0(model,"_",distSpec,"_",archOrder,garchOrder,".RData")
cat("\n","The estimation output is saved as ",savedFile,"\n")
save(myFit,file=savedFile)
dev.new();plot.new();rugarch::plot(myFit,which="all")



#  return(list())
}


.garchMenu <- function(){
  if ("rugarch" %in% (.packages())) {print("package rugarch is loaded")} else {
    eval(parse( text="library(rugarch)"))}




  retAS=get("retDF",envir = .JFEEnv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "Univariate GARCH")

  xBox <- .variableListBox(top, colnames(retAS), title="Pick One")
  xBoxEXO <- .variableListBox(top, c("None",colnames(retAS)), title="External Xs in MEAN", selectmode = "extended")
  xBoxVAREXO <- .variableListBox(top, c("None",colnames(retAS)), title="External Xs in GARCH", selectmode = "extended")


  onOK <- function(){
    home <- .getSelection(xBox)
    exoInd <- .getSelection(xBoxEXO)
    exoGARCH <- .getSelection(xBoxVAREXO)

    if (ncol(retAS) == 0){
      tkmessageBox(message = "You must import a dataset", icon = "error", type = "ok")
      return()
    }

    FREQtype <-  tclvalue(freqVariable)
    model <- tclvalue(modelVariable)
    distribution <- tclvalue(distVariable)

    arch <- tclvalue(archVariable)
    garch <- tclvalue(garchVariable)
    archm <- tclvalue(archmVariable)
    AR <- tclvalue(arVariable)
    MA <- tclvalue(maVariable)
    arfima <- tclvalue(arfimaVariable)

    if (FREQtype=="daily"){
      x=retAS } else {
    transForm=paste0("xts::to.",FREQtype,"(retAS,indexAt='endof',OHLC = FALSE)")
        x=eval(parse(text=transForm))
      }
#print(head(x))
    output=.garch(x,home,exoInd,exoGARCH,model,distribution,arch,garch,archm,AR,MA,arfima)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxEXO),.getFrame(xBoxVAREXO), sticky="n")

rightFrame <- tkframe(top)


freqFrame <- tkframe(rightFrame)
.radioButtons(top,name="freq", buttons=c("Daily", "Week", "Month","Quarter"), values=c("daily", "weekly", "monthly", "quarterly"), labels=c("Daily Frequency (Default)", "Weekly Frequency", "Monthly Frequency","Quarterly Frequency"), title="Frequency Conversion")
freqVariable <- freqVariable
tkgrid(freqFrame,rightFrame,sticky="w")

models=c("sGARCH","gjrGARCH","eGARCH","iGARCH","apARCH")
modelFrame <- tkframe(rightFrame)
.radioButtons(top,name="model", buttons=models, values=c("sGARCH","gjrGARCH","eGARCH","iGARCH","apARCH"), labels=c("standard GARCH","gjr GARCH","exponential GARCH","integrated GARCH","asymmetric power GARCH"), title="GARCH models")
modelVariable <- modelVariable
tkgrid(modelFrame,sticky="w")

Dists=c("norm", "snorm", "std", "sstd", "ged","sged", "nig", "jsu")
distFrame <- tkframe(rightFrame)
.radioButtons(top,name="dist", buttons=Dists, values=Dists, labels=c("Normal Distribution", "skewed Normal Distribution", "Student t Distribution", "skewed Student t Distribution", "GED Distribution","skewed GED Distribution", "Negative Inverse Gaussian Distribution", "Johnson's SU-distribution"), title="Distributions")
distVariable <- distVariable
tkgrid(distFrame,sticky="w")



  ## arch frame
  archFrame <- tkframe(rightFrame)
  archVariable <- tclVar("1")
  archField <- tkentry(archFrame,width="4",textvariable=archVariable)
  tkgrid(tklabel(archFrame,text="ARCH term= ", fg="blue"), archField, sticky="w")
  tkgrid(archFrame,sticky="w")
  ## garch frame
  garchFrame <- tkframe(rightFrame)
  garchVariable <- tclVar("1")
  garchField <- tkentry(garchFrame, width="4", textvariable=garchVariable)
  tkgrid(tklabel(garchFrame, text="GARCH term = ", fg="blue"), garchField, sticky="w")
  tkgrid(garchFrame, sticky="w")

  ## archm frame
  archmFrame <- tkframe(rightFrame)
  archmVariable <- tclVar("FALSE")
  archmField <- tkentry(archmFrame, width="6", textvariable=archmVariable)
  tkgrid(tklabel(archmFrame, text="Garch-in-Mean power. Enter 1 for order", fg="blue"), archmField, sticky="w")
  tkgrid(archmFrame, sticky="w")

  ## AR frame
  arFrame <- tkframe(rightFrame)
  arVariable <- tclVar("0")
  arField <- tkentry(arFrame,width="4",textvariable=arVariable)
  tkgrid(tklabel(arFrame,text="AR in mean= ", fg="blue"), arField, sticky="w")
  tkgrid(arFrame, sticky="w")
  ## MA frame
  maFrame <- tkframe(rightFrame)
  maVariable <- tclVar("0")
  maField <- tkentry(maFrame, width="4", textvariable=maVariable)
  tkgrid(tklabel(maFrame, text="MA in mean = ", fg="blue"), maField, sticky="w")
  tkgrid(maFrame, sticky="w")

  ## arfima frame
  arfimaFrame <- tkframe(rightFrame)
  arfimaVariable <- tclVar("FALSE")
  arfimaField <- tkentry(arfimaFrame, width="6", textvariable=arfimaVariable)
  tkgrid(tklabel(arfimaFrame, text="ARFIMA diff. Enter TRUE for yes ", fg="blue"), arfimaField, sticky="w")
  tkgrid(arfimaFrame, sticky="w")

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




