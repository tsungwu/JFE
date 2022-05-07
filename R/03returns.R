.getReturns <- function(){
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
	temp=print(load(name))
	dataz=eval(parse(text=temp))
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


.iClickReturn_Menu <- function() {

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
iClick::iClick.VisOneReturns(retDF[,x])
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

