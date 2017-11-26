  .JFEenv = new.env()
.getRawData <- function() {
  name <- tclvalue(tkgetOpenFile(

    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dat=eval(parse(text=temp))
  assign("DF", dat, envir = .JFEenv)

  importedFileName=last(unlist(strsplit(name,"/")))
  assign("importedFileName", importedFileName, envir = .JFEenv)
  print(paste("You are loading ",importedFileName,sep=" "))
  print(head(dat,3))

}



.saveWorkSpace<-function() {
  file_name <- tkgetSaveFile(defaultextension = "Rsave")
  if(nchar(fname <- as.character(file_name)))
    save.image(file = file_name)
}

.getJFE <- function(x, mode="any", fail=TRUE){
  if ((!fail) && (!exists(x, mode=mode, envir=.JFEenv, inherits=FALSE))) return(NULL)
  get(x, envir=.JFEenv, mode=mode, inherits=FALSE)
}

#.setOption <- function(option, default, global=TRUE) {
#  opt = default
#  if (global) assign(option, opt, envir=.JFEenv)
#  opt
#}

.variable.list.height=6
.variable.list.width=c(20,Inf)
.title.color = as.character(.Tcl("ttk::style lookup TLabelframe.Label -foreground"))


.getFrame <- function(object) UseMethod(".getFrame")

.getFrame.listbox <- function(object){
  object$frame
}

# functions for building dialog boxes
# the following function is slightly modified, with permission, from Thomas Lumley,
#   "Programmer's Niche: Macros in R," R-News, Sept. 2001, Vol. 1, No. 3, pp.11-13.

.defmacro <- function(..., expr){
  expr <- substitute(expr)
  len <- length(expr)
  expr[3:(len+1)] <- expr[2:len]
  ## delete "macro" variables starting in ..
  expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
  a <- substitute(list(...))[-1]
  ## process the argument list
  nn <- names(a)
  if (is.null(nn)) nn <- rep("", length(a))
  for (i in seq(length.out=length(a))){
    if (nn[i] == "") {
      nn[i] <- paste(a[[i]])
      msg <- paste(a[[i]], gettext("not supplied", domain="R-JFE"))
      a[[i]] <- substitute(stop(foo), list(foo = msg))
    }
  }
  names(a) <- nn
  a <- as.list(a)
  ff <- eval(substitute(
    function(){
      tmp <- substitute(body)
      eval(tmp, parent.frame())
    },
    list(body = expr)))
  ## add the argument list
  formals(ff) <- a
  ## create a fake source attribute
  mm <- match.call()
  mm$expr <- NULL
  mm[[1]] <- as.name("macro")
  expr[[2]] <- NULL # get "local" variable removal out of source
  attr(ff, "source") <- c(deparse(mm), deparse(expr))
  ## return the macro
  ff
}




.variableListBox <- function(parentWindow, variableList, bg="white",selectmode="single", export="FALSE", initialSelection=NULL, listHeight=.variable.list.height, title){
  if (selectmode == "multiple") selectmode <- .getJFE("multiple.select.mode")
  if (length(variableList) == 1 && is.null(initialSelection)) initialSelection <- 0
  frame <- tkframe(parentWindow)
  #minmax <- .getJFE(".variable.list.width")
  minmax <- .variable.list.width
  listbox <- tklistbox(frame, height=min(listHeight, length(variableList)),
                       selectmode=selectmode, background=bg, exportselection=export,
                       width=min(max(minmax[1], nchar(variableList)), minmax[2]))
  scrollbar <- tkscrollbar(frame, command=function(...) tkyview(listbox, ...),repeatinterval=5)
  tkconfigure(listbox, yscrollcommand=function(...) tkset(scrollbar, ...))
  for (var in variableList) tkinsert(listbox, "end", var)
  if (is.numeric(initialSelection)) for (sel in initialSelection) tkselection.set(listbox, sel)
  firstChar <- tolower(substr(variableList, 1, 1))
  len <- length(variableList)

  onClick <- function() tkfocus(listbox)
  toggleSelection <- function(){
    active <- tclvalue(tkindex(listbox, "active"))
    selected <- tclvalue(tkcurselection(listbox))
    if (selected == active) tkselection.clear(listbox, "active") else tkselection.set(listbox, "active")
  }
  tkbind(listbox, "<ButtonPress-1>", onClick)
  if (selectmode == "single") tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
  tkgrid(tklabel(frame, text=title, fg=.title.color, font="JFETitleFont"), columnspan=2, sticky="w")
  tkgrid(listbox, scrollbar, sticky="nw")
  tkgrid.configure(scrollbar, sticky="wns")
  tkgrid.configure(listbox, sticky="ewns")
  result <- list(frame=frame, listbox=listbox, scrollbar=scrollbar,
                 selectmode=selectmode, varlist=variableList)
  class(result) <- "listbox"
  result
}

.getSelection <- function(object) UseMethod(".getSelection")

.getSelection.listbox <- function(object){
  object$varlist[as.numeric(tkcurselection(object$listbox)) + 1]
}


.radioButtons <- .defmacro(window, name, buttons, values=NULL, initialValue=..values[1], labels, title="", title.color=.title.color, right.buttons=FALSE, command=function(){},
                           expr={
                             ..values <- if (is.null(values)) buttons else values
                             ..frame <- paste(name, "Frame", sep="")
                             assign(..frame, tkframe(window))
                             ..variable <- paste(name, "Variable", sep="")
                             assign(..variable, tclVar(initialValue))
                             if(title != ""){
                               tkgrid(tklabel(eval(parse(text=..frame)), text=title, foreground=.title.color, font="JFETitleFont"), columnspan=2, sticky="w")
                             }
                             for (i in 1:length(buttons)) {
                               ..button <- paste(buttons[i], "Button", sep="")
                               if (right.buttons) {
                                 assign(..button, ttkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)),
                                                                 value=..values[i], command=command))
                                 tkgrid(tklabel(eval(parse(text=..frame)), text=labels[i], justify="left"), eval(parse(text=..button)), sticky="w")
                               }
                               else{
                                 assign(..button, ttkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)),
                                                                 value=..values[i], text=labels[i], command=command))
                                 tkgrid(eval(parse(text=..button)), sticky="w")
                               }
                             }
                           }
)



.seriesPlotX <-
  function(x, labels = TRUE, type = "l", col = "indianred2",title = TRUE, grid = TRUE, box = TRUE, rug = TRUE, ...)

  {

    #    stopifnot(is.timeSeries(x))
    N = NCOL(x)
    Units = colnames(x)
    if (length(col) == 1) col = rep(col, times = N)

    # Series Plots:
    for (i in 1:N) {
      X = x[, i]
      plot(x = X, type = type, col = col[i], ann = FALSE, ...)

      # Add Title:
      if (title) {
        title(main = Units[i])
      } else {
        title(...)
      }

      # Add Grid:
      if(grid) grid()

      # Add Box:
      if(box) box()

      # Add Rugs:
      if(rug) rug(as.vector(X), ticksize = 0.01, side = 2, quiet = TRUE)
    }

    # Return Value:
    invisible()
  }







.getPrice <- function() {
  name <- tclvalue(tkgetOpenFile(

    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dat=eval(parse(text=temp))
  assign("Price", dat, envir = .JFEenv)

  importedFileName=last(unlist(strsplit(name,"/")))
  assign("importedFileName", importedFileName, envir = .JFEenv)
  print(paste("You are loading ",importedFileName,sep=" "))
  print(head(dat,3))
}


.priceSummary <- function(){
  Price=get("Price",envir = .JFEenv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "Descriptive Statistics")

  xBox <- .variableListBox(top, colnames(Price), title="Variables (Pick 1 or more)", selectmode = "extended")

  onOK <- function(){
    x <- .getSelection(xBox)
    if (length(x) == 0){
      tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
      return()
    }
    #eval_cmd_chunk("fBasics::basicStats(DF[,x])")

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
  Price=get("Price",envir = .JFEenv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "Plotting")

  xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick one variable)")

  onOK <- function(){
    x <- .getSelection(xBox)
    if (length(x) == 0){
      tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
      return()
    }
    dev.new();plot.new()
    fBasics::seriesPlot(timeSeries::as.timeSeries(Price[,x]))

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
  Price=get("Price",envir = .JFEenv)
  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "ACF Plots")

  xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick 1 )")

  onOK <- function(){
    x <- .getSelection(xBox)
    if (length(x) == 0){
      tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
      return()
    }
    dev.new();plot.new()
    par(mfrow=c(2,1))
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
  Price=get("Price",envir = .JFEenv)

  top <- tktoplevel(borderwidth=10)
  tkwm.title(top, "ACF Plots")

  xBox <- .variableListBox(top, colnames(Price), title="Variables (Pick 1 or more)", selectmode = "extended")

  onOK <- function(){
    x <- .getSelection(xBox)
    if (length(x) == 0){
      tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
      return()
    }
    dev.new();plot.new()
    fBasics::boxPlot(timeSeries::as.timeSeries(Price[,x]))

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

  top <- tktoplevel(borderwidth=45)
  tkwm.title(top, "Charting series")

  importedFileName=get("importedFileName",envir = .JFEenv)
  Price=get("Price",envir = .JFEenv)
  Price=xts::as.xts(Price)

  onOK <- function(){

    if (ncol(Price) == 0){
      tkmessageBox(message = "You must import a dataset.", icon = "error", type = "ok")
      return()
    }

    freqVariable <- freqVariable
    addVariable <- addVariable


    FREQtype <-  tclvalue(freqVariable)
    addType <-   tclvalue(addVariable)
    a <- tclvalue(startVariable)
    b <- tclvalue(endVariable)

    dataz=Price[paste(a,b,sep="/")]
    transForm=paste("xts::to.",FREQtype,"(dataz)",sep="")
    x=eval(parse(text=transForm))

    NAMES=paste(importedFileName,FREQtype,sep=" by ")


    if (addType=="addVo()") {

      print(paste("You are charting",importedFileName,sep=" "))
      dev.new()
      quantmod::chartSeries(x,name=NAMES)}
    else if (addType=="None") {
      print(paste("You are charting",importedFileName,sep=" "))
      dev.new()
      quantmod::chartSeries(x,name=NAMES,TA=NULL)
    }
    else {
      ADD=paste("addVo();",addType,sep="")
      print(paste("You are charting",importedFileName,sep=" "))
      dev.new()
      quantmod::chartSeries(x,name=NAMES,TA=ADD)
    }

  }


  .radioButtons(name="freq", buttons=c("Daily", "Week", "Month","Quarter"), values=c("daily", "weekly", "monthly", "quarterly"), labels=c("Use daily data", "Use weekly freq", "Use monthly freq","Use quarterly freq"), title="Frequency Conversion")

  rightFrame <- tkframe(top)
  freqFrame <- tkframe(rightFrame)
  #  tkgrid(tklabel(rightFrame, text=""))
  tkgrid(freqFrame, rightFrame,sticky="nw")

  LABEL01=c("Chart without volume","Chart with volume","Add Directional Movement Index","Add Bollinger Bands to Chart","Add Commodity Channel Index","Add Contract Expiration Bars","Add Rate Of Change","Add Relative Strength Index","Add Parabolic Stop and Reversal")
  LABEL02=c("Add Stochastic Momentum Indicator","Add William's Percent R to Chart","Add SMA Moving Average to Chart","Add EMA Moving Average","Add DEMA Moving Average to Chart", "Add EVMA Moving Average to Chart", "Add EVWMA Moving Average to Chart","Add ZLEMA Moving Average to Chart","Add Moving Average Convergence Divergence to Chart")
  buttons1=c("None","addVo","addADX", "addBBands","addCCI","addExpiry","addROC","addRSI","addSAR")
  buttons2=c("addSMI","addWPR","addSMA","addEMA","addWMA", "addDEMA","addEVMA","addZLEMA","addMACD")
  values1=c("None","addVo()","addADX()","addBBands()","addCCI()","addExpiry()","addROC()","addRSI()","addSAR()")
  values2=c("addSMI()","addWPR()","addSMA()","addEMA()","addWMA()", "addDEMA()","addEVWMA()","addZLEMA()","addMACD()")

  .radioButtons(name="add",buttons=c(buttons1,buttons2),values=c(values1,values2), labels=c(LABEL01,LABEL02), title="Add")

  leftFrame <- tkframe(top)
  addFrame <- tkframe(leftFrame)
  tkgrid(addFrame, leftFrame,sticky="nw")

  startFrame <- tkframe(rightFrame)
  startVariable <- tclVar(as.character(as.Date(time(Price)[1])))
  startField <- tkentry(startFrame, width="12", textvariable=startVariable)
  tkgrid(tklabel(startFrame, text="Start date = ", fg="blue"), startField, sticky="w")
  tkgrid(startFrame, sticky="w")
  tkgrid.configure(startField, sticky="nw")

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


.iClickPrice <- function (dataz){
  assetPrice=dataz
  print(head(assetPrice))
  iClick::iClick.VisAssetPrice(assetPrice)
}



.iClickPrice_Menu <- function(){

  top <- tktoplevel(borderwidth=10)

  tkwm.title(top, "Execute iClick for asset price")

  Price=get("Price",envir = .JFEenv)

  xBox <- .variableListBox(top, colnames(Price), title="Variable (Pick one variable)")

  onOK <- function(){
    x <- .getSelection(xBox)
    if (length(x) == 0){
      tkmessageBox(message = "You must select a variable.", icon = "error", type = "ok")
      return()
    }
    Dates=as.character(time(Price))
    infile=data.frame(Dates,unclass(Price[,x]))

    .iClickPrice(infile)

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

.getReturns <- function(){
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dataz=eval(parse(text=temp))
  dataz=timeSeries::as.timeSeries(dataz)
  dat=timeSeries::returns(dataz)
  assign("retDF", dat, envir = .JFEenv)
  importedFile=last(unlist(strsplit(name,"/")))
  print(paste("You are loading ",importedFile,sep=" "))
  print(head(dat))
}

#=====Returns data analysis
.retSummary <- function(){
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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
  retDF=get("retDF",envir = .JFEenv)
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




.getReturns4Selection <- function(){
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData}  } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dataz=eval(parse(text=temp))
  dat=timeSeries::returns(timeSeries::as.timeSeries(dataz))
  assign("retAS", dat, envir = .JFEenv)
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
    target=apply(datx,1,mean)
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

  portfolioReturns=apply(dataSelected,1,mean)
  colnames(portfolioReturns)="Equal Weighted Equity Curve"
  bench=target[(T0+1):nrow(datx),1]
  MIN=min(c(as.numeric(cumsum(portfolioReturns)),as.numeric(cumsum(bench))))*0.98
  MAX=max(c(as.numeric(cumsum(portfolioReturns)),as.numeric(cumsum(bench))))*1.15
  fBasics::cumulatedPlot(portfolioReturns,ylab="",col="red",ylim=100*exp(c(MIN,MAX)))
  abline(h=100);lines(100*exp(timeSeries::colCumsums(bench)),col="blue");graphics::legend("topleft", c("Portfolio","Bench"), text.col = c("red","blue"),bty="n")

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
  #assign("SR", SR, envir = .JFEenv)
  #.evalCmdChunk("print(table.Stats(portfolioReturns));cat('\n');print(table.AnnualizedReturns(portfolioReturns));cat('\n');print(SR)")

  return(list(HomeID=as.character(HomeID),Selected=as.character(Selected)))
}


.sharpeIneqMenu <- function(){
  retAS=get("retAS",envir = .JFEenv)
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

    freqVariable <- freqVariable
    selectionVariable <- selectionVariable
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

  .radioButtons(name="freq", buttons=c("Daily", "Week", "Month"), values=c("daily", "daily2weekly", "daily2monthly"), labels=c("Default daily data", "Use weekly freq", "Use monthly freq"), title="Frequency Conversion")

  .radioButtons(name="selection", buttons=c("StdDev", "VaR", "CVaR","AdjustedSharpeRatio","SharpeRatio.annualized", "BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), values=c("StdDev", "VaR", "CVaR","AdjustedSharpeRatio", "SharpeRatio.annualized","BurkeRatio","KellyRatio","PainRatio","BernardoLedoitRatio","MartinRatio","MeanAbsoluteDeviation"), labels=c("Sharpe By Std Dev.", "Sharpe By VaR","Sharpe By CVaR","Adjusted Sharpe", "Annualized Sharpe","Burke Ratio", "Kelly Ratio","Pain Ratio","Bernardo-Ledoit","Martin Ratio","Mean Absolute Deviation"), title="Performance Index")


  rightFrame <- tkframe(top)
  #  tkgrid(tklabel(rightFrame, text=""))
  freqFrame <- tkframe(rightFrame)
  selectionFrame <- tkframe(rightFrame)
  tkgrid(freqFrame, rightFrame,sticky="w")
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



.getReturns4backtesting <- function() {

  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
  if (name == "")
    return(data.frame())
  temp=print(load(name))
  dataz=eval(parse(text=temp))
  dat=timeSeries::returns(timeSeries::as.timeSeries(dataz))
  assign("retAS", dat, envir = .JFEenv)
  cat("Data is imported sucessfully","\n")
  print(tail(dat,3))
}

.backtesting <- function(dataset, assetsRM, bench, Type="MV",Strategy="GMVP",COV="covEstimator",Rf=0,lambda="1m",Constraint){

  if (assetsRM=="None") {
    dat=dataset } else {
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

  #COV=c("covEstimator","covLedoit","ShrinkCC","covStudent","GoldSach","kendallEstimator","mcdEstimator","covOGKEstimator")
  TYPE=Type  # "MV"; "CVaR"; "QLPM"; "MSPS"; "MAD"

  mySpec = portfolioSpec()

  setType(mySpec)=TYPE
  setEstimator(mySpec)=COV
  setRiskFreeRate(mySpec) <- as.numeric(Rf)

  if (Constraint=="Short") {
    setSolver(mySpec)= "solveRshortExact"
  } else if (getType(mySpec)=="CVaR") {
    setSolver(mySpec)= "solveRglpk.CVAR"}  else {
      setSolver(mySpec)= "solveRquadprog"
    }

  #===back testing
  myBacktestSpec = portfolioBacktest()
  if (Strategy=="GMVP") {setStrategyFun(myBacktestSpec)=".GMVPStrategy" } else { setStrategyFun(myBacktestSpec)="tangencyStrategy" }
  setSmootherLambda(myBacktestSpec) = lambda
  #setWindowsHorizon(myBacktestSpec) <- "12m"
  Eq=paste(names(newData)[1], paste(names(newData)[-1], collapse= "+"),sep="~")

  myFormula = as.formula(Eq)
  myBacktest = portfolioBacktesting(myFormula,data = newData, spec = mySpec,constraints = Constraint, trace = FALSE)

  Weights = round(100*myBacktest$weights, 4)

  SmoothPort = portfolioSmoothing(object=myBacktest, trace = FALSE)
  #"backtest = myBacktestSpec" will be needless next version
  #save(SmoothPort,file="..SmoothPort.RData")

  smoothWeights = round(100*SmoothPort$smoothWeights,2)
  dev.new();
  backtestPlot(SmoothPort, cex = 0.6, font = 1, family = "mono", which="all")
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
  retAS=get("retAS",envir = .JFEenv)
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
    CovVariable <- CovVariable
    riskTypeVariable <- riskTypeVariable
    StrategyVariable <- StrategyVariable
    ConstraintVariable <- ConstraintVariable

    COV <-  tclvalue(CovVariable)
    Type <- tclvalue(riskTypeVariable)
    Strategy <- tclvalue(StrategyVariable)
    Constraint <- tclvalue(ConstraintVariable)
    Rf <- tclvalue(rfVariable)
    lambda <- tclvalue(lambdaVariable)

    .backtesting(dataset=retAS,assetsRM,bench,Type,Strategy,COV,Rf,lambda,Constraint)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxRM), sticky="n")

  .radioButtons(name="Cov", buttons=c("covEstimator", "covLedoit","covStudent"), values=c("covEstimator", ".covLedoit",".covStudent"), labels=c("Sample Covariance", "LedoitWolf Shrinkage","Multivariate Student t"), title="Select Risk Estimator")

  .radioButtons(name="riskType", buttons=c("MV", "CVaR"), values=c("MV", "CVaR"), labels=c("Mean-Variance", "CVaR"), title="Portfolio type")

  .radioButtons(name="Strategy", buttons=c("Tangency", "GMVP"), values=c("Tangency", "GMVP"), labels=c("Tangency", "GMVP"), title="Select Portfolio Strategy")

  .radioButtons(name="Constraint", buttons=c("LongOnly", "Short"), values=c("LongOnly", "Short"), labels=c("LongOnly", "Short"), title="Select Portfolio constraint")

  rightFrame <- tkframe(top)
  tkgrid(rightFrame,sticky="w")
  #tkgrid(tklabel(rightFrame, text=""))
  CovFrame <- tkframe(rightFrame)
  riskTypeFrame <- tkframe(rightFrame)
  StrategyFrame <- tkframe(rightFrame)
  ConstraintFrame <- tkframe(rightFrame)
  tkgrid(CovFrame, riskTypeFrame, StrategyFrame,ConstraintFrame,rightFrame,sticky="w")

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



.iClickPortfolio_Menu <- function(){
  retAS=get("retAS",envir = .JFEenv)
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

    TypeVariable <- TypeVariable
    plotsVariable <- plotsVariable
    Type <- tclvalue(TypeVariable)
    plots <- tclvalue(plotsVariable)
    Rf <- tclvalue(rfVariable)
    lambda <- tclvalue(lambdaVariable)

    .iClickPortfolio(dataset=retAS,assetsRM,bench,Type,Rf,lambda,plots)
  }

  tkgrid(.getFrame(xBox),.getFrame(xBoxRM), sticky="n")
  buttonsFrame <- tkframe(top,width=250)
  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  .radioButtons(name="Type", buttons=c("MV", "CVaR"), values=c("MV", "CVaR"), labels=c("Mean-Variance", "CVaR"), title="Type of Portfolio Risk")
  .radioButtons(name="plots", buttons=c("simple", "all"), values=c("simple", "all"), labels=c("Simple", "All"), title="Numbers of plots")

  rightFrame <- tkframe(top)
  #tkgrid(rightFrame,sticky="w")
  #tkgrid(tklabel(rightFrame, text=""))
  plotsFrame <- tkframe(rightFrame)
  TypeFrame <- tkframe(rightFrame)
  tkgrid(plotsFrame, TypeFrame,rightFrame,sticky="w")


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
  COV=c("covEstimator",".covLedoit",".ShrinkCC",".covStudent",".GoldSach","kendallEstimator")


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
      setStrategyFun(myBacktest2)=".GMVPStrategy"
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
      setStrategyFun(myBacktest4)=".GMVPStrategy"
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












###==Ledoit and Wolff(2003)====###
.covLedoit <- function (x, spec = NULL) {
  x.mat = as.matrix(x)
  list(mu = colMeans(x.mat), Sigma = .SKCov(x.mat)$sigma)
}


###==Ledoit and Wolff(2003) Procedure====###
.SKCov <- function(dat) {
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

###==========###
.LPM<-function(x, spec = NULL){
  list(mu = fAssets::assetsLPM(x)$mu, Sigma = fAssets::assetsLPM(x)$Sigma)
}

###===Multivariate Student t=======###
.covStudent <- function (x, spec = NULL) {
  x.mat =as.matrix(x)
  list(mu = colMeans(x.mat), Sigma = MASS::cov.trob(x.mat)$cov)
}

###==========###
.GoldSach <- function(x, spec = NULL){
  x.mat = x
  rho=0.95
  t=nrow(x.mat)
  p=(t-1):0
  w=rho^p
  dat=sqrt(w)*x.mat
  Sig=cov(dat)*t/sum(w)
  list(mu =colMeans(x.mat) , Sigma =Sig)
}

###===Constant Correlation===###
.ShrinkCC <- function (x,spec = NULL) {
  x.mat =x
  t=nrow(x.mat)
  p=(t-1):0
  rho=0.95
  w=rho^p
  list(mu = colMeans(x.mat), Sigma = BurStFin::var.shrink.eqcor(x.mat,weights=1))
}

.GMVPStrategy <-function (data, spec = portfolioSpec(), constraints = "LongOnly", backtest = portfolioBacktest()) {
  strategyPortfolio <- try(minriskPortfolio(data, spec, constraints))
  if (class(strategyPortfolio) == "try-error") {
    strategyPortfolio <- minvariancePortfolio(data,spec,constraints)
  }
  strategyPortfolio
}



