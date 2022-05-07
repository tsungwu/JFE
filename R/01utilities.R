.getRawData <- function() {
  name <- tclvalue(tkgetOpenFile(

   filetypes = "{ {RData Files} {.RData}  {.rda}} { {All Files} * }"))
  if (name == "")
    return(data.frame())
	temp=print(load(name))
	dat=eval(parse(text=temp))
  assign("DF", dat, envir = .JFEEnv)

importedFileName=last(unlist(strsplit(name,"/")))
assign("importedFileName", importedFileName, envir = .JFEEnv)
print(paste("You are loading ",importedFileName,sep=" "))
print(head(dat,3))

}



.saveWorkSpace<-function() {
file_name <- tkgetSaveFile(defaultextension = "Rsave")
if(nchar(fname <- as.character(file_name)))
save.image(file = file_name)
}

.getJFE <- function(x, mode="any", fail=TRUE){
    if ((!fail) && (!exists(x, mode=mode, envir=.JFEEnv, inherits=FALSE))) return(NULL)
    get(x, envir=.JFEEnv, mode=mode, inherits=FALSE)
}

#.setOption <- function(option, default, global=TRUE) {
#     opt = default
#     if (global) assign(option, opt, envir=.JFEEnv)
#     opt
#    }

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



.ttsDS <- function (y,x=NULL, arOrder=2,xregOrder=0,type=NULL) {
#type=c("none","trend","season","both")
  if (!is.null(x)) {
    x=timeSeries::as.timeSeries(x)
    if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")}
  }

  if (!timeSeries::is.timeSeries(y)) {stop("Data must be a timeSeries object.")}

  if (is.null(type)) {type="none" }

  p=max(arOrder,xregOrder)
  colNAMES=c(outer(paste0(names(x),"_L"),0:p,FUN=paste0))
  if (p==0) {
    y=y
    datasetX=x
    ar0=NULL
  } else {
    datasetY=timeSeries::as.timeSeries(embed(y,p+1),timeSeries::time(y)[-c(1:p)])
    y=datasetY[,1]
    ar0=datasetY[,-1]
    colnames(ar0)=paste0("ar",1:p)

    if (is.null(x)) {datasetX=NULL
    } else {
      datasetX=timeSeries::as.timeSeries(embed(x,p+1),timeSeries::time(x)[-c(1:p)])

      colnames(datasetX)=colNAMES
    }
  }

  colnames(y)="y"


  if (min(arOrder)==0) {ar=NULL
  }  else {ar=ar0[,paste0("ar",arOrder)]}



  if (is.null(x)) {X=datasetX} else {
    L.ID=paste0("L",xregOrder)

    IDx=NULL
    for (i in L.ID) {IDx=c(IDx,grep(colNAMES,pattern=i))}
    X=datasetX[,IDx]
  }


  DF <- na.omit(cbind(y,ar,X))
  trend <- 1:nrow(DF)

  if (timeSeries::isRegular(y)) {
    seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
    DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}



  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}

return(DF)
}






