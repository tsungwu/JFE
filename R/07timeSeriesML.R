ttsPlot <- function(object,which,vertical,main=NULL,xlab="Time",ylab="Value",col1="black",col2="red",type="o",ylim=NULL) {

  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  t0=vertical+1
  newData=eval(parse(text=paste0("object$",which,"Data")))
  MAIN=paste0(toupper(object$method), ": ",which, " forecasts")
  if(is.null(main)) {main=MAIN}
  if(is.null(ylim)) {ylim=range(newData)}

  test.start=as.character(timeSeries::time(newData))[t0]

#  dev.new()
  plot(newData[,1], col=col1, type=type,ylim=ylim,main=main,
       xlab=xlab,ylab=ylab);grid()
  abline(v=as.POSIXct(as.character(timeSeries::time(newData))[t0]),col="blue")
  text(as.POSIXct(as.character(timeSeries::time(newData))[t0-10]), max(newData)*0.8, test.start,col="blue")
  lines(newData[,2], col=col2, type=type,ylim=ylim,ylab="",xlab="")
  rug(as.vector(newData[,1]), ticksize = 0.01, side = 2, quiet = TRUE)
}




ttsCaret<-function(y,x=NULL, method,train.end,arOrder=2,xregOrder=0,type,tuneLength =10) {
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")


  if (!is.null(x)) {
    x=timeSeries::as.timeSeries(x)
    if ( nrow(y) != nrow(x) ) {print("Variables must have the same rows.")}
  }

if (!timeSeries::is.timeSeries(y)) {stop("Data must be a timeSeries object.")}

if (is.null(train.end)) {stop("train.end date must be specified.") }

if (is.null(type)) {type="none" }

  train.start=timeSeries::start(y)
  t0=which(as.character(timeSeries::time(y))==train.end)
  test.start=as.character(timeSeries::time(y))[t0+1]
  test.end=as.character(timeSeries::end(y))

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


  #4. Dummies for time features
  trend <- 1:nrow(DF)

  if (timeSeries::isRegular(y)) {
  seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
  DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}



  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}


  trainData=window(DF,start=train.start,end=train.end)
  testData=window(DF,start=test.start,end=test.end)

  dep=colnames(DF)[1]
  eq=as.formula(paste(dep,"~."))

  if (method == "svm") {
output=  caret::train(eq, data = trainData, method = "svmRadial",
        tuneLength = tuneLength, trControl = trainControl(method = "cv"))
  } else {output=  caret::train(eq, data = trainData, method=method,
                                tuneLength = tuneLength, trControl = trainControl(method = "cv"))  }

  # Static multistep forecasting by model fit
  static.pred=as.matrix(predict(output,testData[,-1]))
  rownames(static.pred)=as.character(timeSeries::time(testData))
  colnames(static.pred)="Prediction"
  static.pred=timeSeries::as.timeSeries(static.pred)
  staticData=cbind(Actual=DF[,dep],Prediction=c(predict(output,trainData[,-1]),static.pred))


  # Recursive Forecasts
  # Predict test data: Recursive Forecasts
if (min(arOrder) == 0) {recursive.pred=static.pred} else {


  ARX=window(DF0,start=end(trainData),end=end(testData))

  ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]

  LY.names=names(testData)[grep(names(testData),pattern="^ar+")]
  LX.names=names(testData)[-grep(names(testData),pattern="^ar+")][-1]

  plags=length(ar.names)
  ahead=nrow(ARX)

  recursive.pred=NULL
  for (i in 1:ahead) {#i=1
    if(length(LX.names)==0) {y0=as.numeric(predict(output,ARX[i,]))
    } else {
      y0=as.numeric(predict(output,ARX[i,c(LY.names,LX.names)]))
    }

    if (i < ahead) if (plags==1) {ARX[i+1,ar.names]=y0} else
    { ARX[i+1,ar.names]=c(y0,as.numeric(ARX[i,1:(plags-1)]))}

    recursive.pred=c(recursive.pred,y0)
  }


    recursive.pred=as.matrix(recursive.pred[-1])
}
  rownames(recursive.pred)=as.character(timeSeries::time(testData))
  colnames(recursive.pred)="Prediction"
  recursive.pred=timeSeries::as.timeSeries(recursive.pred)

  recursiveData=cbind(Actual=DF[,dep],Prediction=c(predict(output,trainData[,-1]),recursive.pred))

  # Accuracy of static forecasts
  static.Accuracy=forecast::accuracy(as.ts(testData[,dep]),as.ts(static.pred))
  static.Accuracy[,c(4,5,7)]=static.Accuracy[,c(4,5,7)]/100
  rownames(static.Accuracy)="static forecasts"

  # Accuracy of recursive forecasts
  recursive.Accuracy=forecast::accuracy(as.ts(testData[,dep]),as.ts(recursive.pred))
  recursive.Accuracy[,c(4,5,7)]=recursive.Accuracy[,c(4,5,7)]/100
  rownames(recursive.Accuracy)="recursive forecasts"



  return(list(staticData=staticData,
              recursiveData=recursiveData,
              staticF=cbind(Actual=testData[,dep,drop=FALSE],Prediction=static.pred),
              recursiveF=cbind(Actual=testData[,dep,drop=FALSE],Prediction=recursive.pred),
              static.Accuracy=static.Accuracy,
              recursive.Accuracy=recursive.Accuracy,
              method=method,data=DF))

}




ttsAutoML <-function(y,x=NULL,train.end,arOrder=2,xregOrder=0,maxSecs=30) {
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  h2o::h2o.init()        # Fire up h2o
  invisible(h2o::h2o.no_progress()) # Turn off progress bars

  # Load libraries

  if (!is.null(x)) {
      if ( nrow(y) != nrow(x) ) {stop("Variables must have the same rows.")
      } else {
#        eval(parse( text="library(magrittr)"))
#        eval(parse( text="library(timeSeries)"))
      }
      } else {

#        eval(parse( text="library(magrittr)"))
#        eval(parse( text="library(timeSeries)"))

  }


  if (!timeSeries::is.timeSeries(y)) {stop("The data must be timeSeries object.")}

  if (is.null(train.end)) {stop("The train.end must be specified.") }

  train.start=timeSeries::start(y)
  t0=which(as.character(timeSeries::time(y))==train.end)
  test.start=as.character(timeSeries::time(y))[t0+1]
  test.end=as.character(timeSeries::end(y))

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


  colnames(y)="y"

  ### The beginning of autoML

  timeID=data.frame(date=base::as.Date(as.character(timeSeries::time(y))))

  # Augment (adds data frame columns)
  suppressMessages(timeFeatures <- timetk::tk_augment_timeseries_signature(timeID))
  dim(timeFeatures)
  timeFeatures=as.data.frame(timeFeatures[,-1])
  rownames(timeFeatures)=as.character(timeSeries::time(y))
  timeFeatures=timeSeries::as.timeSeries(timeFeatures)
  data_tbl_aug=tibble::as_tibble(na.omit(cbind(y,ar,timeFeatures))) #The first obs is lost
  data_tbl_clean=data_tbl_aug

  dateID=as.character(as.Date(timeSeries::time(y)))[-1]
  data_tbl_clean=as.data.frame(data_tbl_clean)
  rownames(data_tbl_clean)=dateID

  DF=timeSeries::as.timeSeries(data_tbl_clean)

  trainData = window(DF,start=train.start,end=train.end)
  testData = window(DF,start=test.start,end=test.end)

  # Convert to H2OFrame objects
  train_h2o <- h2o::as.h2o(tibble::as_tibble(trainData))
  test_h2o  <- h2o::as.h2o(tibble::as_tibble(testData))

  # Set names for h2o
  x <- setdiff(names(train_h2o), "y")
  ##################################
  ###=== Estimation of autoML ===###
  ##################################

  automl_models_h2o <-  h2o::h2o.automl(
    x = x,
    y = "y",
    training_frame = train_h2o,
    leaderboard_frame = test_h2o,
    max_runtime_secs = maxSecs,
    stopping_metric = "deviance")

  # Extract leader model
  automl_leader <- automl_models_h2o@leader

  #########################################
  ###=== Direct Multisteps Forecasts ===###
  #########################################

  #Predict train data
  Pred1.dm=as.matrix(h2o::h2o.predict(automl_leader, newdata = train_h2o))

  #Static Prediction of test data
  Pred2.dm=as.matrix(h2o::h2o.predict(automl_leader, newdata = test_h2o))
  static.pred=Pred2.dm
  rownames(static.pred)=as.character(timeSeries::time(testData))
  colnames(static.pred)="Prediction"
  static.pred=timeSeries::as.timeSeries(static.pred)

  Pred.dm=rbind(Pred1.dm,Pred2.dm)

  rownames(Pred.dm)=dateID
  Pred.dm=timeSeries::as.timeSeries(Pred.dm)
  actual=as.matrix(c(trainData$y,testData$y))
  rownames(actual)=dateID
  actual=timeSeries::as.timeSeries(actual)
  staticData=na.omit(cbind(actual,Pred.dm))

  #Recursive Prediction of test data

  if (min(arOrder) == 0) {recursive.pred=static.pred} else {

  DF0=tibble::as_tibble(na.omit(cbind(y,ar0,X, timeFeatures))) #Short of the first obs
  DF0 = as.data.frame(DF0)
  rownames(DF0)=dateID
  DF0=timeSeries::as.timeSeries(DF0)

  ARX = h2o::as.h2o(tibble::as_tibble(window(DF0,start=end(trainData),end=end(y))))
  ahead=nrow(ARX)
  ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]
  plags=length(ar.names)

  LY.names=names(test_h2o)[grep(names(test_h2o),pattern="^ar+")]
  LX.names=names(test_h2o)[-grep(names(test_h2o),pattern="^ar+")][-1]

  dynPred=NULL
  for (i in 1:ahead) {#i=1

    if(length(LX.names)==0) {y0=h2o::h2o.predict(automl_leader, newdata = ARX[i,LY.names])
    } else {
      y0=h2o::h2o.predict(automl_leader, newdata = ARX[i,c(LY.names,LX.names)])
    }

    if (i < ahead) {if (length(ar.names)==1) { ARX[i+1,ar.names]=y0
    } else {
      updates=merge(y0,ARX[i,ar.names][1:(plags-1)])
      colnames(updates)=ar.names
      updates=h2o::as.h2o(updates)
      ARX[i+1,ar.names]=updates }} else {stop}

    dynPred=rbind(dynPred,as.numeric(as.matrix(y0)))
#    Pred2.rm=as.matrix(dynPred[-1])
    recursive.pred=as.matrix(dynPred[-1])
  }
#  recursive.pred=Pred2.rm


  }



  rownames(recursive.pred)=as.character(timeSeries::time(testData))
  colnames(recursive.pred)="Prediction"
  recursive.pred=timeSeries::as.timeSeries(recursive.pred)


  Pred.rm=rbind(Pred1.dm,recursive.pred)
  rownames(Pred.rm)=dateID
  Pred.rm=timeSeries::as.timeSeries(Pred.rm)
  recursiveData=na.omit(cbind(actual,Pred.rm))

  # Accuracy of static forecasts
  static.Accuracy=forecast::accuracy(as.ts(testData$y),as.ts(as.numeric(static.pred)))
  static.Accuracy[,c(4,5,7)]=static.Accuracy[,c(4,5,7)]/100
  rownames(static.Accuracy)="static forecasts"

  # Accuracy of recursive forecasts
  recursive.Accuracy=forecast::accuracy(as.ts(testData$y),as.ts(as.numeric(recursive.pred)))
  recursive.Accuracy[,c(4,5,7)]=recursive.Accuracy[,c(4,5,7)]/100
  rownames(recursive.Accuracy)="recursive forecasts"

  return(list(staticData=staticData,
              recursiveData=recursiveData,
              staticF=cbind(Actual=testData$y,Prediction=static.pred),
              resursiveF=cbind(Actual=testData$y,Prediction=recursive.pred),
              static.Accuracy=static.Accuracy,
              recursive.Accuracy=recursive.Accuracy,
              method="autoML",data=cbind(y,ar,X)))

}





ttsLSTM <- function (y,x=NULL,train.end,arOrder=2,xregOrder=0,type, memoryLoops=10){

  if (!is.null(x)) {
    if ( nrow(y) != nrow(x) ) {stop("Variables must have the same rows.")
    } else {
#      eval(parse( text="library(magrittr)"))
#      eval(parse( text="library(timeSeries)"))
    }
  } else {

#    eval(parse( text="library(magrittr)"))
#    eval(parse( text="library(timeSeries)"))

  }


  if (!timeSeries::is.timeSeries(y)) {stop("The data must be timeSeries object.")}

  if (is.null(train.end)) {stop("The train.end must be specified.") }

  train.start=timeSeries::start(y)
  t0=which(as.character(timeSeries::time(y))==train.end)
  test.start=as.character(timeSeries::time(y))[t0+1]
  test.end=as.character(timeSeries::end(y))

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

  #4. Dummies for time features
  trend <- 1:nrow(y)
  if (timeSeries::isRegular(y)) {
    seasonDummy <- data.frame(forecast::seasonaldummy(as.ts(y)))
    DF0 <- cbind(ar0,X,seasonDummy,trend)
  } else {DF0 <- cbind(ar0,X,trend)}


  if (type=="trend") {DF<-cbind(DF,trend)} else if (type=="sesaon") {DF<-cbind(DF,seasonDummy)
  } else if (type=="both") {DF<-cbind(DF,trend,seasonDummy)
  } else {DF <- DF}


  #  suppressMessages(timeFeatures <- timetk::tk_augment_timeseries_signature(data.frame(date=time(y))))
  #  rownames(timeFeatures)=rownames(y)
  #timeFeatures=as.timeSeries(timeFeatures)

  ## Input variables data
  newData= timeSeries::as.timeSeries(DF)

  trainData=window(newData,start=train.start,end=train.end)
  testData=window(newData,start=test.start,end=test.end)

  train0 = data.frame(value = as.numeric(trainData[,1]), trainData[,-1])
  train = train0[complete.cases(train0), ]

  test0 = data.frame(value = as.numeric(testData[,1]), testData[,-1])
  test = test0[complete.cases(test0), ]

  #DescTools::LCM(nrow(train),nrow(test))
  batch.size = DescTools::GCD(nrow(train),nrow(test))
  nrow(train)/batch.size; nrow(test)/batch.size

  names(train)
  ####################################
  ###=====LSTM modeling begins=====###
  ####################################

  train.new=as.matrix(train) #remove date index
  dimnames(train.new)=NULL
  test.new=as.matrix(test)  #remove date index
  dimnames(test.new)=NULL

  SHAPE=ncol(train.new)
  x.train = array(data = train.new[,-1], dim = c(nrow(train.new), SHAPE, 5))
  y.train = array(data = train.new[,1], dim = c(nrow(train.new), 1))

  x.test = array(data = test.new[,-1], dim = c(nrow(test.new), SHAPE, 5))
  y.test = array(data = test.new[,1], dim = c(nrow(test.new), 1))

  model <- keras::keras_model_sequential()

  model %>%
    keras::layer_lstm(units = 100,
                      input_shape = c(SHAPE, 5),
                      batch_size = batch.size,
                      return_sequences = TRUE,
                      stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_lstm(units = 50,
                      return_sequences = FALSE,
                      stateful = TRUE) %>%
    keras::layer_dropout(rate = 0.5) %>%
    keras::layer_dense(units = 1)

  model %>%
    keras::compile(loss = 'mae', optimizer = 'adam')

  for(i in 1:memoryLoops){
    model %>% keras::fit(x = x.train,
                         y = y.train,
                         batch_size = batch.size,
                         epochs = 1,
                         verbose = 0,
                         shuffle = FALSE)
    model %>% keras::reset_states()}


  # Predict the train data
  Pred1_dm <- as.matrix(predict(model,x.train, batch_size = batch.size))

  # Predict the test data
  Pred2.dm <- as.matrix(predict(model, x.test, batch_size = batch.size))
  static.pred<-Pred2.dm
  # Accuracy of static forecasts
  static.Accuracy=as.matrix(forecast::accuracy(as.ts(Pred2.dm),x=as.ts(y.test)))
  static.Accuracy[,c(4,5,7)]=static.Accuracy[,c(4,5,7)]/100
  rownames(static.Accuracy)="static forecasts"


  train.sample=cbind(actual=y.train,pred=Pred1_dm)
  test.sample=cbind(actual=y.test,pred=Pred2.dm)
  newData.dm=rbind(train.sample,test.sample)
  rownames(newData.dm)=rownames(newData)
  staticData=timeSeries::as.timeSeries(newData.dm)


  ############################################
  ###=== Recursive Multisteps Forecasts ===###
  ############################################

  #Predict test data
  if (min(arOrder) == 0) {recursive.pred=static.pred} else {


  ARX=window(DF0,start=end(trainData),end=end(testData))
  ar.names=names(ARX)[grep(names(ARX),pattern="^ar+")]
  plags=length(ar.names)

  LY.names=names(testData)[grep(names(testData),pattern="^ar+")]
  LX.names=names(testData)[-grep(names(testData),pattern="^ar+")][-1]
  ahead=nrow(ARX)
  rownames(ARX)=NULL

  dynPred=NULL
  for (i in 1:ahead) { #i=1

    if (length(LX.names)==0 & length(LY.names)>0) {x.test0 = array(data = ARX[i,LY.names], dim = c(nrow(ARX)-1,SHAPE, 5))
    } else if (length(LY.names)==0 & length(LX.names)>0) {x.test0 = array(data = ARX[i,LX.names], dim = c(nrow(ARX)-1,SHAPE, 5))

    } else if (length(LX.names)>0 & length(LY.names)>0) {
      x.test0 = array(data = ARX[i,c(LY.names,LX.names)], dim = c(nrow(ARX)-1,SHAPE, 5))
    }

    y0=as.matrix(predict(model,x.test0, batch_size = batch.size))[1]

    if (i < ahead) {if (plags==1) { ARX[i+1,ar.names]=y0
    } else {

      ARX[i+1,ar.names]=c(y0,as.numeric(ARX[i,1:(plags-1)])) }}

    dynPred=c(dynPred,y0)

  }

  #x.test.new = array(data = ARX, dim = c(nrow(ARX), SHAPE, model$input_shape[[3]]))
  #Pred2.rm=model %>% predict(x.test.new, batch_size = batch.size) %>% .[,1]
  Pred2.rm=dynPred[-1]
  recursive.pred=Pred2.rm

  }


  # Calculate Accuracy
  recursive.Accuracy=as.matrix(forecast::accuracy(as.ts(Pred2.rm),x=as.ts(y.test)))
  recursive.Accuracy[,c(4,5,7)]=recursive.Accuracy[,c(4,5,7)]/100
  rownames(recursive.Accuracy)="recursive forecasts";recursive.Accuracy

  train.sample=cbind(actual=y.train,pred=Pred1_dm)
  test.sample=cbind(actual=y.test,pred=Pred2.rm)
  newData.rm=rbind(train.sample,test.sample)
  rownames(newData.rm)=rownames(newData)
  recursiveData=timeSeries::as.timeSeries(newData.rm)


  return(list(staticData=staticData,
              recursiveData=recursiveData,
              staticF=cbind(Actual=testData[,"y"],Prediction=static.pred),
              resursiveF=cbind(Actual=testData[,"y"],Prediction=recursive.pred),
              static.Accuracy=static.Accuracy,
              recursive.Accuracy=recursive.Accuracy,
              method="LSTM",data=newData))
}


