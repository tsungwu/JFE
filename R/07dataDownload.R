getBIS <- function (sheet="Real", type="broad"){
  #BIS effective exchange rates
  cat("Getting", sheet, "Effective Exchange Rate,",type, paste0(substr(sheet,1,1),"EER"))
  eer=paste0("https://www.bis.org/statistics/eer/",type,".xlsx")
  tmp = openxlsx::read.xlsx(eer, sheet=sheet, colNames=T, detectDates = T, startRow = 4)
  #  countryNames=colnames(tmp)[-1]
  #  shortID=tmp[1,][-1]
  eer_ts = tmp[-1,-1]
  rownames(eer_ts)=tmp[-1,1]

  country.info=data.frame(country.names=colnames(tmp)[-1],short.names=t(tmp[1,][-1]))
  rownames(country.info)=NULL
  colnames(country.info)=c("country.names","BIS.ID")
  return(list(data=eer_ts,country.info=country.info,data.info=paste(type,sheet, "Effective Exchange Rate")))
}


getFed <- function (var.name="UNRATE", from="1900-01-01",end=Sys.Date(), do.plot=TRUE){

  url=paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?&id=",
             var.name,"&revision_date=",
             end,
             "&nd=",
             from)
  dat0=read.csv(url)
  dat=dat0[,2,drop=FALSE]
  rownames(dat)=as.character(dat0[,1])
  # xts::as.xts(dat0[,2,drop=FALSE],as.Date(dat0[,1]))
  if (isTRUE(do.plot)) {.seriesPlotX(timeSeries::as.timeSeries(dat))}

  return(list(data=dat))
}



getFrench.Factors<-function(filename="F-F_Research_Data_5_Factors_2x3") {
  url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

  ##Step 1. Download .zip dataset

  if (filename %in% c("F-F_Research_Data_Factors_weekly",
                       "F-F_Research_Data_5_Factors_2x3"))
    { fileCSV=paste0(filename,".csv") } else {  fileCSV=paste0(filename,".CSV") }

 {   fileZIP=paste0(filename,"_CSV.zip")
    Link=paste0(url,fileZIP)
    temp = tempfile(tmpdir=getwd(),fileext = ".zip")
    download.file(Link,temp)
  }


  ##Step 2. Retrieve .csv data


  if (filename %in% c("F-F_Research_Data_Factors_weekly",
             "F-F_Research_Data_Factors_daily",
             "F-F_Research_Data_5_Factors_2x3_daily",
             "F-F_Momentum_Factor_daily"))  {

    tmp0=read.csv(unz(temp, fileCSV),header=FALSE, blank.lines.skip = FALSE)
    S=which(grepl("19", tmp0[1:30,1]))[1];S
    ff.factor0 = read.csv(unz(temp, fileCSV),skip=S-2,header=TRUE, blank.lines.skip = FALSE)
    head(ff.factor0,15)
    tail(ff.factor0)
    end.date=grep("Copyright ",ff.factor0[,1])-2
    if (length(end.date)==0) {end.date=nrow(ff.factor0)} else {
      end.date=end.date
      }

    ff.factor=ff.factor0[1:end.date,]
    colnames(ff.factor)=c("Dates", colnames(ff.factor0[-1]))
    head(ff.factor);tail(ff.factor)

dat=ff.factor
table.names="Only one table"

  } else if (filename %in% c("F-F_Research_Data_Factors", "F-F_Research_Data_5_Factors_2x3")) {
     #Both datasets have two frequencies: monthly and annual


    tmp0=read.csv(unz(temp, fileCSV), header=FALSE, blank.lines.skip = FALSE)
#    head(tmp0,20)
    S=which(grepl("19", tmp0[1:30,1]))[1];S
    ff.factor0 = read.csv(unz(temp, fileCSV), skip=S-2,header=TRUE, blank.lines.skip = FALSE)
#    head(ff.factor0,15)
#    tail(ff.factor0)
    endID=grep("[Aa-zZ] ",ff.factor0[,1])

    if (length(endID)==2) {
    ff.factor.month=ff.factor0[1:(endID[1]-2),]
    ff.factor.annual=ff.factor0[(endID[1]+2):(endID[2]-2),]
      } else if (length(endID)==1) {

    ff.factor.month=ff.factor0[1:(endID[1]-2),]
    ff.factor.annual=ff.factor0[(endID[1]+2):nrow(ff.factor0),]
      }

    table.names=trimws(c("Monthly Factors",ff.factor0[,1][which(grepl("[Aa-zZ]", ff.factor0[,1]))][1]))
    colnames(ff.factor.month)=colnames(ff.factor.annual)=c("Dates",colnames(ff.factor.month)[-1])
#    head(ff.factor.month);tail(ff.factor.month)
#    head(ff.factor.annual);tail(ff.factor.annual)

    dat=list()
    dat[[1]]=ff.factor.month
    dat[[2]]=ff.factor.annual
    names(dat)=table.names


  } else if (filename == "F-F_Momentum_Factor") {

    tmp0=read.csv(unz(temp, fileCSV), header=FALSE, blank.lines.skip = FALSE)
    head(tmp0,20)
    S=which(grepl("19", tmp0[1:30,1]))[1];S
    ff.factor0 = read.csv(unz(temp, fileCSV), skip=S-2,header=TRUE, blank.lines.skip = FALSE)
    end.date=grep("Annual Factors",ff.factor0[,1])-2

    ff.factor.month=ff.factor0[1:end.date,]

    colnames(ff.factor.month)=c("Dates",colnames(ff.factor.month)[-1])
    head(ff.factor.month)
    tail(ff.factor.month)

    start.annual=grep("Annual Factors",ff.factor0[,1])+3
    end.annual=grep("Copyright",ff.factor0[,1])
    if (length(end.annual)==0) {end.annual=nrow(ff.factor0)} else {
      end.annual=end.annual-2
    }


    ff.factor.annual=ff.factor0[start.annual:end.annual,]

    colnames(ff.factor.annual)=c("Dates",colnames(ff.factor.annual)[-1])
    head(ff.factor.annual)
    tail(ff.factor.annual)

    table.names=trimws(c("Monthly Factors",ff.factor0[,1][which(grepl("[Aa-zZ]", ff.factor0[,1]))][1]))
    dat=list()
    dat[[1]]=ff.factor.month
    dat[[2]]=ff.factor.annual
    names(dat)=table.names
  }

  file.remove(temp)
  return(list(data=dat,table.names=table.names,file.name=filename))


}






getFrench.Portfolios<-function(filename="Portfolios_Formed_on_ME") {
  url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

  ##Step 1. Download .zip dataset


  if (grepl("daily",filename)) {
    fileCSV=paste0(filename,".csv")
    fileCSV=gsub(fileCSV,pattern = "daily",replacement = "Daily")

  } else {  fileCSV=paste0(filename,".CSV") }


  {
    fileZIP=paste0(filename,"_CSV.zip")
    Link=paste0(url,fileZIP)
    temp = tempfile(tmpdir=getwd(),fileext = ".zip")
    download.file(Link,temp)

  }


  ##Step 2. Retrieve .csv data

  tmp0=read.delim(unz(temp, fileCSV),header=FALSE,blank.lines.skip = FALSE)
  S=which(grepl("19", tmp0[1:50,1]))[1]

  ff.factor0 = read.csv(unz(temp, fileCSV),skip=S-3,header=FALSE, blank.lines.skip = FALSE)
  head(ff.factor0)
  tail(ff.factor0)
  endOfData=grep("For portfolios formed ",ff.factor0[,1])[1]-3
    if (is.na(endOfData)) {
  endOfData=grep("Copyright ",ff.factor0[,1])-2
  }
  if (length(endOfData)==0) {ff.factor0=ff.factor0} else {
    ff.factor0=ff.factor0[1:(endOfData),]
  }
  tail(ff.factor0)

  table.names=trimws(ff.factor0[,1][which(grepl("[Aa-zZ]", ff.factor0[,1]))])
  colNames=c("Dates",as.character(ff.factor0[2,])[-c(1)])

  table.names.where=which(grepl("[Aa-zZ]", ff.factor0[,1]))

  dat=list()
  for (i in 1:length(table.names)) {#i=5
    if (i==length(table.names)) {
      dat0=ff.factor0[(table.names.where[i]+2):nrow(ff.factor0),]
    } else {

      dat0=ff.factor0[(table.names.where[i]+2):(table.names.where[i+1]-3),]

    }
    colnames(dat0)=colNames

    dat[[i]]=dat0

  }

  names(dat) <- trimws(table.names)

  file.remove(temp)
  return(list(data=dat,table.names=table.names,file.name=filename))

}





getTWSE.fiveSecond<-function(ymd=NULL,skip=2,index.names=NULL){

  if(.Platform$OS.type == "unix") {
    Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
  } else {
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }

  if(is.null(ymd)) {ymd=Sys.Date()}
  if (lubridate::wday(ymd,label=TRUE)=="Sun") {ymd=as.Date(ymd)+1} else if (lubridate::wday(ymd)=="Sat") {ymd=as.Date(ymd)-1}
  ymd0=gsub(ymd,pattern = "-",replacement = "")

  twse_5sec=paste0("https://www.twse.com.tw/exchangeReport/MI_5MINS_INDEX?response=csv&date=",ymd0)
  tmp=read.csv(twse_5sec,skip=skip,sep=",",header=FALSE)
  head(tmp,15)

  dat0=tmp[1:3241,-ncol(tmp)]
  HM=paste(ymd, sub("=","",dat0[,1]))
  dat=dat0[,-1]
  dat=sapply(dat0[,-1],function(x) as.numeric(gsub(x, pattern=",", replacement="")))
  rownames(dat)=HM

  if(is.null(index.names)) {colnames(dat)=paste0("V",seq(ncol(dat)))} else {colnames(dat)=index.names}

  return(list(data=dat))

}



