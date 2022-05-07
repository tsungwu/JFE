riskParityPortfolio <- function(data, covmat="cov",strategy="minrisk",Type="MV") {

  dataUsed0=timeSeries::as.timeSeries(data)
  COV=paste0(covmat, "Estimator")
  mySpec0 = portfolioSpec()
  setEstimator(mySpec0)=COV
  suppressMessages(setType(mySpec0) <- Type)

  if (strategy=="All Assets") {

    RiskParity <- FRAPO::PERC(match.fun(getEstimator(mySpec0))(dataUsed0)$Sigma)
    setWeights(mySpec0)=as.numeric(FRAPO::Weights(RiskParity)/100)
    Portfolio=feasiblePortfolio(dataUsed0, spec=mySpec0, constraints="LongOnly")
    Portfolio@title="All Assets Risk Parity Porfolio"


  } else {

    if (getType(mySpec0)=="CVaR") {setSolver(mySpec0)<- "solveRglpk.CVAR"
    } else if (getType(mySpec0)=="MAD") {setSolver(mySpec0) <- "solveRglpk.MAD"
    } else {setSolver(mySpec0) <- "solveRquadprog"}


    if (strategy=="maxreturn") {setTargetRisk(mySpec0)=sd(apply(dataUsed0,1,mean))
    } else if (strategy=="minrisk") {setTargetReturn(mySpec0)=mean(apply(dataUsed0,2,mean))}

    if (strategy=="GMVP") {strategyP="minvariancePortfolio"} else {strategyP=paste0(strategy,"Portfolio")}

    QP0=match.fun(strategyP)(data=dataUsed0, spec=mySpec0, constraints="LongOnly")
    #output0=round(rbind(getWeights(QP0),getCovRiskBudgets(QP0)),4);rownames(output0)=c("Portfolio Weights","Covariance Risk Budgets")
	#output0

    ID0=which(getWeights(QP0) ==0)
    ID1=which(getWeights(QP0) !=0)

    RiskParity <- FRAPO::PERC(match.fun(getEstimator(mySpec0))(dataUsed0[,ID1])$Sigma)
    specifiedWeight=rep(0,ncol(dataUsed0))
    specifiedWeight[ID1]=as.numeric(FRAPO::Weights(RiskParity)/100)
    setWeights(mySpec0)=specifiedWeight
    Portfolio=feasiblePortfolio(data=dataUsed0, spec=mySpec0, constraints="LongOnly")
    Portfolio@title=QP0@title
  }
  Portfolio
}








riskOptimalPortfolio <- function(data, Type="AveDD",value) {

  dataUsed=timeSeries::as.timeSeries(data)
  FUNC=paste0("P",Type)
  VALUE=paste0(Type,"=",value)
  if (Type=="MaxDD") {
		WEIGHTS=FRAPO::Weights(FRAPO::PMaxDD(dataUsed,MaxDD=value))
  } else if (Type=="AveDD") {WEIGHTS=FRAPO::Weights(FRAPO::PAveDD(dataUsed,AveDD=value))
  } else if (Type=="CDaR") {WEIGHTS=FRAPO::Weights(FRAPO::PCDaR(dataUsed,alpha=value))

  }

  WEIGHTS=round(WEIGHTS,4)

  mySpec = portfolioSpec()
  setWeights(mySpec)=WEIGHTS
  Portfolio=feasiblePortfolio(data=timeSeries::returns(dataUsed), spec=mySpec, constraints="LongOnly")
  Portfolio@spec@model$type="Risk Optimal Portfolio of"
  Portfolio@title=Type
  Portfolio
}
