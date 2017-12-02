covLedoit <- function (data, spec = NULL) {
  x.mat = as.matrix(data)
  list(mu = colMeans(x.mat), Sigma = SKCov(x.mat)$sigma)
}


SKCov <- function(data) {
  dat=data
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

covStudent <- function (data, spec = NULL) {
  ###===Multivariate Student t=======###
  x.mat =as.matrix(data)
  list(mu = colMeans(x.mat), Sigma = MASS::cov.trob(x.mat)$cov)
}


GMVPStrategy <-function (data, spec = portfolioSpec(), constraints = "LongOnly", backtest = portfolioBacktest()) {
  strategyPortfolio <- try(minriskPortfolio(data, spec, constraints))
  if (class(strategyPortfolio) == "try-error") {
    strategyPortfolio <- minvariancePortfolio(data,spec,constraints)
  }
  strategyPortfolio
}


###==========###

GoldSach <- function(data, spec = NULL){
  x.mat = data
  rho=0.95
  t=nrow(x.mat)
  p=(t-1):0
  w=rho^p
  dat=sqrt(w)*x.mat
  Sig=cov(dat)*t/sum(w)
  list(mu =colMeans(x.mat) , Sigma =Sig)
}

###===Constant Correlation===###

ShrinkCC <- function (data,spec = NULL) {
  x.mat =data
  t=nrow(x.mat)
  p=(t-1):0
  rho=0.95
  w=rho^p
  list(mu = colMeans(x.mat), Sigma = BurStFin::var.shrink.eqcor(x.mat,weights=1))
}





###==========###
#.LPM<-function(data, spec = NULL){
#  list(mu = fAssets::assetsLPM(data)$mu, Sigma = fAssets::assetsLPM(data)$Sigma)
#}




