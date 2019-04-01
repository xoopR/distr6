#----------------------------------
# Given density only: D2P; D2Q; D2R
#----------------------------------

# Density functions are defined
binom_d = function(x, size, prob, log){
  dbinom(x,size,prob,log)
}
norm_d = function(x,mean,sd,log){
  dnorm(x,mean,sd,log)
}

dnorm(1:2)
norm_d(1:2,0,1,F)
dbinom(1:2,10,0.2,F)
binom_d(1:2,10,0.2,F)

# Used as a stand-in for R6 properties
getApproxRange = function(pdmf, lower=NULL, upper=NULL, ... ){
  if(is.null(lower)){
    lower = c(0,-10,-100,-1000,-10000,-100000)
    lower_pdmf = pdmf(lower,...)
    lower = lower[which(lower_pdmf == 0)[1]]
  }
  if(is.null(upper)){
    upper = c(0,10,100,1000,10000,100000)
    upper_pdmf = pdmf(upper,...)
    upper = upper[which(upper_pdmf == 0)[1]]
  }
  range = c(lower,upper)
  return(range)
}

# D2P: Cdf defined to be sum or integral from lower limit to q
D2P = function(q,pdmf,type,...){
  if(type=="Cont")
    return(sapply(q, function(q0) integrate(pdmf, lower = -Inf, upper = q0,...)$value))
  else
    return(sapply(q, function(q0) sum(pdmf(0:q0,...))))
}
pnorm(1:2)
D2P(1:2,norm_d,type="Cont",mean=0,sd=1,log=F)
pbinom(1:2,size=10,prob=0.2,log=F)
D2P(1:2,binom_d,type="Disc",size=10,prob=0.2,log=F)

# D2Q = D2P + P2Q : Root of cdf minus quantile is used for continous distributions and for
#   discrete ones it's the infimum over integers greater than q
P2Q <- function(q,pdmf,type,...){
  range = getApproxRange(pdmf,...)
  if(type=="Cont")
    return(sapply(q, function(q0) {uniroot(function(x){pnorm(x) - q0}, range)$root}))
  else{
    range = seq.int(range[1],range[2])
      cdfs = D2P(range,binom_d,type,size=10,prob=0.2,log=F)
    sapply(q, function(q0){
      return(range[which(cdfs > q0)[1]])
    })
  }
}
qnorm(c(0.1,0.6))
P2Q(c(0.1,0.6),norm_d,"Cont",mean=0,sd=1,log=F)
qbinom(c(0.1,0.6,0.45,0.9),10,0.2)
P2Q(c(0.1,0.6,0.45,0.9),binom_d,"Disc",size=10,prob=0.2,log=F,lower=0,upper=10)

#---------------------------------------
# Given distribution only:
#  P2D below and then
#   P2Q above and
#   P2R = P2Q + Q2R (above)
#---------------------------------------

# Distribution functions are defined
binom_p = function(x, size, prob, log){
  pbinom(x,size,prob,log=log)
}
norm_p = function(x,mean,sd){
  pnorm(x,mean,sd)
}

P2D = function(q,cdf,type,lower,upper,...){
  if(type=="Disc"){
    x = unique(sort(c(seq.int(lower,upper),q)))
    return(diff(cdf((lower-1):upper,...))[which(x %in% q)])
  } else {
    x = unique(sort(c(seq.int(lower,upper,by=0.001),q)))
    y = cdf(x,...)
    return(D1ss(x,y)[which(x %in% q)])
  }
}

dnorm(1:2)
P2D(1:2,norm_p,type="Cont",mean=0,sd=1,lower=-10,upper=10)
P2D(1:2,binom_p,"Disc",0,10,size=10,prob=0.2,log=F)
dbinom(1:2,10,0.2)

#---------------------------------------
# Given quantile/inverse CDF only:
#  Q2P - Numerical inversion
#  Q2R - Inverse transform sampling
#  Q2D - Q2P + P2D
#---------------------------------------

Q2R <- function(n, pdmf,type,...){
  P2Q(runif(n),pdmf,type,...)
}
rnorm(10)
Q2R(10,norm_d,"Cont",mean=0,sd=1,log=F)
rbinom(10,10,0.2)
Q2R(10,binom_d,"Disc",size=10,prob=0.2,log=F,lower=0,upper=10)

Q2P <- function(q,quant,type,...){
  if(type=="Cont")
    return(sapply(q, function(q0) {uniroot(function(x) {quant(x,...)-q0},c(0,1))$root}))
  else{
    return(round(sapply(q, function(q0) {uniroot(function(x) {quant(x,...)-q0},c(0,1))$root})))
  }
}
norm_q = function(x){
  qnorm(x)
}
binom_q = function(x,size,prob){
  qbinom(x,size,prob)
}

pnorm(c(0.2,0.567,0.8))
Q2P(c(0.2,0.567,0.8),norm_q,"Cont")
pbinom(c(0.2,0.567,0.8),10,0.2)
Q2P(c(0.2,0.567,0.8),binom_q,"Cont",size=10,prob=0.2)

#---------------------------------------
# Given a simulation of numbers only:
#  R2D - Density estimation
#  R2P - R2D + D2P
#  R2Q - R2D + D2P + D2Q
#---------------------------------------
R2D <- function(rand_dist,x,...){
  approxfun(density(rand_dist(10000000,...)))(x)
}
R2D(rnorm,1)
dnorm(1)