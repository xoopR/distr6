library(magrittr)
#-------------------------------------------------------------
# Some Tests
#-------------------------------------------------------------
B <- BinomialDistribution$new(prob=0.6,size=10)
B$summary()
getParams(B)
B$getParams()
B$r(10)
d(B,10)
B$q(0.3)
B$p(1)
B$getParamByName("size")
B$L2Deriv
B %>% setParamByName("prob",0.2) %>% getParams()
