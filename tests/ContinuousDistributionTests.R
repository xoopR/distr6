devtools::load_all()

dexpo = function(x, log,...){
  m1 = self$getParameterValue("lambda")
  m2 = exp(-1 * self$getParameterValue("lambda") * x)
  return(m1 * m2)
}
cexpo = function(x, lower.tail = T, log.p = F,...){
  m1 = exp(-1 * self$getParameterValue("lambda") * x)
  return(1 - m1)
}
continuousTester = Distribution$new("TestDistr","Continuous Test",support=posReals$new(),
                                  symmetric=TRUE, type = posReals$new(),
                                  distrDomain=posReals$new(),
                                  pdf = dexpo, cdf = cexpo,
                                  parameters = list(list(id = "lambda",
                                                         name = "Rate",
                                                         default = 1,
                                                         settable = TRUE,
                                                         fittable = TRUE,
                                                         class = "numeric",
                                                         lower = 0,
                                                         upper = Inf,
                                                         description = "None")),
                                  paramvalues = list(lambda = 6)
)
continuousTester$setParameterValue(list(lambda = 10))
continuousTester$expectation()
continuousTester$var()
continuousTester$sd()
continuousTester$mode()

CoreStatistics$new(continuousTester)
continuousTester$kthmoment(2); continuousTester$var()
continuousTester$kthmoment(3, type = "central")
continuousTester$kthmoment(4, type = "central")

continuousTester$skewness(); continuousTester$kthmoment(3, type = "standard")
continuousTester$kurtosis(); continuousTester$kthmoment(4, type = "standard")
continuousTester$kurtosisType()

continuousTester$setParameterValue(list(lambda = 10))
continuousTester$mgf(6)
continuousTester$getParameterValue("lambda") / (continuousTester$getParameterValue("lambda") - 6)

continuousTester$setParameterValue(list(lambda = 5))
continuousTester$entropy(base=exp(1))
1-log(continuousTester$getParameterValue("lambda"))

ExoticStatistics$new(continuousTester)
continuousTester$survival(1); 1-continuousTester$cdf(1)
continuousTester$survivalAntiDeriv(); continuousTester$survivalPNorm(p = 1); continuousTester$expectation()

continuousTester$survivalPNorm(p = 2)
continuousTester$pdfPNorm(1)
continuousTester$pdfPNorm(2)
continuousTester$cdfPNorm(1)
continuousTester$cdfPNorm(2)

continuousTester$hazard(3)
continuousTester$pdf(3)/continuousTester$survival(3)
-log(continuousTester$survival(3))
continuousTester$cumHazard(3)

continuousTester$setParameterValue(list(lambda = 1))
continuousTester2 = continuousTester$clone()
continuousTester$pdf(2)
continuousTester2$pdf(2)

continuousTester$convolution(continuousTester2, add = T)

continuousTester3 = Distribution$new(name = "Test3",
                                     pdf = continuousTester$convolution(continuousTester2),
                                     support = posReals$new())
continuousTester4 = Distribution$new(name = "Test4",
                                     pdf = continuousTester$convolution(continuousTester2, add =F),
                                     support = posReals$new())

continuousTester3$pdf(1); dgamma(x = 1, shape = 2)
continuousTester4$pdf(1)
continuousTester3$expectation()
continuousTester4$expectation()
continuousTester3$var()
continuousTester4$var()
continuousTester4$parameters()
