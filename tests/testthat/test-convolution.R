# dexpo = function(x, log,...){
#   m1 = self$getParameterValue("lambda")
#   m2 = exp(-1 * self$getParameterValue("lambda") * x)
#   return(m1 * m2)
# }
# cexpo = function(x, lower.tail = T, log.p = F,...){
#   m1 = exp(-1 * self$getParameterValue("lambda") * x)
#   return(1 - m1)
# }
# continuousTester = Distribution$new("Continuous Test","ContTest",support=posReals$new(),
#                                     symmetric=TRUE, type = posReals$new(zero=T),
#                                     distrDomain=posReals$new(),
#                                     pdf = dexpo, cdf = cexpo,
#                                     parameters = list(list(id = "lambda",
#                                                            name = "Rate",
#                                                            default = 1,
#                                                            settable = TRUE,
#                                                            fittable = TRUE,
#                                                            class = "numeric",
#                                                            lower = 0,
#                                                            upper = Inf,
#                                                            description = "None")),
#                                     paramvalues = list(lambda = 6)
# )
# library(testthat)
#
# dbin = function(x, log,...){
#   m1 = choose(self$getParameterValue(id="size"), x)
#   m2 = self$getParameterValue(id="prob")^x
#   m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
#   return(m1 * m2 * m3)
# }
# discreteTester = Distribution$new("Discrete Test","TestDistr",support=interval$new(0,100),
#                                   symmetric=T, type = posNaturals$new(),
#                                   distrDomain=posNaturals$new(),
#                                   pdf = dbin,
#                                   parameters = list(list(id = "prob",
#                                                          name = "Probability of Success",
#                                                          default = 0.5,
#                                                          value = 0.2,
#                                                          settable = TRUE,
#                                                          fittable = TRUE,
#                                                          class = "numeric",
#                                                          lower = 0,
#                                                          upper = 1,
#                                                          description = "None"),
#                                                     list(id = "size",
#                                                          name = "Number of trials",
#                                                          default = 10,
#                                                          settable = TRUE,
#                                                          fittable = TRUE,
#                                                          class = "integer",
#                                                          lower = 0,
#                                                          upper = Inf,
#                                                          description = "None")),
#                                   decorators = list(CoreStatistics),
#                                   paramvalues = list(size = 100)
# )
# test_that("check continuous convolution functions", {
#   Exp1 = Exponential$new(rate = 1)
#   Exp2 = Exponential$new(rate = 1)
#   ConvE12 = Convolution$new(Exp1, Exp2, support = PosReals$new())
#   decorate(ConvE12, CoreStatistics)
#   expect_equal(ConvE12$pdf(1), dgamma(x = 1, shape = 2))
#   expect_equal(ConvE12$expectation(), 2)
#   expect_equal(continuousTester3$variance(), 2)
#   expect_equal(continuousTester3$expectation(), continuousTester$expectation() + continuousTester2$expectation())
#   expect_equal(continuousTester3$variance(), continuousTester$variance() + continuousTester2$variance())
#   continuousTester4 = Convolution$new(continuousTester, continuousTester2, type = reals$new(), add = F)
#   expect_equal(continuousTester4$expectation(), continuousTester$expectation() - continuousTester2$expectation())
#   expect_equal(continuousTester4$variance(), continuousTester$variance() + continuousTester2$variance())
# })
#
# test_that("check discrete convolution functions",{
#   discreteTester2 = discreteTester$clone()
#   discreteTester2$.__enclos_env__$private$.short_name = "TestDistr2"
#   convTest = Convolution$new(discreteTester, discreteTester2, support = interval$new(0,10))
#   expect_equal(convTest$expectation(), discreteTester$expectation() + discreteTester2$expectation())
#   expect_equal(convTest$variance(), discreteTester$variance() + discreteTester2$variance())
#   expect_silent(convTest$setParameterValue(list(TestDistr_prob = 0.3)))
#   expect_equal(convTest$getInternalModel("TestDistr")$getParameterValue("prob"),0.3)
#   expect_silent(convTest$setParameterValue(list(TestDistr_prob = 0.9)))
#   convTest2 = Convolution$new(convTest, discreteTester2, type = naturals$new(),
#                               support = interval$new(0,10))
#   expect_equal(convTest2$pdf(1:12), dbinom(1:12, size = 6, prob = 0.9))
#   expect_equal(convTest2$expectation(), 6*0.9)
#   discreteTester3 = Convolution$new(discreteTester, discreteTester2, add = F, support = integers$new())
#   expect_equal(discreteTester3$expectation(), discreteTester$expectation() - discreteTester2$expectation())
#   expect_equal(discreteTester3$variance(), discreteTester$variance() + discreteTester2$variance())
# })
