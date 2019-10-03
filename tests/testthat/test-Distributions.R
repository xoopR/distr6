library(testthat)

context("Distributions")

test_that("check constructor", {
  expect_error(Distribution$new(short_name = "Test_Distr"))
  expect_error(Distribution$new(pdf = dbinom))
  expect_silent(Distribution$new("Discrete Test","TestDistr", pdf = dbinom))
  expect_error(Distribution$new(short_name = "Test Distr", pdf = dbinom))
  expect_silent(Distribution$new(short_name = "TestDistr", pdf = dbinom))
  expect_silent(Distribution$new(name = "Test Distr", pdf = dbinom))
  expect_equal(Distribution$new(name = "Test Distr", pdf = dbinom)$strprint(),"TestDistr")
  expect_null(Distribution$new(name = "Test Distr", pdf = dbinom)$parameters())
})

dbin = function(x, log,...){
  m1 = choose(self$getParameterValue("size"), x)
  m2 = self$getParameterValue("prob")^x
  m3 = (1-self$getParameterValue("prob"))^(self$getParameterValue("size") - x)
  return(m1 * m2 * m3)
}
test_that("check support", {
  expect_equal(Distribution$new("Discrete Test",valueSupport = "c", pdf = dbinom)$valueSupport(), "continuous")
  expect_equal(Distribution$new("Discrete Test",valueSupport = "d", pdf = dbinom)$valueSupport(), "discrete")
  expect_equal(Distribution$new("Discrete Test",valueSupport = "m", pdf = dbinom)$valueSupport(), "mixture")
  expect_error(Distribution$new("Discrete Test",valueSupport = "r", pdf = dbinom))
  expect_equal(Distribution$new("Discrete Test", pdf = dbinom)$valueSupport(), "continuous")
})

test_that("check variateForm", {
  dbin = function(x){
    m1 = choose(self$getParameterValue("size"), x)
    m2 = self$getParameterValue("prob")^x
    m3 = (1-self$getParameterValue("prob"))^(self$getParameterValue("size") - x)
    return(m1 * m2 * m3)
  }
  expect_equal(Distribution$new("Discrete Test",variateForm = "u", pdf = dbinom)$variateForm(), "univariate")
  expect_equal(Distribution$new("Discrete Test",variateForm = "mu", pdf = dbinom)$variateForm(), "multivariate")
  expect_equal(Distribution$new("Discrete Test",variateForm = "ma", pdf = dbinom)$variateForm(), "matrixvariate")
  expect_error(Distribution$new("Discrete Test",variateForm = "m", pdf = dbinom))
  expect_error(Distribution$new("Discrete Test",variateForm = "d", pdf = dbinom))
  expect_equal(Distribution$new("Discrete Test", pdf = dbin)$variateForm(), "univariate")
  expect_equal(Distribution$new("Discrete Test", pdf = function(x,y) return("Test"), type = Reals$new(2))$variateForm(), "multivariate")
})

test_that("check multivariate", {
  expect_error(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,z) return("Test"),
                                type = Reals$new(2)))
  expect_error(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,y,z) return("Test"),
                                type = Reals$new(2)))
  expect_silent(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,y) return("Test"),
                                type = Reals$new(2)))
  expect_equal(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                 cdf = function(x,y) return("Test"))$type()$dimension(),2)
})

ps = ParameterSet$new(id = list("prob","size","qprob"), value = list(0.2, 100, 0.8),
                      support = list(Interval$new(0,1), PosNaturals$new(), Interval$new(0,1)),
                      settable = list(TRUE, TRUE, FALSE),
                      updateFunc = list(NULL, NULL,
                                        function(self) 1 - self$getParameterValue('prob')),
                      description = list("Probability of Success", "Number of trials",
                                         "Probability of failure"))

test_that("check r/d/p/q", {
  expect_silent(Distribution$new("Test", pdf = dbin, parameters = ps)$pdf(1))
  expect_output(Distribution$new("Test", pdf=dbin, quantile = function(p, self) {print(self); return(p)})$quantile(0.4))
  expect_null(Distribution$new("Test", pdf = dbinom)$cdf(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$quantile(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$rand(1))
})

test_that("check is", {
  expect_true(Distribution$new("Test", pdf = dbin, parameters = ps)$isPdf)
  expect_false(Distribution$new("Test", pdf = dbin, parameters = ps)$isCdf)
  expect_false(Distribution$new("Test", pdf = dbin, parameters = ps)$isQuantile)
  expect_false(Distribution$new("Test", pdf = dbin, parameters = ps)$isRand)
})

test_that("working_support",{
  set.seed(1)
  expect_silent(expect_equal(Exponential$new()$.__enclos_env__$private$.setWorkingSupport()$.__enclos_env__$private$.getWorkingSupport(),
               list(inf = 0, sup = 36)))
  expect_silent(expect_equal(Binomial$new()$.__enclos_env__$private$.setWorkingSupport()$.__enclos_env__$private$.getWorkingSupport(),
               list(inf = 0, sup = 10)))
  expect_silent(expect_equal(Normal$new()$.__enclos_env__$private$.setWorkingSupport()$.__enclos_env__$private$.getWorkingSupport(),
               list(inf = -8, sup = 8)))
})

test_that("print",{
  expect_equal(print(VectorDistribution$new(distribution = Bernoulli, params = list(list(prob = 0.2), list(prob = 0.8),
                                                                                       list(prob=0.1),list(prob=0.9),list(prob=0.67)))),
                     VectorDistribution$new(distribution = Bernoulli, params = list(list(prob = 0.2), list(prob = 0.8),list(prob=0.1),
                                                                                             list(prob=0.9),list(prob=0.67)))$print(2))
  expect_output(Binomial$new()$print(1))
  expect_output(Binomial$new()$print(5))
})

test_that("suppress",{
  expect_silent(Distribution$new("name","name",Reals$new(),Reals$new(),TRUE,function(x) return(x),
                                 function(x) return(x), function(x) return(x), function(x) return(x),
                                 ps, CoreStatistics, "continuous", "univariate", "test", TRUE, TRUE))
  expect_message(Distribution$new("name","name",Reals$new(),Reals$new(),TRUE,function(x) return(x),
                                 function(x) return(x), function(x) return(x), function(x) return(x),
                                 NULL, CoreStatistics, "continuous", "univariate", "test", FALSE, TRUE))
})
