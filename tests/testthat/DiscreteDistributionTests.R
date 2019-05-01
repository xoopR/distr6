library(testthat)

dbin = function(x, log,...){
 m1 = choose(self$getParameterValue(id="size"), x)
 m2 = self$getParameterValue(id="prob")^x
 m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
 return(m1 * m2 * m3)
}
discreteTester = Distribution$new("Discrete Test","TestDistr",support=interval$new(0,100),
                          symmetric=T, type = posNaturals$new(),
                          distrDomain=posNaturals$new(),
                          pdf = dbin,
                          parameters = list(list(id = "prob",
                                                 name = "Probability of Success",
                                                 default = 0.5,
                                                 value = 0.2,
                                                 settable = TRUE,
                                                 fittable = TRUE,
                                                 class = "numeric",
                                                 lower = 0,
                                                 upper = 1,
                                                 description = "None"),
                                            list(id = "size",
                                                 name = "Number of trials",
                                                 default = 10,
                                                 settable = TRUE,
                                                 fittable = TRUE,
                                                 class = "integer",
                                                 lower = 0,
                                                 upper = Inf,
                                                 description = "None")),
                          decorators = list(CoreStatistics),
                          paramvalues = list(size = 100)
                          )


test_that("check all accessors are working", {
  expect_equal(discreteTester$strprint(), "TestDistr(prob = 0.2, size = 100.0)")
  expect_equal(discreteTester$name(), "Discrete Test")
  expect_equal(discreteTester$short_name(), "TestDistr")
  expect_equal(discreteTester$description(), NULL)
  expect_equal(discreteTester$decorators(), "CoreStatistics")
  expect_equal(discreteTester$valueSupport(), "discrete")
  expect_equal(discreteTester$variateForm(), "univariate")
  expect_true(discreteTester$symmetry())
  expect_equal(discreteTester$getParameterValue("size"), 100)
})

test_that("check parameter getting/setting", {
  expect_error(discreteTester$setParameterValue("sgdsvfd"))
  expect_silent(discreteTester$setParameterValue(list(size = 2, prob = 0.9)))
  expect_equal(discreteTester$getParameterValue("prob"), 0.9)
})

test_that("check basic maths functions as expected", {
  expect_equal(discreteTester$pdf(1), dbinom(1,2,0.9))
  expect_equal(discreteTester$expectation(), 2*0.9)
  expect_equal(discreteTester$var(), 2*0.9*0.1)
})

test_that("check kurtosis and skewness", {
  expect_equal(round(discreteTester$kurtosis(), 2), 2.56)
  expect_equal(discreteTester$kurtosisType(), "leptokurtic")
  expect_equal(round(discreteTester$skewness(), 2), -1.89)
  expect_equal(discreteTester$skewnessType(), "negative skew")
})

test_that("check exotic functions silent",{
  expect_silent(discreteTester$mode())
  expect_silent(discreteTester$kthmoment(2))
  expect_silent(discreteTester$kthmoment(3, type = "standard"))
  expect_silent(discreteTester$pgf(z=2))
  expect_silent(discreteTester$entropy())
})

test_that("check convolution functions",{
  discreteTester2 = discreteTester$clone()
  discreteTester2$.__enclos_env__$private$.short_name = "TestDistr2"
  convTest = Convolution$new(discreteTester, discreteTester2, support = interval$new(0,10))
  expect_silent(convTest$setParameterValue(list(TestDistr_prob = 0.3)))
  expect_equal(convTest$getInternalModel("TestDistr")$getParameterValue("prob"),0.3)
  expect_silent(convTest$setParameterValue(list(TestDistr_prob = 0.9)))
  convTest2 = Convolution$new(convTest, discreteTester2, support = interval$new(0,10))
  expect_equal(convTest2$pdf(1:12), dbinom(1:12, size = 6, prob = 0.9))
  expect_equal(convTest2$expectation(), 6*0.9)
  discreteTester3 = discreteTester + discreteTester2
  discreteTester4 = discreteTester - discreteTester2
  expect_silent(convTest2$setParameterValue(list(TestDistrTestDistr2_TestDistr_size = 5)))
})
