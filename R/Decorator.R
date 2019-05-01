DistributionDecorator <- R6::R6Class("DistributionDecorator")

DistributionDecorator$set("public","initialize",function(distribution){
  if(getR6Class(self) == "DistributionDecorator")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  decorators = distribution$decorators()
  if(!is.null(decorators)){
    decorators = lapply(decorators,get)
  }
  decorators = unique(c(decorators,get(getR6Class(self))))

  assign(paste0(substitute(distribution)),
         Distribution$new(name = distribution$name(),
                          short_name = distribution$short_name(),
                          type = distribution$type(),
                          support = distribution$support(),
                          distrDomain = distribution$distrDomain(),
                          symmetric = as.logical(distribution$symmetry()),
                          pdf = distribution$.__enclos_env__$private$.pdf,
                          cdf = distribution$.__enclos_env__$private$.cdf,
                          quantile = distribution$.__enclos_env__$private$.quantile,
                          rand = distribution$.__enclos_env__$private$.rand,
                          parameters = distribution$.__enclos_env__$private$.parameters,
                          decorators = decorators,
                          valueSupport = distribution$valueSupport(),
                          variateForm = distribution$variateForm(),
                          description = distribution$description()
         ), pos = .GlobalEnv)

  cat(paste(substitute(distribution),"is now decorated with",
            getR6Class(self),"\n"))
})