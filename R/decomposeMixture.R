# decomposeMixture <- function(mix) {
#   disc = unlist(lapply(mix$wrappedModels(),testDiscrete))
#   cont = unlist(lapply(mix$wrappedModels(),testContinuous))
#
#   disc_models = mix$wrappedModels()[disc]
#   cont_models = mix$wrappedModels()[cont]
#
#   # disc_support = lapply(disc_models,support)
#   # cont_support = lapply(cont_models,support)
#
#   disc_weights = mix$.__enclos_env__$private$.weights[disc]
#   cont_weights = mix$.__enclos_env__$private$.weights[cont]
#
#   # return(list(discrete = list(models = disc_models, support = disc_support,
#    weights = disc_weights),
#   #      continuous = list(models = cont_models, support = cont_support,
#   weights = cont_weights)))
#   return(list(models = list(discrete = disc_models, continuous = cont_models),
#               weights = list(discrete = disc_weights, continuous = cont_weights)))
# }
#
# m <- MixtureDistribution$new(list(Normal$new(),Binomial$new(),Gamma$new(),Geometric$new()),
#                              weights=c(1,2,3,4))
# decomposeMixture(m)
