R62S3::R62Fun(Distribution, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(ParameterSet, assignEnvir = topenv())
R62S3::R62Fun(ExoticStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution))
R62S3::R62Fun(CoreStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution))
#
# # #
# # #   # y = sapply(ls(name=".GlobalEnv"),function(x){
# # #   #   if(inherits(get(x),"R6ClassGenerator")){
# # #   #     if(environmentName(get(x)$get_inherit()) == "Distribution_generator")
# # #   #       return(get(x)$classname)
# # #   #     else
# # #   #       return(FALSE)
# # #   #   } else
# # #   #     return(FALSE)
# # #   # })
# # #   # y = (y[y!="FALSE"])
# # #   # lapply(y,R62S3::R62Fun, assignEnvir = parent.env(environment()))
# #
# #
