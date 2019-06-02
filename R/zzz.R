.onAttach <- function(libname, pkgname){
  R62S3::R62S3(Distribution, assignEnvir = environment())
  R62S3::R62S3(ParameterSet, assignEnvir = environment())

  y = sapply(ls(name=".GlobalEnv"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "Distribution_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = (y[y!="FALSE"])
  lapply(y,R62S3::R62S3, assignEnvir = environment())
}
