## Downstream dependencies
There is currently one dependency, `mlr3proba`, which I am the maintainer of. This is expected to break after update and will be patched immediately after `distr6` is on CRAN.

## Test environments
* Fedora Linux, R-devel, clang, gfortran - NOTE (DOI)
* Ubuntu Linux 16.04 LTS, R-release, GCC - NOTE (DOI)

## R CMD check results
There were no WARNINGs or ERRORs.

There were 2 NOTEs:

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2683801
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
 * I have manually checked the DOI and it resolves to the correct URL and works on CRAN
 
 Possibly mis-spelled words in DESCRIPTION:
  Devroye (65:509)
  al (65:337, 65:696)
  et (65:334, 65:693)
  
  * Spelled correctly

