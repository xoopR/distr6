## Test environments
On travis:
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.5.3 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.6.2 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R devel (2020-01-11 r77654) - OK
* x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.5.3 - OK
* x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.6.2 - OK

Local:
 * x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - OK

check_win:
  * x86_64-w64_mingw32 (64-bit); R devel (2020-01-03 r77630) - OK
  * x86_64-w64_mingw32 (64-bit); R 3.5.3 - NOTE (DOI, see below)
  * x86_64-w64_mingw32 (64-bit); R 3.6.2 - OK

R-hub:
 * Fedora Linux, R-devel, clang, gfortran - NOTE (spelling, see below)
 * Ubuntu Linux 16.04 LTS, R-release, GCC - OK
 * Windows Server 2008 R2 SP1, R-devel, 32/64 bit - OK

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

## Downstream dependencies
There are currently no downstream dependencies for this package.
