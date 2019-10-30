## Test environments
On travis:
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.5.3 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.6.1 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R devel (2019-10-30 r77341) - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.5.3 - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.6.1 - OK

On appveyor:
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.6.1 - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R devel (2019-10-26 r77334) - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.5.3 - OK

Local:
 * x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - OK

check_win:
  * x86_64-w64_mingw32 (64-bit); R devel (2019-10-29 r77335) - NOTE (DOI, see below)
  * x86_64-w64_mingw32 (64-bit); R 3.5.3 - NOTE (DOI, see below)
  * x86_64-w64_mingw32 (64-bit); R 3.6.1 - NOTE (DOI, see below)

R-hub:
 * Fedora Linux, R-devel, clang, gfortran - NOTE (spelling, see below)
 * Ubuntu Linux 16.04 LTS, R-release, GCC - 1 NOTE (DOI, see below), 1 WARNING (see below)

## R CMD check results
There were no ERRORs.

There was 1 WARNING:

Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://codecov.io/gh/alan-turing-institute/distr6/branch/master/graph/badge.svg

* This link works normally for me and in all other builds, I am unsure why there's a problem in the Ubuntu Linux 16.04 LTS R-release.

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
  
  * These are respectively a name and the phrase 'et al' (not spelling mistakes)
  

## Downstream dependencies
There are currently no downstream dependencies for this package.
