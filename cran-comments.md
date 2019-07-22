## Patch to fix CRAN violation

This update is to fix the Warning received on Debian systems resulting from an incorrect use of `remotes::install_github` in the vignettes. This has now been removed.

## Test environments
On travis:
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.5.3 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R 3.6.0 - OK
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.5 LTS;  R devel (2019-06-25 r76738) - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.5.3 - OK
*  x86_64-apple-darwin15.6.0 (64-bit); macOS High Sierra 10.13.3; R 3.6.0 - OK

On appveyor:
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.5.3 - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R 3.6.1 - OK
* x86_64-w64-mingw32/x64 (64-bit); Windows Server 2012 R2 x64 (build 9600); R devel (2019-07-20 r76853) - OK

Local:
 * x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - OK
 * x86_64-w64-mingw32/x64 (64-bit); Windows 10 Home 64-bit; R 3.4.0 - OK

check_win:
 * x86_64-w64_mingw32 (64-bit); R 3.5.3 - NOTE (DOI, see below)
 * x86_64-w64_mingw32 (64-bit); R 3.6.1 - NOTE (DOI, see below)
 * x86_64-w64_mingw32 (64-bit); R devel (2019-07-05 r76784) - NOTE (DOI, see below)

R-hub:
 * Fedora Linux, R-devel, clang, gfortran - OK
 * Ubuntu Linux 16.04 LTS, R-release, GCC - NOTE (DOI, see below)
 * Debian Linux, R-devel, GCC - NOTE (imports, see below)
 * Debian Linux, R-devel, clang, ISO-8859-15 locale - NOTE (imports, see below)
 * Windows Server 2008 R2 SP1, R-devel, 32/64 bit - NOTE (DOI, see below)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2683801
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
 * I have manually checked the DOI and it resolves to the correct URL and works on CRAN
 
Namespaces in Imports field not imported from:
  ‘crayon’ ‘expint’ ‘extraDistr’ ‘GoFKernel’ ‘pracma’ ‘R6’ ‘R62S3’
  ‘utils’
  All declared Imports should be used.

 * All declared imports are used in the package via '::'

## Downstream dependencies
There are currently no downstream dependencies for this package.
