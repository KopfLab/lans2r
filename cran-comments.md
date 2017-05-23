Resubmission of a new package after addressing issues raised in the first submission. Unfortunately a new warning appeared on win_builder that seems not related to this package (due to a mismatch between Rcpp and dplyr versions on win-builder) and hopefully will not be an issue for this submission (details below). Thank you for considering it for release.

## Test environments

* local OS X install, R 3.4.0
* Ubuntu 12.04 (on travis-ci), R-oldrel, R-release, R-devel
* Windows Server 2012 R2 x64 on appveyor (oldrel, devel and release)
* win-builder (devel and release)

## R CMD check results

#### on platforms other than win-builder

0 ERRORS | 0 WARNINGS | 0 NOTES

#### on win-builder

0 ERRORS | 1 WARNING | 1 NOTE

The warning (below) appears to be due to a mismatch between the Rcpp and dplyr versions installed on win-builder and has only appeared today. It is unrelated to the package and does not affect it passing all other tests without errors and warnings.

> * checking whether package 'lans2r' can be installed ... WARNING
Found the following significant warnings:
  Warning: Installed Rcpp (0.12.11) different from Rcpp used to build dplyr (0.12.10).
  Warning: Installed R (R Under development (unstable) (2017-05-20 r72713)) different from R used to build dplyr (R Under development (unstable) (2017-05-20 r72708)).

The note (below) is due to this being a new submission and because of spelling-related flagged words in DESCRIPTION. The words (nanometer, spectrometry) are spelled correctly according to Wikipedia (US spelling). The acronym (NanoSIMS) is the recognized way to refer to the type of mass-spectrometry data this package is concerned with and is an important keyword for any potential users to find this package.

> * checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sebastian Kopf <sebastian.kopf@colorado.edu>'
New submission
Possibly mis-spelled words in DESCRIPTION:
  NanoSIMS (2:26, 4:19, 4:56)
  nanometer (3:43)
  spectrometry (4:5)

## Reverse dependencies

This is a new release, there are currently no reverse dependencies for this package.
