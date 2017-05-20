lans2r
======

[![Build Status](https://travis-ci.org/KopfLab/lans2r.svg?branch=master)](https://travis-ci.org/KopfLab/lans2r)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/lans2r?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/lans2r)
[![codecov.io](https://codecov.io/github/KopfLab/lans2r/coverage.svg?branch=master)](https://codecov.io/github/KopfLab/lans2r?branch=master)

Data interface from Look@NanoSIMS to R.

## Introduction

The Look@NanoSIMS (short LANS) Matlab module written by Lubos Polerecky makes it easy to process NanoSIMS data and draw regions of interest (ROI). The **lans2r** package provides a convenient interface to import the ROI data generated by LANS into R for those interested in processing and plotting the data in R. 

Reference: Polerecky L., Adam B., Milucka J., Musat N., Vagner T. and Kuypers M. M. M. (2012) Look@NanoSIMS - a tool for the analysis of nanoSIMS data in environmental microbiology. Environ. Microbiol. 14, 1009–1023.

## Installation

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github('KopfLab/lans2r')
```

## Example

For instructions on how to use this package, please **[check out the example](https://github.com/KopfLab/lans2r/raw/master/vignettes/lans2r.Rmd)** and the resulting [HTML output](https://rawgit.com/KopfLab/lans2r/master/inst/doc/lans2r.html).
