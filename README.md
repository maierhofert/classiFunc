
<!-- README.md is generated from README.Rmd. Please edit that file -->
classiFunc
==========

[![Travis-CI Build Status](https://travis-ci.org/r-lib/testthat.svg?branch=master)](https://travis-ci.org/r-lib/testthat) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/testthat?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/testthat) [![Coverage status](https://codecov.io/gh/r-lib/testthat/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/testthat?branch=master) [![CRAN version](http://www.r-pkg.org/badges/version/testthat)](https://cran.r-project.org/package=testthat)

Overview
--------

The classiFunc package implements methods for functional data classification. The main functions of this package are classiKnn, a k nearest neighbor estimator for functional data, and classiKernel, a kernel estimator for functional data. The package uses efficiently implemented semimetrics to create the distance matrix of the functional observations in the function computeDistMat.

Using classiFunc
----------------

For installation instructions, see below. A hands on introduction to can be found in the [vignette](https://cran.r-project.org/web/packages/classiFunc/vignettes/classiFunc.html). Details on specific functions are in the [reference manual](https://cran.r-project.org/web/packages/classiFunc/classiFunc.pdf).

Issues & Feature Requests
-------------------------

For issues, bugs, feature requests etc. please use the [Github Issues](https://github.com/maierhofert/classiFunc/issues). Input is always welcome.

Installation
------------

You can install the current classiFunc version from CRAN with:

``` r
install.packages("classiFunc")
```

or the latest patched version from Github with:

``` r
# install.packages("devtools")
devtools::install_github("maierhofert/classiFunc")
```

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- ## basic example code -->
<!-- ``` -->
