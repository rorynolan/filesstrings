
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ijtiff <img src="man/figures/logo.png" height="140" align="right">

[![Travis-CI Build
Status](https://travis-ci.org/ropensci/ijtiff.svg?branch=master)](https://travis-ci.org/ropensci/ijtiff)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ropensci/ijtiff?branch=master&svg=true)](https://ci.appveyor.com/project/ropensci/ijtiff)
[![codecov](https://codecov.io/gh/ropensci/ijtiff/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/ijtiff)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ijtiff)](https://cran.r-project.org/package=ijtiff)
![RStudio CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/ijtiff)
![RStudio CRAN monthly
downloads](http://cranlogs.r-pkg.org/badges/ijtiff)
[![Rdocumentation](http://www.rdocumentation.org/badges/version/ijtiff)](http://www.rdocumentation.org/packages/ijtiff)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

[![](https://badges.ropensci.org/164_status.svg)](https://github.com/ropensci/onboarding/issues/164)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00633/status.svg)](https://doi.org/10.21105/joss.00633)
[![DOI](https://zenodo.org/badge/122435081.svg)](https://zenodo.org/badge/latestdoi/122435081)

## Introduction

This is a general purpose TIFF I/O utility for R. The [`tiff`
package](https://cran.r-project.org/package=tiff) already exists for
this purpose but `ijtiff` adds some functionality and overcomes some
bugs therein.

  - `ijtiff` can write TIFF files whose pixel values are real
    (floating-point) numbers; `tiff` cannot.
  - `ijtiff` can read and write *text images*; `tiff` cannot.
  - `tiff` struggles to interpret channel information and gives cryptic
    errors when reading TIFF files written by the *ImageJ* software;
    `ijtiff` works smoothly with these images.

To learn about `ijtiff` and how to use it, visit the package website at
<https://ropensci.github.io/ijtiff>.

## Installation

### `libtiff`

`ijtiff` requires you to have the `libtiff` C library installed. To
install `libtiff`:

  - On **Debian Linux**, try `sudo apt-get install libtiff5-dev`, or if
    that fails, try  
    `sudo apt-get install libtiff4-dev`.
  - On **Fedora Linux**, try `sudo yum install libtiff5-dev`, or if that
    doesn’t work, try  
    `sudo yum install libtiff4-dev`.
  - On **Mac**, you need [Homebrew](https://brew.sh/). Then in the
    terminal, run `brew install libtiff`.
  - On 64-bit **Windows**, no setup is required 😄. If you have 32-bit
    windows, you need to install `libtiff` from
    <http://gnuwin32.sourceforge.net/packages/tiff.htm>.

### Installing the release version of the `ijtiff` R package

You can install `ijtiff` from CRAN (recommended) with:

``` r
install.packages("ijtiff")
```

### Installing the development version of the `ijtiff` R package

You can install the development version from GitHub with:

``` r
devtools::install_github("ropensci/ijtiff")
```

## Acknowledgement

This package uses a lot of code from the original `tiff` package by
Simon Urbanek.

## Advice for Package Authors

If you’re authoring a package which is to depend on `ijtiff` and you’re
using AppVeyor, be sure to force AppVeyor to use 64-bit architecture.
This avoids some peculiarities of 32-bit AppVeyor which cause `ijtiff`
installations to fail. You can see how to do this in the
[appveyor.yml](appveyor.yml) file in this repository.

## Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a github pull request. Feel free to contact me
by creating an issue. Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this
project you agree to abide by its
terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
