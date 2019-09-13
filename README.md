
<!-- README.md is generated from README.Rmd. Please edit that file -->

# niva

<!-- badges: start -->

<!-- badges: end -->

The goal of niva is to provide the neurotrauma research group with
usefull functions in R.

## Installation

This package is currently only available on GitHub. It can be downloaded
as follows:

``` r
# install.packages("devtools")  # If you do not already have devtools
devtools::install_github("flucloxacillin/niva")
```

## Example

This is a basic example which shows you how to use
dw\_combine\_imputations:

``` r
library(niva)

dw_combine_imputations(imputeddata,formula,reg_type,outcome_levels,imputations)
```
