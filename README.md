
# IsotoneOptimization

<!-- badges: start -->
<!-- badges: end -->

The goal of IsotoneOptimization is to implement the Sequential Block Merging algorithm for solving isotonic regression with constraints represented by arbitrary directed acyclic graph.

## Installation

You can install the development version of IsotoneOptimization from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Xiwen1997/IsotoneRpkg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(IsotoneOptimization)
## basic example code

Emat <- t(matrix( c(1,2,  1,3,  1,4,  2,5,  2,6,  3,7,  4,8,  5,8,  6,9,  6,10),2,10))
x_ordered <- Ordered_DAG(rnorm(10), rep(1,10), Emat)
```

