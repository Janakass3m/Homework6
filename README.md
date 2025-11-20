
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Homework6

<!-- badges: start -->

[![R-CMD-check.yaml](https://github.com/Janakass3m/Homework6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Janakass3m/Homework6/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# Homework6: Sparse Numeric Vector Class and Operations

The **Homework6** package implements an S4 class called `sparse_numeric`
for efficiently representing and manipulating numeric vectors that
contain many zeros. This package includes arithmetic operations,
statistical methods, full documentation generated with **roxygen2**,
automated testing using **testthat**, and a pkgdown documentation
website.

The package includes:

- Custom S4 class `sparse_numeric`

- Arithmetic methods (`sparse_add`, `sparse_sub`, `sparse_mult`)

- Cross-product, Euclidean norm, and mean computations

- Standardization method that works without converting to dense format

- Full documentation generated with **roxygen2**

- Automated testing using **testthat**

- GitHub Actions continuous integration

- pkgdown documentation website

This package passes **R CMD check** with *0 errors, 0 warnings, and 0
notes*.

## Installation

You can install the development version of Homework6 like so:

``` r
# install.packages("devtools")
devtools::install_github("JanaKassem/Homework6")
```

## Features

The class stores sparse vectors using:

- value: numeric vector of non-zero entries

- pos: positions of these non-zero values

- length: total length of the full vector

### Implemented Methods

- `sparse_add(x, y)`

- `sparse_sub(x, y)`

- `sparse_mult(x, y)`

- `sparse_crossprod(x, y)`

- `mean(x)`

- `norm(x)`

- `standardize(x)`

- `as(x, "numeric")`

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Homework6)
#> 
#> Attaching package: 'Homework6'
#> The following object is masked from 'package:base':
#> 
#>     norm
## basic example code
x <- as(c(0, 2, 0, 4), "sparse_numeric")
y <- as(c(1, 0, 3, 0), "sparse_numeric")

# Addition
sparse_add(x, y)
#> sparse_numeric(length = 4, nnz = 4)
#>   pos  : 1 2 3 4
#>   value: 1 2 3 4

# Mean
mean(x)
#> [1] 1.5

# Norm
norm(x)
#> [1] 4.472136

# Standardize
standardize(x)
#> sparse_numeric(length = 4, nnz = 4)
#>   pos  : 1 2 3 4
#>   value: -0.783349 0.261116 -0.783349 1.30558
```

## pkgdown Website

The pkgdown documentation website will be available at:
<https://janakass3m.github.io/Homework6/>

## License

This package is released under the MIT License.  
See the `LICENSE.md` file for details.
