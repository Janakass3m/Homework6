# Mean of a sparse_numeric vector

Computes the mean of a `sparse_numeric` vector, treating missing entries
as zeros (i.e., over the full length).

## Usage

``` r
# S4 method for class 'sparse_numeric'
mean(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` object.

- ...:

  Ignored.

## Value

A numeric scalar giving the mean.

## Examples

``` r
x <- as(c(1, 0, 2, 0), "sparse_numeric")
mean(x)
#> [1] 0.75
```
