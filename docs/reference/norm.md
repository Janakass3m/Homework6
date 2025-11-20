# Euclidean norm of a sparse numeric vector

Computes the Euclidean (L2) norm of a `sparse_numeric` vector: the
square root of the sum of squared elements.

## Usage

``` r
norm(x)

# S4 method for class 'sparse_numeric'
norm(x)
```

## Arguments

- x:

  A `sparse_numeric` vector.

## Value

A numeric scalar giving the norm.

## Examples

``` r
x <- as(c(3, 0, 4), "sparse_numeric")
norm(x)
#> [1] 5
```
