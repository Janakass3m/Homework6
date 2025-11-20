# Crossproduct of two sparse numeric vectors

Computes the dot product of two `sparse_numeric` vectors.

## Usage

``` r
sparse_crossprod(x, y)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_crossprod(x, y)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector of the same length.

## Value

A numeric scalar equal to `sum(x * y)`.

## Examples

``` r
x <- as(c(1, 0, 2), "sparse_numeric")
y <- as(c(0, 5, 3), "sparse_numeric")
sparse_crossprod(x, y)
#> [1] 6
```
