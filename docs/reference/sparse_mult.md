# Multiply two sparse numeric vectors element-wise

Element-wise multiplication of two `sparse_numeric` vectors.

## Usage

``` r
sparse_mult(x, y)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_mult(x, y)

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 * e2
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector of the same length.

- e1:

  First `sparse_numeric` vector (for operator methods).

- e2:

  Second `sparse_numeric` vector (for operator methods).

## Value

A `sparse_numeric` vector containing the element-wise product.

## Examples

``` r
x <- as(c(1, 0, 2), "sparse_numeric")
y <- as(c(0, 5, 3), "sparse_numeric")
sparse_mult(x, y)
#> sparse_numeric(length = 3, nnz = 1)
#>   pos  : 3
#>   value: 6
```
