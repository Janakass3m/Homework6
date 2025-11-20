# Plot overlapping non-zero entries of two sparse vectors

`plot` method for `sparse_numeric` vectors. It plots the values of two
sparse vectors at their overlapping non-zero positions.

## Usage

``` r
# S4 method for class 'sparse_numeric,sparse_numeric'
plot(x, y, ...)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector of the same length.

- ...:

  Additional arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).
