# Standardize a sparse numeric vector

Standardizes a `sparse_numeric` vector by subtracting the mean (computed
over all entries including zeros) and dividing by the sample standard
deviation (also over all entries).

## Usage

``` r
standardize(x)

# S4 method for class 'sparse_numeric'
standardize(x)
```

## Arguments

- x:

  A `sparse_numeric` vector.

## Value

A standardized `sparse_numeric` vector with (approximately) mean 0 and
standard deviation 1 when converted to dense form.

## Examples

``` r
x <- as(c(1, 0, 2, 0), "sparse_numeric")
z <- standardize(x)
as(z, "numeric")
#> [1]  0.2611165 -0.7833495  1.3055824 -0.7833495
```
