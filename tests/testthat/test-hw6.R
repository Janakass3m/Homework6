library(testthat)
library(Homework6)

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("validity method works", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid object fails validity", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("coercion produces sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("methods are defined", {
  expect_no_error(getMethod("show", "sparse_numeric"))
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

test_that("generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_crossprod"))
})

test_that("sparse_add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse_mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse_sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse_crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("sparse_add returns sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_s4_class(sparse_add(x, y), "sparse_numeric")
})

test_that("sparse_add computes correctly", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("sparse_add dense vectors", {
  x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("mismatched lengths throw error", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("mean works", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  expect_equal(mean(x), 0.75)
})

test_that("norm works", {
  x <- as(c(3, 0, 4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("standardize returns mean ~0 and sd ~1", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  z <- as(standardize(x), "numeric")
  expect_equal(mean(z), 0, tolerance = 1e-8)
  expect_equal(sd(z), 1, tolerance = 1e-8)
})

test_that("numeric -> sparse -> numeric roundtrip works", {
  v <- c(0, 1, 0, 2, 0)
  s <- as(v, "sparse_numeric")
  v_back <- as(s, "numeric")
  expect_equal(v_back, v)
})

test_that("empty numeric coerces to sparse with length 0", {
  v <- numeric(0)
  s <- as(v, "sparse_numeric")
  expect_s4_class(s, "sparse_numeric")
  expect_equal(length(s), 0L)
  expect_equal(as(s, "numeric"), numeric(0))
})

test_that("mean of zero-length sparse_numeric is NaN", {
  s <- as(numeric(0), "sparse_numeric")
  expect_true(is.nan(mean(s)))
})

test_that("norm of all-zero vector is 0", {
  s <- as(rep(0, 5), "sparse_numeric")
  expect_equal(norm(s), 0)
})

test_that("standardize errors when length <= 1", {
  s <- as(1, "sparse_numeric")  # length 1
  expect_error(standardize(s),
               "standard deviation is undefined for length <= 1")
})

test_that("standardize errors when standard deviation is zero", {
  s <- as(c(2, 2, 2, 2), "sparse_numeric")  # all same value
  expect_error(standardize(s),
               "standard deviation is zero; cannot standardize")
})

test_that("sparse_mult with no overlapping nonzeros returns all zeros", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  z <- sparse_mult(x, y)
  expect_equal(as(z, "numeric"), c(0, 0, 0))
})

test_that("sparse_crossprod with no overlap returns 0", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  cp <- sparse_crossprod(x, y)
  expect_equal(cp, 0)
})

test_that("length method returns full vector length", {
  x <- as(c(0, 5, 0, 3), "sparse_numeric")
  expect_equal(length(x), 4L)
})

test_that("show prints summary with length and nnz", {
  x <- as(c(0, 5, 0, 3), "sparse_numeric")
  out <- capture.output(show(x))
  expect_true(any(grepl("sparse_numeric\\(length = 4, nnz = 2\\)", out)))
})

test_that("show prints '(all zeros)' for all-zero vector", {
  x <- as(rep(0, 5), "sparse_numeric")
  out <- capture.output(show(x))
  expect_true(any(grepl("\\(all zeros\\)", out)))
})

test_that("plot works when there is overlap", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(3, 0, 4), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("plot works when there is no overlap", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  expect_no_error(plot(x, y))
})


test_that("lookup handles empty idx and empty pos", {
  s <- as(rep(0, 5), "sparse_numeric")
  res <- Homework6:::lookup(s, integer(0))
  expect_equal(res, numeric(0))
})

test_that("lookup returns zeros for missing indices", {
  s <- as(c(0, 2, 0), "sparse_numeric")
  res <- Homework6:::lookup(s, c(1L, 2L, 3L))
  expect_equal(res, c(0, 2, 0))
})

test_that("build_sparse drops all-zero values", {
  res <- Homework6:::build_sparse(c(0, 0, 0), 1:3, 3L)
  dense <- as(res, "numeric")
  expect_equal(dense, c(0, 0, 0))
  expect_s4_class(res, "sparse_numeric")
})

test_that("sparse_sub subtracts correctly", {
  x <- as(c(1, 3, 5), "sparse_numeric")
  y <- as(c(1, 1, 2), "sparse_numeric")
  res <- sparse_sub(x, y)
  expect_equal(as(res, "numeric"), c(0, 2, 3))
})

test_that("sparse_mult with overlap multiplies correctly", {
  x <- as(c(1, 2, 0), "sparse_numeric")
  y <- as(c(2, 3, 4), "sparse_numeric")
  res <- sparse_mult(x, y)
  expect_equal(as(res, "numeric"), c(2, 6, 0))
})

test_that("sparse_crossprod with overlap equals dot product", {
  x <- as(c(1, 2, 0), "sparse_numeric")
  y <- as(c(2, 3, 4), "sparse_numeric")
  cp <- sparse_crossprod(x, y)
  expect_equal(cp, 1*2 + 2*3 + 0*4)
})

test_that("operator + calls sparse_add", {
  x <- as(c(0, 2, 0, 4), "sparse_numeric")
  y <- as(c(1, 0, 3, 0), "sparse_numeric")
  op_res <- x + y
  fun_res <- sparse_add(x, y)
  expect_equal(op_res, fun_res)
})

test_that("operator - calls sparse_sub", {
  x <- as(c(5, 2, 1), "sparse_numeric")
  y <- as(c(1, 0, 1), "sparse_numeric")
  op_res <- x - y
  fun_res <- sparse_sub(x, y)
  expect_equal(as(op_res, "numeric"), as(fun_res, "numeric"))
})

test_that("operator * calls sparse_mult", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(2, 0, 4), "sparse_numeric")
  op_res <- x * y
  fun_res <- sparse_mult(x, y)
  expect_equal(as(op_res, "numeric"), as(fun_res, "numeric"))
})
