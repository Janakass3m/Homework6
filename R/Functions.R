## sparse_numeric class, helpers, generics, and methods
## With roxygen2 documentation

#' Homework6: Sparse Numeric Vector Tools
#'
#' Provides an S4 class for sparse numeric vectors and basic operations
#' such as arithmetic, norms, and standardization without converting
#' to dense format.
#'
#' @keywords internal
#' @import methods
#' @importFrom graphics plot grid plot.new title
"_PACKAGE"

# sparse_numeric class

#' Sparse numeric vector class
#'
#' An S4 class for storing numeric vectors in sparse format.
#' Only the non-zero entries are stored, together with their positions
#' and the full vector length.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of 1-based positions of the non-zero values.
#' @slot length Integer scalar giving the total length of the vector
#'   (including zeros).
#'
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

# Validity

setValidity("sparse_numeric", function(object) {
  messages = character()

  if (length(object@length) != 1L || object@length < 0L)
    messages = c(messages, "length must be a single non-negative integer")

  if (length(object@value) != length(object@pos))
    messages = c(messages, "value and pos must have the same length")

  if (length(object@pos)) {
    if (any(is.na(object@pos)))
      messages = c(messages, "pos cannot contain missing values")
    if (any(object@pos < 1L) || any(object@pos > object@length))
      messages = c(messages,
                   "All elements in slot pos must be between 1 and length")
  }

  if (length(messages)) messages else TRUE
})

# Coercion (not exported)

## Coercion: numeric to sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  n = length(from)
  if (n == 0L) {
    return(new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = 0L))
  }
  non_zero = which(from != 0)
  new("sparse_numeric",
      value  = if (length(non_zero)) as.numeric(from[non_zero]) else numeric(0),
      pos    = as.integer(non_zero),
      length = as.integer(n))
})

## Coercion: sparse_numeric to numeric
setAs("sparse_numeric", "numeric", function(from) {
  output = numeric(from@length)
  if (length(from@pos))
    output[from@pos] = from@value
  output
})

# Helper functions (not exported)

check_length = function(x, y) {
  if (x@length != y@length)
    stop("arguments must have the same length", call. = FALSE)
  invisible(TRUE)
}

## Safe lookup into a sparse vector's values by position; returns 0 if missing
lookup = function(s, idx) {
  if (!length(s@pos) || !length(idx)) return(numeric(length(idx)))
  m = match(idx, s@pos, nomatch = 0L)
  output = numeric(length(idx))
  non_zero = m != 0L
  output[non_zero] = s@value[m[non_zero]]
  output
}

## Build a sparse vector from positions and values (drop zeros)
build_sparse = function(vals, idx, n) {
  keep = which(vals != 0)
  if (length(keep)) {
    new("sparse_numeric",
        value  = as.numeric(vals[keep]),
        pos    = as.integer(idx[keep]),
        length = as.integer(n))
  } else {
    new("sparse_numeric",
        value  = numeric(0),
        pos    = integer(0),
        length = as.integer(n))
  }
}

# Generics for sparse operations

#' Add two sparse numeric vectors
#'
#' Element-wise addition of two \code{sparse_numeric} vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector of the same length.
#' @param e1 First \code{sparse_numeric} vector (for operator methods).
#' @param e2 Second \code{sparse_numeric} vector (for operator methods).
#'
#' @return A \code{sparse_numeric} vector containing the element-wise sum.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 0), "sparse_numeric")
#' sparse_add(x, y)
#'
#' @export
#' @exportMethod sparse_add
setGeneric("sparse_add",  function(x, y) standardGeneric("sparse_add"))

#' Subtract two sparse numeric vectors
#'
#' Element-wise subtraction of two \code{sparse_numeric} vectors.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} vector containing \code{x - y}.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 1), "sparse_numeric")
#' sparse_sub(x, y)
#'
#' @export
#' @exportMethod sparse_sub
setGeneric("sparse_sub",  function(x, y) standardGeneric("sparse_sub"))

#' Multiply two sparse numeric vectors element-wise
#'
#' Element-wise multiplication of two \code{sparse_numeric} vectors.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} vector containing the element-wise product.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 3), "sparse_numeric")
#' sparse_mult(x, y)
#'
#' @export
#' @exportMethod sparse_mult
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' Crossproduct of two sparse numeric vectors
#'
#' Computes the dot product of two \code{sparse_numeric} vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector of the same length.
#'
#' @return A numeric scalar equal to \code{sum(x * y)}.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 3), "sparse_numeric")
#' sparse_crossprod(x, y)
#'
#' @export
#' @exportMethod sparse_crossprod
setGeneric("sparse_crossprod",
           function(x, y) standardGeneric("sparse_crossprod"))

# norm() and standardize() generics

#' Euclidean norm of a sparse numeric vector
#'
#' Computes the Euclidean (L2) norm of a \code{sparse_numeric} vector:
#' the square root of the sum of squared elements.
#'
#' @param x A \code{sparse_numeric} vector.
#'
#' @return A numeric scalar giving the norm.
#'
#' @examples
#' x <- as(c(3, 0, 4), "sparse_numeric")
#' norm(x)
#'
#' @export
#' @exportMethod norm
setGeneric("norm", function(x) standardGeneric("norm"))

#' Standardize a sparse numeric vector
#'
#' Standardizes a \code{sparse_numeric} vector by subtracting the mean
#' (computed over all entries including zeros) and dividing by the sample
#' standard deviation (also over all entries).
#'
#' @param x A \code{sparse_numeric} vector.
#'
#' @return A standardized \code{sparse_numeric} vector with (approximately)
#'   mean 0 and standard deviation 1 when converted to dense form.
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' z <- standardize(x)
#' as(z, "numeric")
#'
#' @export
#' @exportMethod standardize
setGeneric("standardize", function(x) standardGeneric("standardize"))


# Methods for sparse operations

#' @rdname sparse_add
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = sort(unique(c(x@pos, y@pos)))
            vx  = lookup(x, idx)
            vy  = lookup(y, idx)
            build_sparse(vx + vy, idx, x@length)
          })

#' @rdname sparse_sub
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = sort(unique(c(x@pos, y@pos)))
            vx  = lookup(x, idx)
            vy  = lookup(y, idx)
            build_sparse(vx - vy, idx, x@length)
          })

#' @rdname sparse_mult
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            ## Only overlapping positions can be non-zero
            idx = intersect(x@pos, y@pos)
            if (!length(idx)) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            vx = lookup(x, idx)
            vy = lookup(y, idx)
            build_sparse(vx * vy, idx, x@length)
          })

#' @rdname sparse_crossprod
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = intersect(x@pos, y@pos)
            if (!length(idx)) return(0.0)
            vx = lookup(x, idx)
            vy = lookup(y, idx)
            sum(vx * vy)
          })

# Arithmetic operator methods

#' @rdname sparse_add
#' @exportMethod "+"
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_sub
#' @exportMethod "-"
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_mult
#' @exportMethod "*"
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# Show and plot methods

#' Display a sparse_numeric object
#'
#' \code{show} method for \code{sparse_numeric} prints the length,
#' number of non-zero entries, and the first few non-zero positions/values.
#'
#' @param object A \code{sparse_numeric} object.
#'
#' @export
#' @exportMethod show
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat(sprintf("sparse_numeric(length = %d, nnz = %d)\n", object@length, nnz))

  if (nnz > 0L) {
    to_show <- min(10L, nnz)
    cat("  pos  : ",
        paste(object@pos[seq_len(to_show)], collapse = " "),
        if (nnz > to_show) " ...", "\n", sep = "")
    cat("  value: ",
        paste(signif(object@value[seq_len(to_show)], 6), collapse = " "),
        if (nnz > to_show) " ...", "\n", sep = "")
  } else {
    cat("  (all zeros)\n")
  }

  invisible(object)
})

#' Plot overlapping non-zero entries of two sparse vectors
#'
#' \code{plot} method for \code{sparse_numeric} vectors. It plots the values
#' of two sparse vectors at their overlapping non-zero positions.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector of the same length.
#' @param ... Additional arguments passed to \code{graphics::plot()}.
#'
#' @export
#' @exportMethod plot
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            check_length(x, y)
            idx = intersect(x@pos, y@pos)
            if (!length(idx)) {
              plot.new()
              title(main = "No overlapping non-zero positions")
              return(invisible(NULL))
            }
            xv = lookup(x, idx)
            yv = lookup(y, idx)
            graphics::plot(xv, yv,
                           xlab = "x (values at overlapping positions)",
                           ylab = "y (values at overlapping positions)",
                           main = "Overlap of non-zero elements",
                           ...)
            graphics::grid()
            invisible(NULL)
          })

# length() and mean() methods

#' Length of a sparse_numeric vector
#'
#' Returns the full length of the underlying vector, including zeros.
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return An integer scalar giving the vector length.
#'
#' @export
#' @exportMethod length
setMethod("length", "sparse_numeric", function(x) {
  x@length
})

#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a \code{sparse_numeric} vector, treating
#' missing entries as zeros (i.e., over the full length).
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A numeric scalar giving the mean.
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' mean(x)
#'
#' @export
#' @exportMethod mean
setMethod("mean",
          signature(x = "sparse_numeric"),
          function(x, ...) {
            n <- x@length
            if (n == 0L) {
              return(NaN)
            }
            sum(x@value) / n
          })

# norm() and standardize() methods

#' @rdname norm
setMethod("norm",
          signature(x = "sparse_numeric"),
          function(x) {
            sqrt(sum(x@value^2))
          })

#' @rdname standardize
setMethod("standardize",
          signature(x = "sparse_numeric"),
          function(x) {
            n <- x@length
            if (n <= 1L) {
              stop("standard deviation is undefined for length <= 1",
                   call. = FALSE)
            }

            # Compute mean and sd using sparse info only
            S1 <- sum(x@value)
            S2 <- sum(x@value^2)
            mu <- S1 / n

            # Sum of squared deviations
            ss <- S2 - (S1^2 / n)
            if (ss < 0 && abs(ss) < 1e-14) ss <- 0

            if (ss == 0) {
              stop("standard deviation is zero; cannot standardize",
                   call. = FALSE)
            }

            var <- ss / (n - 1)
            sd  <- sqrt(var)

            # Build dense standardized vector then convert back to sparse
            dense <- numeric(n)
            if (length(x@pos)) dense[x@pos] <- x@value
            z <- (dense - mu) / sd

            as(z, "sparse_numeric")
          })
