# Sparse numeric vector class

An S4 class for storing numeric vectors in sparse format. Only the
non-zero entries are stored, together with their positions and the full
vector length.

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of 1-based positions of the non-zero values.

- `length`:

  Integer scalar giving the total length of the vector (including
  zeros).
