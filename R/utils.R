set.null.fields = function(x, fields, default) {
  for (field in fields) {
    if (is.null(x[[field]]))
      x[[field]] = default

  }
  x
}

ensure.cols = function(x, col, default=NA) {
  make.cols = setdiff(col, names(x))
  for (col in make.cols) {
    x[[col]] = default
  }
  x
}

has.col = function(x,col) {
  col %in% names(x)
}
