random.string = function(n=1,nchar=8) {
  first = sample(c(letters,LETTERS),n, replace = TRUE)
  chars = sample(c(letters,LETTERS, 0:9),(nchar-1)*n, replace = TRUE)
  if (n == 1) return(paste0(c(first, chars), collapse=""))
  mat = as.data.frame(matrix(c(first,chars), n, nchar))
  do.call(paste0,mat)
}


insert.into.list = function(org.li, insert.li, pos, overwrite.pos=FALSE) {
  if (overwrite.pos) {
    if (pos<=0) return(c(insert.li, org.li))
    if (pos==1) return(c(insert.li, org.li[-pos]))
    if (pos==length(org.li)) return(c(org.li[-pos], insert.li))
    if (pos>length(org.li)) return(c(org.li, insert.li))
    return(c(org.li[1:(pos-1)], insert.li, org.li[(pos+1):length(org.li)]))
  } else {
    if (pos<=1) return(c(insert.li, org.li))
    if (pos>length(org.li)) return(c(org.li, insert.li))
    return(c(org.li[1:(pos-1)], insert.li, org.li[(pos):length(org.li)]))
  }

}

has.field = function(x, fields) {
  fields %in% names(x)
}

set.null.fields = function(x, fields, default) {
  for (field in fields) {
    if (is.null(x[[field]]))
      x[[field]] = default

  }
  x
}

remove.cols = function(x, cols) {
  x[setdiff(names(x),cols)]
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


is.true.vec = function(val) {
  val[is.na(val)] = FALSE
  return(val)
}
