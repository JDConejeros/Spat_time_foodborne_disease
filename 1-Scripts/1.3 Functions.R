# 0.3 Functions ----------------

make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d
}

as_semester <- function(x, period = 6, sep = " S") {
  ym <- as.yearmon(x)
  paste(as.integer(ym), (cycle(ym) - 1) %/% period + 1, sep = sep)
}

