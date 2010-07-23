
list.metrics <- function(which) {
  l <- lapply(ls(pattern=glob2rx(sprintf('*.%s.metric', which)),
                 envir=topenv()), get, envir=topenv())
  l <- l[sapply(l, inherits, sprintf('%s.metric', which))]
  sapply(l, attr, 'name')
}

