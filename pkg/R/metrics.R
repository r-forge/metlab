
metrics <- function(paths, direct.metrics) {
  direct.metrics <- lapply(direct.metrics,
                           get.direct.metric.function)
  
  if ( length(paths) == 1 )
    metrics.pkg(paths, direct.metrics)
  else
    metrics.pkgs(paths, direct.metrics)
}


metrics.pkg <- function(paths, direct.metrics) {
  metrics <- lapply(direct.metrics,
                    function(fn) fn(paths))
  names(metrics) <- sapply(direct.metrics, attr, 'name')

  descr <- read.dcf(file.path(paths, 'DESCRIPTION'))

  try (date <- unname(descr[,'Date']),TRUE)
  if (is.character(date))
    date <- NA

  structure(metrics, class='metrics',
            description=c(package=unname(descr[,'Package']),
              version=unname(descr[,'Version']),
              date=date,
              path=unname(paths)))
}


metrics.pkgs <- function(paths, direct.metrics) {
  structure(lapply(paths, metrics, direct.metrics),
            class='metricscollection')
}


print.metrics <- function(x, ...) {
  descr <- attr(x, 'description')

  cat('Metrics object\n\n')
  cat(sprintf('%s(%s)', descr['package'], descr['version']), '\n\n')
  cat('Direct metrics:\n')
  cat(paste(strwrap(paste(names(x), collapse=', ')),
            collapse='\n'), '\n')
}


print.metricscollection <- function(x, ...) {
  descr <- lapply(x, attr, 'description')
  
  cat('Metrics object collection\n\n')
  cat(paste(strwrap(paste(sapply(descr,
                                 function(d)
                                 sprintf('%s(%s)', d['package'], d['version'])),
                          collapse=', ')), collapse='\n'), '\n\n')
  cat('Direct metrics:\n')
  cat(paste(strwrap(paste(names(x[[1]]), collapse=', ')),
            collapse='\n'), '\n')
}



