
define.derived.metric <- function(name,
                                  definition=function(pkgpath) {}) {
  fnname <- derived.metric.name(name)
  attr(definition, 'name') <- name
  attr(definition, 'class') <- 'derived.metric'
  assign(fnname, definition, envir=topenv())
}


derived.metric.name <- function(name) {
  sprintf('%s.derived.metric', name)
}


get.derived.metric.function <- function(name) {
  fnname <- derived.metric.name(name)
  get(fnname, envir=topenv())
}


list.derived.metrics <- function() {
  list.metrics('derived')
}


derived.metric <- function(name, metrics=NULL, ...) {
  fn <- get.derived.metric.function(name)
  if ( is.null(metrics) )
    fn
  else
    fn(metrics, ...)
}


derived <- derived.metric



### Derived metrics definitions ###

define.derived.metric('src.doc.lineratio',
function(metrics) {
  lines <- sum(metrics$r.files$lines)
  comments <- sum(metrics$r.files$comments.inter) +
    sum(metrics$r.files$comments.intra)

  comments / lines
})


define.derived.metric('characters.line.ratio',
function(metrics) {
  lines <- sum(metrics$r.files$lines)
  characters <- sum(metrics$r.files$characters)
  
  characters / lines
})


