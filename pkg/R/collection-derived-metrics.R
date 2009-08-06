
define.collection.derived.metric <- function(name,
                                             definition=function(pkgpath) {}) {
  fnname <- collection.derived.metric.name(name)
  attr(definition, 'name') <- name
  attr(definition, 'class') <- 'collection.derived.metric'
  assign(fnname, definition, envir=topenv())
}


collection.derived.metric.name <- function(name) {
  sprintf('%s.collection.derived.metric', name)
}


get.collection.derived.metric.function <- function(name) {
  fnname <- collection.derived.metric.name(name)
  get(fnname, envir=topenv())
}


list.collection.derived.metrics <- function() {
  list.metrics('collection.derived')
}


collection.derived.metric <- function(name, metrics.collection=NULL, ...) {
  fn <- get.collection.derived.metric.function(name)
  if ( is.null(metrics.collection) )
    fn
  else
    fn(metrics.collection, ...)
}


cderived <- collection.derived.metric



### Collection derived metrics definition ###

define.collection.derived.metric('programmer.productivity',
function(metrics.collection) {
  lines <- sapply(metrics.collection,
                  function(x) sum(x$r.files$lines))
  
  date <- as.Date(sapply(metrics.collection, attr,
                         'description')['date',])
  
  diff(lines) / as.numeric(diff(date))
})


define.collection.derived.metric('documenter.productivity',
function(metrics.collection) {
  lines <- sapply(metrics.collection,
                  function(x) sum(x$rd.files$lines))
  
  date <- as.Date(sapply(metrics.collection, attr,
                         'description')['date',])
  
  diff(lines) / as.numeric(diff(date))
})

  


  
