
define.direct.metric <- function(name,
                                 definition=function(pkgpath) {},
                                 fnname=name, envir=globalenv()) {

  attr(definition, 'name') <- name
  attr(definition, 'class') <- 'direct.metric'
  assign(fnname, definition, envir=envir)
}


define.internal.direct.metric <- function(name, definition) {
  define.direct.metric(name, definition,
                       internal.direct.metric.name(name),
					   topenv())
}


internal.direct.metric.name <- function(name) {
  sprintf('%s.direct.metric', name)
}


get.direct.metric.function <- function(name) {
  if ( inherits(name, 'direct.metric') )
    return(name)

  fnname <- internal.direct.metric.name(name)
  get(fnname, envir=topenv())
}


list.direct.metrics <- function() {
  list.metrics('direct')
}



### Direct metrics definitions ###

define.internal.direct.metric('authors',
function(pkgpath) {
  desc <- read.dcf(file.path(pkgpath, 'DESCRIPTION'))

  # Heuristic that authors are comma-seperated:
  unname(length(strsplit(desc[,'Author'], ',')[[1]]))   
})


define.internal.direct.metric('dependencies',
function(pkgpath) {
  .dep <- function(which) {
    if ( which %in% colnames(desc) )
	  unname(length(strsplit(desc[,which], ',')[[1]]))
	else
	  0
  }
  
  desc <- read.dcf(file.path(pkgpath, 'DESCRIPTION'))
  

  c(depends=.dep('Depends'),
    imports=.dep('Imports'),
    suggests=.dep('Suggests'))
})


define.internal.direct.metric('namespace',
function(pkgpath) {
  p <- strsplit(pkgpath, .Platform$file.sep)[[1]]
  n <- length(p)
  package <- p[n]
  package.lib <- paste(p[-n], collapse=.Platform$file.sep)

  namespace <- parseNamespaceFile(package, package.lib,
                                  mustExist=FALSE)
  
  table <- sapply(namespace, length)
  table['S3methods'] <- table['S3methods'] / 3

  table
})


define.internal.direct.metric('foreign.language.files',
function(pkgpath) {
  evaluate_ext_files(pkgpath)
})


define.internal.direct.metric('vignettes',
function(pkgpath) {
  rnw <- length(evaluate_rnw_files(pkgpath))
  pdf <- length(evaluate_pdf_files(pkgpath))

  c(rnw=rnw, pdf=pdf)
})


define.internal.direct.metric('data',
function(pkgpath) {
  length(evaluate_data_files(pkgpath))
})


define.internal.direct.metric('citation',
function(pkgpath) {
  citfile <- file.path(pkgpath, 'inst', 'CITATION')
  if ( file_test('-f', citfile) )
  {
		result <- try(length(readCitationFile(citfile)),TRUE)
    if (!is.character(result)){
      result
    }
  }
  NULL
})


define.internal.direct.metric('rd.files',
function(pkgpath) {
  do.call(rbind,
          evaluate_rd_files(pkgpath))
})


define.internal.direct.metric('r.files',
function(pkgpath) {
  do.call(rbind,
          evaluate_rfiles(pkgpath))
})

