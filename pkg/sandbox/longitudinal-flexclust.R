
library(metlab) # source('sourcepkg.R')


### Longitudinal study of the flexclust packages:

options(repos='http://cran.r-project.org')
fcdir <- longitudinal.study('flexclust', './flexclust')


# If the packages are already downloaded and unpacked from a previous
# just, just re-initialize the destination directory:

dir <- reinit.destdir('./flexclust')
dir


### Calculate all direct metrics metlab provides:

list.direct.metrics()

met <- metrics(dir[,3], list.direct.metrics())
met

met1 <- met[[1]]
met1

# `met` is a metrics object collection, i.e., the metrics objects of a
# set of packages with the same direct metrics calculated. `met1` is the
# metrics object of the first package of the collection. For analysis we
# can now use the direct metrics and calculate derived metrics and
# collection-derived metrics. 

metlab:::list.derived.metrics()
metlab:::list.collection.derived.metrics()


### Number of authors (direct metric):

met1$authors
sapply(met, '[[', 'authors')


### Overall lines of source code (direct metric):

sum(met1$r.files$lines)
sapply(met, function(m) sum(m$r.files$lines))

# ... plot:

library(ggplot2)

nlines.r <- sapply(met, function(m) sum(m$r.files$lines))
nlines.rd <- sapply(met, function(m) sum(m$rd.files$lines))

versions <- sapply(met, function(m) attr(m, 'description')['version'])

as.numeric.version <- function(x)
  as.numeric(sapply(strsplit(x, '-'), paste, collapse=''))

nlines <- data.frame(version.str=versions,
                     version.num=as.numeric.version(versions),
                     rd=nlines.rd,
                     r=nlines.r)

qplot(version.str, value, group=variable, colour=variable,
      data=melt(nlines, measure.vars=c('r', 'rd')),
      geom=c('point', 'line'), xlab='Version', ylab='Number of lines')

qplot(version.num, value, group=variable, colour=variable,
      data=melt(nlines, measure.vars=c('r', 'rd')),
      geom=c('point', 'line'), xlab='Version', ylab='Number of lines') +
  scale_x_continuous(breaks=nlines$version.num,
                     labels=nlines$version.str)


### Number of definitions (direct metric):

table(met1$r.files$type)
sapply(met, function(m) table(m$r.files$type))

# ... plot:

as.matrix.deftables <- function(x, names) {
  d <- unique(unlist(sapply(x, names)))
  m <- do.call(cbind, lapply(x, function(x) x[d]))
  dimnames(m) <- list(definitions=d, names=names)
  m
}

defs.tables <- sapply(met, function(m) table(m$r.files$type))
defs <- as.matrix.deftables(defs.tables, versions)

qplot(names, weight=value, group=definitions, fill=definitions,
      geom='bar', data=melt(defs), xlab='Version',
      ylab='Number of definitions')

qplot(names, value, group=definitions, colour=definitions,
      geom='line', data=melt(defs), xlab='Version',
      ylab='Number of definitions')


### Releas date versus version number (only for collection meaningful,
### but not derived):

release.dates <- as.Date(sapply(met, function(m)
                                attr(m, 'description')['date']))
release.versions <- as.numeric.version(versions)

qplot(release.dates, release.versions,
      data=data.frame(release.dates, release.versions),
      geom=c('line', 'point'), xlab='Date', ylab='Version')


### Source code lines to in-source documentation ratio (derived metric):

derived('src.doc.lineratio', met1)
sdl <- sapply(met, derived('src.doc.lineratio'))

qplot(versions, sdl, data=data.frame(versions, sdl),
      geom=c('line', 'point'))


### Mean number of character per source code line (derived metric):

derived('characters.line.ratio', met1)

# Unchanging over time? If yes, maybe the programmer has changed, or
# the programming style.

sapply(met, derived('characters.line.ratio'))

# Interesting for different packages with always the same author
# involved (see cross-sectional study example).


### Programmers and documenters productivity (collection-derived metric):

pp <- cderived('programmer.productivity', met)
dp <- cderived('documenter.productivity', met)

prod <- data.frame(days=as.numeric(diff(release.dates)),
                   programmer=pp,
                   documenter=dp)

qplot(days, value, group=variable, colour=variable,
      geom='line', data=melt(prod, id.vars='days'),
      xlab='Days', ylab='Productivity')

