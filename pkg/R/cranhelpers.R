
available.pkg.versions <- function(pkg, repos=getOption('repos'),
                                   archive='Archive') {
  require(gsubfn)

  .versions <- function(url) {
    versions <- NULL
    temp.file <- 'index.html'
    if ( download.file(url, temp.file, quiet=TRUE) == 0 ) {
      f <- readLines(temp.file)
      versions <- unlist(strapply(f, sprintf('>%s_(.*)\\.tar\\.gz<', pkg),
                                  backref=-1, perl=TRUE))
      unlink(temp.file)
    }
    
    versions
  }
  
  contrib.url <- contrib.url(repos, 'source')
  archive.url <- sprintf('%s/%s/%s', contrib.url, archive, pkg)
  
  current <- .versions(contrib.url)
  archive <- .versions(archive.url)
  
  rbind(cbind(version=current, repository=contrib.url),
        do.call(rbind, lapply(archive, cbind, archive.url)))
}

                                   
download.pkg.versions <- function(pkg, destdir, repos=getOption('repos'),
                                  archive='Archive', ...) {

  a <- available.pkg.versions(pkg, repos=repos, archive=archive)

  dir.create(destdir)

  res <- matrix(NA, nrow=0, ncol=3,
                dimnames=list(NULL, c('package', 'version', 'path')))
  for ( i in seq(len=nrow(a)) ) {
    filename <- sprintf('%s_%s.tar.gz', pkg, a[i,1])
    url <- sprintf('%s/%s', a[i,2], filename)
    download.file(url, file.path(destdir, filename), ...)
    res <- rbind(res, unname(cbind(pkg, a[i,1],
                                   file.path(destdir, filename))))
  }

  res
}


download.pkgs <- function(pkgs, destdir, repos=getOption('repos'), ...) {
  dir.create(destdir)
  res <- download.packages(pkgs, destdir, repos=repos,
                           type='source', ...)
  cbind(package=res[,1],
        version=unlist(strapply(res[,2], '_(.*)\\.tar\\.gz',
          backref=-1, perl=TRUE)),
        path=res[,2])
}


unpack.pkgs <- function(pkgs, destdir, ...) {
  res <- pkgs
  for ( i in seq(len=nrow(pkgs)) ) {
    system(sprintf('tar -zxvf %s -C %s/', pkgs[i,3], destdir), ...)
    system(sprintf('mv %s/%s %s/%s_%s', destdir, pkgs[i,1],
                   destdir, pkgs[i,1], pkgs[i,2]), ...)
    res[i,3] <- sprintf('%s/%s_%s', destdir, pkgs[i,1], pkgs[i,2])
  }
  
  res
}


crosssectional.study <- function(pkgs, destdir, ...) {
  unpack.pkgs(download.pkgs(pkgs, destdir, ...), destdir, ...)
}


longitudinal.study <- function(pkg, destdir, ...) {
  unpack.pkgs(download.pkg.versions(pkg, destdir, ...), destdir)
}


reinit.destdir <- function(destdir) {
  .read.description <- function(pkgpath) {
    path <- file.path(pkgpath, 'DESCRIPTION')
    if ( file.exists(path) )
      c(read.dcf(file.path(pkgpath, 'DESCRIPTION'),
                 fields=c('Package', 'Version')), pkgpath)
    else
      NULL
  }
  
  files <- list.files(destdir, full.names=TRUE)
  dirs <- files[file.info(files)$isdir]
  pkgs <- lapply(dirs, .read.description)
  pkgs <- pkgs[!sapply(pkgs, is.null)]
  structure(do.call(rbind, pkgs),
            dimnames=list(NULL, c('package', 'version', 'path')))
}
