get_nauthors <- function(metric, ...) {
  UseMethod("get_nauthors")
}

get_nauthors.metrics <- function(metric, ...)
{
  return(metric$authors)
}

get_data <- function(metric, ...)
{
  UseMethod("get_data")
}

get_data.metrics <- function(metric, ...)
{
  return(metric$data)
}

get_pdf <- function(metric, ...)
{
  UseMethod("get_pdf")
}


get_pdf.metrics <- function(metric, ...)
{
  return(metric$pdf)
}

get_rnw <- function(metric, ...)
{
  UseMethod("get_rnw")
}

get_rnw.metrics <- function(metric, ...)
{
  return(metric$rnw)
}

get_extensions <- function(metric, ...)
{
  UseMethod("get_extensions")
}

get_extensions.metrics <- function(metric, ...)
{
  return(metric$ext)
}

get_rdfiles <- function(metric,filename,section,what, ...)
{
  UseMethod("get_rdfiles")
}

get_rdfiles.metrics <- function(metric,filename,section,what, ...)
{
  if(missing(metric))
  {
    stop("Argument \"metric\" cannot be empty")
  }

  if(missing(what))
  {
    stop("Argument \"what\" cannot be empty")
  }
  else
  {
    if (what != "lines" && what !="characters" && what !="all")
    {
      stop("This metric does not exist")
    }
  }

  if (!missing(filename))
  {
    tmp <- filename == metric$rdnames
    if (any(tmp))
    {
      index <- which(tmp)

      if (!missing(section))
      {
         tmps <- section == metric$rdfiles[[index]][1]
         if (any(tmps))
         {
          s <- which(tmps)
         }
         else
         {
          warning("This section does not exist. Showing results for all. Rd files")
         }
      }
      
      return(sum(metric$rdfiles[[index]][s,what]))
    }
    else
    {
      warning("This .Rd file does not exist. Showing results for all .Rd files")
    }
  }

  y <- 0
  s <- 0

  if (!missing(section))
  {
    for(i in 1:length(metric$rdfiles))
    {
      tmps <- section == metric$rdfiles[[i]][1]
      if (any(tmps))
      {
        s <- which(tmps)
        y <- y + metric$rdfiles[[i]][s,what]
      }
    }
  }
  else
  {
    for(i in 1:length(metric$rdfiles))
    {
      size <- length(metric$rdfiles[[i]][,1])
      y <- y + metric$rdfiles[[i]][size,what]
    }
  }

  return(y)
}

# filename can be the name of a .R file
# or it can be the name of one function
get_r_metric <- function(metric,filename,what, ...)
{
  UseMethod("get_r_metric")
}

get_r_metric.metrics <- function(metric,filename,component,what, ...)
{
  if(missing(metric))
  {
    stop("Argument \"metric\" cannot be empty")
  }

  if(missing(what))
  {
    stop("Argument \"what\" cannot be empty")
  }

  if (what != "lines" && what != "blank" && what != "intercomment" && what != "intracomment" &&
      what !="characters" && what !="all")
  {
    stop("This metric does not exist")
  }

  if (!missing(filename))
  {
    if( regexpr("[.]R$",filename,extended=F)[1] != -1 )
    {
      tmp <- filename == metric$rnames
      if (any(tmp))
      {
        index <- which(tmp)
        last <- length(metric$rfiles[[index]][,what])

          if (!missing(component))
          {

            x <- component == metric$rfiles[[index]][,"component"]
            if (any(x))
            {
              comp_name <- which(x)
              return(metric$rfiles[[index]][comp_name,what])
            }
            else
            {
              warning("This component does not exist. Showing metric for all components of this .R file")
              return(metric$rfiles[[index]][last,what])
            }

          }
          else return(metric$rfiles[[index]][last,what])
      }
      else warning("This .R file does not exist. Showing the sum of all .R files")
    }
    else warning("This .R file does not exist. Showing the sum of all .R files")
  }

  output <- 0
  for (i in 1:length(metric$rfiles))
  {
    last <- length(metric$rfiles[[i]][,what])
    output <- output + metric$rfiles[[i]][last,what]
  }

  return(output)
}

get_type <- function(metric,filename,component_name)
{
  if (missing(metric)) stop("Missing argument metric.")
  if (missing(filename)) stop("Missing argument filename.")
  if (missing(component_name)) stop("Missing argument compoenent_name.")
  
  if( regexpr("[.]R$",filename,extended=F)[1] != -1 )
  {
      tmp <- filename == metric$rnames
      if (any(tmp))
      {
        index <- which(tmp)
        components <- metric$rfiles[[index]][,"component"]
        out <- component_name == as.character(components)
        if (is.numeric(length(out)))
        {
          return(as.character((metric$rfiles[[index]][out,"type"])))
        }
      }
  }
  stop("This component does not exist")
}