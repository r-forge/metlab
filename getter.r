get_nauthors <- function(metric_file)
{
  return(metric_file$authors)
}

get_data <- function(metric_file)
{
  return(metric_file$data)
}

get_pdf <- function(metric_file)
{
  return(metric_file$pdf)
}

get_rnw <- function(metric_file)
{
  return(metric_file$rnw)
}

get_extensions <- function(metric_file)
{
  return(metric_file$ext)
}

get_rdfiles <- function(metric_file,filename,section,what="lines")
{
  if (what == "lines")
    x <- 2
  else if (what == "characters")
    x <- 3
  else
  {
    x <- 2
    warning("Does not exist this metric. Showing number of lines")
  }
  
  if (!missing(filename))
  {
    tmp <- filename == metric_file$rdnames
    if (any(tmp))
    {
      index <- which(tmp)

      if (!missing(section))
      {
         tmps <- section == metric_file$rdfiles[[index]][1]
         if (any(tmps))
         {
          s <- which(tmps)
         }
         else
         {
          warning("This section does not exist. Showing results for all. Rd files")
         }
      }
      
      return(sum(metric_file$rdfiles[[index]][s,x]))
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
    for(i in 1:length(metric_file$rdfiles))
    {
      tmps <- section == metric_file$rdfiles[[i]][1]
      if (any(tmps))
      {
        s <- which(tmps)
        y <- y + metric_file$rdfiles[[i]][s,x]
      }
    }
  }
  else
  {
    for(i in 1:length(metric_file$rdfiles))
    {
      size <- length(metric_file$rdfiles[[i]][,1])
      y <- y + metric_file$rdfiles[[i]][size,x]
    }
  }

  return(y)
}

# filename can be the name of a .R file
# or it can be the name of one function
get_r_metric <- function(metric_file,filename,what="lines")
{
  if (what == "lines")
    x <- 2
  else if (what == "blank")
    x <- 3
  else if (what == "intercomment")
    x <- 4
  else if (what == "intrarcomment")
    x <- 5
  else if (what == "characters")
    x <- 6
  else
  {
    x <- 2
    warning("Does not exist this metric. Showing number of lines")
  }

  if (!missing(filename))
  {
    if( regexpr("[.]R$",filename,extended=F)[1] != -1 )
    {
      tmp <- filename == metric_file$rnames
      if (any(tmp))
      {
        index <- which(tmp)
        last <- length(metric_file$rfiles[[index]][,x])
        return(metric_file$rfiles[[index]][last,x])
      }
      else
      {
        warning("This .R file does not exist. Showing the sum of all .R files")
      }
    }
    else
    {
      for(i in 1:length(metric_file$rfiles))
      {
        for(j in 1:length(metric_file$rfiles[[i]][,1]))
        {
          tmp <- filename == as.character(metric_file$rfiles[[i]][,1])
          if (any(tmp))
          {
            index <- which(tmp)
            return(metric_file$rfiles[[i]][index,x])
          }
        }
      }
      warning("This component does not exist. Showing the sum of all .R files")
    }
  }

  output <- 0
  for (i in 1:length(metric_file$rfiles))
  {
    last <- length(metric_file$rfiles[[i]][,x])
    output <- output + metric_file$rfiles[[i]][last,x]
  }

  return(output)
}
