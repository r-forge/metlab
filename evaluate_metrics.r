evaluate_description <- function(path_pkg,file_sep)
{
  desc_file <- readLines(paste(path_pkg,file_sep,"DESCRIPTION",sep=""), n=-1)
  for(i in 1:length(desc_file))
  {
    if( regexpr("^[:blank:]\\{0,\\}Author:",desc_file[i],extended=F)[1] != -1 )
    {
      authors <- NULL

      # Concatenating all lines that contains information about authors
      while (regexpr("^[[:blank:]]\\{0,\\}Maintainer:", desc_file[i] ,extended=F)[1] == -1)
       {
         authors <- paste(authors,desc_file[i])
         i <- i + 1
       }

       at_counter <- 0

       # Counts how many @ finds in the concateneted line
       while (regexpr("@", authors ,extended=F)[1] != -1)
       {
         authors <- sub("@","X",authors,extended=F)
         at_counter <- at_counter + 1
       }

       return(at_counter)
    }
  }
}

evaluate_ext_files <- function(path_pkg,file_sep)
{
  directory_info <- file.info(paste(path_pkg,file_sep,"src",sep=""))
  if (is.na(directory_info$isdir)==F)
  {
    extension_list <- "R"
    blacklist <- c("win","h","hpp")

    src_files <- list.files(paste(path_pkg,file_sep,"src",file_sep,sep=""))
    for(i in 1:length(src_files))
    {
      tmp <- src_files[i]
      pieces <- strsplit(tmp,"[.]",extended=F)[[1]]
      if (length(pieces)<=1)
      {
        extension <- "R"
      }
      else
      {
        extension <- pieces[length(pieces)]
      }

      # Check if this extension is already in the extension_list or blacklist
      contains <- any( extension == c(extension_list[],blacklist[])  )

      if (contains == F)
      {
        extension_list <- c(extension_list,extension)
      }
    }
    return(extension_list[-1])
  }
  return(NULL)
}

evaluate_rnw_files <- function(path_pkg,file_sep)
{
  way <- paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep="")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    rnw_files <- list.files(way)
    tmp <- sapply(rnw_files,regexpr,pattern="[.]Rnw$",extended=F )
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_pdf_files <- function(path_pkg,file_sep)
{
  way <- paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep="")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    pdf_files <- list.files(way)
    tmp <- sapply(pdf_files,regexpr,pattern="[.]pdf$",extended=F )
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_data_files <- function(path_pkg,file_sep)
{
  way <- paste(path_pkg,file_sep,"data",sep="")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    data <- list.files(way)
    tmp <- sapply(data,regexpr,pattern="[.]RData$",extended=F )
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_rd_files <- function(path_pkg,file_sep)
{
  rdfiles <- list.files(paste(path_pkg,file_sep,"man",sep=""),pattern="[.]Rd$")
  rdinfo <- vector("list",length(rdfiles))

  for(i in 1:length(rdfiles))
  {
    rdinfo[i] <- eval_each_rd(paste(path_pkg,file_sep,"man",file_sep,rdfiles[i],sep=""))
  }

  return(rdinfo)
}

eval_each_rd <- function(path_file)
{
  tmp <- Rd_parse(path_file)

  section <- as.vector(unlist(lapply(tmp$data$tags, "[[", 1)))
  content <- as.vector(unlist(tmp$data$vals))

  lines <- NULL
  characters <- NULL

  for(i in 1:length(content))
  {
    x <- strsplit(content[i],"\n",extended=F)
    lines <- c(lines,length(x[[1]]))
    y <- nchar(content[i])
    characters <- c(characters,y)
  }

  rdfile <- readLines(path_file, n=-1)
  total <- length(rdfile)

  section <- c(section,"all")
  lines <- c(lines,sum(lines))
  characters <- c(characters,sum(characters))

  df <- data.frame(section,lines,characters)

  return(list(df))
}
