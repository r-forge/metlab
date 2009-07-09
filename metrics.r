metrics <- function(path_pkg)
{
  library(tools)
  file_sep <- base:::.Platform$file.sep

  data_list <- evaluate_data_files(path_pkg,file_sep)
  pdf_list <- evaluate_pdf_files(path_pkg,file_sep)
  rnw_list <- evaluate_rnw_files(path_pkg,file_sep)
  ext_list <- evaluate_ext_files(path_pkg,file_sep)
  number_authors <- evaluate_description(path_pkg,file_sep)
  rd_files_list <- evaluate_rd_files(path_pkg,file_sep)
  r_files_list <- evaluate_rfiles(path_pkg,file_sep)
  
  rdnames <-list.files(paste(path_pkg,file_sep,"man",sep=""),pattern="[.]Rd$")
  rnames <- list.files(paste(path_pkg,file_sep,"R",file_sep,sep=""),pattern="[.]R$")
  
  output <- list( "data"=data_list,
                  "pdf"=pdf_list,
                  "rnw"=rnw_list,
                  "ext"=ext_list,
                  "authors"=number_authors,
                  "rdnames"= rdnames,
                  "rdfiles"= rd_files_list,
                  "rnames"= rnames,
                  "rfiles"=r_files_list )

  class(output) <- "metrics"

  return(output)
}

print.metrics <- function(x)
{
  cat("Number of authors:",x$authors,"\n\n")
  cat("Number of pdf files:",length(x$pdf),"\n",x$pdf,"\n\n")
  cat("Number of Rnw files:",length(x$rnw),"\n",x$rnw,"\n\n")
  cat("Number of data files:",length(x$data),"\n",x$data,"\n\n")
  cat("Number of different extensions in the source files:",length(x$src),"\n",x$src,"\n")
  
  cat("\n","Analysis of each .Rd file:")
  for(i in 1:length(x$rdnames))
  {
    cat("\n", x$rdnames[i],"\n")
    print(x$rdfiles[[i]])
  }
  
  cat("\n\n","Analysis of each .R file:")
  for(i in 1:length(x$rnames))
  {
    cat("\n", x$rnames[i],"\n")
    print(x$rfiles[[i]])
  }
  
}

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
  rnw_files <- list.files(paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep=""))
  tmp <- sapply(rnw_files,regexpr,pattern="[.]Rnw$",extended=F )
  return(names(tmp[tmp!=-1]))
}

evaluate_pdf_files <- function(path_pkg,file_sep)
{
  pdf_files <- list.files(paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep=""))
  tmp <- sapply(pdf_files,regexpr,pattern="[.]pdf$",extended=F )
  return(names(tmp[tmp!=-1]))
}

evaluate_data_files <- function(path_pkg,file_sep)
{
  data <- list.files(paste(path_pkg,file_sep,"data",sep=""))
  return(unlist(data))
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
  
  section <- as.vector(unlist(tmp$data[[2]]))
  content <- as.vector(unlist(tmp$data[[1]]))

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

evaluate_rfiles <- function(path_pkg,file_sep)
{
  rfiles <- list.files(paste(path_pkg,file_sep,"R",file_sep,sep=""),pattern="[.]R$")
  rinfo <- vector("list",length(rfiles))

  for(i in 1:length(rinfo))
  {
    path <- paste(path_pkg,file_sep,"R",file_sep,rfiles[i],sep="")
    rinfo[[i]] <- eval_each_rfile(path)

  }
  return(rinfo)
}

eval_each_rfile <- function(path_file)
{
  content <- as.list(parse(path_file),n=-1)

  inter_comments_vec <- NULL
  intra_comments_vec <- NULL
  lines_vec <- NULL
  blank_vec <- NULL
  characters_vec <- NULL
  components <- NULL

  # for each component
  for(i in 1:length(content))
  {
    r_name <- content[i]
    component_name <- as.character(r_name[[1]][2])

    inter_comments <- 0
    intra_comments <- 0
    blank <- 0
    characters <- 0

    if (component_name != "NULL")
    {
      textfile <- deparse(content[[i]],control = "all", width.cutoff=500,nlines=-1)
    
      # for each line (of this component)
      for (j in 1:length(textfile))
      {
        line_of_code <- textfile[j]

        # count number of characters (excluding blank characters)
        line_tmp <- gsub("[[:blank:]]","",line_of_code,extended=F)
        characters <- characters + nchar(line_tmp)

        # count blank lines
        if(regexpr("^[[:blank:]]\\{0,\\}$",line_of_code,extended="F")[1] != -1)
        {
         blank <- blank+1
        }

        # substitute every symbol ' by a "
        line_of_code <- gsub("\'","\"",line_of_code,extended=F)

        # substitute every symbol \" by X since a \" will be always inside quotes in a correct expression
        line_of_code <- gsub("[\\][\"]","X",line_of_code,extended=F)

        # If there is a symbol # in the beggining (ignoring blank characters)
        if( regexpr("^[[:blank:]]\\{0,\\}[#]",line_of_code,extended=F)[1] != -1 )
        {
          inter_comments <- inter_comments+1
        }
        # If not, then substitute every string (i.e "anything") by X
        else
        {
          while (regexpr("\"", line_of_code ,extended=F)[1] != -1)
          {
            line_of_code <- sub("[\"][^\"]\\{0,\\}[\"]","X",line_of_code,extended=F)
          }

          # If still there is a # symbol, then it has a comment after some code
          if (regexpr("#", line_of_code ,extended=F)[1] != -1)
          {
            intra_comments <- intra_comments+1
          }
        }
      }

    inter_comments_vec <- c(inter_comments_vec,inter_comments)
    intra_comments_vec <- c(intra_comments_vec,intra_comments)
    lines_vec <- c(lines_vec,length(textfile)-1)
    blank_vec <- c(blank_vec,blank)
    characters_vec <- c(characters_vec,characters)
    components <- c(components,component_name)
    }
  }

  # counting things outside components (e.g. comments)
  rfile <- readLines(path_file, n=-1)
  comments_outside <- 0
  blanks_outside <- 0
  characters_outside <- 0
  for(i in 1:length(rfile))
  {
    # if there is a symbol # in the beggining (ignoring blank characters)
    if( regexpr("^[[:blank:]]\\{0,\\}[#]",rfile[i],extended=F)[1] != -1 )
    {
      comments_outside <- comments_outside + 1
      blanks_outside <- blanks_outside + 1
      characters_outside <- characters_outside + nchar(rfile[i])
    }
  }

  components <- c(components,"all:")
  lines_vec <- c(lines_vec,length(rfile))
  blank_vec <- c(blank_vec,blanks_outside+sum(blank_vec))
  inter_comments_vec <- c(inter_comments_vec,comments_outside)
  intra_comments_vec <- c(intra_comments_vec,sum(intra_comments_vec))
  characters_vec <- c(characters_vec,characters_outside+sum(characters_vec))

  return(data.frame("component"=components,"lines"=lines_vec,"blank lines"=blank_vec,
                    "inter"=inter_comments_vec,"intra"=intra_comments_vec,"characters"=characters_vec))

}

# TBD
halstead <- function(path_file)
{
  #path_file = "..\\archetypes\\R\\pcplot.R"
  path_file = "lixo.R"
  content <- parse(path_file,n=-1)
  print_all(content)
}

# TBD
print_all <- function(x)
{
  if( is.symbol(x) || is.character(x) || is.numeric(x) )
  {
    cat(as.character(x),"\n")
  }
  else
  {
    tmp <- as.list(x)
    for(i in 1:length(tmp))
    {
      print_all(tmp[[i]])
    }
  }
}