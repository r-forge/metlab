
evaluate_ext_files <- function(path_pkg,file_sep)
{
#  directory_info <- file.info(paste(path_pkg,file_sep,"src",sep=""))
#  if (is.na(directory_info$isdir)==F)
#    {
#      extension_list <- "R"
#      blacklist <- c("win","h","hpp")
#      
#      src_files <- list.files(paste(path_pkg,file_sep,"src",file_sep,sep=""))
#      for(i in 1:length(src_files))
#        {
#          tmp <- src_files[i]
#          pieces <- strsplit(tmp,"[.]",extended=F)[[1]]
#          if (length(pieces)<=1)
#            {
#       extension <- "R"
#     }
#      else
#      {
#        extension <- pieces[length(pieces)]
#      }
#  
#     # Check if this extension is already in the extension_list or blacklist
#      contains <- any( extension == c(extension_list[],blacklist[])  )
#
#      if (contains == F)
#      {
#        extension_list <- c(extension_list,extension)
#      }
#    }
#    return(extension_list[-1])
#  }
#  return(NULL)

  # Change code to return numbers.
  files <- list.files(file.path(path_pkg, 'src'))
  extensions <- sapply(strsplit(files, '.', fixed=TRUE),
                       function(x) {
                         n <- length(x)
                         if ( n > 1 )
                           x[n]
                         else
                           NA
                       })
  table(extensions, useNA='always', dnn=NULL)
}

evaluate_rnw_files <- function(path_pkg,file_sep)
{
  #way <- paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep="")
  way <- file.path(path_pkg, "inst", "doc")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    rnw_files <- list.files(way)
    tmp <- sapply(rnw_files,regexpr,pattern="[.]Rnw$",extended=F,ignore.case=TRUE)
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_pdf_files <- function(path_pkg,file_sep)
{
  #way <- paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep="")
  way <- file.path(path_pkg, "inst", "doc")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    pdf_files <- list.files(way)
    tmp <- sapply(pdf_files,regexpr,pattern="[.]pdf$",extended=F,ignore.case=TRUE)
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_data_files <- function(path_pkg,file_sep)
{
  #way <- paste(path_pkg,file_sep,"data",sep="")
  way <- file.path(path_pkg, "data")
  directory_info <- file.info(way)
  if (is.na(directory_info$isdir)==F)
  {
    data <- list.files(way)
    tmp <- sapply(data,regexpr,pattern="[.]RData$",extended=F,
                  ignore.case=TRUE)
    return(names(tmp[tmp!=-1]))
  }
  else return(NULL)
}

evaluate_rd_files <- function(path_pkg,file_sep)
{
  rdfiles <- list.files(paste(path_pkg,file_sep,"man",sep=""),
                        pattern="[.]Rd$", ignore.case=TRUE)
  rdinfo <- vector("list",length(rdfiles))

  for(i in 1:length(rdfiles))
  {
    rdinfo[i] <- eval_each_rd(file.path(path_pkg, 'man', rdfiles[i]), rdfiles[i])
  }

  return(rdinfo)
}

eval_each_rd <- function(path_file, name)
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

  #section <- c(section,"all")
  #lines <- c(lines,sum(lines))
  #characters <- c(characters,sum(characters))

  df <- data.frame(file=factor(rep(name, length(section))),
                   section=factor(section),lines,characters)

  return(list(df))
}


evaluate_rfiles <- function(path_pkg,file_sep)
{
  rfiles <- list.files(paste(path_pkg,file_sep,"R",file_sep,sep=""),
                       pattern="[.]R$", ignore.case=TRUE)
  rinfo <- vector("list",length(rfiles))

  for(i in 1:length(rinfo))
  {
    path <- file.path(path_pkg, 'R', rfiles[i])
    rinfo[[i]] <- eval_each_rfile(path, rfiles[i])

  }
  return(rinfo)
}

eval_each_rfile <- function(path_file, name)
{
   content <- parse(path_file,n=-1)

  inter_comments_vec <- NULL
  intra_comments_vec <- NULL
  lines_vec <- NULL
  blank_vec <- NULL
  characters_vec <- NULL
  components_vec <- NULL
  component_type_vec <- NULL

  if (length(content)<1)
    return(data.frame("component"=components_vec,"lines"=lines_vec,"blank lines"=blank_vec,"inter"=inter_comments_vec,"intra"=intra_comments_vec,"characters"=characters_vec,"type"=component_type_vec))

  # for each component
  for(i in 1:length(content))
  {
    out <- analyze_component(content[i])
    inter_comments_vec <- c(inter_comments_vec,as.double(out[1]))
    intra_comments_vec <- c(intra_comments_vec,as.double(out[2]))
    blank_vec <- c(blank_vec,as.double(out[3]))
    characters_vec <- c(characters_vec,as.double(out[4]))
    lines_vec <- c(lines_vec,as.double(out[5]))
    components_vec <- c(components_vec,out[6])
    component_type_vec <- c(component_type_vec,out[7])
 }

  # counting things outside components (e.g. comments and blank lines)
  rfile <- readLines(path_file, n=-1)
  comments_total <- 0
  blanks_total <- 0
  characters_total <- 0
  for(i in seq(along=rfile))
  {
    # if there is a symbol # in the beggining (ignoring blank characters)
    if( regexpr("^[[:blank:]]\\{0,\\}[#]",rfile[i],extended=F)[1] != -1 )
    {
      comments_total <- comments_total + 1
    }
    if((regexpr("^[[:blank:]]\\{1,\\}$",rfile[i],extended="F")[1] != -1)||(rfile[i]==""))
    {
      blanks_total <- blanks_total + 1
    }
    characters_total <- characters_total + nchar(rfile[i])
  }

  #components_vec <- c(components_vec,"all:")
  #component_type_vec <- c(component_type_vec,"")
  #lines_vec <- c(lines_vec,length(rfile))
  #blank_vec <- c(blank_vec,blanks_total)
  #inter_comments_vec <- c(inter_comments_vec,comments_total)
  #intra_comments_vec <- c(intra_comments_vec,sum(intra_comments_vec))
  #characters_vec <- c(characters_vec,characters_total)

  return(data.frame(file=factor(rep(name, length(components_vec))),
                    component=factor(components_vec),
                    type=factor(component_type_vec),
                    lines=lines_vec,
                    characters=characters_vec,
                    lines.blank=blank_vec,
                    comments.inter=inter_comments_vec,
                    comments.intra=intra_comments_vec))
}

analyze_component <- function(content)
{
  component_name <- 0
  component_type <- 0

  if (length(content) > 0 ) {
  if ( length(content[[1]]) > 0 ) {
    if (as.character((content[[1]])=="{"))
    {
      # It is {} (doxygen/roxygen)
      component_name <- "Roxygen/doxygen"
      component_type <- "{}"
    }
    else if (!(regexpr("<-",as.character(content[[1]]),extended=F)[1] != -1) &&
             (!(regexpr("=",as.character(content[[1]]),extended=F)[1] != -1)))
    {
      # Function Calling
      component_name <- as.character(content[[1]][1])
      component_type <- "other"
      
      # Check if there is SetMethod
      if (as.character(component_name) == "setMethod")
      {
        component_name <- as.character(content[[1]][2])
        component_type <- "methodS4"
      }
      else if (as.character(component_name) == "setGeneric")
      {
        component_name <- as.character(content[[1]][2])
        component_type <- "genericS4"
      }
      else if (as.character(component_name) == "setClass")
      {
        component_name <- as.character(content[[1]][2])
        component_type <- "classS4"
      }
      
    }
    else if (regexpr("^function(",as.character(content[[1]][3]),extended=F)[1] != -1)
    {
      # Function declaration
      component_name <- as.character(content[[1]][2])
      component_type <- "function"
      
      # Check if there is UseMethod inside
      if (is_genericS3(content[[1]]) == TRUE)
      {
        component_type <- "genericS3"
      }
      else if (is_methodS3(component_name) == TRUE)
      {
        component_type <- "methodS3"
      }
    }
    else
    {
      # Assignemnt
      component_name <- "<-"
      component_type <- "assignement"
    }
  }}

  out <- counting_component(content,component_name,component_type)

  output <- c(out[1],out[2],out[3],out[4],out[5],out[6],out[7])
  return(output)
}

counting_component <- function(content,component_name,component_type)
{
  inter_comments <- 0
  intra_comments <- 0
  blank <- 0
  characters <- 0

  textfile <- deparse(content,control = "all", width.cutoff=500,nlines=-1)

  # for each line (of this component)
  for (j in 1:length(textfile))
  {
   line_of_code <- textfile[j]

   # count blank lines
   if((regexpr("^[[:blank:]]\\{1,\\}$",line_of_code,extended="F")[1] != -1)||line_of_code=="")
   {
     blank <- blank+1
   }
   # count number of characters (excluding blank characters)
   line_tmp <- gsub("[[:blank:]]","",line_of_code,extended=F)
   characters <- characters + nchar(line_tmp)

   # substitute every symbol \\\" or \\\' by X
   line_of_code <- gsub("[\\][\"]","X",line_of_code,extended=F)
   line_of_code <- gsub("[\\][\']","X",line_of_code,extended=F)

   # If there is a symbol # in the beggining (ignoring blank characters)
   if( regexpr("^[[:blank:]]\\{0,\\}[#]",line_of_code,extended=F)[1] != -1 )
   {
     inter_comments <- inter_comments+1
   }
   # If not, then substitute every string (i.e "anything" or 'anything') by X
   else
   {
     while (regexpr("[\"][^\"]\\{0,\\}[\"]", line_of_code ,extended=F)[1] != -1)
     { line_of_code <- sub("[\"][^\"]\\{0,\\}[\"]","X",line_of_code,extended=F) }

     while (regexpr("[\'][^\']\\{0,\\}[\']", line_of_code ,extended=F)[1] != -1)
     { line_of_code <- sub("[\'][^\']\\{0,\\}[\']","X",line_of_code,extended=F) }

     # If still there is a # symbol, then it has a comment after some code
     if (regexpr("#", line_of_code ,extended=F)[1] != -1)
     {
       intra_comments <- intra_comments+1
     }
   }
  }

  characters <- characters - nchar("expression()")

  output <- c(inter_comments,intra_comments,blank,characters,length(textfile),component_name,component_type)
  return(output)
}

is_genericS3 <- function(content)
{
  text <- deparse(content)
  isgenericS3 <- FALSE

  for(i in 1:length(text))
  {
    line_of_code <- text[i]

   # substitute every symbol \\\" or \\\' by X
   line_of_code <- gsub("[\\][\"]","X",line_of_code,extended=F)
   line_of_code <- gsub("[\\][\']","X",line_of_code,extended=F)

   while (regexpr("[\"][^\"]\\{0,\\}[\"]", line_of_code ,extended=F)[1] != -1)
   { line_of_code <- sub("[\"][^\"]\\{0,\\}[\"]","X",line_of_code,extended=F) }

   while (regexpr("[\'][^\']\\{0,\\}[\']", line_of_code ,extended=F)[1] != -1)
   { line_of_code <- sub("[\'][^\']\\{0,\\}[\']","X",line_of_code,extended=F) }

   line_of_code <- sub("[#].\\{0,\\}","X",line_of_code,extended=F)

    if (regexpr("UseMethod(", line_of_code ,extended=F)[1] != -1)
    {
      isgenericS3 <- TRUE
      break
    }
  }

  return(isgenericS3)
}

is_methodS3 <- function(name)
{
  if (regexpr("[.]", name)[1] != -1) {return(TRUE)}
  else {return(FALSE)}
}
