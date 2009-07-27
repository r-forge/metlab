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
  cat("Number of different extensions in the source files:",length(x$ext),"\n",x$ext,"\n")

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
  notlist <- parse(path_file,n=-1)
  content <- as.list(notlist)

  inter_comments_vec <- NULL
  intra_comments_vec <- NULL
  lines_vec <- NULL
  blank_vec <- NULL
  characters_vec <- NULL
  components_vec <- NULL
  component_type_vec <- NULL

  # for each component
  for(i in 1:length(content))
  {
    if ((is.symbol((content[[i]])[[1]])) && (as.character((content[[i]])[[1]])=="{"))
    {
      # It is {} (doxygen/roxygen)
      inter_comments_vec <- c(inter_comments_vec,0)
      intra_comments_vec <- c(intra_comments_vec,0)
      blank_vec <- c(blank_vec,0)
      characters_vec <- c(characters_vec,2)
      lines_vec <- c(lines_vec,1)
      components_vec <- c(components_vec,"Roxygen/doxygen")
      component_type_vec <- c(component_type_vec,"{}")
    }
    else
    {
      out <- analyze_component(content[[i]],notlist[i])

      inter_comments_vec <- c(inter_comments_vec,as.double(out[1]))
      intra_comments_vec <- c(intra_comments_vec,as.double(out[2]))
      blank_vec <- c(blank_vec,as.double(out[3]))
      characters_vec <- c(characters_vec,as.double(out[4]))
      lines_vec <- c(lines_vec,as.double(out[5]))
      components_vec <- c(components_vec,out[6])
      component_type_vec <- c(component_type_vec,out[7])
     }
 }

  # counting things outside components (e.g. comments and blank lines)
  rfile <- readLines(path_file, n=-1)
  comments_total <- 0
  blanks_total <- 0
  characters_total <- 0
  for(i in 1:length(rfile))
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

  components_vec <- c(components_vec,"all:")
  component_type_vec <- c(component_type_vec,"")
  lines_vec <- c(lines_vec,length(rfile))
  blank_vec <- c(blank_vec,blanks_total)
  inter_comments_vec <- c(inter_comments_vec,comments_total)
  intra_comments_vec <- c(intra_comments_vec,sum(intra_comments_vec))
  characters_vec <- c(characters_vec,characters_total)

  return(data.frame("component"=components_vec,"lines"=lines_vec,"blank lines"=blank_vec,
                    "inter"=inter_comments_vec,"intra"=intra_comments_vec,"characters"=characters_vec,"type"=component_type_vec))

}

analyze_component <- function(content,notlist)
{
  component_name <- 0
  component_type <- 0

  if (!(regexpr("<-",as.character(content[1]),extended=F)[1] != -1) && (!(regexpr("=",as.character(content[1]),extended=F)[1] != -1)))
  {
    # Function Calling
    component_name <- as.character(content[1])
    component_type <- "function calling"
    
    # Check if there is SetMethod
    if (as.character(component_name) == "setMethod")
    {
      component_name <- as.character(content[2])
      component_type <- "methodS4"
    }
    else if (as.character(component_name) == "setGeneric")
    {
      component_name <- as.character(content[2])
      component_type <- "genericS4"
    }
    
  }
  else if (regexpr("^function(",as.character(content[3]),extended=F)[1] != -1)
  {
    # Function declaration
    component_name <- as.character(content[2])
    component_type <- "function"

    # Check if there is UseMethod inside
    tst <- notlist[1]
    if (is_genericS3(tst) == TRUE)
    {
      component_type <- "genericS3"
    }
    else if (is_methodS3(content) == TRUE)
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

  out <- counting_component(content,notlist,component_name,component_type)

  output <- c(out[1],out[2],out[3],out[4],out[5],out[6],out[7])
  return(output)
}

counting_component <- function(content,notlist,component_name,component_type)
{
  inter_comments <- 0
  intra_comments <- 0
  blank <- 0
  characters <- 0

  textfile <- deparse(content,control = "all", width.cutoff=500,nlines=-1)

  if (component_type == "function calling" || component_type == "methodS4" || component_type == "genericS4")
  {
    textfile <- deparse(notlist,control = "all", width.cutoff=500,nlines=-1)
  }

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


  if (component_type == "function")
    length(textfile) <- length(textfile)-1

  if (component_type == "function calling"  || component_type == "methodS4" || component_type == "genericS4")
  {
    characters <- characters - 12 # 12 is nchar("expression()")
  }

  output <- c(inter_comments,intra_comments,blank,characters,length(textfile),component_name,component_type)
  return(output)
}

is_genericS3 <- function(notlist)
{
  text <- deparse(notlist)
  isgenericS3 <- FALSE
  
  for(i in 1:length(text))
  {
    line_of_code <- text[i]

    # substitute every symbol ' by a "
    line_of_code <- gsub("\'","\"",line_of_code,extended=F)

    # substitute every symbol \" by X since a \" will be always inside quotes in a correct expression
    line_of_code <- gsub("[\\][\"]","X",line_of_code,extended=F)

    # substitute every string (i.e "anything") by X
    while (regexpr("\"", line_of_code ,extended=F)[1] != -1)
    {
      line_of_code <- sub("[\"][^\"]\\{0,\\}[\"]","X",line_of_code,extended=F)
    }

    if (regexpr("UseMethod(", line_of_code ,extended=F)[1] != -1)
    {
      isgenericS3 <- TRUE
      break
    }
  }

  return(isgenericS3)
}

is_methodS3 <- function(notlist)
{
  name <- as.character(notlist[2])
  if (regexpr("[.]", name)[1] != -1)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}