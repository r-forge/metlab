metrics <- function(path_pkg)
{
  library(tools)

  #path_pkg = "..\\seqinr_2.0-3"
  #path_pkg = "..\\archetypes_1.0"

  separator <- base:::.Platform$file.sep
  file_sep <- separator

  r_files_list <- evaluate_r_files(path_pkg,file_sep)

  data_files_list <- evaluate_data_files(path_pkg,file_sep)

  rd_files_list <- evaluate_rd_files(path_pkg,file_sep)

  doc_list <- evaluate_doc_files(path_pkg,file_sep)

  src_list <- evaluate_src_files(path_pkg,file_sep)

  number_of_authors <- evaluate_description_file(path_pkg,file_sep)

  tmp <- list(r_files = r_files_list,data_files = data_files_list, rd_files = rd_files_list,
  doc_files = doc_list, src_files = src_list, number_of_authors = number_of_authors)

  return(tmp)
}

evaluate_description_file <- function(path_pkg,file_sep)
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

evaluate_src_files <- function(path_pkg,file_sep)
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

      # Check if this extension is already in the list
      contains <- F
      for(j in 1:length(extension_list))
      {
         if (extension_list[j] == extension)
         {
           contains <- T
         }
      }
      
      # Check if this extension is already in the blacklist
      for(j in 1:length(blacklist))
      {
         if (blacklist[j] == extension)
         {
           contains <- T
         }
      }

      if (contains != T)
      {
        extension_list <- c(extension_list,extension)
      }

    }

    tmp <- list(different_valid_src_files = length(extension_list)-1, valid_extensions = extension_list[-1])
    return(tmp)
  }
}

evaluate_doc_files <- function(path_pkg,file_sep)
{
  docs_files <- list.files(paste(path_pkg,file_sep,"inst",file_sep,"doc",file_sep,sep=""))
  docs <- 0
  docs_titles <- "Documents:"
  for (i in 1:length(docs_files))
  {
      if( regexpr("[.]pdf$",docs_files[i],extended=F)[1] != -1 )
      {
        docs <- docs+1
        docs_titles <- c(docs_titles,docs_files[i])
      }
  }
  
  tmp <- list(number_of_vignettes = docs, vignettes = docs_titles)
  return(tmp)
}


evaluate_rd_files <- function(path_pkg,file_sep)
{
  rd_files <- list.files(paste(path_pkg,file_sep,"man",sep=""))

  lines <- 0

  rdinfo <- NULL

  for(i in 1:length(rd_files))
  {
    lines <- lines + length(readLines(paste(path_pkg,file_sep,"man",file_sep,rd_files[i],sep="")))
    tmp <- analyze_rd(paste(path_pkg,file_sep,"man",file_sep,rd_files[i],sep=""))
    rdinfo <- c(rdinfo,tmp)
  }

  general_info <- list(rd_files = length(rd_files), total_number_of_lines = lines )
  return(general_info,rdinfo)

}

evaluate_data_files <- function(path_pkg,file_sep)
{
  data_files <- list.files(paste(path_pkg,file_sep,"data",sep=""))

  tmp <- list(number_data_files = length(data_files), data_files = data_files)
  return(tmp)
}

evaluate_r_files <- function(path_pkg,file_sep)
{
  r_files <- list.files(paste(path_pkg,file_sep,"R",file_sep,sep=""))

  lines <- 0
  code_lines <- 0
  just_comments <- 0
  code_comments <- 0
  blank <- 0

  files_info <- NULL

  for(i in 1:length(r_files))
  {
    src_path <- paste(path_pkg,file_sep,"R",file_sep,r_files[i],sep="")
    rinfo <- analyze_file(paste(path_pkg,file_sep,"R",file_sep,r_files[i],sep=""),r_files[i])
    lines <- lines + rinfo$number_of_lines
    code_lines <- code_lines + rinfo$number_of_codelines
    just_comments <- just_comments + rinfo$number_of_intercomments
    code_comments <- code_comments + rinfo$number_of_intracomments
    blank <- blank + rinfo$number_of_blanks
    files_info <- c(files_info,rinfo)
  }
  
  general_info <- list(number_r_files = length(r_files), total_number_of_lines = lines,
  total_number_of_intercomments = just_comments, total_number_of_intracomments = code_comments,
  total_number_of_codelines = code_lines, total_number_of_blank_lines = blank)
  
  detailed_info <- files_info
  
  return(list(general_info=general_info, detailed_info = detailed_info))
}

analyze_file <- function(path_file,namefile)
{
  content <- readLines(path_file, n=-1)

  blank <- 0

  for (i in 1:length(content))
  {
    if (content[i] == "")
    {
      blank <- blank+1
    }
  }

  # Lines just with comments
  just_comments <- 0
  
  # Lines with comment and code
  code_comments <- 0

  for (i in 1:length(content))
  {
    line_of_code <- content[i]

    # substitute every symbol ' by a "
    line_of_code <- gsub("\'","\"",line_of_code,extended=F)

    # substitute every symbol \" by X since a \" will be always inside quotes in a correct expression
    line_of_code <- gsub("[\\][\"]","X",line_of_code,extended=F)

    # If there is a symbol # in the beggining (ignoring blank characters)
    if( regexpr("^[[:blank:]]\\{0,\\}[#]",line_of_code,extended=F)[1] != -1 )
    {
      just_comments <- just_comments+1
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
          code_comments <- code_comments+1
       }

    }
  }
  # count the number of lines with code
  code_lines <- length(content) - (blank+just_comments)

  return(list(name = namefile, number_of_lines = length(content), number_of_codelines = code_lines, number_of_intercomments = just_comments,
  number_of_intracomments = code_comments, number_of_blanks = blank))
}


analyze_rd <- function(path_file)
{
  tmp <- Rd_parse(path_file)$data
  info <- NULL
  for(i in 1:length(tmp[[1]]))
  {
    #maybe count number of characters instead of lines ...
    x <- strsplit(tmp[[1]][i],"\n",extended=F)
    size <- length(x[[1]])
    info <- c(info,size)
  }
  
  r <- as.list(info)
  
  return(list(info,tmp[[2]]))
}

