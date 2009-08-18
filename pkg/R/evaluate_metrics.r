
evaluate_ext_files <- function(path_pkg)
{
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

  if (length(extensions)>0)
    table(extensions, useNA='no', dnn=NULL)
  else
    0
}

evaluate_rnw_files <- function(path_pkg)
{
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

evaluate_pdf_files <- function(path_pkg)
{
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

evaluate_data_files <- function(path_pkg)
{
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

evaluate_rd_files <- function(path_pkg)
{
  way <- file.path(path_pkg, 'man')
  rdfiles <- list.files(way,pattern="[.]Rd$", ignore.case=TRUE)
  rdinfo <- vector("list",length(rdfiles))

  for(i in 1:length(rdfiles))
  {
    rdinfo[i] <- eval_each_rd(file.path(way, rdfiles[i]), rdfiles[i])
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

  df <- data.frame(file=factor(rep(name, length(section))),
                   section=factor(section),lines,characters)

  return(list(df))
}


evaluate_rfiles <- function(path_pkg)
{
  way <- file.path(path_pkg,'R')
  rfiles <- list.files(way,pattern="[.]R$", ignore.case=TRUE)
  rinfo <- vector("list",length(rfiles))

  for(i in 1:length(rinfo))
  {
    rinfo[[i]] <- eval_each_rfile(file.path(way,rfiles[i]), rfiles[i])

  }
  return(rinfo)
}

eval_each_rfile <- function(path_file, name)
{
   content <- parse(path_file,n=-1)

  inter_comments_vec <- intra_comments_vec <- lines_vec <- blank_vec <- NULL
  characters_vec <- components_vec <- component_type_vec <- NULL

  if (length(content)<1)
      return(data.frame(file=factor(name),
                    component=factor(NA),
                    type=factor(NA),
                    lines=0,characters=0,lines.blank=0,
                    comments.inter=0,
                    comments.intra=0))
                    
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
    
    # count number of characters (excluding blank characters)
    line_tmp <- gsub("[[:blank:]]","",rfile[i],extended=F)
    characters_total <- characters_total + nchar(line_tmp)
  }
  
  inter_comments_vec <- c(inter_comments_vec, comments_total - sum(inter_comments_vec))
  intra_comments_vec <- c(intra_comments_vec, 0 )
  blank_vec <- c(blank_vec, blanks_total - sum(blank_vec) )
  characters_vec <- c(characters_vec, characters_total - sum(characters_vec) )
  lines_vec <- c(lines_vec, length(rfile) - sum(lines_vec) )
  components_vec <- c(components_vec,"file")
  component_type_vec <- c(component_type_vec,NA)

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
   # If not, then substitute every string (i.e "anything" or 'anything') by X in order
   # to avoid that a # between '' or "" be considered as a comment
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

hal <- function(ct)
{
  content <- ct

  ops <- NULL
  opfc <- NULL

  while(length(content)>0)
  {
    z <- as.list(content[[1]])

    if( is.null(content[[1]]) || is.symbol(content[[1]]) || is.character(content[[1]]) || is.logical(content[[1]]) || is.numeric(content[[1]]) || is.name(content[[1]]) )
    {
      if ( is.null(content[[1]]) )
      {
        ops <- c(ops,"NULL")
      }
      else if (is.logical(content[[1]]) && is.na(content[[1]]))
      {
        ops <- c(ops,"NA")
      }
      else if (is.character(content[[1]]) )
      {
        ops <- c(ops,content[[1]])
      }
      else
      {
        arith <- c("+","-","*","^","%%","%/%","/")
        compare <- c("==",">","<","!=","<=",">=")
        logic <- c("&","|")
        reserved_words <- c("if","else","repeat","while","function","for","in","next","break")
        extra <- c("||","&&","<-",":",";","->","return","=")

        operators_list <- c(arith,compare,logic,reserved_words,extra)

        if (any(as.character(content[[1]]) == operators_list))
        {
          ops <- c(ops,as.character(content[[1]]))
        }
        else if (all(as.character(content[[1]]) != c('{','}','(',')','[',']','[[',']]')))
        {
          opfc <- c(opfc,as.character(content[[1]]))
        }
      }

      content <- content[-1]
    }
    else if(is.list(z) && length(z) == 4 && as.character(z[[1]]) == "function" )
    {
        arguments <- attr(z[[2]],"names")
        arguments <- arguments[arguments != ""]

        langs <- NULL
        defaults <- NULL
        for (i in 1:length(z[[2]]))
        {
          if (is.language(z[[2]][[i]]))
          {
            if (is.null(z[[2]][[i]])==FALSE && as.character(z[[2]][[i]])[1]!="")
              langs<- c(z[[2]][[i]],langs)
          }
          else
          {
            if(is.null(z[[2]][[i]])==FALSE)
            {
              if (as.character(z[[2]][[i]])!="")
                defaults <- c(z[[2]][[i]],defaults)
            }
            else
            {
              defaults <- c("NULL",defaults)
            }
          }
        }

        arguments <- c(arguments,rep("=",times=(length(langs)+length(defaults))))

        content <- content[-1]
        content <- c(langs,arguments,defaults,z[[1]],z[[3]],content)

    }
    else if ((as.character(z[[1]]) == "if") && length(z)>3)
    {
        content <- content[-1]
        content <- c(z[[1]],z[[2]],z[[3]],"else",z[[4]],content)
    }
    else if ((as.character(z[[1]]) == "for") && length(z)>3)
    {
        content <- content[-1]
        content <- c(z[[1]],z[[2]],"in",z[[3]],z[[4]],content)
    }
    else
    {
        arguments <- attr(z,"names")
        arguments <- arguments[arguments != ""]
        arguments <- c(arguments,rep("=",times=length(arguments)))

        content <- content[-1]
        content <- c(z,arguments,content)

    }

  } # close while


  # This part of the code detects which strings are functions calling and therefore operators

  content <- deparse(ct)

  # substitute every symbol \\\" or \\\' by X
  # goal: remove every string from the code (comments had been already removed)
  for (i in 1:length(content))
  {
    content[i] <- gsub("[\\][\"]","",content[i],extended=F)
    content[i] <- gsub("[\\][\']","",content[i],extended=F)

    while (regexpr("[\"][^\"]\\{0,\\}[\"]", content[i] ,extended=F)[1] != -1)
    { content[i] <- sub("[\"][^\"]\\{0,\\}[\"]","",content[i],extended=F) }

    while (regexpr("[\'][^\']\\{0,\\}[\']", content[i] ,extended=F)[1] != -1)
    { content[i] <- sub("[\'][^\']\\{0,\\}[\']","",content[i],extended=F) }
  }

  opfctrue <- NULL

  for (j in 1:length(opfc))
  {
    for (i in 1:length(content))
    {
      expr <- paste(opfc[j],"[[:blank:]]\\{0,\\}[(]",sep="")
      if (regexpr(expr, content[i] ,extended=F)[1] != -1)
      {
        content[i] <- sub(expr,"",content[i],extended=F)
        opfctrue <- c(opfctrue,opfc[j])
        opfc <- opfc[-j]
      }
    }
   }

  # opfctrue: some operators
  # c(ops,opfc): operators and operands

  return(list("mix"=c(ops,opfc),"operators"=opfctrue))
}

get_operators <- function(ops,fcops)
{
  opfctrue <- fcops

  # operators
  arith <- c("+","-","*","^","%%","%/%","/")
  compare <- c("==",">","<","!=","<=",">=")
  logic <- c("&","|")
  reserved_words <- c("if","else","repeat","while","function","for","in","next","break")
  extra <- c("||","&&","<-",":",";","->","return","=")

  # other things are operands, including function callings and function declarations
  quantity_vec <- NULL
  operators_vec <- NULL

  operators_list <- c(arith,compare,logic,reserved_words,extra)

  for(i in 1:length(operators_list))
  {
    logicvec <- operators_list[i] == ops
    if (any(logicvec))
    {
      quantity <- logicvec[logicvec==TRUE]
      quantity_vec <- c(quantity_vec,length(quantity))
      operators_vec <- c(operators_vec,operators_list[i])
      ops <- ops[ops != operators_list[i]]
    }
  }

  while (length(opfctrue)>0)
  {
    logicvec <- opfctrue[1] == opfctrue
    quantity <- logicvec[logicvec==TRUE]
    quantity_vec <- c(quantity_vec,length(quantity))
    operators_vec <- c(operators_vec,opfctrue[1])
    opfctrue <- opfctrue[logicvec==FALSE]
  }

  list("quantity_vec"=quantity_vec,"operators_vec"=operators_vec,"operands"=ops)
}

get_operands <- function(operands)
{
  ops <- operands
  quantity_vec <- NULL
  operands_vec <- NULL

  while(length(ops)>0)
  {
    logicvec <- ops == ops[1]
    quantity <- logicvec[logicvec==TRUE]
    quantity_vec <- c(quantity_vec,length(quantity))
    operands_vec <- c(operands_vec,ops[1])
    ops <- ops[ops != ops[1]]

  }

  list("quantity_vec"=quantity_vec,"operands_vec"=operands_vec)
}


halstead <- function(rfile)
{
  content <- rfile
  ops <- hal(content)

  values <- get_operators(ops$mix,ops$operators)

  #  N1 = total number of operators
  N1 <- sum(values$quantity_vec)
  # n1 = number of distinct operators
  n1 <- length(values$operators_vec)

  val <- get_operands(values$operands)

  #  N2 = total number of operands
  N2 <- sum(val$quantity_vec)
  # n2 = number of distinct operands
  n2 <- length(val$operands_vec)

  n <- n1 + n2 # vocabulary
  H <- n1*log(n1) + n2*log(n2) # length
  N <- N1 + N2 # real Length
  V <- N*log(n) # volume
  D <- (n1*N2)/(2*n2) # level of difficulty
  E <- V*D # Effort
  T <- E/18 # time of implementation in seconds
  B <- V/3000 # Estimated number of bugs

  list("operators"=values$operators_vec,"operators.quantities"=values$quantity_vec,
       "operands"=val$operands_vec,"operands.quantities"=val$quantity_vec,
       "N1"=N1,"n1"=n1,"N2"=N2,"n2"=n2,"n"=n,"H"=H,"N"=N,"V"=V,"D"=D,"E"=E,"T"=T,"B"=B)
  
  #data.frame("N1"=N1,"n1"=n1,"N2"=N2,"n2"=n2,"n"=n,"H"=H,"V"=V,"D"=D,"E"=E,"T"=T,"B"=B)
}

evaluate_halstead <- function(pkgpath)
{
  way <- file.path(pkgpath,'R')
  rfiles <- list.files(way,pattern="[.]R$", ignore.case=TRUE)

  info <- vector("list",length(rfiles))

  for(i in 1:length(rfiles))
  {
    info[[i]] <- halstead(parse(file.path(way,rfiles[i])))
  }

  return(info)
}
