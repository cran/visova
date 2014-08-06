
### File R/visovaMethods.R
### Part of the R package visova
print.visova <- function(x, tag=NULL, file="", ...){

  if(is.null(tag))
    tag <- getOption("visova.print.tag")
  
  if(!tag %in% getOption("visova.tags"))
        stop(paste(tag,
        "is not a valid option. Set tag to NULL or one of the following:\n",
                   paste(getOption("visova.tags"), collapse=", ")))
      
  tag <- ifelse( tag %in% c("chartid", "type", "html"), tag, paste(".", tag, sep=""))

  output <- unlist(x)
  tag.names <- names(output)
  .id <- apply(t(tag), 2, function(y)
               grep(paste("\\", y, sep=""), tag.names)
               )
  cat(output[.id], file=file, ...)
  
}


isServerRunning <- function() {
  #tools:::httpdPort > 0L
  get("httpdPort", envir=environment(startDynamicHelp)) > 0
}

visova.httpd.handler <- function(path, query, ...) {
  path <- gsub("^/custom/visova/", "", path)
  f <- sprintf("%s%s%s",
               tempdir(),
               .Platform$file.sep,
                 path) 
  list(file=f,
       "content-type"="text/html",
       "status code"=200L)
}

plot.visova <- function(x, tag=NULL, ...){

  if(missing(tag)) ## Has tag being actively set?
    tag <- getOption("visova.plot.tag")

  if(is.null(tag) | !('visova' %in% class(x))){  ## Open browser window if tag is NULL
    if(!isServerRunning() ) {
      startDynamicHelp()
    }
    
    env <- get( ".httpd.handlers.env", asNamespace("tools"))
    env[["visova"]] <- visova.httpd.handler
    root.dir <- tempdir()
    
    ## Write the whole visualisation into a html file
    if('visova' %in% class(x)){
      ## Write the pure chart html code into a separate file
      chart.txt <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>%s</title>
  <meta http-equiv="content-type" content="text/html;charset=utf-8" />
  <style type="text/css">
    body {
          color: #444444;
          font-family: Arial,Helvetica,sans-serif;
          font-size: 75%%;
    }
    a {
          color: #4D87C7;
          text-decoration: none;
    }
  </style>
</head>
<body>
<p>
  HTML code of the VISOVA plot visualization.<br />
  </p>
<p><textarea rows="50" name="html" cols="80">
%s
</textarea>
</p>
</body>
</html>
'    
      chart.txt <- sprintf(chart.txt, x$chartid,gsub(">","&gt;",gsub("<","&lt;",
                                                                     paste(unlist(x$html$chart), collapse="\n"))))
      cat(chart.txt, file=file.path(root.dir, paste("Chart_", x$chartid, ".html", sep="")))      
      file <- file.path(root.dir, paste(x$chartid ,".html", sep=""))
    }else{## not a visova object
      basex <- basename(x)
      if(length(grep("htm", substr(basex, nchar(basex)-3,nchar(basex)))) < 1)
        warning("The file does not appear to be an html file.\n")
      file.copy(from=x, to=file.path(root.dir, basex),...)
      file <- file.path(root.dir, basex)
    }    
    print(x, file=file)    
    .url <- sprintf("http://127.0.0.1:%s/custom/visova/%s",
                    #tools:::httpdPort,
                    get("httpdPort", envir=environment(startDynamicHelp)),
                    basename(file))
    if(interactive()){
      browseURL(.url, ...)
    }else{ ## not interactive modus     
      browseURL(.url, browser='false',...)
    }
    invisible(file)
  }else{ ## givs.plot.tag not NULL
    if('visova' %in% class(x)){
      return(print(x, tag=tag))
    }else{
      return(print(x))
    }
  }  
}

