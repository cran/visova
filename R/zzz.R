
### File R/zzz.R
### Part of the R package visova

.onLoad<- function(lib, pkg,...)
{

  options(visova.plot.tag=NULL)
  options(visova.print.tag="html")

  ## Set possible visova.out.options
  ## Output from
  ## unique(unlist(strsplit(names(unlist( visovaTable(data.frame(x=1, y=1)))),".", fixed=TRUE)))

  visova.tags <- c("type",  "chartid", "html", "header", "chart", "jsHeader", "jsData", "jsDrawChart",
                 "jsDisplayChart", "jsFooter", "jsChart", "divChart", "caption", "footer")
  options(visova.tags=visova.tags)
  
  setMethod("toJSON", "Date",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.Date(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              
              tmp <- paste("new Date(",y,",",m,",",d,")",sep="")
              paste(tmp, collapse=", ")
            })
  
  setMethod("toJSON", "POSIXct",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.POSIXct(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              H <- as.numeric(format(dt,"%H"))
              M <- as.numeric(format(dt,"%M"))
              S <- as.numeric(format(dt,"%S"))
              
              tmp <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
              paste(tmp, collapse=", ")
            })
  
  setMethod("toJSON", "POSIXlt",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.POSIXlt(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              H <- as.numeric(format(dt,"%H"))
              M <- as.numeric(format(dt,"%M"))
              S <- as.numeric(format(dt,"%S"))
              
              tmp <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
              paste(tmp, collapse=", ")
            })
    
  invisible()
}

.onAttach <- function(lib, pkg,...){
  packageStartupMessage(visovaWelcomeMessage())
}
visovaWelcomeMessage <- function(){
  
  paste("\n",     
        "Welcome to VISOVA version 1.0", "\n",
        "\n",
        "Contact: <alivalabarbaros@gmail.com>\n",
        "\n",
        "To suppress the this message use:\n",
        "suppressPackageStartupMessages(library(visova))\n",  
          sep="")
}
