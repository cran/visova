
### File R/visovaCoreCharts.R
### Part of the R package visova

corrMatOrder <- function(data, order=c("ANOVA", "AOE", "FPC", "hclust", "alphabetic","original"),
        hclust.method = c("complete", "ward", "single", "average",
                        "mcquitty", "median", "centroid"), groupCol)
{
	if (order == "ANOVA"){
		ord = vector()
		for(i in 1:length(names(data))){
			
			if(names(data)[i] != groupCol){
			fm = as.formula(paste(names(data)[i],"~",groupCol))
			sum_aov = summary(aov(formula = fm, data = data))
			ord = append(ord,sum_aov[[1]]$"F\ value"[1])
			
			}
		}

		ord = rank(ord)
		
	}
	gr_index = which(colnames(data)==groupCol) 
	  
	g_c <- subset( data, select = gr_index )
  	
	data_n <- subset( data, select = -gr_index )
 	data_n <- data_n[complete.cases(data_n),]
	corr = as.matrix(cor(data_n))
	covv = as.matrix(cov(data_n))
        order <- match.arg(order)
        hclust.method <- match.arg(hclust.method)

        ## reorder the variables using the angular order of the eigenvectors
        if (order == "AOE") {
                x.eigen <- eigen(corr)$vectors[, 1:2]
                e1 <- x.eigen[, 1]
                e2 <- x.eigen[, 2]
                alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
                ord <- order(alpha)
        }

        ## reorder the variables using the first principal component
        if (order == "FPC") {
                x.eigen <- eigen(corr)$vectors[, 1:2]
                e1 <- x.eigen[, 1]
                ord <- order(e1)
        }

        ## reorder the variables in alphabet ordering
        if(order =="alphabetic"){
                ord <- sort(rownames(corr))
        }



        ## reorder the variables using hclhust
        if(order == "hclust"){
                ord <- order.dendrogram(as.dendrogram(hclust(as.dist(1-corr),
                method = hclust.method)))
        }

	if(order == "original"){
		ord <- c(1:dim(corr)[1])
	
	}	
  
        return(ord)
}



visovaParallelCord <- function(data, groupCol, order=c("ANOVA","AOE", "FPC", "hclust", "alphabetic","original"),hclust.method = c("complete", "ward", "single", "average","mcquitty", "median", "centroid"), colors = c("turquoise","red","lime","purple","orange","blue","yellow","magenta","gray","olive","green","teal","navy"))
{##, editor


#Correlation heuristic for column ordering
  col_ordering = corrMatOrder(data,order,hclust.method, groupCol) 
  if (order == "hclust"){n_order = paste("(",order, "-",hclust.method,")") }else{ n_order =paste("(",order,")")}
	
    data <- data[complete.cases(data),]
	gr_index = which(colnames(data)==groupCol) 
  	g_c <- subset( data, select = gr_index )
 	data <- subset( data, select = -gr_index ) 
	data <- subset(data, select=col_ordering)

  	data[, (dim(data)[2]+1)] <- g_c

	groups = subset(data, select=groupCol)
 	group_types = table(groups)
    group_names = names(group_types)

    
    #COLOR SELECTION CHECKING
    
    maincolors= c("turquoise","red","lime","purple","orange","blue","yellow","magenta","gray","olive","green","teal","navy")

    inside = FALSE
    inside2 = FALSE
    
    if(identical(maincolors,colors)){
        paste(cat("\nNO Color Selection Detected...\n"))
        paste(cat("DEFAULT Color Order:", maincolors,"\n"))
        inside2 = TRUE
    }
    
    if( length(colors) < length(group_names) ){
        
        paste(cat("\nMISSING Color Selection...\n"))
        paste(cat("DEFAULT Color Order:", maincolors,"\n"))
    }
    
    if( length(colors) >= length(group_names) ){
    
    for(i in 1:length(colors)){
    
        if(!is.element(colors[i], maincolors)){
           paste(cat("\nINVALID Color Selection...\n"))
           paste(cat("DEFAULT Color Order:", maincolors,"\n"))
           colors = maincolors
           inside = TRUE
           }
    }
    
        if(!inside && !inside2){
            paste(cat("\nColor Selection Detected...\n"))
            paste(cat("Color Order:", colors,"\n"))

        }
    
    }
    

    
    data2 = data.matrix(data)
	data = data.frame(data2) 


#Calculating group averages and appending them into the data as rows   
  groups = subset(data, select=groupCol)
  group_types = unique(groups)

  group_types = sort(as.vector(as.matrix(group_types)))
  groups = as.vector(as.matrix(groups))
  for (i in 1:length(group_types)){

	temp = subset(data, data[,groupCol] == group_types[i])
	if(i==1){

		df = colMeans(temp, na.rm = FALSE, dims = 1)
	}
	else{
        	df = rbind(df, colMeans(temp, na.rm = FALSE, dims = 1))
        }
  }

    data = rbind(data,df)

  xvar = ""
  yvar = ""
  options = ""
  chartid = "visovaParallelCoordinates"
  visovaCoreChart(data, groups, group_names, colors, groupCol, n_order, xvar, yvar, options, chartid, chart.type="parallelC")
}


visovaCoreChart <- function(data, groups, group_names, colors, groupCol, n_order,  xvar="", yvar="", options=list(), chartid, chart.type){
  
  dataName <- deparse(substitute(data))
  
  my.options <- list(visova=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(xvar=xvar, yvar=yvar,
                       allowed=c("string", "number", "date", "datetime"))
                     )
  
  
  checked.data <- visovaCheckCoreChartData(data)

  
  output <- visovaChart(type=chart.type, checked.data=checked.data, groups, group_names, colors, groupCol, n_order, options=my.options, chartid=chartid, package="corechart")
  
  return(output)
}


visovaCheckCoreChartData <- function(data){
  
  if(!is.data.frame(data)){
    stop("Error: data has to be a data.frame.")
  }

  if(!any(sapply(data, is.numeric))){
    stop("Error: Your data has to have at least one numerical column.")
  }
  
  return(data)
}

