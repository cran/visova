### Author: Ali Vala Barbaros
### File R/visova.R
### Part of the R package visova


visovaChart <- function(type, checked.data, groups, groupCol, order, options, chartid, package, formats = NULL){
  
  Chart = visova(type=type, checked.data, groups, groupCol, order, options=options, chartid=chartid, package, formats=formats)
  chartid <- Chart$chartid
  htmlChart <- Chart$chart
  
  htmlScaffold <- visovaHtmlWrapper(title="", chartid=chartid, dataName=options$dataName,
                                  type=tolower(type))
  
  output <- structure(
    list(type=Chart$type,
         chartid=Chart$chartid,
         html=list(header=htmlScaffold[["htmlHeader"]],
                   chart=unlist(htmlChart),
                   caption=htmlScaffold[["htmlCaption"]],
                   footer=htmlScaffold[["htmlFooter"]])),
    class=c("visova", "list")
  )
  
  return(output)
}

stderr <- function(x) ifelse( length(x)>1, sd(x)/sqrt(length(x)), 0 )

visova <- function(type="", data, groups, groupCol, order,  options, chartid, package, formats=NULL){

  group_types = unique(groups)
  group_types = sort(as.vector(as.matrix(group_types)))
  group_types2 = group_types
  groups = paste(groups,',', collapse='')
  groups = paste('[',substr(groups, 1, nchar(groups)-1),']')
  group_types = paste(group_types,',', collapse='')
  group_types = paste('[',substr(group_types, 1, nchar(group_types)-1),']')
  all_stderr = vector()
#  ndata = as.matrix(data[1:(dim(data)[1]-length(group_types)),])
  
  for(col in 1:(dim(data)[2]-1)){
  	for (i in 1:length(group_types2)){
		temp = subset(data, data[,groupCol] == group_types2[i])
		temp = as.matrix(temp)
		all_stderr = append(all_stderr, stderr(temp[,col])) 		
  	}
  }
 # all_stderr = apply(ndata,2,stderr) 
  all_stderr = paste(round(all_stderr,3),',', collapse='')
  all_stderr = paste('[',substr(all_stderr, 1, nchar(all_stderr)-1),']')
  data[,groupCol] = NULL

  if( ! is.data.frame(data) ){
    stop("Data has to be a data.frame. See ?data.frame for more details.")
  }
  
  ## we need a unique chart id to have more than one chart on the same page
  ## we use type and date to create the chart id
  if(missing(chartid)){
    ##    chartid <- paste(type, format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), basename(tempfile(pattern="")),sep="_")
    chartid <- paste(type, basename(tempfile(pattern="")),sep="ID")
    
  }
  if(missing(package)){
    package <- type
  }
  
  output <- visovaFormat(data)
  data.type <- output$data.type
  data.json <- output$json
  
  ## check for not allowed data types
  checkTypes <- data.type %in% options$data$allowed 
  if(sum(!checkTypes)){
    message <- paste("Only the following data types are allowed: ", 
                     paste(options$data$allowed, collapse=", "), "\n",
                     "However, ", 
                     paste(names(data)[!checkTypes], collapse=", "), " is of type ", 
                     paste(data.type[!checkTypes], collapse=", "),"\n", sep="", collapse=" ")
    stop(message)
  }  
  
  ## check for NumberFormat objects
  ## formats will be a named list of strings. Names correspond to objects in the data frame.
  jsFormats <- ''
  if (!is.null(formats)) {
    if (!inherits(formats,'list'))
      warning('formats object exists but is not a list. Ignoring')
    
    if (!all(names(formats) %in% names(data))) 
      warning('formats object contains names that do not exist in data.')
    
    for (idx in c(1:length(formats))) {
      jsFormats <- paste(jsFormats
                         , paste('  var dataFormat',idx,' = new google.visualization.NumberFormat({pattern:"',formats[idx],'"});',sep="")
                         , paste('  dataFormat',idx,'.format(data, ', match(names(formats[idx]), names(data)) - 1,');',sep="")
                         , sep = "\n"
      )
    }
    
  }
  
  jsHeader1 <- '
<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>
<script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/qtip2/2.1.0/jquery.qtip.min.js"></script>
<script type="text/javascript">
//visova.js
// Declare a unique namespace.
var vizObj = {};
var strArr = new Array(0);
var x_coords = new Array();
var y_coords = new Array();
var line_nu = 0; 
var group_nu = 0;
var colors = ["#2ABDFF","#FA4900","#8AFF00","#B652DB","#F47D24","#4265E9"];
var alpha_colors = ["#C2ECFF","#FABDA2","#D3FFA1","#CA9ADB","#F5C098","#9EAEE8"];
	

// Class constructor. Parameter container is a DOM elementon the client that
// that will contain the visualization.
vizObj.ParallelC = function(container) {
  this.containerElement = container;
}

function drawAxes(ctx,data,canvasWidth,canvasHeight) {
	var l_n = 0;
	for (var col = 50; col < ((canvasWidth-50)); col = col + ((canvasWidth-50)/data.getNumberOfColumns())) 
	{
		ctx.strokeStyle = "#BDBDBD";
		ctx.beginPath();
		ctx.moveTo(col,30);
		ctx.lineTo(col,canvasHeight - 30);	
		ctx.closePath();
		ctx.stroke();
		l_n++;
	}
	line_nu = l_n;
}

function setMaxMin(ctx,data,canvasWidth,canvasHeight) {
	var x = 55;
	for (var col = 0; col < data.getNumberOfColumns(); col++) 
	{
		var colMax = 0;
		var colMin = 0;

		if (data.getColumnType(col) == "number")
		{
			colMax = data.getColumnRange(col).max;
			colMin = data.getColumnRange(col).min;
		}
		else
		{
			colMax = data.getDistinctValues(col).length-1;
			if (colMax == colMin)
			{
				colMin = -1;
			}
			else
			{
				colMin = 0;
			}
		}
			ctx.StrokeStyle = "#6E6E6E";

			ctx.beginPath();
			ctx.moveTo(x-10,50);
			ctx.lineTo(x,50);	
			ctx.closePath();
			ctx.stroke();
			
			try
			{
				ctx.font = "10px Verdana";
				if (data.getColumnType(col) == "number")
				{
					ctx.fillText(colMax,x,50);
				}
				else
				{
					//ctx.fillText(data.getDistinctValues(col)[data.getDistinctValues(col).length-1],x,50);
					ctx.fillText(data.getDistinctValues(col)[colMax],x,50);
				}
			}
			catch(e)
			{
				ctx.mozTextStyle = "10px Verdana";
				ctx.fillStyle = "black";
				ctx.translate(x,50);
				if (data.getColumnType(col) == "number")
				{
					ctx.mozDrawText(colMax);
				}
				else
				{
					//ctx.mozDrawText(data.getDistinctValues(col)[data.getDistinctValues(col).length-1]);
					ctx.mozDrawText(data.getDistinctValues(col)[colMax]);
				}
				ctx.translate(-x,-50);
			}
			
			if (colMin != -1 && (colMax != colMin))
			{
			//label for max
			ctx.beginPath();
			ctx.moveTo(x-10,canvasHeight-50);
			ctx.lineTo(x,canvasHeight-50);	
			ctx.closePath();
			ctx.stroke();
			
			try
			{
				ctx.font = "10px Verdana";
				if (data.getColumnType(col) == "number")
				{
					ctx.fillText(colMin,x,canvasHeight-45);
				}
				else
				{
					
					ctx.fillText(data.getDistinctValues(col)[0],x,canvasHeight-45);	
				}
			}
			catch(e)
			{
				ctx.mozTextStyle = "10px Verdana";
				ctx.fillStyle = "black";
				ctx.translate(x,canvasHeight-45);
				if (data.getColumnType(col) == "number")
				{
					ctx.mozDrawText(colMin);
				}
				else
				{
					//ctx.mozDrawText(data.getDistinctValues(col)[0]);
					ctx.mozDrawText(data.getDistinctValues(col)[colMin]);
				}
				ctx.translate(-x,-canvasHeight+45);
			}
			}
			if (data.getColumnType(col) != "number")
			{
				var y = 50;
				for (var i = 1; i < (data.getDistinctValues(col).length - 1) ; i++)
				{
					
					y = y + (((canvasHeight-100)/(data.getDistinctValues(col).length-1)));
					ctx.StrokeStyle = "#6E6E6E";
					ctx.beginPath();
					ctx.moveTo(x-10,y);
					ctx.lineTo(x,y);	
					ctx.closePath();
					ctx.stroke();
					
					try
					{
						ctx.font = "10px Verdana";
						if (data.getColumnType(col) == "number")
						{
							ctx.fillText(colMax,x,y);
						}
						else
						{
							ctx.fillText(data.getDistinctValues(col)[data.getDistinctValues(col).length-i-1],x,y);
						}
					}
					catch(e)
					{
						ctx.mozTextStyle = "10px Verdana";
						ctx.fillStyle = "black";
						ctx.translate(x,y);
						if (data.getColumnType(col) == "number")
						{
							ctx.mozDrawText(colMax);
						}
						else
						{
							ctx.mozDrawText(data.getDistinctValues(col)[data.getDistinctValues(col).length-i-1]);
						}
						ctx.translate(-x,-y);
					}

					//
				}
			}
			
		x= x + ((canvasWidth-50)/data.getNumberOfColumns());
	}

}



function normalizeTable(data) 
{
	for (var col = 0; col < data.getNumberOfColumns(); col++) 
	{
		if (data.getColumnType(col) == "number")
		{
			var step = data.getColumnRange(col);
			for (var row = 0; row < data.getNumberOfRows(); row++) 
			{
				data.setCell(row,col,data.getValue(row,col)-(step.min));		
			}
		}
	}
}

'
jsHeader2 <- 
'
var mouseX = 999, mouseY = 999;
 
function plotGraph(ctx,data,options,canvasHeight,canvasWidth) 
{


	var groups = %s;
	var group_types = %s;
	var all_stderr = %s;
	index2 = 0;
	group_nu = group_types.length;

	
	for (var row2 = 0; row2 < data.getNumberOfRows(); row2++) 
	{
		if(row2 < (data.getNumberOfRows() - group_types.length)){
			for(var index=0; index < group_types.length; index++){
				if (groups[row2] == group_types[index]){
					ctx.strokeStyle = alpha_colors[index];
					break;
				}
			}
		}
		else{
			ctx.strokeStyle = colors[index2];
			ctx.lineWidth = 3;
			index2++;
		}
	ctx.moveTo(50,50);
	var x = 50;
		for (var col = 1; col < data.getNumberOfColumns(); col++) 
		{
			
			var step = data.getColumnRange(col-1);			
			
			ctx.beginPath();		
			if (data.getColumnType(col-1) != "number")
			{
				var val = findDistinctColVal(data,row2,col-1);
				if ((data.getDistinctValues(col-1).length-1) < 2)
				{
					ctx.moveTo(x,50);
				}
				else
				{
					ctx.moveTo(x,(canvasHeight)-(((val)/((data.getDistinctValues(col-1).length-1)/(canvasHeight-100)))+50));
				}
			}
			else
			{
				if (step.max == 0)
				{
					ctx.moveTo(x,50);
					if(col==1){
						x_coords.push(x);
						y_coords.push(50);
					}
				}
				else
				{
					var temp_y =(canvasHeight)-(((data.getValue(row2, col-1))/((step.max)/(canvasHeight-100)))+50); 
					ctx.moveTo(x,temp_y);
					if(col==1){
						x_coords.push(x);
						y_coords.push(temp_y);
					}	
				}
			}
			
			x= x + ((canvasWidth-50)/data.getNumberOfColumns());
			
			
			step = data.getColumnRange(col);
			if (data.getColumnType(col) != "number")
			{
				var val = findDistinctColVal(data,row2,col);
				if ((data.getDistinctValues(col).length-1) < 2)
				{
					ctx.lineTo(x,50);				
				}
				else
				{
					ctx.lineTo(x,(canvasHeight)-(((val)/((data.getDistinctValues(col).length-1)/(canvasHeight-100)))+50));				
				}
			}
			else
			{
				if (step.max == 0)
				{
					ctx.lineTo(x,50);
					x_coords.push(x);
					y_coords.push(50);	
				}
				else
				{
					var temp_y = (canvasHeight)-(((data.getValue(row2, col))/((step.max)/(canvasHeight-100)))+50);
					ctx.lineTo(x,temp_y);
					x_coords.push(x);
					y_coords.push(temp_y);	
				}
			}
			
			ctx.closePath();
        		ctx.lineJoin = "round";
			ctx.stroke();
		}

	}
//drawing stderr
var x = 50;
findex = 0;
        for (var col = 0; col < data.getNumberOfColumns(); col++){
		var step = data.getColumnRange(col); 
		for (var index = 0; index < group_types.length; index++){
			upper = data.getValue(data.getNumberOfRows()-group_types.length+index,col)+all_stderr[findex];
			lower = data.getValue(data.getNumberOfRows()-group_types.length+index,col)-all_stderr[findex]; 	
			console.log("all_stderr:",findex,all_stderr[findex]); 
			ctx.strokeStyle = colors[index];
                        ctx.lineWidth = 1.9;
		 	if (data.getColumnType(col) == "number"){
				//console.log(x)
				ctx.beginPath();
				ctx.moveTo(x,(canvasHeight)-((upper/((step.max)/(canvasHeight-100)))+50));					
				ctx.lineTo(x,(canvasHeight)-((lower/((step.max)/(canvasHeight-100)))+50));
        			ctx.moveTo(x-10,(canvasHeight)-((upper/((step.max)/(canvasHeight-100)))+50));
				ctx.lineTo(x+10,(canvasHeight)-((upper/((step.max)/(canvasHeight-100)))+50));
				ctx.moveTo(x-10,(canvasHeight)-((lower/((step.max)/(canvasHeight-100)))+50));
                                ctx.lineTo(x+10,(canvasHeight)-((lower/((step.max)/(canvasHeight-100)))+50));

	 			ctx.closePath();
                		ctx.stroke();
			}
			findex++;
        	}
	x= x + ((canvasWidth-50)/data.getNumberOfColumns());
	}

return(all_stderr);

}


'
jsHeader2 <- sprintf(jsHeader2, groups, group_types, all_stderr)
jsHeader3 <-
'
function findDistinctColVal(data,row,col)
{
	for (var k = 0; k < data.getDistinctValues(col).length; k++)
	{
		if (data.getDistinctValues(col)[k] == data.getValue(row, col))
		{
			return k
		}
	}
}
function writeLabels(ctx,data,canvasHeight,canvasWidth) 
{
x = 30;
	for (var col = 0; col < data.getNumberOfColumns(); col++) 
	{
		
		ctx.fillStyle = "#6E6E6E";
		try
		{
			ctx.font = "10px Verdana";
			ctx.fillText(data.getColumnLabel(col),x,canvasHeight-5);
		}
		catch(e)
		{
			ctx.mozTextStyle = "10px Verdana";
			ctx.fillStyle = "#6E6E6E";
			ctx.translate(x,canvasHeight-5);
			ctx.mozDrawText(data.getColumnLabel(col));
			ctx.translate(-x,-canvasHeight+5);
		}
		x= x + ((canvasWidth-50)/data.getNumberOfColumns());
	}
}
 
function writeTitle(ctx,options,canvasHeight,canvasWidth)
{
		ctx.fillStyle = "black";
		try
		{
			ctx.font = "16px Verdana bold";
			ctx.fillText(options.title,(canvasWidth/2)-((options.title.length)/2)-250,20);
		}
		catch(e)
		{
			ctx.mozTextStyle = "12px Verdana";
			ctx.fillStyle = "black";
			ctx.translate((canvasWidth/2)-((options.title.length)/2)-100,20);
			ctx.mozDrawText(options.title);
			ctx.translate(-(canvasWidth/2)+((options.title.length)/2)+100,-20);
			
		}
}

// Main drawing logic.
vizObj.ParallelC.prototype.draw = function(initial_data, data, options) {
	
	this.containerElement.innerHTML = \'<canvas id="graph_parallelC" style="position: absolute; left: 0; top: 0;"></canvas>\';
	//canvas properties
	
	if (options.width == undefined)
	{
		canvasWidth = 500;
	}
	else
	{
		canvasWidth = options.width;
	}
	if (options.height == undefined)
	{
		canvasHeight = 500;
	}
	else
	{
		canvasHeight = options.height;
	}
	
	document.getElementById("graph_parallelC").setAttribute("width",canvasWidth);
	document.getElementById("graph_parallelC").setAttribute("height",canvasHeight);
	
	var canvas = document.getElementById("graph_parallelC");
 
 
if (canvas.getContext)
  {
	//var strArr = new Array(1);
	var ctx = canvas.getContext("2d");
	ctx.lineWidth=1;
	ctx.lineCap = "round";
	
	//drawing axes
	drawAxes(ctx,data,canvasWidth,canvasHeight);
	
	
	//set max & min points on each axes
	setMaxMin(ctx,data,canvasWidth,canvasHeight);

	
	
	//normalizing data
	normalizeTable(data);
	
	
	//plotting data
	all_stderr = plotGraph(ctx,data,options,canvasHeight,canvasWidth);


	//adding labels
	writeLabels(ctx,data,canvasHeight,canvasWidth);

	//adding chart title
	writeTitle(ctx,options,canvasHeight,canvasWidth);
	
	createsvgs(initial_data, x_coords, y_coords, line_nu, group_nu, all_stderr);
	
  }
  else
  {
	this.containerElement.innerHTML = \'<span>This feature is not compatible with the brower you are currently using.</span>\';
  }
  
  
}

// Utility function to escape HTML special characters
vizObj.ParallelC.prototype.escapeHtml = function(text) {
  if (text == null)
    return \'\';
}
'
  jsHeader  <- paste(jsHeader1, jsHeader2, jsHeader3) 
  jsHeader  <- paste(infoString(type),   jsHeader , sep="\n")
  
  jsData <- '
// jsData 
google.load("visualization", "1");
// Set callback to run when API is loaded
google.setOnLoadCallback(drawVisualization);  
function visovaData%s () {
var data = new google.visualization.DataTable();
var datajson =
%s;
%s
data.addRows(datajson);
return(data);
}
'
jsData <- sprintf(jsData, chartid,
                    data.json,
                    paste(paste("data.addColumn('", data.type, "','",
                                names(data.type), "');", sep=""), collapse="\n"))
  
  jsDisplayChart <- '
// jsDisplayChart
'
  jsDrawChart <- '
// jsDrawChart
function drawVisualization(){
var data = visovaData%s();
var initial_data = visovaData%s();
var vis = new vizObj.ParallelC(document.getElementById("%s"));
vis.draw(initial_data, data, {width:window.innerWidth, height:window.innerHeight-100, title: \'VISOVA Coordinates Plot with Group Averages -- Column Ordering Method=%s\', lineColor:"#FE2E2E"});

}

function createsvgs(initial_data, x_coords, ycoords, line_nu, group_nu, all_stderr){

$(document).ready(function () {

s_index = x_coords.length -(line_nu * group_nu);
e_index = x_coords.length;

ini_HTML = "<svg id=\'bbb\' style= \'position: absolute; left: 0; top:0 ; fill:gray;stroke:black;stroke-width:4;opacity:0.5\'><rect x=\'20\' y=\'1\' rx=\'10\' ry=\'10\' width=\'1190\' height=\'30\'/></svg><svg id=\'aaa\' style= \'position: absolute; left: 0; top:0 ; fill:gray;stroke:black;stroke-width:2;opacity:0.4\'><rect x=\'20\' y=\'650\' rx=\'10\' ry=\'10\' width=\'1190\' height=\'30\'/></svg>"; 

var dist = 1190/group_nu;
l_add = "<svg style= \'position: absolute; left: 0; top:0;\'>";
var dist2 =50;
for(var indexl=0; indexl<group_nu; indexl++){
	addll = "<rect x=" + dist2 + " y=\'658\' width=\'12\' height=\'12\'style=\'fill:"+colors[indexl]+";stroke:black;stroke-width:1;stroke-opacity:0.9\'/><text x="+(dist2+20)+" y=\'669\' fill=\'black\'>GROUP-"+(indexl+1)+"</text>";
	l_add = l_add + addll;
	dist2 = dist2+dist;
}

ini_HTML = ini_HTML + l_add + "</svg>";

initialHTML = "<svg id=\'aaa\' style= \'position: absolute; left: 0; top: 0;\'><defs><filter id=\'f1\' x=\'0\' y=\'0\'><feGaussianBlur in=\'SourceGraphic\' stdDeviation=\'15\' /></filter><style type=\'text/css\'><![CDATA[";

add="";

footerHTML = "</svg>";
progHTML = "";
var ids = new Array();
var inner = 0;


var tchs = new Array();

for(var index0 = 0; index0<group_nu; index0++ ){
	tch = "touchable"+index0; 
	tchs.push(tch);
	add0 = "."+tch+"{ fill: transparent; stroke: transparent; stroke-width: 0.05cm; }."+tch+":hover { fill:" + colors[index0] +"; stroke:white; stroke-width: 0.04cm; }"

	if(add==""){
		add = add0;
	}else{
		add = add + add0;
	}
		
}

initialHTML= ini_HTML + initialHTML+add+"]]></style></defs>";

	
	for (var index = s_index; index < e_index; index++){
		circle_id = "crc" + index;
		ids.push(circle_id);

		middleHTML = "<circle id=" + circle_id + " class=\'"+tchs[Math.floor((index-s_index)/line_nu)] +"\' cx=\'" + x_coords[index] + "\' cy=\'" + y_coords[index] +"\' r=\'5\' />";
		if (progHTML == ""){
			progHTML = middleHTML;
		}else{
		        progHTML = progHTML + middleHTML; 	
		     }
 	}
	
final = initialHTML + progHTML + footerHTML;
$("body").append(final);

var adj_stderr = new Array();
for(var j=0; j<group_nu; j++){
	var index = j;
	for (var i = 0; i<line_nu; i++){
		if(i==0){adj_stderr.push(all_stderr[j]);}
		else{ 
			index = index + group_nu;
			adj_stderr.push(all_stderr[index]);
		    }
	}
}

//CREATING TOOLTIPS TEXTS
var innertext = new Array();
g_l = 0;
for (var row = initial_data.getNumberOfRows()-group_nu ; row < initial_data.getNumberOfRows() ; row++){
	g_l++;
	for(var col = 0; col<line_nu ; col++){
		temp = "Group - "+g_l+"<br/>"+ initial_data.getColumnLabel(col) +" Mean: " +Math.round(initial_data.getValue(row,col)*100)/100;		 
		innertext.push(temp);	
	}
}

//CREATING TOOLTIPS
	for (var index = 0; index < ids.length; index++){
		$("#"+ids[index]).qtip({
    			content: {
        		text: innertext[index] + "<br/>Standard Error: "+adj_stderr[index]
			},
			show: {
            			effect: function() {
                		$(this).slideDown();
            			}
        		},
        		hide: {
            			effect: function() {
                		$(this).slideUp();
            			}
        		},
			style: {
				classes: \'qtip-dark qtip-shadow qtip-rounded \' 		
   			}

	
		});
	}

});
}
'
  jsDrawChart <- sprintf(jsDrawChart, chartid, chartid, chartid, order, chartid)
  
  jsFooter  <- '
// jsFooter
</script>
'
  
  jsChart <- '
<!-- jsChart -->  
'
#  jsChart  <- sprintf(jsChart, chartid)
  
  
  divChart <- '
<!-- divChart -->
%s  
<div id="%s"
  style="width: %spx; height: %spx;">
</div>
'

  divChart <- sprintf(divChart,
                      ifelse(!is.null(options$visova$visova.editor),
                             sprintf("<input type='button' onclick='openEditor%s()' value='%s'/>",
                                     chartid,as.character(options$visova$visova.editor)),''),
                      chartid,
                      ifelse(!(is.null(options$visova$width) || (options$visova$width == "")),options$visova$width,1200),
                      ifelse(!(is.null(options$visova$height) || (options$visova$height == "")),options$visova$height,600)
  )
  
  output <- list(chart=list(jsHeader=jsHeader,
                            jsData=jsData,
                            jsDrawChart=jsDrawChart,
                            jsDisplayChart=jsDisplayChart,
                            jsFooter=jsFooter,
                            jsChart=jsChart,
                            divChart=divChart),
                 type=type, chartid=chartid)
  return(output)
}



###############

toJSONarray <- function(dtf){
  ## Thanks to Sebastian Kranz for this function
  ## Thanks also to Wei Luo: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis
  
  #restore.point("toJSONarray")
  clnms <- colnames(dtf)
  
  # Transforms a vector into a vector of JSON strings that
  # can be pasted together afterwards
  toJSONvec <- function(vec) {
    #restore.point("name.value")
    na.row <- is.na(vec)
    if(is(vec,'integer')){
      ret <- vec
    } else if (is(vec,'numeric')) {
      # Round to 10 points after the decimal as before
      ret <- as.character(signif(vec,digits=10))
    } else if (is(vec,'logical')) {
      ret <- tolower(as.character(vec))
    } else if (is(vec,'Date')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      ret <- paste("new Date(",y,",",m,",",d,")",sep="")
    } else if (is(vec,'POSIXct') | is(vec,'POSIXlt')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      H <- as.numeric(format(vec,"%H"))
      M <- as.numeric(format(vec,"%M"))
      S <- as.numeric(format(vec,"%S"))
      ret <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
    } else {
      quote <- '"';
      vec <- gsub('"', '\\\\"', vec)
      ret <- paste(quote, vec, quote, sep='')
    }
    ret[na.row] <- "null"
    ret
  }
  # Transform columns depending on data type and store in a list
  objs <- lapply(dtf,toJSONvec)
  # Remove names just for the case that one column name was sep or collapse
  names(objs) <- NULL
  # Paste columns together
  str <- do.call(paste,c(objs,list(sep=",\n")))
  
  # Add [ ] and paste rows together
  res <- paste('[\n ', paste("[\n",str,"\n]",collapse=',\n'), ' \n]',sep="")
  return(res)
}


visovaFormat <- function(data){
  #restore.point("visovaFormat")
  ## Create a list where the Google DataTable type of all variables will be stored
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  varTypes <- sapply(varNames,
                     function(.x){
                       switch(class(x[[.x]])[1],"integer"="number",
                              "numeric"="number",
                              "character"="string",
                              "factor"="string",
                              "logical"="boolean",
                              "Date"="date",
                              "POSIXct"="datetime",
                              "POSIXlt"="datetime")
                     }
  )
  
  ## factor to character
  x.df <- as.data.frame(
    lapply(x,
           function(a){
             if (is.factor(a)) as.character(a) else a
           }
    ),
    stringsAsFactors=FALSE
  )
  
  ## The function is specified above
  json <- toJSONarray(x.df)
  output <- list(
    data.type = unlist(varTypes),
    json = json
  )
  
  output$json <-fixBackslash(output$json)
  
  return(output)
}

fixBackslash <- function(x){
  x <-  gsub("\\\\\\\\", "\\\\", x)
  return(x)
}


check.location <- function(x){
  y = as.character(x)
  if (! is.character(y))
    stop(paste("The column has to be of character format. Currently it is", class(x)))
  y
}

check.char <- function(x){
  y = as.character(x)
  if (! is.character(y))
    stop(paste("The column has to be of character format. Currently it is", class(x)))
  y
}

check.date <- function(x){
  y = as.Date(x)
  if (class(y)!="Date")
    stop(paste("The column has to be of date format. Currently it is", class(x)))
  y
}

check.datetime <- function(x){
  if(! any(class(x) %in% c("POSIXct", "POSIXlt")) )
    stop(paste("The column has to be of datetime format (POSIXct or POSIXlt). Currently it is", class(x)))
  x
}

check.num <- function(x){
  y = as.numeric(x)
  if (! is.numeric(y))
    stop(paste("The column has to be of numeric format. Currently it is", class(x)))
  y
}

check.num.pos <- function(x){
  y = as.numeric(x)
  if (! is.numeric(y))
    stop(paste("The column has to be of numeric format. Currently it is", class(x)))
  if (any(x<0))
    stop(paste("The column has to be > 0."))       
  y
}

    
checkSquareCurlBracketOps <- function(char){
  
  first <- substr(char, 1,1)
  n <- nchar(char)
  last <- substr(char, n, n)
  ifelse(first %in% c("{","[") & last %in% c("}", "]"), TRUE, FALSE)
}

visovaHtmlWrapper <- function(title, dataName, chartid, type){
  
  htmlHeader <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>%s</title>
<meta http-equiv="content-type" content="text/html;charset=utf-8" />
<link type="text/css" rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/qtip2/2.1.0/jquery.qtip.min.css" />
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
.qtip-content {
    font-size:12px;
    font-weight: bold;
}

</style>
</head>
<body>
'
  htmlHeader <- sprintf(htmlHeader,chartid) 
  
  htmlFooter <- '
</body>
<!-- htmlFooter -->

<span>
<br/><br/><br/>
<br/><br/><br/>
// VISOVA package version 1.0<br/>
//  %s &#8226;
</span>
</div>
</html>
'
   
  htmlFooter <- sprintf(htmlFooter, R.Version()$version.string)  
  htmlCaption <- ""
#  htmlCaption <- sprintf('<div><span>Data: %s &#8226; VISOVA Chart ID: <a href="Chart_%s.html">%s</a></span><br />' ,
#                         dataName, chartid, chartid)
  
  return(list(htmlHeader=htmlHeader,
              htmlFooter=htmlFooter,
              htmlCaption=htmlCaption
  ))
}


## taken from lattice by Deepayan Sarkar
modifyList <- function (x, val) {
 # stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      modifyList(x[[v]], val[[v]])
    else val[[v]]
  }
  x
}

## info string 

infoString <- function(type=""){
  result <- string("",file="",append=FALSE)
  info <- R.Version()
  BCOMMENT <- "<!-- "
  ECOMMENT <- " -->\n"
  
  result <- result + BCOMMENT + type + " generated in " +
    info$language + " " + info$major + "." + info$minor + 
    " by VISOVA " 
  result <- result + BCOMMENT + date() + ECOMMENT
  result$text
}

## define class string with nice '+' paste, taken from xtable.

"+.string" <- function(x,y) {
  x$text <- paste(x$text,as.string(y)$text,sep="")
  return(x)
}

print.string <- function(x,...) {
  cat(x$text,file=x$file,append=x$append)
  return(invisible())
}

string <- function(text,file="",append=FALSE) {
  x <- list(text=text,file=file,append=append)
  class(x) <- "string"
  return(x)
}

as.string <- function(x,file="",append=FALSE) {
  if (is.null(attr(x,"class",exact=TRUE)))
    switch(data.class(x),
           character=return(string(x,file,append)),
           numeric=return(string(as.character(x),file,append)),
           stop("Cannot coerse argument to a string"))
  if (class(x)=="string")
    return(x)
  stop("Cannot coerse argument to a string")
}

is.string <- function(x) {
  return(class(x)=="string")
}
