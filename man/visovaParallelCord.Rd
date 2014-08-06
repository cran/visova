\name{visovaParallelCord}
\alias{visovaParallelCord}
\title{
 A visualization of Analysis of Variance (VISOVA) with Parallel Coordinates Plot
}
\description{
  The visovaParallelCord function reads a data.frame and plot the Parallel Coordinate
  Plot of VISOVA.
}
\usage{
visovaParallelCord(data, groupCol, order, hclust.method)
}

\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a parallel coordinates plot
  }
  \item{groupCol}{
    name of the column which contains the category levels of data
  }
 \item{order}{
Character, the ordering method of the columns/variables in the parallel coordinates plot..
        \itemize{
                \item \code{"original"} for original order (default).
                \item \code{"ANOVA"} for the column order that uses group-wise ANOVA results.
                \item \code{"AOE"} for the angular order of the eigenvectors.
                \item \code{"FPC"} for the first principal component order.
                \item \code{"hclust"} for the hierarchical clustering order.
                \item \code{"alphabet"} for alphabetical order.
		Look at the reference paper for further details.
        }

}

  \item{hclust.method}{
Character, the agglomeration method to be used when \code{order} is \code{\link{hclust}}. This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.

Look at the reference paper for further details.
}
}
\references{
VISOVA: an integration of Visualization and ANOVA, The UCLA VISOVA Elite(2014)
}
\author{
  Ali Vala Barbaros \email{alivalabarbaros@gmail.com}
}
\section{Warning}{
  All of the charts are rendered by the browser and some of the libraries like "google jsapi", "qtip2", "jquery" need to be imported , therefore, a modern browser with an Internet connection is required to display charts.
  Data frame should not include any character values other than group column which has to be indicated when this function is called.
}
\examples{
plot(visovaParallelCord(auto[,2:14],"Origin",order="AOE"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="ANOVA"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="original"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="FPC"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="alphabetic"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="complete"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="average"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="median"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="ward"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="single"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="mcquitty"))
plot(visovaParallelCord(auto[,2:14],"Origin",order="hclust", hclust.method="centroid"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="original"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="ANOVA"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="AOE"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="FPC"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="alphabetic"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="complete"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="average"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="median"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="ward"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="single"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="mcquitty"))
plot(visovaParallelCord(ReactionTime[,2:14],"Group",order="hclust", hclust.method="centroid"))

}
\keyword{ visova }
