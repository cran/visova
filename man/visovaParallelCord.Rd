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
visovaParallelCord(data, groupCol, order, hclust.method, colors)
}

\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a parallel coordinates plot
  }
  \item{groupCol}{
Character, the name of the column which contains the category levels of data
  }
 \item{order}{
Character, the ordering method of the columns/variables in the parallel coordinates plot..
        \itemize{
                \item \code{"original"} for original order (default).
                \item \code{"ANOVA"} for the column order that uses group-wise ANOVA results.
                \item \code{"AOE"} for the angular order of the eigenvectors.
                \item \code{"FPC"} for the first principal component order.
                \item \code{"hclust"} for the hierarchical clustering order.
                \item \code{"alphabetic"} for alphabetical order.
                
		Look at the reference paper for further details.
        }

}

  \item{hclust.method}{
Character, the agglomeration method to be used when \code{order} is \code{hclust}. This should be one of the

  \itemize{
                \item \code{"ward"} for the Ward's Hierarchical Linkage Method .
                \item \code{"single"} for the Single Hierarchical Linkage Method.
                \item \code{"complete"} for the Complete Hierarchical Linkage Method.
                \item \code{"average"} for the Average Hierarchical Linkage Method.
                \item \code{"mcquitty"} for the Mcquitty's Hierarchical Linkage Method.
                \item \code{"median"} for the Median Hierarchical Linkage Method
                \item \code{"centroid"} for the Centroid Hierarchical Linkage Method.

        Look at the reference paper for further details.
}
}
 \item{colors}{
Character vector, the individual group colors may be specified by using this field. Otherwise default colors are used. The name of the colors that are indicated in this field should not less than the number of groups. Below colors must be used:

    \itemize{
                \item \code{"turquoise"}: rgb(0,206,209)
                \item \code{"red"}: rgb(255,0,0)
                \item \code{"lime"}: rgb(0,255,0)
                \item \code{"purple"}: rgb(128,0,128)
                \item \code{"orange"}: rgb(255,140,0)
                \item \code{"blue"}: rgb(0,0,255)
                \item \code{"yellow"}: rgb(255,255,0)
                \item \code{"magenta"}: rgb(255,0,255)
                \item \code{"gray"}: rgb(128,128,128)
                \item \code{"olive"}: rgb(128,128,0))
                \item \code{"green"}: rgb(0,128,0)
                \item \code{"teal"}: rgb(0,128,128)
                \item \code{"navy"}: rgb(0,0,128)

    }
}


}
\references{
VISOVA: an integration of Visualization and ANOVA; A.V. Barbaros, D.S. Parker; The UCLA VISOVA Elite(2014)
}
\author{
  Ali Vala Barbaros \email{alivalabarbaros@gmail.com}
}
\section{Warning}{
  All of the charts are rendered by the browser and some of the libraries like "google jsapi", "qtip2", "jquery" need to be imported , therefore, a modern browser with an Internet connection is required to display charts.
  Data frame should not include any character values other than group column which has to be indicated when this function is called.
}
\examples{


plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="complete"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="average"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="median"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="ward"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="single"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="mcquitty"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="hclust", hclust.method="centroid"))

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],"AGE_GENDER_GroupName",order="alphabetic"))
plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],"AGE_GENDER_GroupName",order="AOE"))
plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],"AGE_GENDER_GroupName",order="ANOVA"))


}
\keyword{ visova }
