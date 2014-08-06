\name{ReactionTime}
\alias{ReactionTime}
\docType{data}
\title{
LA2K Reaction Time Dataset according to different age groups
}
\description{
LA2K Reaction Time Dataset, UCLA research about gaining understanding about the space of human variation.
}
\usage{data(ReactionTime)}
\format{
  A data frame with 950 observations on the following 14 variables.
  \describe{
    \item{\code{PTID}}{a numeric vector, 
    
    Patient ID}
    
    \item{\code{ANT_MEAN_RTCON}}{a numeric vector, 
    
    Mean Reaction Time Congruent}
    
    \item{\code{ANT_MEAN_RTINC}}{a numeric vector, 
    
    Mean Reaction Time Incongruent}
    
    \item{\code{ANT_NN_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Neutral Neutral Trial Pairs}
    
    \item{\code{ANT_CI_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Congruent Incongruent Trial Pairs}
    
    \item{\code{ANT_MEAN_RTNEU}}{a numeric vector, 
    
    Mean Reaction Time for Neutral Trials}
    
    \item{\code{ANT_IC_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Congruent Trial Pairs}
    
    \item{\code{ANT_IN_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Neutral Trial Pairs}
    
    \item{\code{ANT_II_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Incongruent Trial Pairs}
    
    \item{\code{ANT_CN_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Congruent Neutral Trial Pairs}
    
    \item{\code{ANT_NC_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Neutral.Congruent Trial Pairs}
    
    \item{\code{ANT_NI_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Neutral Incongruent Trial Pairs}
    
    \item{\code{ANT_CC_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Congruent Congruent Trial Pairs}
    
    \item{\code{Group}}{a numeric vector, Indicates six different age and gender groups
			
            
            Group-1:Male and (20_or_less or 21_25 or 26_30)
			
            
            Group-2:Male and (31_35 or 36_40 or 41_45)
			
            
            Group-3:Male and (46_50 or 51_55 or 56_60 or 61_or_more)
			
            
            Group-4:Female and (20_or_less or 21_25 or 26_30)
			
            
            Group-5:Female and (31_35 or 36_40 or 41_45)
			
            
            Group-6:Female and (46_50 or 51_55 or 56_60 or 61_or_more)
			}
  }
}

\references{
VISOVA: an integration of Visualization and ANOVA, The UCLA VISOVA Elite, 2014
}
\examples{
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
\keyword{datasets}
