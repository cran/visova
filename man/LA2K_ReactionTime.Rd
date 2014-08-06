\name{LA2K_ReactionTime}
\alias{LA2K_ReactionTime}
\docType{data}
\title{
LA2K Reaction Time Dataset according to the different age/gender groups
}
\description{
LA2K Reaction Time Dataset, UCLA research about gaining understanding about the space of human variation.
}
\usage{data(LA2K_ReactionTime)}
\format{
  A data frame with 950 observations on the following 27 variables.
  \describe{
    \item{\code{PTID}}{a numeric vector, 
    
    Patient ID}
    
    \item{\code{ANT_MEAN_RTCON}}{a numeric vector, 
    
    Mean Reaction Time for Congruent Trials}
    
    \item{\code{ANT_MEAN_RTINC}}{a numeric vector, 
    
    Mean Reaction Time for Incongruent Trials}
    
    \item{\code{ANT_MEAN_RTNEU}}{a numeric vector, 
    
    Mean Reaction Time for Neutral Trials}

    \item{\code{ANT_CC_MN_RT}}{a numeric vector,
    
    Mean Reaction Time for the Second Trial of Congruent Congruent Trial Pairs}

    \item{\code{ANT_CI_MN_RT}}{a numeric vector,
    
    Mean Reaction Time for the Second Trial of Congruent Incongruent Trial Pairs}

    \item{\code{ANT_CN_MN_RT}}{a numeric vector,
    
    Mean Reaction Time for the Second Trial of Congruent Neutral Trial Pairs}

    \item{\code{ANT_IC_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Congruent Trial Pairs}

    \item{\code{ANT_II_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Incongruent Trial Pairs}

    \item{\code{ANT_IN_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Incongruent Neutral Trial Pairs}

    \item{\code{ANT_NC_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Neutral.Congruent Trial Pairs}

    \item{\code{ANT_NI_MN_RT}}{a numeric vector,
    
    Mean Reaction Time for the Second Trial of Neutral Incongruent Trial Pairs}

    \item{\code{ANT_NN_MN_RT}}{a numeric vector, 
    
    Mean Reaction Time for the Second Trial of Neutral Neutral Trial Pairs}
    
    \item{\code{SMNM_MANIP_MNRT}}{a numeric vector, 
    
    Mean Reaction Time for Manipulation Task}
    
    \item{\code{SMNM_MAIN_MNRT}}{a numeric vector, 
    
    Mean Reaction Time for Maintenance Task}
    
    \item{\code{TS_SHORTCONGNOSWRT}}{a numeric vector, 
    
    Short Cue interval congruent no switch trial mean reaction time}

    \item{\code{TS_LONGCONGNOSWRT}}{a numeric vector,
    
    Long Cue interval congruent no switch trial mean reaction time }
    
    \item{\code{TS_SHORTINCONGNOSWRT}}{a numeric vector,
    
    Short Cue interval incongruent no switch trial mean reaction time.}
    
    \item{\code{TS_LONGINCONGNOSWRT}}{a numeric vector,
    
    Long Cue interval incongruent no switch trial mean reaction time }
    
    \item{\code{TS_SHORTCONGSWRT}}{a numeric vector,
    
    Short Cue interval congruent switch trial mean reaction time }
    
    \item{\code{TS_LONGCONGSWRT}}{a numeric vector,
    
    Long Cue interval congruent switch trial mean reaction time}
    
    \item{\code{TS_SHORTINCONGSWRT}}{a numeric vector,
    
    Short Cue interval incongruent switch trial mean reaction time }
    
    \item{\code{TS_LONGINCONGSWRT}}{a numeric vector,
    
    Long Cue interval incongruent switch trial mean reaction time }
    
    \item{\code{VMNM_MANIP_MNRT}}{a numeric vector,

    Mean Reaction Time for Manipulation task }

    \item{\code{VMNM_MAIN_MNRT}}{a numeric vector,
    
    Mean Reaction Time for Maintenance task }
       
    \item{\code{AGE_GENDER_GroupName}}{a numeric vector, Indicates six different age and gender groups
			
            
            Male and (20_or_less or 21_25 or 26_30)
			
            
            Male and (31_35 or 36_40 or 41_45)
			
            
            Male and (46_50 or 51_55 or 56_60 or 61_or_more)
			
            
            Female and (20_or_less or 21_25 or 26_30)
			
            
            Female and (31_35 or 36_40 or 41_45)
			
            
            Female and (46_50 or 51_55 or 56_60 or 61_or_more)
    }
    
    \item{\code{AGE_GroupName}}{a numeric vector, Indicates six different age and gender groups
			
            
            Age 21-25
			
            
            Age 26-30
			
            
            Age 31-35
			
            
            Age 36-40
			
            
            Age 41-45
			
            
            Age 46-50
    }

    
  }
}

\references{

UCLA Consortium for Neuropsychiatric Phenomics (CNP) Research about 2000 healthy volunteers in Los Angeles area. 
\url{http://www.phenomics.ucla.edu/LA2K\% 20Flier_121609.pdf}

}


\examples{

plot(visovaParallelCord(LA2K_ReactionTime[,2:26],"AGE_GroupName",
order="original", colors=c("turquoise","red","orange","lime","purple","navy")))

plot(visovaParallelCord(LA2K_ReactionTime[,2:26],"AGE_GroupName",
order="alphabetic", colors=c("red","green","orange","teal","purple","navy")))

plot(visovaParallelCord(LA2K_ReactionTime[,2:26],"AGE_GroupName",
order="ANOVA", colors=c("olive","green","orange","red","purple","navy")))

plot(visovaParallelCord(LA2K_ReactionTime[,2:26],"AGE_GroupName",
order="AOE", colors=c("turquoise","red","orange","lime","purple","navy")))

plot(visovaParallelCord(LA2K_ReactionTime[,2:26],"AGE_GroupName",
order="FPC", colors=c("yellow","green","orange","red","purple","navy")))

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

plot(visovaParallelCord(LA2K_ReactionTime[,c(2:13, 27)],
"AGE_GENDER_GroupName",order="alphabetic"))
}
\keyword{datasets}
