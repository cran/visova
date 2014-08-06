\name{ELIZA_Additional_Measures}
\alias{ELIZA_Additional_Measures}
\docType{data}
\title{
LA2K ELIZA additional measures dataset according to the different age/gender/weight/BMI/Hispanic origin/Darkroom preferance groups
}
\description{
LA2K ELIZA additional measures dataset, UCLA research about gaining understanding about the space of human variation.
}
\usage{data(ELIZA_Additional_Measures)}
\format{
  A data frame with 984 observations on the following 17 variables.
  \describe{
    \item{\code{PTID}}{a numeric vector, 
    
    Patient ID}
    
    \item{\code{COLORDEFICIENCY}}{a numeric vector, 
    
    The indicator of Color Deficiency based on number of plates that the subject couldn't complete}
    
    \item{\code{ETOTAL}}{a numeric vector}
    
    \item{\code{HT}}{a numeric vector, 
    
    The subject's height value}

    \item{\code{BMI}}{a numeric vector,
    
    The subject's Body Mass Index value}

    \item{\code{WT}}{a numeric vector,
    
    The subject's weight value}

    \item{\code{VISUALACUITY}}{a numeric vector,
    
    Visual Acuity both eyes}

    \item{\code{SCHOOL_BACK}}{a categorical variable,
    
    Ever held back in school? (0,1)}

    \item{\code{E}}{a numeric vector }

    \item{\code{LA2KHEALTH_SCORE}}{a numeric vector, 
    
    LA2K Total Health Score}

    \item{\code{CIGS}}{a categorical variable, 
    
    Have you ever smoked cigarettes on a daily basis? (0..2)}

    \item{\code{AGE_GroupName}}{a numeric vector, Indicates six different age groups
			
            
            Age 21-25
			
            
            Age 26-30
			
            
            Age 31-35
			
            
            Age 36-40
			
            
            Age 41-45
			
            
            Age 46-50
    }

    \item{\code{BMI_GroupID}}{a character vector, Indicates five different BMI groups
			
            
            Morbid
			
            
            Normal 
			
            
            Obese
			
            
            Overweight
			
            
            Underweight

    }


    \item{\code{DARKROOM_GroupID}}{a character vector, Indicates sleeping in a dark room preference
			
            
            No_Dark_Room_Preference
			
            
            Prefer_Dark_Room_Sleep

         }
         
         
     \item{\code{GENDER_GroupID}}{a character vector, Indicates subject's gender
			
            
            Female
			
            
            Male
			
    }
       
      \item{\code{HISPANIC_GroupID}}{a character vector, Indicates whether subject's origin is hispanic or not
			
            
            Hispanic_origin
			
            
            not_Hispanic_origin
			
            
    }

        \item{\code{WEIGHT_GroupName}}{a character vector, Indicates ten different weight groups
			
            
            WT 100 or less
			
            
            WT 101-120 
			
            
            WT 121-140
			
            
            WT 141-160
			
            
            WT 161-180
            

            WT 181-200
            

            WT 201-220
            

            WT 221-240
            
            
            WT 241-260
            
            
            WT 261 or more 

    }


    
  }
}

\references{

UCLA Consortium for Neuropsychiatric Phenomics (CNP) Research about 2000 healthy volunteers in Los Angeles area. 
\url{http://www.phenomics.ucla.edu/LA2K\% 20Flier_121609.pdf}

}


\examples{

plot(visovaParallelCord(ELIZA_Additional_Measures[,c(2:11,15)],
"GENDER_GroupID",order="original", colors=c("red","navy")))

plot(visovaParallelCord(ELIZA_Additional_Measures[,c(2:11,16)],
"HISPANIC_GroupID",order="original", colors=c("orange","turquoise")))

plot(visovaParallelCord(ELIZA_Additional_Measures[,c(2:11,14)],
"DARKROOM_GroupID",order="original", colors=c("green","blue")))

plot(visovaParallelCord(ELIZA_Additional_Measures[,c(2:11,13)],
"BMI_GroupID",order="original", colors=c("turquoise","green","orange","red","purple")))

plot(visovaParallelCord(ELIZA_Additional_Measures[,c(2:11,17)],
"WEIGHT_GroupName",order="original"))

}
\keyword{datasets}
