#' t-test contrasts for one-way repeated measures
#' 
#' @param df A long format dataframe
#' @param subject name of the subject column
#' @param dv name of the dependent variable column
#' @param condition name of the condition for the one-way manipulation
#' @param A_levels character vector, including the names of the levels for the first side of the contrast
#' @param B_levels character vector, including the names of the levels for the second side of the contrast
#' @param contrast_weights numeric vector, including the contrast weights for all levels, in the same order as they are listed in A and B levels
#' @return t.test summary
#' @export

t_contrast_rm <- function(df, 
                       subject,
                       dv,
                       condition,
                       A_levels,
                       B_levels, 
                       contrast_weights){
  
  library(dplyr)
  library(tidyr)

  wide_df <- df %>%
    select(subject,dv,condition) %>%
    filter(condition %in% c(A_levels,B_levels) == TRUE) %>%
    spread(condition,dv)
  
  names(contrast_weights)<-c(A_levels,B_levels)
  
  contrast_vector<-rep(0, dim(wide_df)[2])
  for(i in 1:length(contrast_weights)){
    weight <- contrast_weights[i]
    c_name <- names(contrast_weights[i])
    wide_df[,c_name] <-wide_df[,c_name] *weight
    contrast_vector <- contrast_vector+wide_df[,c_name]
  }
    
  return(t.test(contrast_vector))
  
}


