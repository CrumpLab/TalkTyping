#' t-test contrasts for one-way repeated measures
#' 
#' @param df A long format dataframe
#' @param subject name of the subject column
#' @param dv name of the dependent variable column
#' @param condition name of the condition for the one-way manipulation
#' @param A_levels character vector, including the names of the levels for the first side of the contrast
#' @param B_levels character vector, including the names of the levels for the second side of the contrast
#' @param contrast_weights numeric vector, including the contrast weights for all levels, in the same order as they are listed in A and B levels
#' @param report logical, TRUE = the t.test summary will print out even if you assign the object to a variable, FALSE = no extra printing
#' @return t.test summary
#' @examples 
#' 
#' some_data <- data.frame(s = rep(1:10,3),
#'                        measure = rnorm(30,0,1),
#'                        manipulation = as.factor(rep(c("A","B","C"),each=10)))
#' t_contrast_rm(df = some_data,
#'               subject = "s",
#'               dv = "measure",
#'               condition = "manipulation",
#'               A_levels = c("A"),
#'               B_levels = c("B","C"),
#'               contrast_weights = c(-1,.5,.5))
#' 
#' @export

t_contrast_rm <- function(df, 
                       subject,
                       dv,
                       condition,
                       A_levels,
                       B_levels, 
                       contrast_weights,
                       report = TRUE){
  
  library(dplyr)
  library(tidyr)

  wide_df <- df %>%
    select(subject,dv,condition) %>%
    filter(!!as.name(condition) %in% c(A_levels,B_levels) == TRUE) %>%
    spread(!!as.name(condition),dv)
  
  names(contrast_weights)<-c(A_levels,B_levels)
  
  contrast_vector<-rep(0, dim(wide_df)[1])
  for(i in 1:length(contrast_weights)){
    weight <- contrast_weights[i]
    c_name <- names(contrast_weights[i])
    wide_df[,c_name] <-wide_df[,c_name] *weight
    contrast_vector <- contrast_vector + wide_df[,c_name]
  }
  
  if(report==TRUE){
    print(t.test(contrast_vector))  
  }
  return(t.test(contrast_vector))
  
}


