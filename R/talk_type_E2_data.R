#' @title Data for E2 of TalkType project
#' @description This data set is for Experiment 2 of the TalkType project. See the manuscript for a description of the design for Experiment 2.
#' @format A data frame with 55831 rows and 11 variables:
#' \describe{
#'   \item{\code{iksis}}{integer interkeystroke interval in ms}
#'   \item{\code{letters}}{character letter identity}
#'   \item{\code{words}}{character current typed word}
#'   \item{\code{closest}}{character computed closest word to compare for accuracy }
#'   \item{\code{accuracy}}{integer 1= accurate, 0 = type}
#'   \item{\code{distance}}{integer Edit distance}
#'   \item{\code{propCorrect}}{double proportion of correct letters in word}
#'   \item{\code{suppression}}{character conditions for the verbal suppression manipulation}
#'   \item{\code{subject}}{character subject code}
#'   \item{\code{LetterPosition}}{integer Serial position of letter in word}
#'   \item{\code{LetterType}}{character grouping factor, first letter vs other (middle) letters} 
#'   \item{\code{letter_accuracy}}{character 1= correct, 0 = incorrect, estimates of letter level accuracy} 
#'}
#' @source \url{https://github.com/CrumpLab/TalkTyping/data}
#' @details Experiment E2 was conducted in lab, using in-house LIVECODE scripts controlling the experiment.
"talk_type_E2_data"