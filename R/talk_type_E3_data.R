#' @title Data for E3 of TalkType project
#' @description This data set is for Experiment 3 of the TalkType project. See the manuscript for a description of the design for Experiment 3.
#' @format A data frame with 67733 rows and 12 variables:
#' \describe{
#'   \item{\code{iksis}}{integer interkeystroke interval in ms}
#'   \item{\code{letters}}{character letter identity}
#'   \item{\code{words}}{character current typed word}
#'   \item{\code{closest}}{character computed closest word to compare for accuracy }
#'   \item{\code{accuracy}}{word level, integer 1= accurate, 0 = type}
#'   \item{\code{distance}}{integer Edit distance}
#'   \item{\code{propCorrect}}{double proportion of correct letters in word}
#'   \item{\code{delay}}{character conditions for delayed auditory feedback condition}
#'   \item{\code{subject}}{character subject code}
#'   \item{\code{LetterPosition}}{integer Serial position of letter in word}
#'   \item{\code{LetterType}}{character grouping factor, first letter vs other (middle) letters} 
#'   \item{\code{letter_accuracy}}{numeric 1= correct, 0 = incorrect, estimates of letter level accuracy} 
#'}
#' @source \url{https://github.com/CrumpLab/TalkTyping/data}
#' @details Experiment E3 was conducted in lab, using in-house LIVECODE scripts controlling the experiment.
"talk_type_E3_data"