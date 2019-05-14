#' @title Data for E1B of TalkType project
#' @description This data set is for Experiment 1B of the TalkType project. See the manuscript for a description of the design for Experiment 1B.
#' @format A data frame with 37105 rows and 14 variables:
#' \describe{
#'   \item{\code{V1}}{integer row number}
#'   \item{\code{iksis}}{integer interkeystroke interval in ms}
#'   \item{\code{letters}}{character letter identity}
#'   \item{\code{words}}{character current typed word}
#'   \item{\code{closest}}{character computed closest word to compare for accuracy }
#'   \item{\code{accuracy}}{integer 1= accurate, 0 = type}
#'   \item{\code{distance}}{integer ÎEdit distance}
#'   \item{\code{propCorrect}}{double proportion of correct letters in word}
#'   \item{\code{suppression}}{character condition code}
#'   \item{\code{subject}}{character subject code}
#'   \item{\code{VoiceCondition}}{character use inner voice or speak aloud}
#'   \item{\code{ChunkCondition}}{character say letters or say words}
#'   \item{\code{LetterPosition}}{integer Serial position of letter in word}
#'   \item{\code{LetterType}}{character grouping factor, first letter vs other (middle) letters} 
#'}
#' @source \url{https://github.com/CrumpLab/TalkTyping/data}
#' @details Experiment E1B was conducted in lab, using in-house LIVECODE scripts controlling the experiment.
"talk_type_E1B_data"