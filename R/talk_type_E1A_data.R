#' @title Data for E1A of TalkType project
#' @description This data set is for Experiment 1a of the TalkType project. See the manuscript for a description of the design for Experiment 1a.
#' @format A data frame with 18277 rows and 7 variables:
#' \describe{
#'   \item{\code{subject}}{integer subject code, contains data from 40 participants}
#'   \item{\code{linguistic_unit}}{Factor say_letter = subjects were instructed to say the letters they were typing using their inner voice, say_word = subjects were instructed to say the words they were typinh using their inner voice.}
#'   \item{\code{iksis}}{integer Interkeystroke interval in milliseconds. The temporal interval between the previous keystroke and the current keystroke}
#'   \item{\code{letters}}{Factor the identity of the typed character}
#'   \item{\code{errors}}{integer Accuracy code, 1 = correct, 0 = type}
#'   \item{\code{LetterPosition}}{integer Serial letter position in word}
#'   \item{\code{LetterType}}{Factor Categorical position, First, middle, last, or space} 
#'}
#' @source \url{https://github.com/CrumpLab/TalkTyping/data}
#' @details For experiment 1A, typing tests were programmed for the online environment using HTML and JavaScript and conducted in subjects' web-browsers.
"talk_type_E1A_data"