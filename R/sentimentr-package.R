#' Calulate Text Polarity Sentiment
#'
#' Calulate text polarity sentiment at the sentence level and optionally
#' aggregate by rows or grouping variable(s).
#' @docType package
#' @name sentimentr
#' @aliases sentimentr package-sentiment
NULL

#' Polarity Lookup Key
#'
#' A \pkg{data.table} dataset containing an augmented version of Hu & Liu's (2004)
#' positive/negative word list as sentiment lookup values.
#'
#' @details
#' \itemize{
#'   \item x. Words
#'   \item y. Sentiment values (+1, -1)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name polarity_table
#' @usage data(polarity_table)
#' @format A data frame with 6776 rows and 2 variables
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer
#' reviews. National Conference on Artificial Intelligence.
#'
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL

#' Valence Shifters
#'
#' A \pkg{data.table} dataset containing a vector of valence shifter words that
#' can alter a polarized word's meaning and a numic key for negators (1),
#' amplifiers(2), and de-amplifiers (3).
#'
#' @details
#' Valence shifters are words that alter or intensify the meaning of the polarized
#' words and include negators and amplifiers. Negators are, generally, adverbs
#' that negate sentence meaning; for example the word like in the sentence, "I do
#' like pie.", is given the opposite meaning in the sentence, "I do not like
#' pie.", now containing the negator not. Amplifiers are, generally, adverbs or
#' adjectives that intensify sentence meaning. Using our previous example, the
#' sentiment of the negator altered sentence, "I seriously do not like pie.", is
#' heightened with addition of the amplifier seriously.  Whereas de-amplifiers
#' decrease the intensity of a polarized word as in the sentence "I barely like
#' pie"; the word "barely" deamplifies the word like.
#'
#' @details
#' \itemize{
#'   \item x. Valence shifter
#'   \item y. Number key value corresponding to:
#' \tabular{lr}{
#'   \bold{Valence Shifter}     \tab \bold{Value}\cr
#'   Negator     \tab 1 \%\cr
#'   Amplifier  \tab 2 \%\cr
#'  De-amplifier  \tab 3 \%\cr
#' }
#' }
#'
#' @docType data
#' @keywords datasets
#' @name valence_shifters_table
#' @usage data(valence_shifters_table)
#' @format A data frame with 85 rows and 2 variables
NULL


#' Sam I Am Text
#'
#' A dataset containing a character vector of the text from Seuss's 'Sam I Am'.
#'
#' @docType data
#' @keywords datasets
#' @name sam_i_am
#' @usage data(sam_i_am)
#' @format A character vector with 169 elements
#' @references Seuss, Dr. (1960). Green Eggs and Ham.
NULL


#' 2012 U.S. Presidential Debates
#'
#' A dataset containing a cleaned version of all three presidential debates for
#' the 2012 election.
#'
#' @details
#' \itemize{
#'   \item person. The speaker
#'   \item tot. Turn of talk
#'   \item dialogue. The words spoken
#'   \item time. Variable indicating which of the three debates the dialogue is from
#' }
#'
#' @docType data
#' @keywords datasets
#' @name presidential_debates_2012
#' @usage data(presidential_debates_2012)
#' @format A data frame with 2912 rows and 4 variables
NULL
