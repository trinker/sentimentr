#' Calculate Text Polarity Sentiment
#'
#' Calculate text polarity sentiment at the sentence level and optionally
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
#' @format A data frame with 6781 rows and 2 variables
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer
#' reviews. National Conference on Artificial Intelligence.
#'
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL

#' Valence Shifters
#'
#' A \pkg{data.table} dataset containing a vector of valence shifter words that
#' can alter a polarized word's meaning and a numeric key for negators (1),
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
#' @format A data frame with 89 rows and 2 variables
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

#' Polarity Lookup Key 2
#'
#' A \pkg{data.table} dataset containing an augmented version of Baccianella,
#' Esuli and Sebastiani's (2010) positive/negative word list as sentiment lookup
#' values.  This list has be restructured to long format.  A polarity value
#' was assigned by taking the difference between the original data set's
#' negative and positive attribution (\code{PosScore - NegScore}).  All rows
#' with a zero polarity were removed from the data set as well as any duplicated
#' in the valence shifter's data set.
#'
#' @details
#' \itemize{
#'   \item x. Words
#'   \item y. Sentiment values
#' }
#'
#' @docType data
#' @keywords datasets
#' @name sentiword
#' @usage data(sentiword)
#' @format A data frame with 20104 rows and 2 variables
#' @references Baccianella S., Esuli, A. and Sebastiani, F. (2010). SentiWordNet
#' 3.0: An Enhanced Lexical Resource for Sentiment Analysis and Opinion Mining.
#' International Conference on Language Resources and Evaluation.
#'
#' \url{http://sentiwordnet.isti.cnr.it/}
NULL



#' Emoticons Data Set
#'
#' A dataset containing common emoticons (adapted from
#' \href{http://www.lingo2word.com/lists/emoticon_listH.html}{Popular Emoticon List}).
#'
#' @details
#' \itemize{
#'   \item x. The graphic representation of the emoticon
#'   \item y. The meaning of the emoticon
#' }
#'
#' @docType data
#' @keywords datasets
#' @name emoticons
#' @usage data(emoticons)
#' @format A data frame with 75 rows and 2 variables
#' @references \url{http://www.lingo2word.com/lists/emoticon_listH.html}
NULL


#' Cannon G3 Camera Product Reviews From Amazon
#'
#' A dataset containing Amazon product reviews for the Cannon G3 Camera.  This
#' data set was compiled by Hu and Liu (2004).  Where a sentence contains for
#' than one opinion score and average of all scores is used.
#'
#' @details
#' \itemize{
#'   \item number. The review number.
#'   \item opinion.score. Hu and Liu (2004)'s average opionion rating for a
#'   sentence.   Negative and positive reflects dirrection, a negative or positive
#'   sentiment.  Opinion strength varies between 3 (strongest), and 1 (weakest).
#'   \item review. The text from the review.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name cannon_reviews
#' @usage data(cannon_reviews)
#' @format A data frame with 45 rows and 2 variables
#' @references
#' Minqing Hu and Bing Liu. "Mining and summarizing customer reviews".
#'    Proceedings of the ACM SIGKDD International Conference on
#'    Knowledge Discovery & Data Mining (KDD-04), 2004.
#'
#' Minqing Hu and Bing Liu. "Mining Opinion Features in Customer
#'    Reviews." Proceedings of Nineteeth National Conference on
#'    Artificial Intellgience (AAAI-2004), 2004.
#'
#' \url{http://www.cs.uic.edu/~liub/FBS/FBS.html}
NULL
