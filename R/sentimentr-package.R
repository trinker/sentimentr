#' Calculate Text Polarity Sentiment
#'
#' Calculate text polarity sentiment at the sentence level and optionally
#' aggregate by rows or grouping variable(s).
#' @docType package
#' @name sentimentr
#' @aliases sentimentr package-sentiment
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




#' Cannon G3 Camera Product Reviews From Amazon
#'
#' A dataset containing Amazon product reviews for the Cannon G3 Camera.  This
#' data set was compiled by Hu and Liu (2004).  Where a sentence contains for
#' than one opinion score and average of all scores is used.
#'
#' @details
#' \itemize{
#'   \item number. The review number.
#'   \item opinion.score. Hu and Liu (2004)'s average opinion rating for a
#'   sentence.   Negative and positive reflects direction, a negative or positive
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
#' Minqing Hu and Bing Liu. (2004). Mining and summarizing customer reviews.
#'    Proceedings of the ACM SIGKDD International Conference on
#'    Knowledge Discovery & Data Mining (KDD-04).
#'
#' Minqing Hu and Bing Liu. (2004)."Mining Opinion Features in Customer
#'    Reviews. Proceedings of Nineteeth National Conference on
#'    Artificial Intellgience (AAAI-2004).
#'
#' \file{https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL


#' Kotzias Reviews
#'
#' A dataset containing a list of 4 review data sets.  Each data set contains
#' sentences with a postive (1) or negative review (-1) taken from reviews of
#' products, movies, & restaurants.  The data, compiled by Kotzias, Denil, De Freitas,
#' & Smyth (2015), was originally taken from amazon.com, imdb.com, & yelp.com.
#' Kotzias et al. (2015) provide the following description in the README:
#' "For each website, there exist 500 positive and
#' 500 negative sentences. Those were selected randomly for larger datasets of
#' reviews. We attempted to select sentences that have a clearly positive or
#' negative connotaton [sic], the goal was for no neutral sentences to be selected.
#' This data set has been manipulated from the original to be split apart by
#' element (sentence split).  The original 0/1 metric has also been converted
#' to -1/1.  Please cite Kotzias et al. (2015) if you reuse the data here.
#'
#' @details Each data set contains a dataframe of:
#' \itemize{
#'   \item text. The sentences from the review.
#'   \item rating. A human scoring of the text.
#'   \item element_id. An index for the original text element (row number).
#'   \item sentence_id. A sentence number from 1-n within each \code{element_id}.
#' }
#'
#'
#' @docType data
#' @keywords datasets
#' @name kotzias_reviews
#' @usage data(kotzias_reviews)
#' @format A list with 3 elements
#' @references Kotzias, D., Denil, M., De Freitas, N. & Smyth,P. (2015). From
#' group to individual labels using deep features. Proceedings of the 21th ACM
#' SIGKDD International Conference on Knowledge Discovery and Data Mining.
#' 597-606. \url{http://mdenil.com/media/papers/2015-deep-multi-instance-learning.pdf} 
NULL


#' Ratings Data Set
#'
#' A dataset containing common ratings.
#'
#' @details
#' \itemize{
#'   \item x. The graphic representation of the rating
#'   \item y. The meaning of the rating
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ratings
#' @usage data(ratings)
#' @format A data frame with 35 rows and 2 variables
NULL


#' Grades Data Set
#'
#' A dataset containing common grades.
#'
#' @details
#' \itemize{
#'   \item x. The graphic representation of the grade
#'   \item y. The meaning of the grade
#' }
#'
#' @docType data
#' @keywords datasets
#' @name grades
#' @usage data(grades)
#' @format A data frame with 15 rows and 2 variables
NULL

