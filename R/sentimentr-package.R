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
#'   \item sentiment. Hu and Liu (2004)'s average opinion rating for a
#'   sentence.   Negative and positive reflects direction, a negative or positive
#'   sentiment.  Opinion strength varies between 3 (strongest), and 1 (weakest).
#'   \item text. The text from the review.
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
#'    Artificial Intelligence (AAAI-2004).
#'
#' \file{https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL



#' Movie Reviews 
#' 
#' A dataset containing sentiment scored movie reviews from a Kaggle competition
#' posted by University of Michigan SI650.  The data was originally collected 
#' from opinmind.com.
#' 
#' @details 
#' \itemize{ 
#'   \item sentiment. A numeric sentiment score
#'   \item text. The text from the review 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name kaggle_movie_reviews 
#' @usage data(kaggle_movie_reviews) 
#' @format A data frame with 7,086 rows and 3 variables 
#' @references \url{https://www.kaggle.com/c/si650winter11/data}
NULL 


#' Sentiment Scored New York Times Articles
#' 
#' A dataset containing Hutto & Gilbert's (2014) sentiment scored New York Times 
#' articles.
#' 
#' @details 
#' \itemize{ 
#'   \item sentiment. A numeric sentiment score
#'   \item text. The text from the article
#' } 
#' 
#' Vadar's Liscense:
#'
#' The MIT License (MIT)
#'
#' Copyright (c) 2016 C.J. Hutto
#'
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#'
#' The above copyright notice and this permission notice shall be included in all
#' copies or substantial portions of the Software.
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' @docType data 
#' @keywords datasets 
#' @name nyt_articles 
#' @usage data(nyt_articles) 
#' @format A data frame with 5,179 rows and 5 variables 
#' @references
#' Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model
#' for Sentiment Analysis of Social Media Text. Eighth International Conference
#' on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.
#'
#' \url{https://github.com/cjhutto/vaderSentiment}
NULL 


#' Student Course Evaluation Comments
#' 
#' A dataset containing a subset of comments and rating from Welch & Mihalcea's 
#' (2017) data set filtered to include comments with a one or more unambiguous 
#' sentiment rating.
#' 
#' @details 
#' \itemize{ 
#'   \item sentiment. A numeric sentiment score
#'   \item text. The text from the evaluation
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name course_evaluations 
#' @usage data(course_evaluations) 
#' @format A data frame with 566 rows and 3 variables 
#' @references Welch, C. and Mihalcea, R. (2017). Targeted sentiment to 
#' understand student comments. In Proceedings of the International Conference 
#' on Computational Linguistics (COLING 2016). \cr \cr
#' \url{http://web.eecs.umich.edu/~mihalcea/downloads.html#GroundedEmotions}
NULL 


#' Hotel Reviews
#' 
#' A dataset containing a random sample (n = 5000 of 1,621,956) of Wang, Lu, & 
#' Zhai's (2011) hotel reviews data set scraped by the authors from 
#' \url{http://www.tripadvisor.com}.
#' 
#' @details 
#' \itemize{ 
#'   \item sentiment. The overall rating for the experience
#'   \item text. The text review of the hotel
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name hotel_reviews 
#' @usage data(hotel_reviews) 
#' @format A data frame with 5000 rows and 5 variables 
#' @references Wang, H., Lu, Y., and Zhai, C. (2011). Latent aspect rating 
#' analysis without aspect keyword supervision. In Proceedings of the 17th ACM 
#' SIGKDD Conference on Knowledge Discovery and Data Mining (KDD'2011), 618-626. \cr \cr
#' \url{http://sifaka.cs.uiuc.edu/~wang296/Data/index.html}
NULL 




#' Kotzias Reviews: Amazon Cells
#'
#' A dataset containing a list of 4 review data sets.  Each data set contains
#' sentences with a positive (1) or negative review (-1) taken from reviews of
#' products, movies, & restaurants.  The data, compiled by Kotzias, Denil, De Freitas,
#' & Smyth (2015), was originally taken from amazon.com, imdb.com, & yelp.com.
#' Kotzias et al. (2015) provide the following description in the README:
#' "For each website, there exist 500 positive and
#' 500 negative sentences. Those were selected randomly for larger datasets of
#' reviews. We attempted to select sentences that have a clearly positive or
#' negative connotation [sic], the goal was for no neutral sentences to be selected.
#' This data set has been manipulated from the original to be split apart by
#' element (sentence split).  The original 0/1 metric has also been converted
#' to -1/1.  Please cite Kotzias et al. (2015) if you reuse the data here.
#'
#' @details
#' \itemize{
#'   \item sentiment. A human scoring of the text.#' 
#'   \item text. The sentences from the review.
#' }
#'
#' @docType data 
#' @keywords datasets 
#' @name kotzias_reviews_amazon_cells 
#' @usage data(kotzias_reviews_amazon_cells) 
#' @format A data frame with 1,067 rows and 2 variables 
#' @references Kotzias, D., Denil, M., De Freitas, N. & Smyth,P. (2015). From
#' group to individual labels using deep features. Proceedings of the 21th ACM
#' SIGKDD International Conference on Knowledge Discovery and Data Mining.
#' 597-606. \url{http://mdenil.com/media/papers/2015-deep-multi-instance-learning.pdf} 
NULL



#' Kotzias Reviews: IMBD
#'
#' A dataset containing a list of 4 review data sets.  Each data set contains
#' sentences with a positive (1) or negative review (-1) taken from reviews of
#' products, movies, & restaurants.  The data, compiled by Kotzias, Denil, De Freitas,
#' & Smyth (2015), was originally taken from amazon.com, imdb.com, & yelp.com.
#' Kotzias et al. (2015) provide the following description in the README:
#' "For each website, there exist 500 positive and
#' 500 negative sentences. Those were selected randomly for larger datasets of
#' reviews. We attempted to select sentences that have a clearly positive or
#' negative connotation [sic], the goal was for no neutral sentences to be selected.
#' This data set has been manipulated from the original to be split apart by
#' element (sentence split).  The original 0/1 metric has also been converted
#' to -1/1.  Please cite Kotzias et al. (2015) if you reuse the data here.
#'
#' @details
#' \itemize{
#'   \item sentiment. A human scoring of the text.#' 
#'   \item text. The sentences from the review.
#' }
#'
#' @docType data 
#' @keywords datasets 
#' @name kotzias_reviews_imdb 
#' @usage data(kotzias_reviews_imdb) 
#' @format A data frame with 1,041 rows and 2 variables 
#' @references Kotzias, D., Denil, M., De Freitas, N. & Smyth,P. (2015). From
#' group to individual labels using deep features. Proceedings of the 21th ACM
#' SIGKDD International Conference on Knowledge Discovery and Data Mining.
#' 597-606. \url{http://mdenil.com/media/papers/2015-deep-multi-instance-learning.pdf} 
NULL



#' Kotzias Reviews: Yelp
#'
#' A dataset containing a list of 4 review data sets.  Each data set contains
#' sentences with a positive (1) or negative review (-1) taken from reviews of
#' products, movies, & restaurants.  The data, compiled by Kotzias, Denil, De Freitas,
#' & Smyth (2015), was originally taken from amazon.com, imdb.com, & yelp.com.
#' Kotzias et al. (2015) provide the following description in the README:
#' "For each website, there exist 500 positive and
#' 500 negative sentences. Those were selected randomly for larger datasets of
#' reviews. We attempted to select sentences that have a clearly positive or
#' negative connotation [sic], the goal was for no neutral sentences to be selected.
#' This data set has been manipulated from the original to be split apart by
#' element (sentence split).  The original 0/1 metric has also been converted
#' to -1/1.  Please cite Kotzias et al. (2015) if you reuse the data here.
#'
#' @details
#' \itemize{
#'   \item sentiment. A human scoring of the text.#' 
#'   \item text. The sentences from the review.
#' }
#'
#' @docType data 
#' @keywords datasets 
#' @name kotzias_reviews_yelp 
#' @usage data(kotzias_reviews_yelp) 
#' @format A data frame with 1,040 rows and 2 variables 
#' @references Kotzias, D., Denil, M., De Freitas, N. & Smyth,P. (2015). From
#' group to individual labels using deep features. Proceedings of the 21th ACM
#' SIGKDD International Conference on Knowledge Discovery and Data Mining.
#' 597-606. \url{http://mdenil.com/media/papers/2015-deep-multi-instance-learning.pdf} 
NULL

