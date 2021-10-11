#' Polarity Score (Sentiment Analysis)
#'
#' Approximate the sentiment (polarity) of text by sentence.  This function allows
#' the user to easily alter (add, change, replace) the default polarity an 
#' valence shifters dictionaries to suit the context dependent needs of a particular
#' data set.  See the \code{polarity_dt} and \code{valence_shifters_dt} arguments
#' for more information.  Other hyper-parameters may add additional fine tuned 
#' control of the algorithm that may boost performance in different contexts.
#'
#' @param text.var The text variable.  Can be a \code{get_sentences} object or
#' a raw character vector though \code{get_sentences} is preferred as it avoids
#' the repeated cost of doing sentence boundary disambiguation every time
#' \code{sentiment} is run.
#' @param polarity_dt A \pkg{data.table} of positive/negative words and
#' weights with x and y as column names.  The \pkg{lexicon} package has several 
#' dictionaries that can be used, including: 
#' \itemize{
#'   \item \code{lexicon::hash_sentiment_jockers_rinker}
#'   \item \code{lexicon::hash_sentiment_jockers}
#'   \item \code{lexicon::emojis_sentiment}
#'   \item \code{lexicon::hash_sentiment_emojis}
#'   \item \code{lexicon::hash_sentiment_huliu}
#'   \item \code{lexicon::hash_sentiment_loughran_mcdonald}
#'   \item \code{lexicon::hash_sentiment_nrc}
#'   \item \code{lexicon::hash_sentiment_senticnet}
#'   \item \code{lexicon::hash_sentiment_sentiword}
#'   \item \code{lexicon::hash_sentiment_slangsd}
#'   \item \code{lexicon::hash_sentiment_socal_google}
#' }
#' Additionally, the 
#' \code{as_key} function can be used to make a sentiment frame suitable for
#' \code{polarity_dt}.  This takes a 2 column data.frame with the first column
#' being words and the second column being polarity values.  Note that as of 
#' version 1.0.0 \pkg{sentimentr} switched from the Liu & HU (2004) dictionary
#' as the default to Jocker's (2017) dictionary from the \pkg{syuzhet} package.
#' Use \code{lexicon::hash_sentiment_huliu} to obtain the old behavior.
#' @param valence_shifters_dt A \pkg{data.table} of valence shifters that
#' can alter a polarized word's meaning and an integer key for negators (1),
#' amplifiers [intensifiers] (2), de-amplifiers [downtoners] (3) and adversative 
#' conjunctions (4) with x and y as column names.
#' @param hyphen The character string to replace hyphens with.  Default replaces
#' with nothing so 'sugar-free' becomes 'sugarfree'.  Setting \code{hyphen = " "}
#' would result in a space between words (e.g., 'sugar free').  Typically use 
#' either " " or default "".
#' @param amplifier.weight The weight to apply to amplifiers/de-amplifiers 
#' [intensifiers/downtoners] (values from 0 to 1).  This value will multiply the 
#' polarized terms by 1 + this value.
#' @param n.before The number of words to consider as valence shifters before
#' the polarized word.  To consider the entire beginning portion of a sentence
#' use \code{n.before = Inf}.
#' @param n.after The number of words to consider as valence shifters after
#' the polarized word.  To consider the entire ending portion of a sentence
#' use \code{n.after = Inf}.
#' @param question.weight The weighting of questions (values from 0 to 1).
#' Default is 1.  A 0 corresponds with the belief that questions (pure questions)
#' are not polarized.  A weight may be applied based on the evidence that the
#' questions function with polarized sentiment.  In an opinion tasks such as a
#' course evalaution the questions are more likely polarized, not designed to
#' gain information.  On the other hand, in a setting with more natural dialogue,
#' the question is less likely polarized and is likely to function as a means
#' to gather information.
#' @param adversative.weight The weight to give to adversative conjunctions or 
#' contrasting conjunctions (e.g., "but") that overrule the previous clause 
#' (Halliday & Hasan, 2013).  Weighting a contrasting statement stems from the 
#' belief that the adversative conjunctions like "but", "however", and "although" 
#' amplify the current clause and/or down weight the prior clause.  If an 
#' adversative conjunction is located before the polarized word in the context 
#' cluster the cluster is up-weighted 1 + number of occurrences of the 
#' adversative conjunctions before the polarized word times the
#' weight given (\eqn{1 + N_{adversative\,conjunctions} * z_2} where \eqn{z_2} 
#' is the \code{adversative.weight}).  Conversely, an adversative conjunction 
#' found after the polarized word in a context cluster down weights the cluster 
#' 1 - number of occurrences of the adversative conjunctions after the polarized 
#' word times the weight given (\eqn{1 + N_{adversative\,conjunctions}*-1 * z_2}).  
#' These are added to the deamplifier and amplifier weights and thus the down 
#' weight is constrained to -1 as the lower bound.  Set to zero to remove 
#' adversative conjunction weighting.
#' @param neutral.nonverb.like logical.  If \code{TRUE}, and 'like' is found
#' in the \code{polarity_dt}, when the word 'like' is preceded by one of the 
#' following linking verbs: \code{"'s"}, \code{"was"}, \code{"is"}, \code{"has"}, 
#' \code{"am"}, \code{"are"}, \code{"'re"}, \code{"had"}, or \code{"been"} it is 
#' neutralized as this non-verb form of like is not likely polarized.  This is a 
#' poor man's part of speech tagger, maintaining the balance between speed and 
#' accuracy.  The word 'like', as a verb, tends to be polarized and is usually 
#' preceded by a noun or pronoun, not one of the linking verbs above.  This 
#' hyper parameter doesn't always yield improved results depending on the context 
#' of where the text data comes from.  For example, it is likely to be more 
#' useful in literary works, where like is often used in non-verb form, than 
#' product comments.  Use of this parameter will add compute time, this must be 
#' weighed against the need for accuracy and the likeliness that more accurate 
#' results will come from setting this argument to \code{TRUE}.
#' @param missing_value A value to replace \code{NA}/\code{NaN} with.  Use
#' \code{NULL} to retain missing values.
#' @param retention_regex A regex of what characters to keep.  All other 
#' characters will be removed.  Note that when this is used all text is lower 
#' case format.  Only adjust this parameter if you really understand how it is 
#' used.  Note that swapping the \code{\\\\p{L}} for \code{[^[:alpha:];:,\']} may 
#' retain more alpha letters but will likely decrease speed.  See examples below 
#' for how to test the need for \code{\\\\p{L}}.
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} of:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{sentiment}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
#'   \item  word_count - Word count
#'   \item  sentiment - Sentiment/polarity score (note: sentiments less than zero is negative, 0 is neutral, and greater than zero positive polarity)
#' }
#' @references Jockers, M. L. (2017). Syuzhet: Extract sentiment and plot arcs 
#' from text. Retrieved from https://github.com/mjockers/syuzhet
#' 
#' Hu, M., & Liu, B. (2004). Mining opinion features in customer
#' reviews. National Conference on Artificial Intelligence.
#' 
#' Halliday, M. A. K. & Hasan, R. (2013). Cohesion in English. New York, NY: Routledge.
#' 
#' \url{https://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
#'
#' \url{http://hedonometer.org/papers.html} Links to papers on hedonometrics
#' @export
#' @family sentiment functions
#' @seealso Original URL: https://github.com/trestletech/Sermon-Sentiment-Analysis
#' @note The polarity score is dependent upon the polarity dictionary used.
#' This function defaults to a combined and augmented version of Jocker's (2017) 
#' [originally exported by the \pkg{syuzhet} package] & Rinker's augmented Hu & Liu (2004) 
#' dictionaries in the \pkg{lexicon} package, however, this may not be appropriate, for 
#' example, in the context of children in a classroom.  The user may (is 
#' encouraged) to provide/augment the dictionary (see the \code{as_key} 
#' function).  For instance the word "sick" in a high school setting may mean 
#' that something is good, whereas "sick" used by a typical adult indicates 
#' something is not right or negative connotation (\strong{deixis}).
#' @details The equation used by the algorithm to assign value to polarity of
#' each sentence fist utilizes the sentiment dictionary to tag polarized words.  
#' Each paragraph
#' (\eqn{p_i = \{s_1, s_2, ..., s_n\}}{p_i = \{s_1, s_2, ... s_n\}}) composed of
#' sentences, is broken into element sentences
#' (\eqn{s_i,j = \{w_1, w_2, ..., w_n\}}{s_i,j = \{w_1, w_2, ... w_n\}}) where \eqn{w}
#' are the words within sentences.  Each sentence (\eqn{s_j}) is broken into a
#' an ordered bag of words.  Punctuation is removed with the exception of pause
#' punctuations (commas, colons, semicolons) which are considered a word within
#' the sentence.  I will denote pause words as \eqn{cw} (comma words) for
#' convenience.  We can represent these words as an i,j,k notation as
#' \eqn{w_{i,j,k}}.  For example \eqn{w_{3,2,5}} would be the fifth word of the
#' second sentence of the third paragraph.  While I use the term paragraph this
#' merely represent a complete turn of talk.  For example t may be a cell level
#' response in a questionnaire composed of sentences.
#'
#' The words in each sentence (\eqn{w_{i,j,k}}) are searched and compared to a
#' dictionary of polarized words (e.g., Jockers (2017) dictionary found in 
#' the \pkg{lexicon} package).  Positive (\eqn{w_{i,j,k}^{+}}{w_i,j,k^+}) and
#'  negative (\eqn{w_{i,j,k}^{-}}{w_i,j,k^-}) words are tagged with a \eqn{+1} 
#'  and \eqn{-1} respectively.  I will denote polarized words as \eqn{pw} for 
#'  convenience. These will form a polar cluster (\eqn{c_{i,j,l}}{c_i,j,l}) 
#'  which is a subset of the a sentence 
#'  (\eqn{c_{i,j,l} \subseteq s_i,j }{l_i,j,l \subseteq s_i,j}).
#'
#' The polarized context cluster (\eqn{c_{i,j,l}}) of words is pulled from around
#' the polarized word (\eqn{pw}) and defaults to 4 words before and two words
#' after \eqn{pw}) to be considered as valence shifters.  The cluster can be represented as
#' (\eqn{c_{i,j,l} = \{pw_{i,j,k - nb}, ..., pw_{i,j,k} , ..., pw_{i,j,k - na}\}}{c_i,j,l = \{pw_i,j,k - nb, ..., pw_i,j,k , ..., pw_i,j,k - na\}}),
#' where \eqn{nb} & \eqn{na} are the parameters \code{n.before} and \code{n.after}
#' set by the user.  The words in this polarized context cluster are tagged as
#' neutral (\eqn{w_{i,j,k}^{0}}{w_i,j,k^0}), negator (\eqn{w_{i,j,k}^{n}}{w_i,j,k^n}),
#' amplifier [intensifier]] (\eqn{w_{i,j,k}^{a}}{w_i,j,k^a}), or de-amplifier
#' [downtoner] (\eqn{w_{i,j,k}^{d}}{w_i,j,k^d}). Neutral words hold no value in 
#' the equation but do affect word count (\eqn{n}).  Each polarized word is then 
#' weighted (\eqn{w}) based on the weights from the \code{polarity_dt} argument 
#' and then further weighted by the function and number of the valence shifters 
#' directly surrounding the positive or negative word (\eqn{pw}).  Pause 
#' (\eqn{cw}) locations (punctuation that denotes a pause including commas, 
#' colons, and semicolons) are indexed and considered in calculating the upper 
#' and lower bounds in the polarized context cluster. This is because these marks 
#' indicate a change in thought and words prior are not necessarily connected 
#' with words after these punctuation marks.  The lower bound of the polarized 
#' context cluster is constrained to 
#' \eqn{\max \{pw_{i,j,k - nb}, 1, \max \{cw_{i,j,k} < pw_{i,j,k}\}\}} and the upper bound is
#' constrained to \eqn{\min \{pw_{i,j,k + na}, w_{i,jn}, \min \{cw_{i,j,k} > pw_{i,j,k}\}\}}
#' where \eqn{w_{i,jn}} is the number of words in the sentence.
#'
#' The core value in the cluster, the polarized word is acted upon by valence
#' shifters. Amplifiers (intensifiers) increase the polarity by 1.8 (.8 is the default weight
#' (\eqn{z})).  Amplifiers (\eqn{w_{i,j,k}^{a}}) become de-amplifiers if the context
#' cluster contains an odd number of negators (\eqn{w_{i,j,k}^{n}}).  De-amplifiers
#' (downtoners) work to decrease the polarity.  Negation (\eqn{w_{i,j,k}^{n}}) acts on
#' amplifiers/de-amplifiers as discussed but also flip the sign of the polarized
#' word.  Negation is determined by raising -1 to the power of the number of
#' negators (\eqn{w_{i,j,k}^{n}}) + 2.  Simply, this is a result of a belief that two
#' negatives equal a positive, 3 negatives a negative and so on.
#'
#' The adversative conjunctions (i.e., 'but', 'however', and 'although') also 
#' weight the context cluster.  A adversative conjunction before the polarized 
#' word (\eqn{w_{adversative\,conjunction}, ..., w_{i, j, k}^{p}}) up-weights 
#' the cluster by 
#' \eqn{1 + z_2 * \{|w_{adversative\,conjunction}|, ..., w_{i, j, k}^{p}\}} 
#' (.85 is the default weight (\eqn{z_2})).  An adversative conjunction after 
#' the polarized word down-weights the cluster by
#' \eqn{1 + \{w_{i, j, k}^{p}, ..., |w_{adversative\,conjunction}| * -1\} * z_2}.  
#' The number of occurrences before and after the polarized word are multiplied by
#' 1 and -1 respectively and then summed within context cluster.  It is this
#' value that is multiplied by the weight and added to 1. This
#' corresponds to the belief that an adversative conjunction makes the next 
#' clause of greater values while lowering the value placed on the prior clause.
#'
#' The researcher may provide a weight \eqn{z} to be utilized with
#' amplifiers/de-amplifiers (default is .8; de-amplifier weight is constrained
#' to -1 lower bound).  Last, these weighted context clusters (\eqn{c_{i,j,l}}{c_i,j,l}) are
#' summed (\eqn{c'_{i,j}}{c'_i,j}) and divided by the square root of the word count (\eqn{\sqrt{w_{i,jn}}}{\sqrtn w_i,jn}) yielding an \strong{unbounded
#' polarity score} (\eqn{\delta}{C}) for each sentence.
#'
#' \deqn{\delta=\frac{c'_{i,j}}{\sqrt{w_{i,jn}}}}{C=c'_i,j,l/\sqrt(w_i,jn)}
#'
#' Where:
#'
#' \deqn{c'_{i,j}=\sum{((1 + w_{amp} + w_{deamp})\cdot w_{i,j,k}^{p}(-1)^{2 + w_{neg}})}}
#'
#' \deqn{w_{amp}= (w_{b} > 1) + \sum{(w_{neg}\cdot (z \cdot w_{i,j,k}^{a}))}}
#'
#' \deqn{w_{deamp} = \max(w_{deamp'}, -1)}
#'
#' \deqn{w_{deamp'}= (w_{b} < 1) + \sum{(z(- w_{neg}\cdot w_{i,j,k}^{a} + w_{i,j,k}^{d}))}}
#'
#' \deqn{w_{b} = 1 + z_2 * w_{b'}}
#'
#' \deqn{w_{b'} = \sum{\\(|w_{adversative\,conjunction}|, ..., w_{i, j, k}^{p}, w_{i, j, k}^{p}, ..., |w_{adversative\,conjunction}| * -1}\\)}
#'
#' \deqn{w_{neg}= \left(\sum{w_{i,j,k}^{n}}\right) \bmod {2}}
#'
#' @importFrom data.table := .N .SD
## @importFrom lexicon hash_sentiment_jockers_rinker
#' @examples
#' mytext <- c(
#'    'do you like it?  But I hate really bad dogs',
#'    'I am the best friend.',
#'    "Do you really like it?  I'm not a fan",
#'    "It's like a tree."
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `sentiment` is run.  For small batches the loss is minimal.
#' \dontrun{
#' sentiment(mytext)
#' }
#' 
#' ## preferred method avoiding paying the cost 
#' mytext <- get_sentences(mytext)
#' sentiment(mytext)
#' sentiment(mytext, question.weight = 0)
#'
#' sam_dat <- get_sentences(gsub("Sam-I-am", "Sam I am", sam_i_am))
#' (sam <- sentiment(sam_dat))
#' plot(sam)
#' plot(sam, scale_range = TRUE, low_pass_size = 5)
#' plot(sam, scale_range = TRUE, low_pass_size = 10)
#'     
#' \dontrun{## legacy transform functions from suuzhet
#' plot(sam, transformation.function = syuzhet::get_transformed_values)
#' plot(sam, transformation.function = syuzhet::get_transformed_values,  
#'     scale_range = TRUE, low_pass_size = 5)
#' }
#' 
#' y <- get_sentences(
#'     "He was not the sort of man that one would describe as especially handsome."
#' )
#' sentiment(y)
#' sentiment(y, n.before=Inf)
#' 
#' \dontrun{## Categorize the polarity (tidyverse vs. data.table):
#' library(dplyr)
#' sentiment(mytext) %>%
#' as_tibble() %>%
#'     mutate(category = case_when(
#'         sentiment < 0 ~ 'Negative', 
#'         sentiment == 0 ~ 'Neutral', 
#'         sentiment > 0 ~ 'Positive'
#'     ) %>%
#'     factor(levels = c('Negative', 'Neutral', 'Positive'))
#' )
#' 
#' library(data.table)
#' dt <- sentiment(mytext)[, category := factor(fcase(
#'         sentiment < 0, 'Negative', 
#'         sentiment == 0, 'Neutral', 
#'         sentiment > 0, 'Positive'
#'     ), levels = c('Negative', 'Neutral', 'Positive'))][]
#' dt
#' }
#' 
#' dat <- data.frame(
#'     w = c('Person 1', 'Person 2'),
#'     x = c(paste0(
#'         "Mr. Brown is nasty! He says hello. i give him rage.  i will ",
#'         "go at 5 p. m. eastern time.  Angry thought in between!go there"
#'     ), "One more thought for the road! I am going now.  Good day and good riddance."),
#'     y = state.name[c(32, 38)], 
#'     z = c(.456, .124),
#'     stringsAsFactors = FALSE
#' )
#' sentiment(get_sentences(dat$x))
#' sentiment(get_sentences(dat))
#' 
#' \dontrun{
#' ## tidy approach
#' library(dplyr)
#' library(magrittr)
#' 
#' hu_liu_cannon_reviews %>%
#'    mutate(review_split = get_sentences(text)) %$%
#'    sentiment(review_split)
#' }
#' 
#' ## Emojis
#' \dontrun{
#' ## Load R twitter data
#' x <- read.delim(system.file("docs/r_tweets.txt", package = "textclean"), 
#'     stringsAsFactors = FALSE)
#' 
#' x
#' 
#' library(dplyr); library(magrittr)
#' 
#' ## There are 2 approaches
#' ## Approach 1: Replace with words
#' x %>%
#'     mutate(Tweet = replace_emoji(Tweet)) %$%
#'     sentiment(Tweet)
#' 
#' ## Approach 2: Replace with identifier token
#' combined_emoji <- update_polarity_table(
#'     lexicon::hash_sentiment_jockers_rinker,
#'     x = lexicon::hash_sentiment_emojis
#' )
#' 
#' x %>%
#'     mutate(Tweet = replace_emoji_identifier(Tweet)) %$%
#'     sentiment(Tweet, polarity_dt = combined_emoji)
#'     
#' ## Use With Non-ASCII
#' ## Warning: sentimentr has not been tested with languages other than English.
#' ## The example below is how one might use sentimentr if you believe the 
#' ## language you are working with are similar enough in grammar to for
#' ## sentimentr to be viable (likely Germanic languages)
#' ## english_sents <- c(
#' ##     "I hate bad people.",
#' ##     "I like yummy cookie.",
#' ##     "I don't love you anymore; sorry."
#' ## )
#' 
#' ## Roughly equivalent to the above English
#' danish_sents <- stringi::stri_unescape_unicode(c(
#'     "Jeg hader d\\u00e5rlige mennesker.", 
#'     "Jeg kan godt lide l\\u00e6kker is.", 
#'     "Jeg elsker dig ikke mere; undskyld."
#' ))
#' 
#' danish_sents
#' 
#' ## Polarity terms
#' polterms <- stringi::stri_unescape_unicode(
#'     c('hader', 'd\\u00e5rlige', 'undskyld', 'l\\u00e6kker', 'kan godt', 'elsker')
#' )
#' 
#' ## Make polarity_dt
#' danish_polarity <- as_key(data.frame(
#'     x = stringi::stri_unescape_unicode(polterms),
#'     y = c(-1, -1, -1, 1, 1, 1)
#' ))
#' 
#' ## Make valence_shifters_dt
#' danish_valence_shifters <- as_key(
#'     data.frame(x='ikke', y="1"), 
#'     sentiment = FALSE, 
#'     comparison = NULL
#' )
#' 
#' sentiment(
#'     danish_sents, 
#'     polarity_dt = danish_polarity, 
#'     valence_shifters_dt = danish_valence_shifters, 
#'     retention_regex = "\\d:\\d|\\d\\s|[^\\p{L}',;: ]"
#' )
#' 
#' ## A way to test if you need [:alpha:] vs \\p{L} in `retention_regex`:
#' 
#' ## 1. Does it wreck some of the non-ascii characters by default?
#' sentimentr:::make_sentence_df2(danish_sents) 
#' 
#' ## 2.Does this?
#' sentimentr:::make_sentence_df2(danish_sents, "\\d:\\d|\\d\\s|[^\\p{L}',;: ]")
#' 
#' ## If you answer yes to #1 but no to #2 you likely want \\p{L}
#' }
sentiment <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
    valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
    adversative.weight = .25, neutral.nonverb.like = FALSE, missing_value = 0, 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){
    
    UseMethod('sentiment')
    
}



#' @export
#' @method sentiment get_sentences_character
sentiment.get_sentences_character <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
    valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
    adversative.weight = .25, neutral.nonverb.like = FALSE, missing_value = 0, 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){

    sentences <- id2 <- pol_loc <- comma_loc <- P <- non_pol <- lens <-
            cluster_tag <- w_neg <- neg <- A <- a <- D <- d <- wc <- id <-
            T_sum <- N <- . <- b <- before <- NULL

    ## check to ake sure valence_shifters_dt polarity_dt are mutually exclusive
    if(any(valence_shifters_dt[[1]] %in% polarity_dt[[1]])) {
        stop('`polarity_dt` & `valence_shifters_dt` not mutually exclusive')
    }

    ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
    ## that have spaces
    posneg <- polarity_dt[[1]]
    words <- c(posneg, valence_shifters_dt[[1]])
    space_words <-  words[grep("\\s", words)]

    sents <- text.var
    
    # break rows into count words
    sent_dat <- make_sentence_df2(sents, retention_regex = retention_regex)
    # buts <- valence_shifters_dt[valence_shifters_dt[[2]] == 4,][['x']]
    # 
    # if (length(buts) > 0){
    #     buts <- paste0('(', paste(buts, collapse = '|'), ')')
    #     sent_dat[, sentences := gsub(buts, ', \\1', sentences, ignore.case = TRUE, perl = TRUE)][]
    # }

    ## replace like when preposition
    if (neutral.nonverb.like && 'like' %in% polarity_dt[[1]]) {
       
        sent_dat[, 'sentences' := stringi::stri_replace_all_regex(
                sentences,
                like_preverbs_regex,
                '$1 $1$3',
                opts_regex = stringi::stri_opts_regex(case_insensitive=TRUE)
            )
        ][]
    }
 
    # space fill (~~), break into words 
    if (length(space_words) == 0){
        sent_dat[, 'words' := list(make_words(sentences, hyphen = hyphen))]
    } else {
        sent_dat[, 'words' := list(make_words(space_fill_senti(sentences, space_words), hyphen = hyphen))]
    }
    
    # make sentence id for each row id
    sent_dat[, id2:=seq_len(.N), by='id']

    ## Make the data frame long by stretching out words in sentences
    word_dat <- sent_dat[, .(words = unlist(words)), by = c('id', 'id2')]

    ## 1. add polarity word potential locations (seq along) and the
    ##    value for polarized word and polarity value = P
    ## 2. add comma locations
    word_dat[, pol_loc := seq_len(.N), by=c('id', 'id2')]
    word_dat[, comma_loc := pol_loc]
    word_dat[, "P"] <- polarity_dt[word_dat[["words"]]][[2]]
    word_dat[, pol_loc := ifelse(is.na(P), NA, pol_loc)]
    word_dat[, comma_loc := ifelse(words %in% c(';', ':',  ','), comma_loc, NA)]

    ## Get position of polarized word (hits = pol_loc)
    ## Get length of words vect
    word_dat <- word_dat[, .(words=list(words), lens = sum(!is.na(pol_loc)), pol_loc=list(rm_na(pol_loc)),
    	comma_loc=list(rm_na(comma_loc)), P= list(rm_na(P))), by = c('id', 'id2')][, 
    	    lens := ifelse(lens == 0, 1, lens)][]

    ## Unlist the pol_loc and P repeating everything else
    # word_dat <- cbind(
    #     word_dat[rep(seq_len(nrow(word_dat)), lens), .SD, .SD = c('id', 'id2', 'words', 'comma_loc')],
    #     word_dat[, .(pol_loc = unlist(pol_loc),
    # 	    P = unlist(P), lens = sapply(words, length)), by = c('id', 'id2')][, id := NULL][, id2 := NULL]
    # )
    
    word_dat <- word_dat[, .(words, comma_loc, pol_loc = unlist(pol_loc),
	    P = unlist(P), lens = sapply(words, length)), by = c('id', 'id2')]

    ## Grab the cluster of non-polarity words (n.before/n.after taking into account comma locs)
    cols2 <- c('id', 'id2', 'pol_loc', 'P')
    word_dat <- word_dat[, non_pol :=  list(comma_reducer(words, comma_loc, pol_loc, lens, n.before, n.after))][,
    	list(words, non_pol, lens = sapply(words, length)), by = cols2]
  
    # ## stretch by prior polarized word hits ## removed 2017-12-06 [improper recycling]
    # word_dat <- suppressWarnings(word_dat[, .(words, pol_loc = unlist(pol_loc),
    # 	comma_loc = unlist(comma_loc), P = unlist(P),
    # 	lens = sapply(words, length)), by = c('id', 'id2')])
    # 
    # ## Grab the cluster of non-polarity words (n.before/n.after taking into account comma locs
    # cols2 <- c('id', 'id2', 'pol_loc', 'P')
    # word_dat <- word_dat[, non_pol :=  list(comma_reducer(words, comma_loc, pol_loc, lens, n.before, n.after))][,
    # 	list(words, non_pol, lens = sapply(words, length)), by = cols2]

    ## save just polarized data for later merge
    pol_dat <- word_dat[, c("id", "id2", "pol_loc", "P"), with=FALSE]


    ## grab just desired columns needed for valence shifters and stretch by words
    # word_dat <- word_dat[, .(non_pol = unlist(non_pol)), by = c("id", "id2", "pol_loc")][,
    #     before:=grepl('<B>$', non_pol)][, ## slower regex vs fixed solution
    #     non_pol:=gsub('<B>$', '', non_pol)]
    
    ## grab just desired columns needed for valence shifters and stretch by words 
    word_dat <- word_dat[, .(non_pol = unlist(non_pol)), by = c("id", "id2", "pol_loc")][, 
        before:=grepl('<B>', non_pol, fixed = TRUE)][,
        non_pol:=sub_str(non_pol, before)]    
   
    ## tag nonpol cluster as negator (1) , amplifier (2), or deamplifier (3)
    word_dat[, "cluster_tag"] <- valence_shifters_dt[word_dat[["non_pol"]]][[2]]
    
    # ## determine what words come before the polarized word
    # word_dat[, before := 1 - 2*cumsum(before), .(id, id2, pol_loc)]

    but_dat <- word_dat[cluster_tag == "4", list(
    	b =  before*sum2(cluster_tag %in% "4")),
        by = c("id", "id2", "pol_loc", "before")][, before := NULL][,
            list(b = 1 + adversative.weight*sum2(b)), by = c("id", "id2", "pol_loc")]
    
## Note that an imrpovement to accuracy could be gained by dropping all words 
##   before adv. conj that comes before pol word or all words after a adv. 
##   conj. that comes after a pol word.  The trade off is speed....
    
    ## Get counts of negators (neg), amplifiers (a), and deamplifiers (d)
    ## neg is changed to a simple 0/1 if it flips the sign or not
    word_dat <- word_dat[, list(
    	neg =  sum2(cluster_tag %in% "1"),
    	a =  sum2(cluster_tag %in% "2"),
    	d =  sum2(cluster_tag %in% "3")), by = c("id", "id2", "pol_loc")]

    word_dat <- merge(word_dat, but_dat, by = c("id", "id2", "pol_loc"), all.x = TRUE)

    ## calculate the overall +/- shift of the poalrized word by summing the negators
    word_dat[, w_neg := neg %% 2]

    ## merge original word counts, polarized word scores, & valence shifter scores
    merge1 <-  merge(pol_dat, sent_dat[,  c("id", "id2", "wc"), with=FALSE],
        by = c("id", "id2"))

    data.table::setkey(merge1, "id", "id2", "pol_loc")
    sent_dat <- merge(merge1, word_dat, by = c("id", "id2", "pol_loc")) 

    ## add the amplifier/deamplifier & total raw sentiment scores
    sent_dat[, 
        A :=  ((1  - w_neg) * a)* amplifier.weight][,
    	D := ((-w_neg)*a - d) * amplifier.weight][, 
    	    
        A := A + ifelse(!is.na(b) & b > 1, b, 0)][,
    	D := D - ifelse(!is.na(b) & b == 1, b, 0)][,     	    
    	    
    	D := ifelse(D <= -1, -.999, D)][,
        T := (1 + c(A + D))*(P*((-1)^(2 + w_neg)))
    ]

    ## Aggregate (sum) at the sentence level
    sent_dat <- sent_dat[, list(T_sum=sum(T), N = unique(wc)), by=list(id, id2)]

    ## Finish by dividing sentiment raw score by sqrt of word count
    sent_dat[, sentiment := T_sum/sqrt(N)][, sentiment := ifelse(is.na(sentiment) & N > 0, 0, sentiment)]

    ## weight questions if weight not set to 1
    if (question.weight != 1) {
        q_locs <- stringi::stri_detect_regex(unlist(sents), "\\?\\s*$")
        q_locs[is.na(q_locs)] <- FALSE
        sent_dat[q_locs, "sentiment" := sentiment*question.weight]
    }

    # By sentence
    out <- stats::setNames(sent_dat[, c("id", "id2", "N", "sentiment"), with = FALSE],
        c("element_id", "sentence_id", "word_count", "sentiment"))

    if (!is.null(missing_value)){
        out[, 'sentiment' := replace_na(sentiment, y = missing_value)]
    }

    class(out) <- unique(c("sentiment", class(out)))
    sentences <- new.env(FALSE)
    sentences[["sentences"]] <- sents
    attributes(out)[["sentences"]] <- sentences
    out[]
}

sub_str <- function(x, locs) {
    #locs <- grepl('<B>', x, fixed = TRUE)
    x[locs] <- substring(x[locs], 1, nchar(x[locs]) - 3)
    x
}

like_preverbs <- c("'s", 'was', 'is', 'has', 'am', 'are', "'re", 'had', 'been')
like_preverbs_regex <- paste0('\\b(', paste(like_preverbs, collapse = '|'), ')(\\s+)(like\\b)')


#' @export
#' @method sentiment character
sentiment.character <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
    valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
    adversative.weight = .25, neutral.nonverb.like = FALSE, missing_value = 0, 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){

    split_warn(text.var, 'sentiment', ...)
    
    sents <- get_sentences(text.var)
    sentiment(sents, polarity_dt = polarity_dt, 
        valence_shifters_dt = valence_shifters_dt, hyphen = hyphen,
        amplifier.weight = amplifier.weight, n.before = n.before, 
        n.after = n.after, question.weight = question.weight,
        adversative.weight = adversative.weight, missing_value = missing_value, 
        retention_regex = retention_regex,
        neutral.nonverb.like = neutral.nonverb.like,  ...)
  
}

#' @export
#' @method sentiment get_sentences_data_frame
sentiment.get_sentences_data_frame <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
    valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
    adversative.weight = .25, neutral.nonverb.like = FALSE, missing_value = 0, 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){
 
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], "get_sentences", "get_sentences_character")

    sent_out <- sentiment(x, polarity_dt = polarity_dt, 
            valence_shifters_dt = valence_shifters_dt, hyphen = hyphen,
            amplifier.weight = amplifier.weight, n.before = n.before, 
            n.after = n.after, question.weight = question.weight,
            adversative.weight = adversative.weight, missing_value = missing_value, 
            neutral.nonverb.like = neutral.nonverb.like, retention_regex = retention_regex, ...)
    
    out <- cbind(text.var, sent_out[, c('word_count',  'sentiment')])

    class(out) <- unique(c("sentiment", class(out)))
    sentences <- new.env(FALSE)
    sentences[["sentences"]] <- x
    attributes(out)[["sentences"]] <- sentences
    out[]      
}

replace_na <- function(x, y = 0) {x[is.na(x)] <- y; x}

#' Plots a sentiment object
#'
#' Plots a sentiment object.
#'
#' @param x The sentiment object.
#' @param transformation.function A transformation function to smooth the sentiment
#' scores.
#' @param \ldots Other arguments passed to \code{\link[syuzhet]{get_transformed_values}}.
#' @details Utilizes Matthew Jocker's \pkg{syuzhet} package to calculate smoothed
#' sentiment across the duration of the text.
#' @return Returns a \pkg{ggplot2} object.
#' @method plot sentiment
#' @importFrom syuzhet get_dct_transform
#' @export
plot.sentiment <- function(x, transformation.function = syuzhet::get_dct_transform, ...){

    x <- stats::na.omit(x[["sentiment"]])
    
    if (length(x) < 3) stop('Output contains less than 3 observations.  Cannot plot n < 3', call. = FALSE)
    
    if (length(x) < 100 && isTRUE(all.equal(syuzhet::get_dct_transform, transformation.function))) {
        x <- stats::approx(x = seq_along(x), y = x, n = 100)[['y']]
    }    
    
    m <- transformation.function(x, ...)

    dat <- data.frame(
        Emotional_Valence = m,
        Duration = seq_along(m)
    )

    ggplot2::ggplot(dat, ggplot2::aes_string('Duration', 'Emotional_Valence')) +
        ggplot2::geom_path(size=1, color="blue") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.margin = grid::unit(c(5.1, 15.1, 4.1, 2.1), "pt")) +
        ggplot2::ylab("Emotional Valence") +
        ggplot2::theme(panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(label=function(x) paste0(x, "%"),
            expand = c(0,0), limits = c(0,100))

}

 