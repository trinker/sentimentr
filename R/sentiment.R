#' Polarity Score (Sentiment Analysis)
#'
#' Approximate the sentiment (polarity) of text by sentence.
#'
#' @param text.var The text variable.
#' @param polarity_dt A \pkg{data.table} of positive/negative words and
#' weights with x and y as column names..
#' @param valence_shifters_dt A \pkg{data.table} of valence shifters that
#' can alter a polarized word's meaning and a numic key for negators (1),
#' amplifiers(2), and de-amplifiers (3) with x and y as column names.
#' @param amplifier.weight The weight to apply to amplifiers/deamplifiers (values
#' from 0 to 1).  This value will multiply the polarized terms by 1 + this
#' value.
#' @param n.before The number of words to consider as valence shifters before
#' the polarized word.
#' @param n.after The number of words to consider as valence shifters after
#' the polarized word.
#' @param question.weight The weighting of questions (values from 0 to 1).
#' Default is 1.  A 0 corresponds with the belief that questions (pure questions)
#' are not polarized.  A weight may be applied based on the evidence that the
#' questions function with polarized sentiment.
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} of:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{sentiment}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
#'   \item  word_count - Word count
#'   \item  sentiment - Sentiment/polarity score
#' }
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer
#' reviews. National Conference on Artificial Intelligence.
#'
#' \url{http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
#'
#' \url{http://hedonometer.org/papers.html} Links to papers on hedonometrics
#' @keywords sentiment, polarity
#' @export
#' @seealso \url{https://github.com/trestletech/Sermon-Sentiment-Analysis}
#' @note The polarity score is dependent upon the polarity dictionary used.
#' This function defaults to the word polarity dictionary used by Hu, M., &
#' Liu, B. (2004), however, this may not be appropriate for the context of
#' children in a classroom.  The user may (is encouraged) to provide/augment the
#' dictionary (see the \code{sentiment_frame} function).  For instance the word
#' "sick" in a high school setting may mean that something is good, whereas
#' "sick" used by a typical adult indicates something is not right or negative
#' connotation (\strong{deixis}).
#' @details The equation used by the algorithm to assign value to polarity of
#' each sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to
#' tag polarized words.  A context cluster (\eqn{x_i^{T}}{x_i^T}) of words is
#' pulled from around this polarized word (default 4 words before and two words
#' after) to be considered as valence shifters.  The words in this context
#' cluster are tagged as neutral (\eqn{x_i^{0}}{x_i^0}), negator
#' (\eqn{x_i^{N}}{x_i^N}), amplifier (\eqn{x_i^{a}}{x_i^a}), or de-amplifier
#' (\eqn{x_i^{d}}{x_i^d}). Neutral words hold no value
#' in the equation but do affect word count (\eqn{n}).  Each polarized word is
#' then weighted \eqn{w} based on the weights from the \code{polarity.frame}
#' argument and then further weighted by the number and position of the valence
#' shifters directly surrounding the positive or negative word.  The researcher
#' may provide a weight \eqn{c} to be utilized with amplifiers/de-amplifiers
#' (default is .8; deamplifier weight is constrained to -1 lower bound).  Last,
#' these context cluster (\eqn{x_i^{T}}{x_i^T}) are summed and divided by the
#' square root of the word count (\eqn{\sqrt{n}}{\sqrtn}) yielding an unbounded
#' polarity score (\eqn{\delta}{C}).  Note that context clusters containing a
#' comma before the polarized word will only consider words found after the
#' comma.
#'
#' \deqn{\delta=\frac{x_i^T}{\sqrt{n}}}{C=x_i^2/\sqrt(n)}
#'
#' Where:
#'
#' \deqn{x_i^T=\sum{((1 + c(x_i^{A} - x_i^{D}))\cdot w(-1)^{\sum{x_i^{N}}})}}{x_i^T=\sum((1 + c * (x_i^A - x_i^D)) * w(-1)^(\sumx_i^N))}
#'
#' \deqn{x_i^{A}=\sum{(w_{neg}\cdot x_i^{a})}}{x_i^A=\sum(w_neg * x_i^a)}
#'
#' \deqn{x_i^D = \max(x_i^{D'}, -1)}{x_i^D = max(x_i^D', -1)}
#'
#' \deqn{x_i^{D'}= \sum{(- w_{neg}\cdot x_i^{a} + x_i^{d})}}{x_i^D'=\sum(- w_neg * x_i^a + x_i^d)}
#'
#' \deqn{w_{neg}= \left(\sum{x_i^{N}}\right) \bmod {2}}{w_neg= (\sumx_i^N) mod 2}
#'
#' @importFrom data.table := .N
#' @examples
#' mytext <- c(
#'    'do you like it?  But I hate really bad dogs',
#'    'I am the best friend.',
#'    'Do you really like it?  I\'m not a fan'
#' )
#' sentiment(mytext)
#' sentiment(mytext, question.weight = 0)
#'
#' sentiment(gsub("Sam-I-am", "Sam I am", sam_i_am))
sentiment <- function(text.var, polarity_dt = sentimentr::polarity_table,
    valence_shifters_dt = sentimentr::valence_shifters_table,
    amplifier.weight = .8, n.before = 4, n.after = 2, question.weight = 1, ...){

    sentences <- id2 <- pol_loc <- comma_loc <- P <- non_pol <- lens <-
            cluster_tag <- w_neg <- neg <- A <- a <- D <- d <- wc <- id <-
            T_sum <- N <- . <- NULL

    ## check to ake sure valence_shifters_dt polarity_dt are mutually exclusive
    if(any(valence_shifters_dt[[1]] %in% polarity_dt[[1]])) {
        stop('`polarity_frame` & `valence_shifters_dt` not mutually exclusive')
    }

    ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
    ## that have spaces
    posneg <- polarity_dt[[1]]
    words <- c(posneg, valence_shifters_dt[[1]])
    space_words <-  words[grep("\\s", words)]

    # break rows into sentences, count words
    # space fill (~~), break into words
    sents <- get_sents(text.var)
    sent_dat <- make_sentence_df2(sents)
    sent_dat[, 'words' := make_words(space_fill(sentences, space_words))]

    # make sentence id for each row id
    sent_dat[, id2:=seq_len(.N), by='id']

    ## Make the data frame long by stretching out words in sentences
    word_dat <- sent_dat[, .(words = unlist(words)), by = c('id', 'id2')]

    ## 1. add polarity word potential locations (seq along) and the
    ##    value for polarized word
    ## 2. add comma locations
    word_dat[, pol_loc:=seq_len(.N), by=c('id', 'id2')]
    word_dat[, comma_loc:=pol_loc]
    word_dat[, "P"] <- polarity_dt[word_dat[["words"]]][[2]]
    word_dat[, pol_loc:=ifelse(is.na(P), NA, pol_loc)]
    word_dat[, comma_loc:=ifelse(words %in% c(";", ":", ","), comma_loc, NA)]

    ## Get position of polarized word (hits = pol_loc)
    ## Get length of words vect
    word_dat <- word_dat[, .(words=list(words), pol_loc=list(rm_na(pol_loc)),
    	comma_loc=list(rm_na(comma_loc)), P= list(rm_na(P))), by = c('id', 'id2')]

    ## stretch by prior polarized word hits
    word_dat <- suppressWarnings(word_dat[, .(words, pol_loc = unlist(pol_loc),
    	comma_loc = unlist(comma_loc), P = unlist(P),
    	lens = sapply(words, length)), by = c('id', 'id2')])

    ## Grab the cluster of non-polarity words (n.before/n.after taking into account [,;:]
    cols2 <- c('id', 'id2', 'pol_loc', 'P')
    word_dat <- word_dat[, non_pol :=  comma_reducer(words, comma_loc, pol_loc, lens, n.before, n.after)][,
    	list(words, non_pol, lens = sapply(words, length)), by = cols2]

    ## save just polarized data for later merge
    pol_dat <- word_dat[, c("id", "id2", "pol_loc", "P"), with=FALSE]

    ## grabd just desired columns needed for valence shifters and stretch by words
    word_dat <- word_dat[, .(non_pol = unlist(non_pol)), by = c("id", "id2", "pol_loc")]

    ## tag nonpol cluster as negator (1) , amplifier (2), or deamplifier (3)
    word_dat[, "cluster_tag"] <- valence_shifters_dt[word_dat[["non_pol"]]][[2]]

    ## Get counts of negators (neg), amplifiers (a), and deamplifiers (d)
    ## neg is changed to a simple 0/1 if it flips the sign or not
    word_dat <- word_dat[, list(
    	neg =  sum2(cluster_tag %in% "1"),
    	a =  sum2(cluster_tag %in% "2"),
    	d =  sum2(cluster_tag %in% "3")), by = c("id", "id2", "pol_loc")]

     ## calculate the overall +/- shift of the poalrized word by summing the negators
     word_dat[, w_neg := neg %% 2]

     ## merge original word counts, polarized word scores, & valence shifter scores
     sent_dat <- merge(merge(pol_dat, sent_dat[,  c("id", "id2", "wc"), with=FALSE],
         by = c("id", "id2")),	word_dat, by = c("id", "id2", "pol_loc"))

     ## add the amplifier/deamplifier & total raw sentiment scores
     sent_dat[, A :=  ((1  - w_neg) * a)* amplifier.weight][,
     	D := ((-w_neg)*a - d) * amplifier.weight][, D := ifelse(D < -1, -1, D)][,
     		T := (1 + c(A + D))*(P*((-1)^(sum(neg))))]

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
     stats::setNames(sent_dat[, c("id", "id2", "N", "sentiment"), with = FALSE],
         c("element_id", "sentence_id", "word_count", "sentiment"))
}


abbr_rep <- lapply(list(
  Titles   = c('jr', 'mr', 'mrs', 'ms', 'dr', 'prof', 'sr', 'sen', 'rep',
         'rev', 'gov', 'atty', 'supt', 'det', 'rev', 'col','gen', 'lt',
         'cmdr', 'adm', 'capt', 'sgt', 'cpl', 'maj'),

  Entities = c('dept', 'univ', 'uni', 'assn', 'bros', 'inc', 'ltd', 'co',
         'corp', 'plc'),

  Months   = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul',
         'aug', 'sep', 'oct', 'nov', 'dec', 'sept'),

  Days     = c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun'),

  Misc     = c('vs', 'etc', 'no', 'esp', 'cf', 'al', 'mt'),

  Streets  = c('ave', 'bld', 'blvd', 'cl', 'ct', 'cres', 'dr', 'rd', 'st')
), function(x){
    fl <- sub("(^[a-z])(.+)", "\\1", x)
    sprintf("[%s%s]%s", fl, toupper(fl), sub("(^[a-z])(.+)", "\\2", x))
})

period_reg <- paste0(
    "(?:(?<=[a-z])\\.\\s(?=[a-z]\\.))",
        "|",
    "(?:(?<=([ .][a-z]))\\.)(?!(?:\\s[A-Z]|$)|(?:\\s\\s))",
        "|",
    "(?:(?<=[A-Z])\\.\\s(?=[A-Z]\\.))",
        "|",
    "(?:(?<=[A-Z])\\.(?=\\s[A-Z][A-Za-z]))"
)


sent_regex <- sprintf("((?<=\\b(%s))\\.)|%s|(%s)",
    paste(unlist(abbr_rep), collapse = "|"),
    period_reg,
	'\\.(?=\\d+)'
)
