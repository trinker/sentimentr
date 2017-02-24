#' Polarity Score (Sentiment Analysis)
#'
#' Approximate the sentiment (polarity) of text by sentence.
#'
#' @param text.var The text variable.
#' @param polarity_dt A \pkg{data.table} of positive/negative words and
#' weights with x and y as column names.
#' @param valence_shifters_dt A \pkg{data.table} of valence shifters that
#' can alter a polarized word's meaning and an integer key for negators (1),
#' amplifiers(2), and de-amplifiers (3) with x and y as column names.
#' @param hyphen The character string to replace hyphens with.  Default replaces
#' with nothing so 'sugar-free' becomes 'sugarfree'.  Setting \code{hyphen = " "}
#' would result in a space between words (e.g., 'sugar free').
#' @param amplifier.weight The weight to apply to amplifiers/de-amplifiers (values
#' from 0 to 1).  This value will multiply the polarized terms by 1 + this
#' value.
#' @param n.before The number of words to consider as valence shifters before
#' the polarized word.  To consider the entire beginning portion of a sentence
#' use \code{n.before = Inf}.
#' @param n.after The number of words to consider as valence shifters after
#' the polarized word.  To consider the entire ending portion of a sentence
#' use \code{n.after = Inf}.
#' @param question.weight The weighting of questions (values from 0 to 1).
#' Default is 1.  A 0 corresponds with the belief that questions (pure questions)
#' are not polarized.  A weight may be applied based on the evidence that the
#' questions function with polarized sentiment.
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
#' @param missing_value A value to replace \code{NA}/\code{NaN} with.  Use
#' \code{NULL} to retain missing values.
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
#' Halliday, M. A. K. & Hasan, R. (2013). Cohesion in English. New York, NY: Routledge.
#' 
#' \url{http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
#'
#' \url{http://hedonometer.org/papers.html} Links to papers on hedonometrics
#' @keywords sentiment, polarity
#' @export
#' @family sentiment functions
#' @seealso \url{https://github.com/trestletech/Sermon-Sentiment-Analysis}
#' @note The polarity score is dependent upon the polarity dictionary used.
#' This function defaults to the word polarity dictionary used by Hu, M., &
#' Liu, B. (2004), however, this may not be appropriate for the context of
#' children in a classroom.  The user may (is encouraged) to provide/augment the
#' dictionary (see the \code{as_key} function).  For instance the word
#' "sick" in a high school setting may mean that something is good, whereas
#' "sick" used by a typical adult indicates something is not right or negative
#' connotation (\strong{deixis}).
#' @details The equation used by the algorithm to assign value to polarity of
#' each sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to
#' tag polarized words.  Each paragraph
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
#' modified version of Hu, M., & Liu, B.'s (2004) dictionary of polarized words.
#' Positive (\eqn{w_{i,j,k}^{+}}{w_i,j,k^+}) and negative
#' (\eqn{w_{i,j,k}^{-}}{w_i,j,k^-}) words are tagged with a \eqn{+1} and \eqn{-1}
#' respectively.  I will denote polarized words as \eqn{pw} for convenience. These
#' will form a polar cluster (\eqn{c_{i,j,l}}{c_i,j,l}) which is a subset of the a
#' sentence (\eqn{c_{i,j,l} \subseteq s_i,j }{l_i,j,l \subseteq s_i,j}).
#'
#' The polarized context cluster (\eqn{c_{i,j,l}}) of words is pulled from around
#' the polarized word (\eqn{pw}) and defaults to 4 words before and two words
#' after \eqn{pw}) to be considered as valence shifters.  The cluster can be represented as
#' (\eqn{c_{i,j,l} = \{pw_{i,j,k - nb}, ..., pw_{i,j,k} , ..., pw_{i,j,k - na}\}}{c_i,j,l = \{pw_i,j,k - nb, ..., pw_i,j,k , ..., pw_i,j,k - na\}}),
#' where \eqn{nb} & \eqn{na} are the parameters \code{n.before} and \code{n.after}
#' set by the user.  The words in this polarized context cluster are tagged as
#' neutral (\eqn{w_{i,j,k}^{0}}{w_i,j,k^0}), negator (\eqn{w_{i,j,k}^{n}}{w_i,j,k^n}),
#' amplifier (\eqn{w_{i,j,k}^{a}}{w_i,j,k^a}), or de-amplifier
#' (\eqn{w_{i,j,k}^{d}}{w_i,j,k^d}). Neutral words hold no value in the equation but
#' do affect word count (\eqn{n}).  Each polarized word is then weighted (\eqn{w})
#' based on the weights from the \code{polarity_dt} argument and then further
#' weighted by the function and number of the valence shifters directly surrounding the
#' positive or negative word (\eqn{pw}).  Pause (\eqn{cw}) locations
#' (punctuation that denotes a pause including commas, colons, and semicolons)
#' are indexed and considered in calculating the upper and lower bounds in the
#' polarized context cluster. This is because these marks indicate a change in
#' thought and words prior are not necessarily connected with words after these
#' punctuation marks.  The lower bound of the polarized context cluster is
#' constrained to \eqn{\max \{pw_{i,j,k - nb}, 1, \max \{cw_{i,j,k} < pw_{i,j,k}\}\}} and the upper bound is
#' constrained to \eqn{\min \{pw_{i,j,k + na}, w_{i,jn}, \min \{cw_{i,j,k} > pw_{i,j,k}\}\}}
#' where \eqn{w_{i,jn}} is the number of words in the sentence.
#'
#' The core value in the cluster, the polarized word is acted uppon by valence
#' shifters. Amplifiers increase the polarity by 1.8 (.8 is the default weight
#' (\eqn{z})).  Amplifiers (\eqn{w_{i,j,k}^{a}}) become de-amplifiers if the context
#' cluster contains an odd number of negators (\eqn{w_{i,j,k}^{n}}).  De-amplifiers
#' work to decrease the polarity.  Negation (\eqn{w_{i,j,k}^{n}}) acts on
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
#' summed (\eqn{c'_{i,j}}{c'_i,j}) and divided by the square root of the word count (\eqn{\sqrt{w_{i,jn}}}{\sqrtn w_i,jn}) yielding an unbounded
#' polarity score (\eqn{\delta}{C}) for each sentence.
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
#' (sam <- sentiment(gsub("Sam-I-am", "Sam I am", sam_i_am)))
#' plot(sam)
#' plot(sam, scale_range = TRUE, low_pass_size = 5)
#' plot(sam, scale_range = TRUE, low_pass_size = 10)
#' y <- "He was not the sort of man that one would describe as especially handsome."
#' sentiment(y)
#' sentiment(y, n.before=Inf)
sentiment <- function(text.var, polarity_dt = lexicon::hash_sentiment,
    valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
    adversative.weight = .85, missing_value = 0, ...){

    sentences <- id2 <- pol_loc <- comma_loc <- P <- non_pol <- lens <-
            cluster_tag <- w_neg <- neg <- A <- a <- D <- d <- wc <- id <-
            T_sum <- N <- . <- b <- before <- NULL

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
    sents <- get_sents(gsub("(\\s*)([;:,]+)", " \\2", text.var))
    sent_dat <- make_sentence_df2(sents)
    sent_dat[, 'words' := list(make_words(space_fill(sentences, space_words), hyphen = hyphen))]

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
    word_dat <- word_dat[, non_pol :=  list(comma_reducer(words, comma_loc, pol_loc, lens, n.before, n.after))][,
    	list(words, non_pol, lens = sapply(words, length)), by = cols2]

    ## save just polarized data for later merge
    pol_dat <- word_dat[, c("id", "id2", "pol_loc", "P"), with=FALSE]

    ## grab just desired columns needed for valence shifters and stretch by words
    word_dat <- word_dat[, .(non_pol = unlist(non_pol)), by = c("id", "id2", "pol_loc")]

    ## tag nonpol cluster as negator (1) , amplifier (2), or deamplifier (3)
    word_dat[, "cluster_tag"] <- valence_shifters_dt[word_dat[["non_pol"]]][[2]]
    word_dat[, before := 1 - 2*cumsum(non_pol == "*"), .(id, id2, pol_loc)]

    but_dat <- word_dat[cluster_tag == "4", list(
    	b =  before*sum2(cluster_tag %in% "4")),
        by = c("id", "id2", "pol_loc", "before")][, before := NULL][,
            list(b = 1 + adversative.weight*sum2(b)), by = c("id", "id2", "pol_loc")]

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
    sent_dat <- merge(merge(pol_dat, sent_dat[,  c("id", "id2", "wc"), with=FALSE],
        by = c("id", "id2")),	word_dat, by = c("id", "id2", "pol_loc"))

    ## add in the adversative weights
    sent_dat[, a := a + ifelse(!is.na(b) & b > 1, b, 0)]
    sent_dat[, d := d + ifelse(!is.na(b) & b < 1, b, 0)]

    ## add the amplifier/deamplifier & total raw sentiment scores
    sent_dat[, A :=  ((1  - w_neg) * a)* amplifier.weight][,
    	D := ((-w_neg)*a - d) * amplifier.weight][, D := ifelse(D < -1, -1, D)][,
    		T := (1 + c(A + D))*(P*((-1)^(2 + w_neg)))]

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

replace_na <- function(x, y = 0) {x[is.na(x)] <- y; x}

#' Plots a sentiment object
#'
#' Plots a sentiment object.
#'
#' @param x The sentiment object.
#' @param \ldots Other arguments passed to \code{\link[syuzhet]{get_transformed_values}}.
#' @details Utilizes Matthew Jocker's \pkg{syuzhet} package to calculate smoothed
#' sentiment across the duration of the text.
#' @return Returns a \pkg{ggplot2} object.
#' @method plot sentiment
#' @export
plot.sentiment <- function(x, ...){

    m <- syuzhet::get_transformed_values(stats::na.omit(x[["sentiment"]]), ...)

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

