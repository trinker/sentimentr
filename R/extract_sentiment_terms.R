#' Extract Sentiment Words
#' 
#' Extract the sentiment words from a text.
#' 
#' @param text.var The text variable.
#' @param polarity_dt A \pkg{data.table} of positive/negative words and
#' weights with x and y as column names.
#' @param hyphen The character string to replace hyphens with.  Default replaces
#' with nothing so 'sugar-free' becomes 'sugarfree'.  Setting \code{hyphen = " "}
#' would result in a space between words (e.g., 'sugar free').
#' @param retention_regex A regex of what characters to keep.  All other 
#' characters will be removed.  Note that when this is used all text is lower 
#' case format.  Only adjust this parameter if you really understand how it is 
#' used.  Note that swapping the \code{\\\\{p}} for \code{[^[:alpha:];:,\']} may 
#' retain more alpha letters but will likely decrease speed.
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} with columns of positive and 
#' negative terms.  In addition, the attributes \code{$counts} and \code{$elements}
#' return an aggregated count of the usage of the words and a detailed sentiment
#' score of each word use.  See the examples for more.
#' @export
#' @importFrom data.table .N :=
#' @examples
#' library(data.table)
#' set.seed(10)
#' x <- get_sentences(sample(hu_liu_cannon_reviews[[2]], 1000, TRUE))
#' sentiment(x)
#' 
#' pol_words <- extract_sentiment_terms(x)
#' pol_words
#' pol_words$sentence
#' pol_words$neutral
#' data.table::as.data.table(pol_words)
#' 
#' attributes(extract_sentiment_terms(x))$counts
#' attributes(extract_sentiment_terms(x))$elements
#' 
#' \dontrun{
#' library(wordcloud)
#' library(data.table)
#' 
#' set.seed(10)
#' x <- get_sentences(sample(hu_liu_cannon_reviews[[2]], 1000, TRUE))
#' sentiment_words <- extract_sentiment_terms(x)
#' 
#' sentiment_counts <- attributes(sentiment_words)$counts
#' sentiment_counts[polarity > 0,]
#' 
#' par(mfrow = c(1, 3), mar = c(0, 0, 0, 0))
#' ## Positive Words
#' with(
#'     sentiment_counts[polarity > 0,],
#'     wordcloud(words = words, freq = n, min.freq = 1,
#'           max.words = 200, random.order = FALSE, rot.per = 0.35,
#'           colors = brewer.pal(8, "Dark2"), scale = c(4.5, .75)
#'     )
#' )
#' mtext("Positive Words", side = 3, padj = 5)
#' 
#' ## Negative Words
#' with(
#'     sentiment_counts[polarity < 0,],
#'     wordcloud(words = words, freq = n, min.freq = 1,
#'           max.words = 200, random.order = FALSE, rot.per = 0.35,
#'           colors = brewer.pal(8, "Dark2"), scale = c(4.5, 1)
#'     )
#' )
#' mtext("Negative Words", side = 3, padj = 5)
#' 
#' sentiment_counts[, 
#'     color := ifelse(polarity > 0, 'red', 
#'         ifelse(polarity < 0, 'blue', 'gray70')
#'     )]
#' 
#' ## Positive & Negative Together
#' with(
#'     sentiment_counts[polarity != 0,],
#'     wordcloud(words = words, freq = n, min.freq = 1,
#'           max.words = 200, random.order = FALSE, rot.per = 0.35,
#'           colors = color, ordered.colors = TRUE, scale = c(5, .75)
#'     )
#' )
#' mtext("Positive (red) & Negative (blue) Words", side = 3, padj = 5)
#' }
extract_sentiment_terms  <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker,
    hyphen = "", retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){

    UseMethod('extract_sentiment_terms')   
}

#' @export
#' @method extract_sentiment_terms get_sentences_character
extract_sentiment_terms.get_sentences_character <- function(text.var, 
    polarity_dt = lexicon::hash_sentiment_jockers_rinker, hyphen = "", 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){

    sentences <- sentence <- sentence_id <- P <- polarity <- n <- words <- N <- . <- NULL

    ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
    ## that have spaces
    posneg <- polarity_dt[[1]]
    words <- posneg    
    space_words <-  words[grep("\\s", words)]

    # break rows into count words
    # space fill (~~), break into words
    sents <- text.var
   
    sent_dat <- make_sentence_df2(sents, retention_regex = retention_regex)
    sent_dat[, 'words' := list(make_words(space_fill_senti(sentences, space_words), hyphen = hyphen))]

    # make sentence id for each row id
    sent_dat[, sentence_id:=seq_len(.N), by='id']

    ## Make the data frame long by stretching out words in sentences
    word_dat <- sent_dat[, .(words = unlist(words)), by = c('id', 'sentence_id')]

    ## add polarity word potential locations (seq along) and the
    ## value for polarized word
    word_dat[, "P"] <- polarity_dt[word_dat[["words"]]][[2]]
    word_dat[, P := ifelse(is.na(P), 0, P)][]
    out_prime <- word_dat[!words %in% c(',', ''), ]

    data.table::setnames(out_prime, c('id', 'P'), c('element_id', 'polarity'))


    out <- data.table::dcast(
        out_prime[, list(senti = ifelse(polarity == 0, 'neutral', 
            ifelse(polarity < 0, 'negative', 'positive'))), by = c('element_id', 'sentence_id', 'words')], 
        element_id + sentence_id ~ senti, 
        list, value.var = 'words'
    )[, sentence := unlist(sents)][]
    
    nms <- colnames(out)
    checks <- c("negative", "neutral", "positive")
    missing <- checks[!checks %in% nms]
    
    if (length(missing) > 1){
        
        empty <- lapply(seq_len(nrow(out)), function(i) character(0))
        if ('negative' %in% missing) {
            out[['negative']] <- empty
        }
        if ('neutral' %in% missing) {
            out[['neutral']] <- empty
        }
        if ('positive' %in% missing) {
            out[['positive']] <- empty        
        }    
        data.table::setcolorder(out, c("element_id", "sentence_id", "negative", 
           "neutral", "positive", "sentence"))
        
        out <- out[]
        
    }    
    
    class(out) <- unique(c("extract_sentiment_terms", class(out)))

    attributes(out)[["counts"]] <- out_prime[, list(n = .N), by = c("polarity", "words")][order(-polarity, -n), ]
    data.table::setcolorder(attributes(out)[["counts"]], c('words', 'polarity', 'n'))

    attributes(out)[["elements"]] <- out_prime

    out
}

#' @export
#' @method extract_sentiment_terms get_sentences_data_frame
extract_sentiment_terms.get_sentences_data_frame  <- function(text.var, 
    polarity_dt = lexicon::hash_sentiment_jockers_rinker, hyphen = "", 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){
    
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], 
        "get_sentences", "get_sentences_character")

    extract_sentiment_terms(x, polarity_dt = polarity_dt, hyphen = hyphen, 
        retention_regex = retention_regex, ...)

}


#' @export
#' @method extract_sentiment_terms character
extract_sentiment_terms.character  <- function(text.var, 
    polarity_dt = lexicon::hash_sentiment_jockers_rinker, hyphen = "", 
    retention_regex = "\\d:\\d|\\d\\s|[^[:alpha:]',;: ]", ...){

    split_warn(text.var, 'extract_sentiment', ...)

    sents <- get_sentences(text.var)      
    extract_sentiment_terms(sents, polarity_dt = polarity_dt, hyphen = hyphen, 
        retention_regex = retention_regex, ...)

    
}

#' Prints an extract_sentiment_terms Object
#' 
#' Prints an extract_sentiment_terms object
#' 
#' @param x An extract_sentiment_terms object.
#' @param \ldots ignored
#' @method print extract_sentiment_terms
#' @export 
print.extract_sentiment_terms <- function(x, ...){

    print(rm_class(x, 'extract_sentiment_terms')[, !c('neutral', "sentence"), with=FALSE]  )
    
}
