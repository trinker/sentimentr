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
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} with columns of positive and 
#' negative terms.
#' @export
#' @importFrom data.table .N :=
#' @examples
#' library(data.table)
#' set.seed(10)
#' x <- sample(cannon_reviews[[3]], 1000, TRUE)
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
extract_sentiment_terms  <- function(text.var, polarity_dt = lexicon::hash_sentiment_huliu,
    hyphen = "", ...){

    sentences <- sentence <- sentence_id <- P <- polarity <- n <- words <- N <- . <- NULL

    ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
    ## that have spaces
    posneg <- polarity_dt[[1]]
    words <- posneg    
    space_words <-  words[grep("\\s", words)]

    # break rows into sentences, count words
    # space fill (~~), break into words
    sents <- get_sents(gsub("(\\s*)([;:,]+)", " \\2", text.var))
    sent_dat <- make_sentence_df2(sents)
    sent_dat[, 'words' := list(make_words(space_fill(sentences, space_words), hyphen = hyphen))]

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
    class(out) <- unique(c("extract_sentiment_terms", class(out)))

    attributes(out)[["counts"]] <- out_prime[, list(n = .N), by = c("polarity", "words")][order(-polarity, -n), ]
    data.table::setcolorder(attributes(out)[["counts"]], c('words', 'polarity', 'n'))

    attributes(out)[["elements"]] <- out_prime

    out
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
