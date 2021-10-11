#' Extract Emotion Words
#' 
#' Extract the emotion words from a text.
#' 
#' 
#' @param text.var The text variable.  Can be a \code{get_sentences} object or
#' a raw character vector though \code{get_sentences} is preferred as it avoids
#' the repeated cost of doing sentence boundary disambiguation every time
#' \code{emotion} is run.
#' @param emotion_dt A \pkg{data.table} with a \code{token} and \code{emotion}
#' column (\code{tokens} are nested within the \code{emotion}s.  The table
#' cannot contain any duplicate rows and must have the \code{token} column set
#' as the key column (see \code{?data.table::setkey}).  The default emotion
#' table is \code{lexicon::hash_nrc_emotions}.
#' @param un.as.negation logical.  If \code{TRUE} then emotion words prefixed
#' with an 'un-' are treated as a negation.  For example,\code{"unhappy"} would 
#' be treated as \code{"not happy"}.  If an emotion word has an un- version in the
#' \code{emotion_dt} then no substitution is performed and an optional warning
#' will be given. 
#' @param retention_regex A regex of what characters to keep.  All other 
#' characters will be removed.  Note that when this is used all text is lower 
#' case format.  Only adjust this parameter if you really understand how it is 
#' used.  Note that swapping the \code{\\\\{p}} for \code{[^[:alpha:];:,\']} may 
#' retain more alpha letters but will likely decrease speed.
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} with a columns of emotion terms.
#' @export
#' @importFrom data.table .N :=
#' @examples
#' \dontrun{
#' mytext <- c(
#'     "I am not afraid of you",
#'     NA,
#'     "",
#'     "I love it [not really]", 
#'     "I'm not angry with you", 
#'     "I hate it when you lie to me.  It's so humiliating",
#'     "I'm not happpy anymore.  It's time to end it",
#'     "She's a darn good friend to me",
#'     "I went to the terrible store",
#'     "There is hate and love in each of us",
#'     "I'm no longer angry!  I'm really experiencing peace but not true joy.",
#'     
#'     paste("Out of the night that covers me, Black as the Pit from pole to", 
#'       "pole, I thank whatever gods may be For my unconquerable soul.",
#'       "In the fell clutch of circumstance I have not winced nor cried",
#'       "aloud. Under the bludgeonings of chance My head is bloody, but unbowed.",
#'       "Beyond this place of wrath and tears Looms but the Horror of the", 
#'       "shade, And yet the menace of the years Finds, and shall find, me unafraid.",
#'       "It matters not how strait the gate, How charged with punishments", 
#'       "the scroll, I am the master of my fate: I am the captain of my soul."
#'     )    
#'     
#' )
#' 
#' mytext2 <- get_sentences(mytext)
#' emotion(mytext2)
#' 
#' emo_words <- extract_emotion_terms(mytext2)
#' emo_words
#' emo_words$sentence
#' emo_words[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
#' 
#' attributes(emo_words)$counts
#' attributes(emo_words)$elements
#' 
#' ## directly ona  character string (not recommended: use `get_sentences` first)
#' extract_emotion_terms(mytext)
#'
#' brady <- get_sentences(crowdflower_deflategate)
#' brady_emo <- extract_emotion_terms(brady)
#' 
#' brady_emo
#' attributes(brady_emo)$counts
#' attributes(brady_emo)$elements
#' }
extract_emotion_terms  <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    un.as.negation = TRUE, retention_regex = "[^[:alpha:];:,\']",
    ...){

    UseMethod('extract_emotion_terms')   
}

#' @export
#' @method extract_emotion_terms get_sentences_character
extract_emotion_terms.get_sentences_character <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    un.as.negation = TRUE, retention_regex = "[^[:alpha:];:,\']", 
    ...){

    sentences <- sentence <- sentence_id <- n <- words <- N <- . <- NULL
    token <- hit <- emotion_count <- word_count <- NULL


    emotion <- emo_loc <- comma_loc <- negator_loc <- is_emo <- y <- NULL
    is_negator <- is_negated <- emotion_count <- token <- hit <- word_count <- NULL
    
    ## Ensure emotion_dt conforms to standards
    is_emotion(emotion_dt)

    lens <- lengths(text.var)

    ## make table of elements, sentence id, and sentences
    element_map <- data.table::data.table(
        element_id = rep(seq_along(lens), lens),
        sentence_id = unlist(lapply(lens, seq_len)),
        token = unlist(text.var)
    )[, list(token = tolower(unlist(token))), by = c('element_id', 'sentence_id')]

    ## Chack for spaces in the emotion list to ensure the tokenizer keeps them
    space_words <-  emotion_dt[['token']][grep("\\s", emotion_dt[['token']])]

    ## Convert un prefix to not
    if (isTRUE(un.as.negation)){
        

        tokens <- unique(emotion_dt[['token']])
        uns <- paste0('un', tokens) 
        keeps <- tokens[tokens %in% uns]
        
        regex <- paste0('\\b(un)(', paste(keeps, collapse = '|'), ')\\b')
        element_map <- element_map[, token := stringi::stri_replace_all_regex(token, regex, 'not $2')][]
        
    }    
    
    ## count words, tokenize
    tidied <- element_map[,
        token := space_fill(token, space_words)][,
        token := stringi::stri_replace_all_regex(
                stringi::stri_replace_all_regex(
                    stringi::stri_replace_all_regex(token, '[!.;:?]$', ''), 
                    retention_regex, ' '), 
                '[;:,]\\s+', ' [;:,] ')]


    tidied[['token']] <- as.list(stringi::stri_split_regex(tidied[['token']], '\\s+'))
    
    tidied <- tidied[,
            list(token = stringi::stri_replace_all_regex(unlist(token), '~~', ' ')), 
                by =c('element_id', 'sentence_id')]
    
    
    data.table::setkey(tidied, 'token')

    ## merge to find the emotion words and count them (emotion is n of words that are emotional)
    counts_prime <- merge(tidied, emotion_dt, all.x=TRUE, allow.cartesian=TRUE)
    
    out_prime <- data.table::copy(counts_prime)[, 
        token := ifelse(is.na(emotion), NA, token)][]

    ## Make the counts data.tables that can be fed as attributes and used for things like wordclouds
    ## counts broad
    counts <- data.table::copy(counts_prime)[, 
        is_emotion := as.integer(!is.na(emotion))][][
            token != '[;:,]', list(n = .N), keyby = c('token', 'emotion', 'is_emotion')]
    
    data.table::setnames(counts, c('token', "emotion", 'is_emotion'), c('words', "emotion_type", "emotion"))
    
    ## counts element level
    elements <- data.table::copy(counts_prime)[, 
        is_emotion := as.integer(!is.na(emotion))][
            token != '[;:,]',][]
    
    data.table::setnames(elements, c('token', "emotion", 'is_emotion'), c('words', "emotion_type", "emotion"))
    
    
    ## compute main output
    out <- data.table::dcast(out_prime, element_id + sentence_id ~ emotion, 
        list, value.var = 'token')[, 'NA' := NULL][]
    
    nms <- colnames(out)
    checks <- sort(unique(emotion_dt[['emotion']]))
    missing <- checks[!checks %in% nms]
    
    if (length(missing) > 0){
        
        missed <- as.data.frame(lapply(missing, function(x) rep(character(0), nrow(out))))
        colnames(missed) <- missing
        out <- data.table::data.table(out, missed)
        
    }    
    
    out[['sentence']] <- unlist(text.var)

    class(out) <- unique(c("extract_emotion_terms", class(out)))
    attributes(out)[["counts"]] <- counts[order(-emotion, -n), ]
    attributes(out)[["elements"]] <- elements[order(-emotion, words), ]

    out
}

#' @export
#' @method extract_emotion_terms get_sentences_data_frame
extract_emotion_terms.get_sentences_data_frame  <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    un.as.negation = TRUE, retention_regex = "[^[:alpha:];:,\']", 
    ...){
    
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], 
        "get_sentences", "get_sentences_character")

    extract_emotion_terms(x, emotion_dt = emotion_dt, 
        un.as.negation = un.as.negation, retention_regex = retention_regex, ...)

}


#' @export
#' @method extract_emotion_terms character
extract_emotion_terms.character  <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    un.as.negation = TRUE, retention_regex = "[^[:alpha:];:,\']", 
    ...){
    
    split_warn(text.var, 'extract_emotion', ...)

    sents <- get_sentences(text.var)      
    extract_emotion_terms(sents, emotion_dt = emotion_dt, 
        un.as.negation = un.as.negation, retention_regex = retention_regex, ...)

    
}

#' Prints an extract_emotion_terms Object
#' 
#' Prints an extract_emotion_terms object
#' 
#' @param x An extract_emotion_terms object.
#' @param \ldots ignored
#' @method print extract_emotion_terms
#' @export 
print.extract_emotion_terms <- function(x, ...){

    print(rm_class(x, 'extract_emotion_terms') )
    
}


