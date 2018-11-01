#' Extract Profanity Words
#' 
#' Extract the profanity words from a text.
#' 
#' 
#' @param text.var The text variable.  Can be a \code{get_sentences} object or
#' a raw character vector though \code{get_sentences} is preferred as it avoids
#' the repeated cost of doing sentence boundary disambiguation every time
#' \code{profanity} is run.
#' @param profanity_list A atomic character vector of profane words.  The 
#' \pkg{lexicon} package has lists that can be used, including: 
#' \itemize{
#'   \item \code{lexicon::profanity_alvarez}
#'   \item \code{lexicon::profanity_arr_bad}
#'   \item \code{lexicon::profanity_banned}
#'   \item \code{lexicon::profanity_zac_anger}
#' }
#' @param \ldots Ignored.
#' @return Returns a \pkg{data.table} with a columns of profane terms.
#' @export
#' @importFrom data.table .N :=
#' @examples
#' \dontrun{
#' bw <- sample(lexicon::profanity_alvarez, 4)
#' mytext <- c(
#'    sprintf('do you %s like this %s?  It is %s. But I hate really bad dogs', bw[1], bw[2], bw[3]),
#'    'I am the best friend.',
#'    NA,
#'    sprintf('I %s hate this %s', bw[3], bw[4]),
#'    "Do you really like it?  I'm not happy"
#' )
#' 
#'
#' x <- get_sentences(mytext)
#' profanity(x)
#' 
#' prof_words <- extract_profanity_terms(x)
#' prof_words
#' prof_words$sentence
#' prof_words$neutral
#' prof_words$profanity
#' data.table::as.data.table(prof_words)
#' 
#' attributes(extract_profanity_terms(x))$counts
#' attributes(extract_profanity_terms(x))$elements
#'
#'
#' brady <- get_sentences(crowdflower_deflategate)
#' brady_swears <- extract_profanity_terms(brady)
#' 
#' attributes(extract_profanity_terms(brady))$counts
#' attributes(extract_profanity_terms(brady))$elements
#' }
extract_profanity_terms  <- function(text.var, 
    profanity_list = unique(tolower(lexicon::profanity_alvarez)),
    ...){

    UseMethod('extract_profanity_terms')   
}

#' @export
#' @method extract_profanity_terms get_sentences_character
extract_profanity_terms.get_sentences_character <- function(text.var, 
    profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...){

    sentences <- sentence <- sentence_id <- n <- words <- N <- . <- NULL
    token <- hit <- profanity_count <- word_count <- NULL


    ## Ensure profanity_list conforms to standards
    profanity_list <- fix_profanity_list(profanity_list)
        
    ## Add "~~" holder for any words `polarity_frame` & `valence_shifters_dt`
    ## that have spaces
    profanes <- data.table::data.table(token = profanity_list, profanity = TRUE)
    space_words <-  profanity_list[grep("\\s", profanity_list)]

    # break rows into count words
    # space fill (~~), break into words
    sents <- text.var

    lens <- lengths(text.var)


    element_map <- data.table::data.table(
        element_id = rep(seq_along(lens), lens),
        sentence_id = unlist(lapply(lens, seq_len)),
        token = unlist(text.var)
    )

    tidied <- element_map[, list(token = tolower(unlist(token))), by = c('element_id', 'sentence_id')][,
        word_count := count_words(token)][,
        token := space_fill(token, space_words)][,
        token := stringi::stri_replace_all_regex(
            stringi::stri_replace_all_regex(token, '[!.;:?]$', ''), '[;:,]\\s+', ' ')][,
        token := stringi::stri_split_regex(token, '\\s+')][,
        list(token = stringi::stri_replace_all_regex(unlist(token), '~~', ' ')), 
            by =c('element_id', 'sentence_id', 'word_count')][]

    out_prime <- out <- merge(tidied, profanes, all.x=TRUE)[, 
        profanity := ifelse(is.na(profanity), 'neutral', 'profanity')][]


    out <- data.table::dcast(out, element_id + sentence_id ~ profanity, 
        list, value.var = 'token')
    
    nms <- colnames(out)
    checks <- c("neutral", "profanity")
    missing <- checks[!checks %in% nms]
    
    if (length(missing) > 0){
        
        empty <- lapply(seq_len(nrow(out)), function(i) character(0))
        if ('neutral' %in% missing) {
            out[['neutral']] <- empty
        }
        if ('profanity' %in% missing) {
            out[['profanity']] <- empty        
        }    
        data.table::setcolorder(out, c("element_id", "sentence_id", 
           "neutral", "profanity", "sentence"))
        
        out <- out[]
        
    }    
    
    out[['sentence']] <- unlist(text.var)

    class(out) <- unique(c("extract_profanity_terms", class(out)))

    attributes(out)[["counts"]] <- out_prime[, 
        profanity := as.integer(profanity == 'profanity')][, 
        list(n = .N), by = c("profanity", "token")][order(-profanity, -n), ][]

    data.table::setnames(attributes(out)[["counts"]], old = "token", new = "words")
    data.table::setcolorder(attributes(out)[["counts"]], c('words', 'profanity', 'n'))

    attributes(out)[["elements"]] <- out_prime[, word_count := NULL][]
    data.table::setnames(attributes(out)[["elements"]], old = "token", new = "words")
    data.table::setcolorder(attributes(out)[["elements"]], c("element_id", "sentence_id", "words", 'profanity'))

    out
}

#' @export
#' @method extract_profanity_terms get_sentences_data_frame
extract_profanity_terms.get_sentences_data_frame  <- function(text.var, 
    profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...){
    
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], 
        "get_sentences", "get_sentences_character")

    extract_profanity_terms(x, profanity_list = profanity_list, ...)

}


#' @export
#' @method extract_profanity_terms character
extract_profanity_terms.character  <- function(text.var, 
    profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...){

    split_warn(text.var, 'extract_profanity', ...)

    sents <- get_sentences(text.var)      
    extract_profanity_terms(sents, profanity_list = profanity_list, ...)

    
}

#' Prints an extract_profanity_terms Object
#' 
#' Prints an extract_profanity_terms object
#' 
#' @param x An extract_profanity_terms object.
#' @param \ldots ignored
#' @method print extract_profanity_terms
#' @export 
print.extract_profanity_terms <- function(x, ...){

    print(rm_class(x, 'extract_profanity_terms')[, !c('neutral', "sentence"), with=FALSE]  )
    
}


