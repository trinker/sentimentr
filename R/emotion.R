#' Compute Emotion Rate
#' 
#' Detect the rate of emotion at the sentence level.  This method uses a simple
#' dictionary lookup to find emotion words and then compute the rate per sentence.
#' The \code{emotion} score ranges between 0 (no emotion used) and 1 (all
#' words used were emotional).  Note that a single emotion phrase would count as 
#' just one in the \code{emotion_count} column but would count as two words in
#' the \code{word_count} column.
#' 
#' @param text.var The text variable.  Can be a \code{get_sentences} object or
#' a raw character vector though \code{get_sentences} is preferred as it avoids
#' the repeated cost of doing sentence boundary disambiguation every time
#' \code{sentiment} is run.
#' @param emotion_dt A \pkg{data.table} with a \code{token} and \code{emotion}
#' column (\code{tokens} are nested within the \code{emotion}s.  The table
#' cannot contain any duplicate rows and must have the \code{token} column set
#' as the key column (see \code{?data.table::setkey}).  The default emotion
#' table is \code{lexicon::hash_nrc_emotions}.
#' @param valence_shifters_dt A \pkg{data.table} of valence shifters that
#' can alter a polarized word's meaning and an integer key for negators (1),
#' amplifiers [intensifiers] (2), de-amplifiers [downtoners] (3) and adversative 
#' conjunctions (4) with x and y as column names.  For this purpose only 
#' negators is required/used.
#' @param drop.unused.emotions logical.  If \code{TRUE} unused/unfound emotion
#' levels will not be included in the output.
#' @param un.as.negation logical.  If \code{TRUE} then emotion words prefixed
#' with an 'un-' are treated as a negation.  For example,\code{"unhappy"} would 
#' be treated as \code{"not happy"}.  If an emotion word has an un- version in the
#' \code{emotion_dt} then no substitution is performed and an optional warning
#' will be given. 
#' @param un.as.negation.warn logical.  If \code{TRUE} and if 
#' \code{un.as.negation} id \code{TRUE}, then a warning will be given if the 
#' -un version of an emotion term is already found within the \code{emotion_dt}.
#' Note that the default \code{emotion_dt}, \code{lexicon::hash_nrc_emotions}, 
#' will not give a warning unless it is explicitly set to do so.  There are
#' a number of emotion words in \code{lexicon::hash_nrc_emotions} that contain
#' un- prefixed versions already in the dictionary. Use:
#' \code{emotion('', un.as.negation.warn = TRUE)} to see these un- prefixed
#' emotion words that are contained within \code{lexicon::hash_nrc_emotions}.
#' @param n.before The number of words to consider as negated before
#' the emotion word.  To consider the entire beginning portion of a sentence
#' use \code{n.before = Inf}.  Note that a comma, colon, or semicolon acts as a 
#' boundary for considered words.  Only words between the emotion word and these
#' punctuation types will be considered.
#' @param n.after The number of words to consider as negated after
#' the emotion word.  To consider the entire ending portion of a sentence
#' use \code{n.after = Inf}.  Note that a comma, colon, or semicolon acts as a 
#' boundary for considered words.  Only words between the emotion word and these
#' punctuation types will be considered.
#' @param retention_regex A regex of what characters to keep.  All other 
#' characters will be removed.  Note that when this is used all text is lower 
#' case format.  Only adjust this parameter if you really understand how it is 
#' used.  Note that swapping the \code{\\\\{p}} for \code{[^[:alpha:];:,\']} may 
#' retain more alpha letters but will likely decrease speed.
#' @param \ldots ignored.
#' @return Returns a \pkg{data.table} of:
#' \itemize{
#'   \item element_id - The id number of the original vector passed to \code{emotion}
#'   \item sentence_id - The id number of the sentences within each \code{element_id}
#'   \item word_count - Word count
#'   \item emotion_type - Type designation from the \code{emotion} column of the \code{emotion_dt} table
#'   \item emotion_count - Count of the number of emotion words of that \code{emotion_type}
#'   \item emotion - A score of the percentage of emotion words of that \code{emotion_type}
#' }
#' @family emotion functions
#' @export
#' @references Plutchik, R. (1962). The emotions: Facts and theories, and a new 
#' model. Random House studies in psychology. Random House.\cr\cr
#' Plutchik, R. (2001). The nature of emotions: Human emotions have deep 
#' evolutionary roots, a fact that may explain their complexity and provide tools 
#' for clinical practice. American Scientist , 89 (4), 344-350. 
#' @importFrom data.table :=
#' @examples
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
#'       "pole, I thank whatever gods may be For my unconquerable soul."
#'      ),
#'     paste("In the fell clutch of circumstance I have not winced nor cried",
#'         "aloud. Under the bludgeonings of chance My head is bloody, but unbowed."
#'     ),
#'     paste("Beyond this place of wrath and tears Looms but the Horror of the", 
#'         "shade, And yet the menace of the years Finds, and shall find, me unafraid."
#'     ),
#'     paste("It matters not how strait the gate, How charged with punishments", 
#'         "the scroll, I am the master of my fate: I am the captain of my soul."
#'     )    
#'     
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `emotion` is run
#' emotion(mytext)
#' 
#' ## preferred method avoiding paying the cost 
#' split_text <- get_sentences(mytext)
#' (emo <- emotion(split_text))
#' emotion(split_text, drop.unused.emotions = TRUE)
#' 
#' \dontrun{
#' plot(emo)
#' plot(emo, drop.unused.emotions = FALSE)
#' plot(emo, facet = FALSE)
#' plot(emo, facet = 'negated')
#' 
#' library(data.table)
#' fear <- emo[
#'     emotion_type == 'fear', ][, 
#'     text := unlist(split_text)][]
#'     
#' fear[emotion > 0,]
#' 
#' brady <- get_sentences(crowdflower_deflategate)
#' brady_emotion <- emotion(brady)
#' brady_emotion
#' }
emotion <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    valence_shifters_dt = lexicon::hash_valence_shifters, 
    drop.unused.emotions = FALSE, un.as.negation = TRUE,
    un.as.negation.warn = isTRUE(all.equal(valence_shifters_dt, lexicon::hash_nrc_emotions)), 
    n.before = 5, n.after = 2, retention_regex = "[^[:alpha:];:,\']", ...) {
    
    UseMethod('emotion')
    
}



#' @export
#' @method emotion get_sentences_character
emotion.get_sentences_character <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    valence_shifters_dt = lexicon::hash_valence_shifters, 
    drop.unused.emotions = FALSE, un.as.negation = TRUE,
    un.as.negation.warn = isTRUE(all.equal(valence_shifters_dt, lexicon::hash_nrc_emotions)), 
    n.before = 5, n.after = 2, retention_regex = "[^[:alpha:];:,\']", ...) {
    
    emotion <- emo_loc <- comma_loc <- negator_loc <- is_emo <- y <- NULL
    is_negator <- is_negated <- emotion_count <- token <- hit <- word_count <- NULL
    
    ## Ensure emotion_dt conforms to standards
    is_emotion(emotion_dt)

    lens <- lengths(text.var)
# browser()
    ## make table of elements, sentence id, and sentences
    element_map <- data.table::data.table(
        element_id = rep(seq_along(lens), lens),
        sentence_id = unlist(lapply(lens, seq_len)),
        token = unlist(text.var)
    )[, list(token = trimws(tolower(unlist(token)))), by = c('element_id', 'sentence_id')][,
        word_count := count_words(token)]

    ## Chack for spaces in the emotion list to ensure the tokenizer keeps them
    space_words <-  emotion_dt[['token']][grep("\\s", emotion_dt[['token']])]

    ## Convert un prefix to not
    if (isTRUE(un.as.negation)){
        

        tokens <- unique(emotion_dt[['token']])
        uns <- paste0('un', tokens) 
        keeps <- tokens[tokens %in% uns]
       
        if (length(keeps) > 0 && un.as.negation.warn) {
            warning(
                paste0(
                    'The following tokens were aleady found in the `emotion_dt` and will not be converted via `un.as.negation`:\n\n',
                    paste(paste0('    -', keeps), collapse = '\n')
                ),
                call. = FALSE
            )
        }

        regex <- paste0('\\b(un)(', paste(gsub('^un', '', keeps, perl = TRUE), collapse = '|'), ')\\b')
        element_map <- element_map[, token := stringi::stri_replace_all_regex(token, regex, 'not $2')][]
        
    }

    ## count words, tokenize
    tidied <- element_map[,
        token := space_fill(token, space_words)][,
        token := stringi::stri_replace_all_regex(
                stringi::stri_replace_all_regex(
                    stringi::stri_replace_all_regex(token, '[!.;:?]$', ''), 
                    retention_regex, 
                    ' '
                ), 
                '[;:,]\\s+', 
                ' [;:,] '
            )
        ]


    tidied[['token']] <- as.list(stringi::stri_split_regex(tidied[['token']], '\\s+'))

    tidied <- tidied[,
            list(token = stringi::stri_replace_all_regex(unlist(token), '~~', ' ')), 
                by =c('element_id', 'sentence_id', 'word_count')][, 
            emo_loc := seq_len(.N), by=c('element_id', 'sentence_id')][, 
            comma_loc := emo_loc][, 
            negator_loc := emo_loc][]

    data.table::setkey(tidied, 'token')

    ## merge to find the emotion words and count them (emotion is n of words that are emotional)
    out <- merge(tidied, emotion_dt, all.x=TRUE, allow.cartesian=TRUE)[, 
            emo_loc := ifelse(is.na(emotion), NA, emo_loc)][, 
            comma_loc := ifelse(token == '[;:,]', comma_loc, NA)][, 
            list(word_count = word_count, emo_loc = emo_loc, token = token,
                comma_loc = comma_loc,  negator_loc = negator_loc, 
                emotion = emotion,
                is_emo = sum(!is.na(emo_loc)) > 0), 
                by = c('element_id', 'sentence_id')][]

    ##-------------------------------------START--------------------------------
    ## IN THIS PORTION WE IDENTIFY EMOTIONS THAT ARE NEGATED
    
    ## grab only rows
    emo_dat <- out[is_emo == TRUE,]
    #non_emo_dat <- out[is_emo != TRUE,]

    if (nrow(emo_dat) > 0){
        
        valence_shifters_dt <- valence_shifters_dt[y == 1,]
        emo_dat[['is_negator']] <- valence_shifters_dt[emo_dat[['token']]][['y']] %in% '1'

        if (sum(emo_dat[['is_negator']]) > 0) {
            emo_dat <- emo_dat[, negator_loc := ifelse(is_negator, negator_loc, NA)][, 
                list(
                    #emotion = list(rm_na(emotion)),  
                    emo_loc = list(rm_na(unique(emo_loc))),            
                    comma_loc = list(rm_na(unique(comma_loc)))  ,
                    negator_loc = list(rm_na(unique(negator_loc)))   
                ), by = c('element_id', 'sentence_id')][
                    !is.na(negator_loc),
                ][, 
                    list(emo_loc = unlist(emo_loc), comma_loc, negator_loc), by = c('element_id', 'sentence_id')][, 
                    is_negated := negated_emotion(emo_loc, comma_loc, negator_loc, n.before, n.after)][,
                    c('element_id', 'sentence_id',  'emo_loc', 'is_negated')][]
        
        } else {
            emo_dat <- unique(emo_dat[,c('element_id', 'sentence_id', 'emo_loc')])[, is_negated := FALSE][]
        }

        out <- merge(
            out[,c('element_id', 'sentence_id', 'word_count', 'emotion', 'emo_loc')],
            emo_dat, 
            all.x=TRUE, 
            by = c('element_id', 'sentence_id', 'emo_loc'),
            allow.cartesian = TRUE
        )[, 
            emotion := ifelse(is_negated %in% TRUE, paste0(emotion, '_negated'), emotion)][,
                 c('element_id', 'sentence_id', 'word_count', 'emotion')][, 
            hit := !is.na(emotion)][,
            list(emotion_count = sum(hit)), 
                by = c('element_id', 'sentence_id', 'word_count', 'emotion')][]
            

 
    } else {
        out <- out[,c('element_id', 'sentence_id', 'word_count', 'emotion')][,
            emotion_count := 0][]
    }

    ##-------------------------------------END----------------------------------

    ## Spread wide to fill in missing cells with zero; fill in missing columns
    out <- data.table::dcast(
        out,
        element_id + sentence_id + word_count ~ emotion, 
        value.var = 'emotion_count', 
        fill = 0
    )

    possible_cols <- unique(emotion_dt[['emotion']])
    if (isTRUE(un.as.negation)) {
        possible_cols <- c(possible_cols, paste0(possible_cols, '_negated'))
    }
     
    possible_cols <- sort(possible_cols)
    
    missed_columns <- possible_cols[! possible_cols %in% colnames(out)]
# browser()
    if (length(missed_columns) > 0 && !drop.unused.emotions){
        missed <- as.data.frame(lapply(missed_columns, function(x) rep(0L, nrow(out))))
        colnames(missed) <- missed_columns
        out <- data.table::data.table(out, missed)
    }
    
    
    out <- out[, 'NA' := NULL][]

    ## Gather tall and make into a proportion
    out <- data.table::melt(
        out, 
        id.vars = c('element_id', 'sentence_id', 'word_count'), 
        variable.name = 'emotion_type', 
        value.name = 'emotion_count'
    )[, emotion := emotion_count/word_count][,
        emotion := ifelse(is.na(emotion), 0, emotion)][]

    data.table::setorderv(out, c("element_id", 'sentence_id', 'emotion_type'))

    class(out) <- unique(c("emotion", class(out)))
    sentences <- new.env(FALSE)

    sentences[["sentences"]] <- text.var
    attributes(out)[["sentences"]] <- sentences
    attributes(out)[["zero_emotion_type"]] <- missed_columns
    
    out

}


is_emotion <- function(x, name = 'emotion_dt', ...){
    
    ## check that it is data.table 
    if(!data.table::is.data.table(x)){
        stop(
            paste0('`', name, 
                '` is not a data.table object.\n\nIt should also be distinct & have an \'emotion\' and \'token\' column', 
                '\nwith the \'token\' column set as the key (see `?data.table::setkey`)'
            ), call. = FALSE
        )    
    }

    ## check that it has a token and emotion column   
    if(!all(c("token", "emotion") %in% colnames(x))){
        stop(
            paste0('`', name, 
                '` does not have an \'emotion\' and \'token\' column.', 
                '\n\nIt should also be distinct & have the \'token\' column\nset as the key (see `?data.table::setkey`)'
            ), call. = FALSE
        )   
    }  
    
    ## check that it has a key set
    if(is.null(attributes(x)[['sorted']]) | attributes(x)[['sorted']] != 'token'){
        stop(
            paste0('`', name, 
                '` does not have the \'token\' column\nset as the key (see `?data.table::setkey`).',
                '\n\nIt should also be distinct'
            ), call. = FALSE
        )   
    }        
    
    ## ensure distinct rows
    if (anyDuplicated(x)){
        dupes <- which(duplicated(x))
        
        if (length(dupes) > 50){
            dupes <- paste0(paste(dupes[1:50], collapse = ', '), '...[truncated]...')
        } else {
            dupes <- paste(dupes, collapse = ', ')
        }
        
        stop(
            paste0('`', name, 
                '` should be distinct.  The following row(s) are duplicated:\n\n',
                dupes, '\n\nUse `duplicated` to identify duplicate rows.'
            ), call. = FALSE
        )              
    }
}
    
## Function to determine if an emotional location is negated    
negated_emotion <- function(emoloc, commaloc, negatorloc, leftwindow, rightwindow){

    unlist(Map(function(el2, cl2, nl2) {

        ## if no comma location set to infinite
        if (length(cl2) == 1 && is.na(cl2)) cl2 <- Inf
        
        ## get window 
        ## reduce window based on commas [ensure left bound is at least 1)
        ## compute negation --> sum(negations_within_window) %% 2 == 1
        sum(
            max(cl2[cl2 < el2], el2 - leftwindow, 1) <= nl2 &
            nl2 <= min(cl2[cl2 > el2], el2 + rightwindow)
        ) %% 2 == 1


    }, emoloc, commaloc, negatorloc))    
    
}


#' @export
#' @method emotion character
emotion.character <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    valence_shifters_dt = lexicon::hash_valence_shifters, 
    drop.unused.emotions = FALSE, un.as.negation = TRUE,
    un.as.negation.warn = isTRUE(all.equal(valence_shifters_dt, lexicon::hash_nrc_emotions)), 
    n.before = 5, n.after = 2, retention_regex = "[^[:alpha:];:,\']", ...) {

    split_warn(text.var, 'emotion', ...)
    
    sents <- get_sentences(text.var)
    emotion(
        text.var = sents, emotion_dt = emotion_dt, 
        valence_shifters_dt = valence_shifters_dt, 
        drop.unused.emotions = drop.unused.emotions,
        un.as.negation = un.as.negation, 
        un.as.negation.warn = un.as.negation.warn, 
        retention_regex = retention_regex,
        n.before = n.before, 
        n.after = n.after,
        
        ...
    )
  
}


#' @export
#' @method emotion get_sentences_data_frame
emotion.get_sentences_data_frame <- function(text.var, 
    emotion_dt = lexicon::hash_nrc_emotions, 
    valence_shifters_dt = lexicon::hash_valence_shifters, 
    drop.unused.emotions = FALSE, un.as.negation = TRUE,
    un.as.negation.warn = isTRUE(all.equal(valence_shifters_dt, lexicon::hash_nrc_emotions)), 
    n.before = 5, n.after = 2, retention_regex = "[^[:alpha:];:,\']", ...) {
 
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], "get_sentences", "get_sentences_character")

    sent_out <- emotion(
        text.var = x, emotion_dt = emotion_dt, 
        valence_shifters_dt = valence_shifters_dt, 
        drop.unused.emotions = drop.unused.emotions, 
        un.as.negation = un.as.negation, 
        un.as.negation.warn = un.as.negation.warn, 
        n.before = n.before, 
        n.after = n.after,
        retention_regex = retention_regex, 
        ...
    )
    
    out <- cbind(text.var, sent_out[, c('word_count',  'emotion_type', 'emotion_count', 'emotion')])

    class(out) <- unique(c("emotion", class(out)))
    sentences <- new.env(FALSE)
    sentences[["sentences"]] <- x
    attributes(out)[["sentences"]] <- sentences
    out[]   

}




#' Plots a emotion object
#'
#' Plots a emotion object.
#'
#' @param x The emotion object.
#' @param transformation.function A transformation function to smooth the emotion
#' scores.
#' @param drop.unused.emotions  logical.  If \code{TRUE} unused/unfound emotion
#' levels will not be included in the output.
#' @param facet logical or one of \code{c('emotion', 'negated')}.  If \code{TRUE} 
#' or \code{'emotion'} the plot will be facetted by Emotion Type.  If \code{FALSE} 
#' all types will be plotted in the same window.  If \code{"negated"} the emotions
#' will be in the same plot window but broken out by negated or non-negated types.
#' @param \ldots Other arguments passed to \code{\link[syuzhet]{get_transformed_values}}.
#' @details Utilizes Matthew Jocker's \pkg{syuzhet} package to calculate smoothed
#' emotion across the duration of the text.
#' @return Returns a \pkg{ggplot2} object.
#' @method plot emotion
#' @importFrom syuzhet get_dct_transform
#' @export
plot.emotion <- function(x, transformation.function = syuzhet::get_dct_transform, 
    drop.unused.emotions = TRUE, facet = TRUE, ...){

    
    Negated <- Emotion <- emotion_type <- NULL
    
    atts <- attributes(x)
    
    if (isTRUE(drop.unused.emotions)){
        x <- x[!emotion_type %in% atts[['zero_emotion_type']],]        
    }

    
    x <- droplevels(x)
    
    m <- lapply(split(x[['emotion']], x[['emotion_type']]), function(y){

        y <- rm_na(y)
        
        if (length(y) < 3) stop('Output contains less than 3 observations.  Cannot plot n < 3', call. = FALSE)
        
        if (length(y) < 100 && isTRUE(all.equal(syuzhet::get_dct_transform, transformation.function))) {
            y <- stats::approx(x = seq_along(y), y = y, n = 100)[['y']]
        }
        
        trans <- transformation.function(y, ...)
        
        data.frame(
            Emotional_Valence = trans,
            Duration = seq_along(trans)
        )
    })
    
    dat <- textshape::tidy_list(m, 'Emotion')

    if (facet == 'negated'){
        dat <- data.table::as.data.table(dat)[, 
            Negated := ifelse(grepl('_negated', Emotion, fixed = TRUE), 'Negated Emotion', 'Emotion')][, 
            Emotion := gsub('_negated', '', Emotion, fixed = TRUE)][]
    }

    out <- ggplot2::ggplot(dat, ggplot2::aes_string('Duration', 'Emotional_Valence')) +
        ggplot2::geom_path(size=1, ggplot2::aes_string(color= 'Emotion')) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.margin = grid::unit(c(5.1, 15.1, 4.1, 2.1), "pt")) +
        ggplot2::ylab("Emotional Propensity") +
        ggplot2::theme(panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(label=function(x) paste0(x, "%"),
            expand = c(0,0), limits = c(0,100)) 
    
    if (isTRUE(facet) | facet == 'emotion'){
        out <- out +
            ggplot2::facet_wrap(.~Emotion) +
            ggplot2::theme(legend.position = 'none')
    }

    if (facet == 'negated'){
        out <- out +
            ggplot2::facet_wrap(.~Negated, ncol = 1) 
    }
    
    out

}
