#' Compute Profanity Rate
#' 
#' Detect the rate of profanity at the sentence level.  This method uses a simple
#' dictionary lookup to find profane words and then compute the rate per sentence.
#' The \code{profanity} score ranges between 0 (no profanity used) and 1 (all
#' words used were profane).  Note that a single profane phrase would count as 
#' just one in the \code{profanity_count} column but would count as two words in
#' the \code{word_count} column.
#' 
#' @param text.var The text variable.  Can be a \code{get_sentences} object or
#' a raw character vector though \code{get_sentences} is preferred as it avoids
#' the repeated cost of doing sentence boundary disambiguation every time
#' \code{sentiment} is run.
#' @param profanity_list A atomic character vector of profane words.  The 
#' \pkg{lexicon} package has lists that can be used, including: 
#' \itemize{
#'   \item \code{unique(tolower(lexicon::profanity_alvarez))}
#'   \item \code{lexicon::profanity_arr_bad}
#'   \item \code{lexicon::profanity_banned}
#'   \item \code{lexicon::profanity_zac_anger}
#'   \item \code{lexicon::profanity_racist}
#' }
#' @param \ldots ignored.
#' @return Returns a \pkg{data.table} of:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{profanity}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
#'   \item  word_count - Word count
#'   \item  profanity_count - Count of the number of profane words
#'   \item profanity - A score of the percentage of profane words
#' }
#' @family profanity functions
#' @export
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' bw <- sample(unique(tolower(lexicon::profanity_alvarez)), 4)
#' mytext <- c(
#'    sprintf('do you like this %s?  It is %s. But I hate really bad dogs', bw[1], bw[2]),
#'    'I am the best friend.',
#'    NA,
#'    sprintf('I %s hate this %s', bw[3], bw[4]),
#'    "Do you really like it?  I'm not happy"
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `profanity` is run
#' profanity(mytext)
#' 
#' ## preferred method avoiding paying the cost 
#' mytext2 <- get_sentences(mytext)
#' profanity(mytext2)
#' 
#' plot(profanity(mytext2))
#' 
#' brady <- get_sentences(crowdflower_deflategate)
#' brady_swears <- profanity(brady)
#' brady_swears
#' 
#' ## Distribution of profanity proportion for all comments
#' hist(brady_swears$profanity)
#' sum(brady_swears$profanity > 0)
#' 
#' ## Distribution of proportions for those profane comments
#' hist(brady_swears$profanity[brady_swears$profanity > 0])
#' 
#' combo <- combine_data()
#' combo_sentences <- get_sentences(crowdflower_deflategate)
#' racist <- profanity(combo_sentences, profanity_list = lexicon::profanity_racist)
#' combo_sentences[racist$profanity > 0, ]$text
#' extract_profanity_terms(
#'     combo_sentences[racist$profanity > 0, ]$text, 
#'     profanity_list = lexicon::profanity_racist
#' )
#' 
#' ## Remove jerry, que, and illegal from the list
#' library(textclean)
#' 
#' racist2 <- profanity(
#'     combo_sentences, 
#'     profanity_list = textclean::drop_element_fixed(
#'         lexicon::profanity_racist, 
#'         c('jerry', 'illegal', 'que')
#'     )
#' )
#' combo_sentences[racist2$profanity > 0, ]$text
#' }
profanity <- function(text.var, profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...) {
    
    UseMethod('profanity')
    
}


#' @export
#' @method profanity get_sentences_character
profanity.get_sentences_character <- function(text.var, profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...) {


    ## Ensure profanity_list conforms to standards
    profanity_list <- fix_profanity_list(profanity_list)
    
    token <- hit <- profanity_count <- word_count <- NULL
    
    lens <- lengths(text.var)

    ## make table of elements, sentence id, and sentences
    element_map <- data.table::data.table(
        element_id = rep(seq_along(lens), lens),
        sentence_id = unlist(lapply(lens, seq_len)),
        token = unlist(text.var)
    )

    ## Chack for spaces in the profanity list to ensure the tokenizer keeps them
    profanes <- data.table::data.table(token = profanity_list, hit = TRUE)
    space_words <-  profanity_list[grep("\\s", profanity_list)]

    ## count words, tokenize
    tidied <- element_map[, list(token = tolower(unlist(token))), by = c('element_id', 'sentence_id')][,
        word_count := count_words(token)][,
        token := space_fill(token, space_words)][,
        token := stringi::stri_replace_all_regex(
            stringi::stri_replace_all_regex(token, '[!.;:?]$', ''), '[;:,]\\s+', ' ')][,
        token := stringi::stri_split_regex(token, '\\s+')][,
        list(token = stringi::stri_replace_all_regex(unlist(token), '~~', ' ')), 
            by =c('element_id', 'sentence_id', 'word_count')][]

    ## merge to find the profane words and count them (profanity is % of words that are profane)
    out <- merge(tidied, profanes, all.x=TRUE)[, 
        hit := !is.na(hit)][,
        list(profanity_count = sum(hit)), 
            by = c('element_id', 'sentence_id', 'word_count')][,
        profanity := profanity_count/word_count][,
        profanity := ifelse(is.na(profanity), 0, profanity)][]

    data.table::setorderv(out, c("element_id", 'sentence_id'))

    class(out) <- unique(c("profanity", class(out)))
    sentences <- new.env(FALSE)

    sentences[["sentences"]] <- text.var
    attributes(out)[["sentences"]] <- sentences
    out

}


#' @export
#' @method profanity character
profanity.character <- function(text.var, profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...) {

    split_warn(text.var, 'profanity', ...)
    
    sents <- get_sentences(text.var)
    profanity(text.var = sents, profanity_list = profanity_list, ...)
  
}


#' @export
#' @method profanity get_sentences_data_frame
profanity.get_sentences_data_frame <- function(text.var, profanity_list = unique(tolower(lexicon::profanity_alvarez)), ...) {
 
    x <- make_class(text.var[[attributes(text.var)[['text.var']]]], "get_sentences", "get_sentences_character")

    sent_out <- profanity(text.var = x, profanity_list = profanity_list, ...)
    
    out <- cbind(text.var, sent_out[, c('word_count',  'profanity_count', 'profanity')])

    class(out) <- unique(c("profanity", class(out)))
    sentences <- new.env(FALSE)
    sentences[["sentences"]] <- x
    attributes(out)[["sentences"]] <- sentences
    out[]   

}




#' Plots a profanity object
#'
#' Plots a profanity object.
#'
#' @param x The profanity object.
#' @param transformation.function A transformation function to smooth the profanity
#' scores.
#' @param \ldots Other arguments passed to \code{\link[syuzhet]{get_transformed_values}}.
#' @details Utilizes Matthew Jocker's \pkg{syuzhet} package to calculate smoothed
#' profanity across the duration of the text.
#' @return Returns a \pkg{ggplot2} object.
#' @method plot profanity
#' @importFrom syuzhet get_dct_transform
#' @export
plot.profanity <- function(x, transformation.function = syuzhet::get_dct_transform, ...){


    x <- stats::na.omit(x[["profanity"]])
    
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
        ggplot2::ylab("Profanity Propensity") +
        ggplot2::theme(panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(label=function(x) paste0(x, "%"),
            expand = c(0,0), limits = c(0,100))

}
