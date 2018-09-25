#' Create/Manipulate Hash Keys
#'
#' \code{as_key} - Create your own hash keys from a data frame for use in key
#' arguments such as \code{polarity_dt} in the \code{sentiment} function.
#'
#' @param x A \code{\link[base]{data.frame}} with the first column containing
#' polarized words and the second containing polarity values.
#' @param comparison A \code{\link[base]{data.frame}} to compare to \code{x}.
#' If elements in \code{x}'s column 1 matches \code{comparison}'s column 1 the
#' accompanying row will be removed from \code{x}.  This is useful to ensure
#' \code{polarity_dt} words are not also found in \code{valence_shifters_dt} in
#' \code{\link[sentimentr]{sentiment}}.  Use \code{comparison = NULL} to skip
#' this comparison.
#' @param sentiment logical.  If \code{TRUE} checking expects column 2 of the
#' input keys/\code{\link[base]{data.frame}} are expected to be numeric.
#' @param \ldots ignored.
#' @return Returns a \pkg{data.table} object that can be used as a hash key.
#' @keywords key hash lookup
#' @details For updating keys via \code{update_key} note that a 
#' \code{polarity_dt} and \code{valence_shifters_dt} are the primary dictionary 
#' keys used in the \pkg{sentimentr} package.  The \code{polarity_dt} takes a 
#' 2 column \code{data.frame} (named x and y) with the first column being 
#' character and containing the words and the second column being numeric values
#' that are positive or negative.  \code{valence_shifters_dt} takes a 2 column 
#' \code{data.frame} (named x and y) with the first column being character and 
#' containing the words and the second column being integer corresponding to:
#' (1) negators, (2) amplifiers, (3) de-amplifiers, and (4) dversative 
#' conjunctions (i.e., 'but', 'however', and 'although').  Also, note that if 
#' you are updating a \code{valence_shifters_dt} you need an appropriate 
#' \code{comparison}; most likely, \code{comparison = sentimentr::polarity_dt}.  
#' @export
#' @rdname as_key
#' @examples
#' key <- data.frame(
#'     words = sample(letters),
#'     polarity = rnorm(26),
#'     stringsAsFactors = FALSE
#' )
#'
#' (mykey <- as_key(key))
#'
#' ## Looking up values
#' mykey[c("a", "k")][[2]]
#'

#' ## Drop terms from key
#' update_key(mykey, drop = c("f", "h"))
#'
#' ## Add terms to key
#' update_key(mykey, x = data.frame(x = c("dog", "cat"), y = c(1, -1)))
#'
#' ## Add terms & drop to/from a key
#' update_key(mykey, drop = c("f", "h"), x = data.frame(x = c("dog", "cat"), y = c(1, -1)))
#' 
#' ## Explicity key type (wrapper for `update_key` for sentiment table.
#' ## See `update_valence_shifter_table` a corresponding valence shifter updater.
#' library(lexicon)
#' updated_hash_sentiment <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_huliu,
#'     x = data.frame(
#'         words = c('frickin', 'hairy'),
#'         polarity = c(-1, -1),
#'         stringsAsFactors = FALSE
#'     )
#' )
#' 
#' ## Checking if you have a key
#' is_key(mykey)
#' is_key(key)
#' is_key(mtcars)
#' is_key(update_key(mykey, drop = c("f", "h")))
#'
#' ## Using syuzhet's sentiment lexicons
#' \dontrun{
#' library(syuzhet)
#' (bing_key <- as_key(syuzhet:::bing))
#' as_key(syuzhet:::afinn)
#' as_key(syuzhet:::syuzhet_dict)
#' 
#' sam <- gsub("Sam-I-am", "Sam I am", sam_i_am)
#' sentiment(sam, , polarity_dt = bing_key)
#' 
#' ## The nrc dictionary in syuzhet requires a bit of data wrangling before it 
#' ## is in the correct shape to convert to a key.  
#' 
#' library(syuzhet)
#' library(tidyverse)
#' 
#' nrc_key <- syuzhet:::nrc %>% 
#'     dplyr::filter(
#'         sentiment %in% c('positive', 'negative'),
#'         lang == 'english'
#'     ) %>%
#'     dplyr::select(-lang) %>% 
#'     mutate(value = ifelse(sentiment == 'negative', value * -1, value)) %>%
#'     dplyr::group_by(word) %>%
#'     dplyr::summarize(y = mean(value)) %>%
#'     sentimentr::as_key()
#'     
#' sentiment(sam, polarity_dt = nrc_key)
#' 
#' ## The lexicon package contains a preformatted nrc sentiment hash table that 
#' ## can be used instead.
#' sentiment(sam, polarity_dt = lexicon::hash_sentiment_nrc)
#' }
#'
#' ## Using 2 vectors of words
#' \dontrun{
#' install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
#' require("tm.lexicon.GeneralInquirer")
#'
#' positive <- terms_in_General_Inquirer_categories("Positiv")
#' negative <- terms_in_General_Inquirer_categories("Negativ")
#'
#' geninq <- data.frame(
#'     x = c(positive, negative),
#'     y = c(rep(1, length(positive)), rep(-1, length(negative))),
#'     stringsAsFactors = FALSE
#' ) %>%
#'     as_key()
#'
#' geninq_pol <- with(presidential_debates_2012,
#'     sentiment_by(dialogue,
#'     person,
#'     polarity_dt = geninq
#' ))
#'
#' geninq_pol %>% plot()
#' }
as_key <- function(x, comparison = lexicon::hash_valence_shifters, sentiment = TRUE, ...){

    stopifnot(is.data.frame(x))

    culprits <- NULL
    if (length(x[[1]]) != length(unique(x[[1]]))) {
        tab <- table(x[[1]])
        culprits <- paste(paste0("   * ", sort(names(tab[tab > 1]))), collapse = "\n")
        warning("One or more terms in the first column are repeated. Terms must be unique.\n  ",
            "I found the following likely culprits:\n\n", culprits, "\n\nThese terms have been dropped\n")
    }

    if (any(grepl("[A-Z]", x[[1]]))) {
        culprits2 <- grep("[A-Z]", x[[1]], value=TRUE)
        culprits2 <- paste(paste0("   * ", culprits2), collapse = "\n")
        warning("One or more terms in the first column contain capital letters. Capitals are ignored.\n  ",
            "I found the following suspects:\n\n", culprits2, "\n\nThese terms have been lower cased.\n")
        x[[1]] <- tolower(x[[1]])
    }

    
    if (is.factor(x[[1]])) {
        warning("Column 1 was a factor...\nConverting to character.")
        x[[1]] <- as.character(x[[1]])
    }

    if (!is.character(x[[1]])) stop("Column 1 must be character")
    if (isTRUE(sentiment) && !is.numeric(x[[2]])) stop("Column 2 must be numeric")

    colnames(x) <- c("x", "y")

    if (!is.null(comparison)) {
        if (any(x[[1]] %in% comparison[[1]])) {
            culprits3 <- paste(paste0("   * ",  x[[1]][x[[1]] %in% comparison[[1]]]), collapse = "\n")
            warning("One or more terms in the first column appear as terms in the comparison.\n  ",
            "I found the following dubious fellas:\n\n", culprits3, "\n\nThese terms have been removed.\n")
                    
        }           
        x <- x[!x[["x"]] %in% comparison[["x"]], ]
    }
    data.table::setDT(x)
    x <- x[order(x),]

    if (!is.null(culprits)) x <- x[!x %in% sort(names(tab[tab > 1])), ]
    data.table::setkey(x, "x")
    x
}

#' Convert data.frame to a Hash Key
#'
#' \code{update_key} - Add/remove terms to a current key.
#'
#' @param key A \pkg{sentimentr} hash key.
#' @param drop A vector of terms to drop.
#' @export
#' @rdname as_key
update_key <- function(key, drop = NULL, x = NULL,
    comparison = lexicon::hash_valence_shifters, sentiment = FALSE, ...){

    stopifnot(is_key(key, sentiment = sentiment))

    if (!is.null(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
    
    if (any(grepl("[A-Z]", x[[1]]))) {
        culprits2 <- grep("[A-Z]", x[[1]], value=TRUE)
        culprits2 <- paste(paste0("   * ", culprits2), collapse = "\n")
        warning("One or more terms in the first column contain capital letters. Capitals are ignored.\n  ",
            "I found the following suspects:\n\n", culprits2, "\n\nThese terms have been lower cased.\n")
        x[[1]] <- tolower(x[[1]])
    }

    key1 <- data.table::copy(key)

    if (!is.null(drop)){
        key1 <- key1[!x %in% drop, ]
    }

    if (!is.null(x)){
        key2 <- as_key(x, comparison = comparison, sentiment = sentiment)
        key1 <- rbind(key1, key2)
    }

    if (!is.null(comparison)) {
        key1 <- key1[!key1[["x"]] %in% comparison[["x"]], ]
    }

    key1 <- key1[!duplicated(x)]
    #key1 <- key1[order(key1),]
    data.table::setkey(key1, "x")
    key1
}

#' Convert data.frame to a Hash Key
#'
#' \code{update_polarity_table} - Wrapper for \code{update_key} specifically for
#' updating polarity tables.
#' 
#' @export
#' @rdname as_key
update_polarity_table <- update_key


#' Convert data.frame to a Hash Key
#'
#' \code{update_valence_shifter_table} - Wrapper for \code{update_key} 
#' specifically for updating valence shifter tables.
#' 
#' @export
#' @rdname as_key
update_valence_shifter_table <- function(key, drop = NULL, x = NULL,
    comparison = lexicon::hash_sentiment_jockers_rinker, sentiment = FALSE, ...){

    update_key(key = key, drop = drop, x = x, comparison = comparison, sentiment = sentiment, ...)
}



#' Convert data.frame to a Hash Key
#'
#' \code{is_key} - Logical check if an object is a key.
#'
#' @export
#' @rdname as_key
is_key <- function(key, sentiment = TRUE){
    if (isTRUE(sentiment)) {
        col2 <- is.numeric(key[["y"]])
    } else{
        col2 <- is.numeric(key[["y"]]) | is.character(key[["y"]])
    }
    data.table::is.data.table(key) && all.equal(colnames(key), c("x", "y")) &&
        is.character(key[["x"]]) && (col2)
}



