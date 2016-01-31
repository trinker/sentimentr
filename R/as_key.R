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
#' @export
#' @rdname as_key
#' @examples
#' key <- data.frame(
#'     words = sample(LETTERS),
#'     polarity = rnorm(26),
#'     stringsAsFactors = FALSE
#' )
#'
#' (mykey <- as_key(key))
#'
#' ## Looking up values
#' mykey[c("A", "K")][[2]]
#'

#' ## Drop terms from key
#' update_key(mykey, drop = c("F", "H"))
#'
#' ## Add terms to key
#' update_key(mykey, x = data.frame(x = c("Dog", "Cat"), y = c(1, -1)))
#'
#' ## Add terms & drop to/from a key
#' update_key(mykey, drop = c("F", "H"), x = data.frame(x = c("Dog", "Cat"), y = c(1, -1)))
#'
#' ## Checking if you have a key
#' is_key(mykey)
#' is_key(key)
#' is_key(mtcars)
#' is_key(update_key(mykey, drop = c("F", "H")))
#'
#' ## Using syuzhet's sentiment lexicons
#' \dontrun{
#' library(syuzhet)
#' as_key(syuzhet:::bing)
#' as_key(syuzhet:::afinn)
#' nrc <- data.frame(
#'     words = rownames(syuzhet:::nrc),
#'     polarity = syuzhet:::nrc[, "positive"] - syuzhet:::nrc[, "negative"],
#'     stringsAsFactors = FALSE
#' )
#'
#' as_key(nrc[nrc[["polarity"]] != 0, ])
#'
#' sentiment(gsub("Sam-I-am", "Sam I am", sam_i_am), as_key(syuzhet:::bing))
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
as_key <- function(x, comparison = sentimentr::valence_shifters_table, sentiment = TRUE, ...){

    stopifnot(is.data.frame(x))

    culprits <- NULL
    if (length(x[[1]]) != length(unique(x[[1]]))) {
        tab <- table(x[[1]])
        culprits <- paste(paste0("   * ", sort(names(tab[tab > 1]))), collapse = "\n")
        warning("One or more terms in the first column are repeated. Terms must be unique.\n  ",
            "I found the following likely culprits:\n\n", culprits, "\n\nThese terms have been dropped\n")
    }

    if (is.factor(x[[1]])) {
        warning("Column 1 was a factor...\nConverting to character.")
        x[[1]] <- as.character(x[[1]])
    }

    if (!is.character(x[[1]])) stop("Column 1 must be character")
    if (isTRUE(sentiment) && !is.numeric(x[[2]])) stop("Column 2 must be numeric")

    colnames(x) <- c("x", "y")

    if (!is.null(comparison)) {
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
    comparison = sentimentr::valence_shifters_table, sentiment = FALSE, ...){

    stopifnot(is_key(key, sentiment = sentiment))

    key1 <- data.table::copy(key)

    if (!is.null(drop)){
        key1 <- key[!x %in% drop, ]
    }

    if (!is.null(x)){
        key2 <- as_key(x, sentiment = sentiment)
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



