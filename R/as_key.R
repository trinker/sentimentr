as_key <- function(x, comparison = sentimentr::valence_shifters_table, sentiment = TRUE, ...){

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



