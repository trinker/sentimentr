#get_sents <- function(x) {
#    x <- stringi::stri_replace_all_regex(stringi::stri_trans_tolower(x), sent_regex, "")
#    stringi::stri_split_regex(x, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)\\s")
#}

get_sents <- function(x) {
    x <- stringi::stri_replace_all_regex(stringi::stri_trans_tolower(x), sent_regex, "")
    stringi::stri_split_regex(x, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
}

add_row_id <- function(x){
    lens <- lapply(x, length)
    rep(seq_along(lens), unlist(lens))
}

count_words <- function(x){
    stringi::stri_count_words(x)
}

make_words <- function(x){
    lapply(stringi::stri_split_regex(gsub("^\\s+|\\s+$", "", x), "[[:space:]]|(?=[,;:])"), function(y) gsub('~{2,}', ' ', y))
}

count_row_length <- function(x){
    x <- stringi::stri_count_words(x)
    x[is.na(x) | x == 0] <- 1
    x
}

#' @importFrom data.table :=
make_sentence_df <- function(x){

    indx <- wc <- NULL

    sents <- get_sents(x)
    ids <- add_row_id(sents)
    text.var <- gsub("[^a-z,;: ]|\\d:\\d|\\d ", "", unlist(sents))
    dat <- data.frame(
        id = ids,
        sentences = text.var,
    	wc = count_words(text.var),
        stringsAsFactors = FALSE
    )
    data.table::setDT(dat)
    dat[, indx:= wc < 1, by=c('id', 'sentences', 'wc')][(indx), c('sentences', 'wc'):=NA, with=FALSE][, indx:=NULL]
}



#' @importFrom data.table :=
make_sentence_df2 <- function(sents){

    indx <- wc <- NULL

    ids <- add_row_id(sents)
    text.var <- gsub("[^a-z,;: ]|\\d:\\d|\\d ", "", unlist(sents))
    dat <- data.frame(
        id = ids,
        sentences = text.var,
    	wc = count_words(text.var),
        stringsAsFactors = FALSE
    )
    data.table::setDT(dat)
    dat[, indx:= wc < 1, by=c('id', 'sentences', 'wc')][(indx), c('sentences', 'wc'):=NA, with=FALSE][, indx:=NULL]
}

.mgsub <- function (pattern, replacement, text.var, fixed = TRUE,
	order.pattern = fixed, ...) {

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, ...)
    }

    text.var
}

space_fill <- function(x, doubles){

  .mgsub(doubles, gsub("\\s+", "~~", doubles), x)

}


comma_reducer <- function(wrds, cl, pl, len, nb, na){
	Map(function(wrds2, cl2, pl2, len2){

        if (is.na(pl2) | is.na(cl2)) return(wrds2[-pl2])

        # Find the upper and lower bound using words n.before and n. after
        lb <- pl2 - nb
        lb[lb < 1] <- 1
        ub <- pl2 + na
        ub[ub > len2] <- len2

        # take into account upper and lower looking for [,:;]
		lower <- ifelse(any(cl2 < pl2), max(cl2[cl2 < pl2]) + 1, lb)
	    upper <- ifelse(any(cl2 > pl2), min(cl2[cl2 > pl2]) - 1, ub)

	    ## grab these words in the upper and lower w/o the polarized word
        ind <- lower:upper

	    ind <- ind[!ind %in% pl2]
	    if(identical(integer(0), ind)) return(wrds2[-pl2])
        wrds2[ind]
	}, wrds, cl, pl, len)
}


rm_na <- function(x) {
	log2NA(x[!is.na(x)])
}

log2NA <- function(x) {
	x[identical(logical(0), x)] <- NA
	x
}

sum2 <- function(x) sum(x, na.rm = TRUE)

